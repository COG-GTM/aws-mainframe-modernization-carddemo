#!/usr/bin/env python3
"""CardDemo program-level coverage proof. Input: graph.json from callgraph.py. Read-only over app/**."""
import json, re, os, sys, collections
ROOT = sys.argv[1] if len(sys.argv) > 1 else '.'
OUTDIR = os.path.abspath(sys.argv[2]) if len(sys.argv) > 2 else os.path.join(os.path.abspath(ROOT), 'functional/CARDDEMO/inventory')
G = json.load(open(os.path.join(OUTDIR, 'graph.json')))
os.chdir(ROOT)
progs = G['programs']
def cite(path, pat):
    for i, l in enumerate(open(path, encoding='latin1'), 1):
        if re.search(pat, l): return f'{path}:{i}'
    return f'{path}:?'
MENU = {'COSGN00C', 'COMEN01C', 'COADM01C'}
RUNTIME = {'CEE3ABD', 'CEEDAYS', 'MQOPEN', 'MQGET', 'MQPUT', 'MQPUT1', 'MQCLOSE', 'CBLTDLI', 'DSNTIAC'}
fwd = collections.defaultdict(set)   # caller -> {(callee, cite)}
for caller, callee, kind, c, res in G['edges']:
    if caller.startswith(('JOB:', 'TRAN:')): continue
    if kind in ('CALL', 'CALL-VIA-COPYBOOK', 'CICS-XCTL', 'CICS-LINK') and callee not in RUNTIME:
        fwd[caller].add((callee, c))
    elif kind in ('CICS-XCTL-DYNAMIC', 'CICS-LINK-DYNAMIC') and res != 'UNRESOLVED':
        for lit in re.findall(r'([A-Z0-9]{8})@', res):
            if lit in progs: fwd[caller].add((lit, c + ' (resolved ' + res.split(',')[0] + ')'))
# variable-held literals resolved by hand (VALUE + MOVE + XCTL cites)
MANUAL = [
 ('COPAUS0C', 'COPAUS1C', 'app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:323 (WS-PGM-AUTH-DTL VALUE :34, MOVE :316)'),
]
for a, b, c in MANUAL: fwd[a].add((b, c))
# Hand resolution of every dynamic dispatch the script leaves UNRESOLVED (CardDemo_inventory.md 2.1), keyed by call site.
# FORWARD: program edge inside the stream; DISPATCH: hub option -> stream root; RETURN: back to the hub or to
# CDEMO-FROM-PROGRAM (B5); SELF: pseudo-conversational RETURN TRANSID to the program's own trancode.
def table(cpy): return re.findall(r"'(CO[A-Z0-9]{5,6})'", open(cpy, encoding='latin1').read())
A, T = 'app/app-authorization-ims-db2-mq/cbl/', 'app/app-transaction-type-db2/cbl/'
SITE = {
 'app/cbl/COMEN01C.cbl:156': ('DISPATCH', table('app/cpy/COMEN02Y.cpy'), 'CDEMO-MENU-OPT-PGMNAME table app/cpy/COMEN02Y.cpy'),
 'app/cbl/COMEN01C.cbl:185': ('DISPATCH', table('app/cpy/COMEN02Y.cpy'), 'CDEMO-MENU-OPT-PGMNAME table app/cpy/COMEN02Y.cpy'),
 'app/cbl/COADM01C.cbl:146': ('DISPATCH', table('app/cpy/COADM02Y.cpy'), 'CDEMO-ADMIN-OPT-PGMNAME table app/cpy/COADM02Y.cpy'),
 'app/cbl/COCRDLIC.cbl:538': ('FORWARD', ['COCRDSLC'], 'LIT-CARDDTLPGM VALUE :196, MOVE :526'),
 'app/cbl/COCRDLIC.cbl:566': ('FORWARD', ['COCRDUPC'], 'LIT-CARDUPDPGM VALUE :204, MOVE :554'),
 A + 'COPAUS1C.cbl:368': ('FORWARD', ['COPAUS0C'], 'WS-PGM-AUTH-SMRY VALUE :34, MOVE :168,:185'),
 T + 'COTRTLIC.cbl:620': ('RETURN', ['COADM01C'], 'PF3 (:591-623): LIT-ADMINPGM VALUE :47, MOVE :603; or CDEMO-FROM-PROGRAM :605. PF2 to COTRTUPC is the separate XCTL :648'),
 T + 'COTRTUPC.cbl:457': ('RETURN', ['COADM01C'], 'LIT-ADMINPGM VALUE :209, MOVE :440; or CDEMO-FROM-PROGRAM :442'),
 'app/cbl/COCRDLIC.cbl:402': ('RETURN', ['COMEN01C'], 'LIT-MENUPGM VALUE :187'),
 'app/cbl/COACTVWC.cbl:349': ('RETURN', ['COMEN01C'], 'LIT-MENUPGM VALUE :168, MOVE :336; or CDEMO-FROM-PROGRAM :338'),
 'app/cbl/COACTUPC.cbl:956': ('RETURN', ['COMEN01C'], 'LIT-MENUPGM VALUE :557, MOVE :939; or CDEMO-FROM-PROGRAM :941'),
 'app/cbl/COCRDSLC.cbl:331': ('RETURN', ['COMEN01C'], 'LIT-MENUPGM VALUE :179, MOVE :318; or CDEMO-FROM-PROGRAM :320'),
 'app/cbl/COCRDUPC.cbl:473': ('RETURN', ['COMEN01C'], 'LIT-MENUPGM VALUE :235, MOVE :451; or CDEMO-FROM-PROGRAM :453'),
 'app/cbl/COACTVWC.cbl:402': ('SELF', ['CAVW'], 'LIT-THISTRANID VALUE :145'),
 'app/cbl/COACTUPC.cbl:1015': ('SELF', ['CAUP'], 'LIT-THISTRANID VALUE :535'),
 'app/cbl/COCRDLIC.cbl:615': ('SELF', ['CCLI'], 'LIT-THISTRANID VALUE :181'),
 'app/cbl/COCRDSLC.cbl:402': ('SELF', ['CCDL'], 'LIT-THISTRANID VALUE :165'),
 'app/cbl/COCRDUPC.cbl:554': ('SELF', ['CCUP'], 'LIT-THISTRANID VALUE :221'),
 T + 'COTRTUPC.cbl:567': ('SELF', ['CTTU'], 'LIT-THISTRANID VALUE :203'),
}
site_caller = {c: caller for caller, callee, kind, c, res in G['edges']}
for c, (k, ts, why) in SITE.items():
    if k == 'FORWARD':
        for t in ts: fwd[site_caller[c]].add((t, c + ' (' + why + ')'))
S = []  # id, kind, entry/trigger, proof, roots
def tran(t, csd='app/csd/CARDDEMO.CSD'): return cite(csd, rf'DEFINE TRANSACTION\({t}\)')
S.append(('ONL-00 Sign-on + menus', 'ONLINE', 'CC00 (also CM00, CA00)', tran('CC00') + '; ' + tran('CM00') + '; ' + tran('CA00'), ['COSGN00C', 'COMEN01C', 'COADM01C']))
main = [('M01 Account View', 'COACTVWC', 'CAVW'), ('M02 Account Update', 'COACTUPC', 'CAUP'), ('M03 Credit Card List', 'COCRDLIC', 'CCLI'),
        ('M04 Credit Card View', 'COCRDSLC', 'CCDL'), ('M05 Credit Card Update', 'COCRDUPC', 'CCUP'), ('M06 Transaction List', 'COTRN00C', 'CT00'),
        ('M07 Transaction View', 'COTRN01C', 'CT01'), ('M08 Transaction Add', 'COTRN02C', 'CT02'), ('M09 Transaction Reports', 'CORPT00C', 'CR00'),
        ('M10 Bill Payment', 'COBIL00C', 'CB00'), ('M11 Pending Authorization View', 'COPAUS0C', 'CPVS')]
for n, p, t in main:
    csd = 'app/app-authorization-ims-db2-mq/csd/CRDDEMO2.csd' if p.startswith('COPAU') else 'app/csd/CARDDEMO.CSD'
    S.append(('ONL-' + n, 'ONLINE', f'main menu option {int(n[1:3])} (COMEN01C) / trancode {t}', cite('app/cpy/COMEN02Y.cpy', f"'{p}'") + '; ' + tran(t, csd), [p]))
adm = [('A01 User List', 'COUSR00C', 'CU00'), ('A02 User Add', 'COUSR01C', 'CU01'), ('A03 User Update', 'COUSR02C', 'CU02'), ('A04 User Delete', 'COUSR03C', 'CU03'),
       ('A05 Transaction Type List/Update (Db2)', 'COTRTLIC', 'CTLI'), ('A06 Transaction Type Maintenance (Db2)', 'COTRTUPC', 'CTTU')]
for n, p, t in adm:
    csd = 'app/app-transaction-type-db2/csd/CRDDEMOD.csd' if p.startswith('COTRT') else 'app/csd/CARDDEMO.CSD'
    S.append(('ONL-' + n, 'ONLINE', f'admin menu option {int(n[1:3])} (COADM01C) / trancode {t}', cite('app/cpy/COADM02Y.cpy', f"'{p}'") + '; ' + tran(t, csd), [p]))
S.append(('ONL-CP00 Authorization request processor (MQ)', 'ONLINE (MQ-triggered, no screen)', 'CP00', tran('CP00', 'app/app-authorization-ims-db2-mq/csd/CRDDEMO2.csd'), ['COPAUA0C']))
S.append(('ONL-CDRA Account inquiry via MQ', 'ONLINE (MQ request/response)', 'CDRA', tran('CDRA', 'app/app-vsam-mq/csd/CRDDEMOM.csd'), ['COACCT01']))
S.append(('ONL-CDRD System date via MQ', 'ONLINE (MQ request/response)', 'CDRD', tran('CDRD', 'app/app-vsam-mq/csd/CRDDEMOM.csd'), ['CODATE01']))
S.append(('ONL-CDV1 (BLOCKED: COCRDSEC absent)', 'ONLINE', 'CDV1', tran('CDV1') + '; PROGRAM ' + cite('app/csd/CARDDEMO.CSD', r'DEFINE PROGRAM\(COCRDSEC\)'), ['COCRDSEC']))
def job(j, f, pgm): return cite(f, rf'EXEC +PGM={pgm}|PARM=.*{pgm}|RUN PROGRAM\({pgm}\)')
B = [('POSTTRAN', 'app/jcl/POSTTRAN.jcl', 'CBTRN02C'), ('INTCALC', 'app/jcl/INTCALC.jcl', 'CBACT04C'), ('TRANREPT', 'app/jcl/TRANREPT.jcl', 'CBTRN03C'),
     ('CREASTMT', 'app/jcl/CREASTMT.JCL', 'CBSTM03A'), ('READACCT', 'app/jcl/READACCT.jcl', 'CBACT01C'), ('READCARD', 'app/jcl/READCARD.jcl', 'CBACT02C'),
     ('READCUST', 'app/jcl/READCUST.jcl', 'CBCUS01C'), ('READXREF', 'app/jcl/READXREF.jcl', 'CBACT03C'), ('CBEXPORT', 'app/jcl/CBEXPORT.jcl', 'CBEXPORT'),
     ('CBIMPORT', 'app/jcl/CBIMPORT.jcl', 'CBIMPORT'), ('WAITSTEP', 'app/jcl/WAITSTEP.jcl', 'COBSWAIT'),
     ('CBPAUP0J', 'app/app-authorization-ims-db2-mq/jcl/CBPAUP0J.jcl', 'CBPAUP0C'), ('LOADPADB', 'app/app-authorization-ims-db2-mq/jcl/LOADPADB.JCL', 'PAUDBLOD'),
     ('UNLDPADB', 'app/app-authorization-ims-db2-mq/jcl/UNLDPADB.JCL', 'PAUDBUNL'), ('UNLDGSAM', 'app/app-authorization-ims-db2-mq/jcl/UNLDGSAM.JCL', 'DBUNLDGS'),
     ('MNTTRDB2', 'app/app-transaction-type-db2/jcl/MNTTRDB2.jcl', 'COBTUPDT')]
for j, f, p in B: S.append(('BAT-' + j, 'BATCH', f'job {j}', job(j, f, p), [p]))
members = {}
for sid, kind, trig, proof, roots in S:
    seen, stack = set(), list(roots)
    while stack:
        p = stack.pop()
        if p in seen: continue
        seen.add(p)
        for q, _ in fwd.get(p, ()):
            if q in MENU and not sid.startswith('ONL-00'): continue
            stack.append(q)
    members[sid] = seen
where = collections.defaultdict(list)
for sid, m in members.items():
    for p in m: where[p].append(sid)
absent = sorted({p for m in members.values() for p in m if p not in progs})
exclusive = sorted(p for p in progs if len(where[p]) == 1)
shared = sorted(p for p in progs if len(where[p]) > 1)
unreach = sorted(p for p in progs if len(where[p]) == 0)
out = {'streams': [{'id': s[0], 'kind': s[1], 'trigger': s[2], 'proof': s[3], 'programs': sorted(members[s[0]])} for s in S],
       'exclusive': exclusive, 'shared': {p: where[p] for p in shared}, 'unreachable': unreach, 'absent': absent,
       'forward_edges': {k: sorted(v) for k, v in fwd.items()}}
json.dump(out, open(os.path.join(OUTDIR, 'coverage.json'), 'w'), indent=1)
print(f'programs in module file set: {len(progs)}')
print(f'exclusive (exactly 1 stream): {len(exclusive)}  shared (>1 stream): {len(shared)}  unreachable: {len(unreach)}')
print(f'CHECK {len(progs)} = {len(exclusive)} + {len(shared)} + {len(unreach)} -> {len(progs) == len(exclusive) + len(shared) + len(unreach)}')
print('absent (referenced by an entry point, not in repo):', absent)
print('unreachable:', unreach)
# Independent checks (the bucket sum above balances by construction; these do not).
roots = {r for *_, rs in S for r in rs}
declared = collections.defaultdict(list)
for caller, callee, kind, c, res in G['edges']:
    if kind in ('CSD-TRANSACTION', 'JCL-EXEC', 'JCL-TSO-RUN', 'JCL-IMS-PARM'):
        declared[callee].append(c)
fails = 0
# A. every runtime declaration of a module program is a stream root, and every root is declared
undeclared_roots = sorted(r for r in roots if r not in declared)
# declared trancodes that are not separately selectable streams, each with its proof
DECL_NOT_ROOT = {
 'COPAUS1C': 'CPVD is the pseudo-conversational re-entry of COPAUS1C (WS-CICS-TRANID VALUE CPVD '
             'app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:36, RETURN TRANSID :203); a cold start '
             '(EIBCALEN = 0, :165-169) XCTLs to COPAUS0C, so it runs inside ONL-M11',
}
# every declared target counts, whether or not its source is in the repo (an absent program behind a live trancode is still an entry point)
unrooted_decls = sorted(d for d in declared if d not in roots and d not in DECL_NOT_ROOT)
for d, why in DECL_NOT_ROOT.items(): print('CHECK-A exception', d, '-', why)
print(f'CHECK-A declarations: {len(declared)} declared targets, {len(roots)} stream roots; '
      f'roots without declaration {undeclared_roots}; declared programs not a root {unrooted_decls}')
fails += bool(undeclared_roots or unrooted_decls)
# B. every program-to-program edge in edges.tsv is attributed to the stream(s) of its caller
def targets(callee, kind, res):
    if kind.endswith('-TRANSID') or kind.endswith('-TRANSID-DYNAMIC') or kind == 'CALL-RUNTIME': return []
    if 'DYNAMIC' in kind: return [t for t in re.findall(r'([A-Z0-9#@$]{1,8})@', res) if t in progs]
    return [callee] if callee in progs else []
unattributed = []
for caller, callee, kind, c, res in G['edges']:
    if caller not in progs: continue
    for t in targets(callee, kind, res):
        if t == caller or t in MENU: continue
        for sid in where[caller]:
            if t not in members[sid]: unattributed.append((sid, caller, t, c))
print(f'CHECK-B edge attribution: {len(unattributed)} program-to-program edges outside their caller\'s stream', unattributed[:10])
fails += bool(unattributed)
# C. an unreachable program has no inbound edge and no runtime declaration
inbound = collections.defaultdict(list)
for caller, callee, kind, c, res in G['edges']:
    for t in (targets(callee, kind, res) if caller in progs else ([callee] if callee in progs else [])):
        if t != caller: inbound[t].append(c)
bad_unreach = [(p, inbound[p], declared.get(p)) for p in unreach if inbound[p] or declared.get(p)]
print(f'CHECK-C unreachable: {len(unreach)} unreachable, with inbound edge or declaration: {bad_unreach}')
fails += bool(bad_unreach)
# D. every UNRESOLVED dynamic edge has a hand resolution, and each resolution is consistent with the catalog
csd_tran = {caller[5:]: callee for caller, callee, kind, c, res in G['edges'] if kind == 'CSD-TRANSACTION'}
unres = [(caller, c) for caller, callee, kind, c, res in G['edges'] if res == 'UNRESOLVED']
missing = [c for _, c in unres if c not in SITE]
bad = []
for caller, c in unres:
    if c not in SITE: continue
    k, ts, why = SITE[c]
    src_text = open(c.rsplit(':', 1)[0], encoding='latin1').read()
    for t in ts:
        if k != 'DISPATCH' and f"'{t}'" not in src_text: bad.append((c, t, 'literal not present in the caller source'))
        if k == 'DISPATCH' and t not in roots: bad.append((c, t, 'dispatch target is not a stream root'))
        if k == 'FORWARD' and any(t not in members[sid] for sid in where[caller]): bad.append((c, t, 'forward target outside the caller stream'))
        if k == 'RETURN' and t not in MENU: bad.append((c, t, 'return target is not the navigation hub'))
        if k == 'SELF' and csd_tran.get(t) != caller: bad.append((c, t, 'trancode is not defined for the caller'))
stale = [c for c in SITE if c not in dict((x, 1) for _, x in unres)]
print(f'CHECK-D unresolved sites: {len(unres)} unresolved, {len(missing)} without a hand resolution {missing}, '
      f'{len(bad)} inconsistent {bad}, {len(stale)} stale resolutions {stale}')
fails += bool(missing or bad or stale)
print('INDEPENDENT CHECKS', 'PASS' if not fails else f'FAIL ({fails})')
for p in shared: print('SHARED', p, where[p])
for s in out['streams']: print(s['id'], '|', s['programs'])
sys.exit(1 if fails else 0)
