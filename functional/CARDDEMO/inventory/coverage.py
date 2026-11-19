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
 ('COCRDLIC', 'COCRDSLC', 'app/cbl/COCRDLIC.cbl:538 (LIT-CARDDTLPGM VALUE :196, MOVE :526)'),
 ('COCRDLIC', 'COCRDUPC', 'app/cbl/COCRDLIC.cbl:566 (LIT-CARDUPDPGM VALUE :204, MOVE :554)'),
 ('COPAUS0C', 'COPAUS1C', 'app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl:323 (WS-PGM-AUTH-DTL VALUE :34, MOVE :316)'),
]
for a, b, c in MANUAL: fwd[a].add((b, c))
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
for p in shared: print('SHARED', p, where[p])
for s in out['streams']: print(s['id'], '|', s['programs'])
