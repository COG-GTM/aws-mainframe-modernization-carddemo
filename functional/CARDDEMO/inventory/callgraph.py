#!/usr/bin/env python3
"""CardDemo inventory call-graph extractor (analysis tooling, read-only over app/**).
Emits TSV edges: caller, callee, kind, cite, resolution."""
import re, glob, os, sys, json
ROOT = os.path.abspath(sys.argv[1] if len(sys.argv) > 1 else '.')
OUTDIR_ARG = os.path.abspath(sys.argv[2]) if len(sys.argv) > 2 else None
os.chdir(ROOT)
RUNTIME = {'CEE3ABD','CEEDAYS','MQOPEN','MQGET','MQPUT','MQPUT1','MQCLOSE','MQCONN','MQDISC','CBLTDLI','DSNTIAUL'}
UTIL_PGMS = {'IDCAMS','IEFBR14','IEBGENER','SORT','SDSF','DFSRRC00','IKJEFT01','IKJEFT1B','FTP','DFHCSDUP'}
src = sorted(glob.glob('app/**/*.cbl', recursive=True) + glob.glob('app/**/*.CBL', recursive=True))
asm = sorted(glob.glob('app/**/*.asm', recursive=True))
progs = {os.path.splitext(os.path.basename(p))[0].upper(): p for p in src + asm}
edges = []
def code_lines(path):
    out = []
    for i, l in enumerate(open(path, encoding='latin1').read().splitlines(), 1):
        if len(l) > 6 and l[6] in '*/': continue
        out.append((i, l[7:72] if len(l) > 7 else ''))
    return out
def is_para(t):
    return bool(re.match(r'[A-Z0-9][\w-]*\.\s*$', t)) and not t.startswith(' ')
def literals_at_site(lines, k, var, verbs):
    """Literals that can reach the dispatch at lines[k]. SITE: MOVEs to var after the previous dispatch
    on var (or the paragraph start) and before k. If that region only defaults an empty var
    (IF var = LOW-VALUES/SPACES), the value normally arrives from another paragraph, so add the MOVEs
    in other paragraphs that are not consumed by a dispatch in their own paragraph (INBOUND).
    Falls back to every MOVE/VALUE in the program (PROGRAM-WIDE)."""
    disp = re.compile(r'\b(?:' + verbs + r')\b.*\b' + re.escape(var) + r'\b')
    def para_start(x):
        for j in range(x, -1, -1):
            if is_para(lines[j][1]): return j
        return 0
    lo = 0
    for j in range(k - 1, -1, -1):
        t = lines[j][1]
        if is_para(t) or disp.search(t):
            lo = j + 1; break
    near = literals_moved(lines[lo:k], var)
    region = ' '.join(t for _, t in lines[lo:k])
    if near and not re.search(re.escape(var) + r'\s*=\s*(?:LOW-VALUES|SPACES)', region):
        return near, 'SITE'
    here = para_start(k)
    inbound = []
    for lit, n in literals_moved(lines, var):
        x = next(ix for ix, (ln, _) in enumerate(lines) if ln == n)
        if para_start(x) == here: continue
        consumed = False
        for y in range(x + 1, len(lines)):
            if is_para(lines[y][1]): break
            if disp.search(lines[y][1]): consumed = True; break
        if not consumed: inbound.append((lit, n))
    if near or inbound: return near + inbound, 'SITE+INBOUND'
    return literals_moved(lines, var), 'PROGRAM-WIDE'
def literals_moved(lines, var):
    lits = []
    for i, t in lines:
        m = re.search(r"MOVE\s+'([A-Z0-9#@$]{1,8})'\s+TO\s+(?:[\w-]+\s+)*?" + re.escape(var) + r'\b', t)
        if m: lits.append((m.group(1), i))
        m = re.search(r'\b' + re.escape(var) + r"\b.*VALUE\s+'([A-Z0-9#@$]{1,8})'", t)
        if m: lits.append((m.group(1), i))
    return lits
for path in src:
    caller = os.path.splitext(os.path.basename(path))[0].upper()
    lines = code_lines(path)
    text = [t for _, t in lines]
    for k, (i, t) in enumerate(lines):
        m = re.search(r"\bCALL\s+['\"]([A-Z0-9#@$]+)['\"]", t)
        if m:
            c = m.group(1)
            edges.append((caller, c, 'CALL-RUNTIME' if c in RUNTIME else 'CALL', f'{path}:{i}', 'STATIC'))
            continue
        t_nolit = re.sub(r"'[^']*'|\"[^\"]*\"", "''", t)
        m = re.search(r"(?<![\w-])CALL\s+([A-Z][\w-]+)", t_nolit)
        if m:
            var = m.group(1); lits, scope = literals_at_site(lines, k, var, 'CALL')
            res = (','.join(f'{l}@{path}:{n}' for l, n in lits) + ('' if scope == 'SITE' else f' [{scope}]')) if lits else 'UNRESOLVED'
            edges.append((caller, var, 'CALL-DYNAMIC', f'{path}:{i}', res))
        for verb in ('XCTL', 'LINK', 'START', 'RETURN'):
            if re.search(r'\b' + verb + r'\b', t) and (re.search(r'EXEC\s+CICS', t) or (k and re.search(r'EXEC\s+CICS\s*$', lines[k-1][1]))):
                window = ' '.join(x for _, x in lines[k:k+6])
                window = window.split('END-EXEC')[0]
                key = 'TRANSID' if verb in ('START', 'RETURN') else 'PROGRAM'
                m2 = re.search(key + r"\s*\(\s*('?)([\w-]+)'?\s*(?:\(([\w-]+)\))?\s*\)", window)
                if not m2: continue
                tgt = m2.group(2)
                if m2.group(1):
                    edges.append((caller, tgt, 'CICS-' + verb + ('' if verb != 'RETURN' else '-TRANSID'), f'{path}:{i}', 'STATIC'))
                else:
                    lits, scope = literals_at_site(lines, k, tgt, 'XCTL|LINK|START|RETURN')
                    res = (','.join(f'{l}@{path}:{n}' for l, n in lits) + ('' if scope == 'SITE' else f' [{scope}]')) if lits else 'UNRESOLVED'
                    edges.append((caller, tgt + (f'({m2.group(3)})' if m2.group(3) else ''), 'CICS-' + verb + ('-TRANSID' if verb == 'RETURN' else '') + '-DYNAMIC', f'{path}:{i}', res))
cpys = {os.path.splitext(os.path.basename(c))[0].upper(): c for c in glob.glob('app/**/cpy/*', recursive=True)}
for path in src:
    caller = os.path.splitext(os.path.basename(path))[0].upper()
    for i, t in code_lines(path):
        m = re.search(r"\b(?:COPY|INCLUDE)\s+'?([A-Z0-9]+)'?", t)
        if not m or m.group(1) not in cpys: continue
        cp = cpys[m.group(1)]
        clines = code_lines(cp)
        for j, ct in clines:
            mm = re.search(r"\bCALL\s+'([A-Z0-9#@$]+)'", ct)
            if mm:
                edges.append((caller, mm.group(1), 'CALL-VIA-COPYBOOK', f'{cp}:{j}', f'COPY {m.group(1)}@{path}:{i}'))
            mm = re.search(r"(?<![\w-])CALL\s+([A-Z][\w-]+)", re.sub(r"'[^']*'", "''", ct))
            if mm:
                lits = literals_moved(code_lines(path), mm.group(1)) or literals_moved(clines, mm.group(1))
                res = ','.join(f'{l}@{path}' for l, n in lits) or 'UNRESOLVED'
                edges.append((caller, mm.group(1), 'CALL-DYNAMIC-VIA-COPYBOOK', f'{cp}:{j}', f'COPY {m.group(1)}@{path}:{i}; {res}'))
for path in sorted(glob.glob('app/**/*.jcl', recursive=True) + glob.glob('app/**/*.JCL', recursive=True) + glob.glob('app/**/*.prc', recursive=True)):
    job = os.path.splitext(os.path.basename(path))[0].upper()
    for i, l in enumerate(open(path, encoding='latin1').read().splitlines(), 1):
        if l.startswith('//*'): continue
        m = re.match(r'//(\S+)\s+EXEC\s+PGM=([A-Z0-9#@$]+)', l)
        if m:
            edges.append(('JOB:' + job, m.group(2), 'JCL-UTILITY' if m.group(2) in UTIL_PGMS else 'JCL-EXEC', f'{path}:{i}', 'STEP=' + m.group(1)))
        m = re.match(r'//(\S+)\s+EXEC\s+(?:PROC=)?([A-Z0-9#@$]+)\s*,?', l)
        if m and 'PGM=' not in l:
            edges.append(('JOB:' + job, 'PROC:' + m.group(2), 'JCL-PROC', f'{path}:{i}', 'STEP=' + m.group(1)))
        m = re.search(r"PARM='(?:BMP|DLI),([A-Z0-9#@$]+),", l)
        if m:
            edges.append(('JOB:' + job, m.group(1), 'JCL-IMS-PARM', f'{path}:{i}', 'DFSRRC00 PARM'))
        m = re.search(r'RUN PROGRAM\((\w+)\)', l)
        if m:
            edges.append(('JOB:' + job, m.group(1), 'JCL-UTILITY' if m.group(1) in RUNTIME else 'JCL-TSO-RUN', f'{path}:{i}', 'IKJEFT01/DSN'))
for path in ['app/csd/CARDDEMO.CSD'] + sorted(glob.glob('app/app-*/csd/*')):
    cur = None
    for i, l in enumerate(open(path, encoding='latin1').read().splitlines(), 1):
        m = re.search(r'DEFINE TRANSACTION\((\w+)\)', l)
        if m: cur = (m.group(1), i)
        m2 = re.search(r'\bPROGRAM\((\w+)\)', l)
        if cur and m2 and 'DEFINE PROGRAM' not in l:
            edges.append(('TRAN:' + cur[0], m2.group(1), 'CSD-TRANSACTION', f'{path}:{cur[1]}', f'PROGRAM@{path}:{i}'))
            cur = None
OUTDIR = OUTDIR_ARG or os.path.join(ROOT, 'functional/CARDDEMO/inventory')
json.dump({'programs': progs, 'edges': edges}, open(os.path.join(OUTDIR, 'graph.json'), 'w'), indent=1)
print('caller\tcallee\tkind\tcite\tresolution')
for e in edges: print('\t'.join(e))
