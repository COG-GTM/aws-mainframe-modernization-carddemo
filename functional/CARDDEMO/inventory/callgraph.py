#!/usr/bin/env python3
"""Mechanical call graph of app/ (read-only). Usage: callgraph.py REPO_ROOT > edges.tsv

Edge kinds: JCL_EXEC (EXEC PGM=), JCL_PROC (EXEC <proc>), JCL_DSN_RUN (IKJEFT01 RUN PROGRAM), JCL_IMS (DFSRRC00 PARM),
CSD_TRAN (DEFINE TRANSACTION -> PROGRAM), SCHED (CA-7 / Control-M job chain), CALL_STATIC, CALL_DYNAMIC (resolved via
VALUE/MOVE literals in the same source, else UNRESOLVED), CICS_XCTL / CICS_LINK (literal or resolved variable).
"""
import os, re, sys
from pathlib import Path

ROOT = Path(sys.argv[1] if len(sys.argv) > 1 else '.').resolve()
APP = ROOT / 'app'
EXT_PROGRAMS = {  # system / runtime modules, not part of the inventory (no source in app/)
    'IDCAMS', 'SDSF', 'IKJEFT01', 'IEFBR14', 'IEBGENER', 'SORT', 'DFSRRC00', 'DFHCSDUP', 'DSNTIAUL',
    'CBLTDLI', 'IKJEFT1B', 'FTP', 'CEE3ABD', 'CEEDAYS', 'MQOPEN', 'MQCLOSE', 'MQPUT', 'MQPUT1', 'MQGET'}

def rel(p): return str(p.relative_to(ROOT))

def programs():
    out = {}
    for p in sorted(APP.rglob('*')):
        if p.is_file() and p.suffix.lower() in ('.cbl', '.asm') and '/cbl' in str(p.parent).replace(str(APP), '') + '/' or (p.is_file() and p.suffix.lower() == '.asm'):
            out[p.stem.upper()] = rel(p)
    return out

def code_lines(p):
    """Yield (lineno, text) of COBOL code lines (fixed format: skip comment indicator col 7, cut at col 72)."""
    for i, line in enumerate(p.read_text(errors='replace').splitlines(), 1):
        if len(line) > 6 and line[6] in '*/':
            continue
        yield i, line[6:72] if len(line) > 6 else ''

def copybook(name):
    for d in APP.rglob('*'):
        if d.is_dir() and d.name.startswith('cpy'):
            for c in (d / name, d / (name + '.cpy'), d / (name + '.CPY')):
                if c.is_file():
                    return c
    return None

def source_text(p):
    """Code text of a source plus the copybooks it COPYs (one level)."""
    parts = [t for _, t in code_lines(p)]
    for name in re.findall(r"\bCOPY\s+([A-Z0-9@#$-]+)", ' '.join(parts), re.I):
        c = copybook(name.upper())
        if c:
            parts += [t for _, t in code_lines(c)]
    return ' '.join(parts)

NOLIT = re.compile(r"'[^']*'|\"[^\"]*\"")

def literal_values(p, progs=()):
    """Map data-name -> literal values it can hold: VALUE clause, MOVE 'lit' TO name, MOVE a TO b (closure).
    Key '*TABLE*' = every VALUE literal that names an inventory program (menu tables of FILLERs)."""
    vals, text = {}, source_text(p)
    for m in re.finditer(r"\b\d\d\s+([A-Z0-9-]+)\s+PIC\s+X\(\d+\)\s+VALUE\s+'([A-Z0-9@#$]+)\s*'", text, re.I):
        vals.setdefault(m.group(1).upper(), set()).add(m.group(2).upper())
    for m in re.finditer(r"MOVE\s+'([A-Z0-9@#$]+)\s*'\s+TO\s+([A-Z0-9-]+)", text, re.I):
        vals.setdefault(m.group(2).upper(), set()).add(m.group(1).upper())
    moves = re.findall(r"MOVE\s+([A-Z][A-Z0-9-]*)\s+TO\s+([A-Z][A-Z0-9-]*)", NOLIT.sub(' ', text), re.I)
    for _ in range(5):
        for a, b in moves:
            if a.upper() in vals:
                vals.setdefault(b.upper(), set()).update(vals[a.upper()])
    vals['*TABLE*'] = {v.upper() for v in re.findall(r"VALUE\s+'([A-Z0-9@#$]{1,8})\s*'", text, re.I)} & set(progs)
    return vals

def resolve(var, vals):
    if vals.get(var):
        return [(v, '') for v in sorted(vals[var])]
    if vals.get('*TABLE*'):
        return [(v, '_TABLE') for v in sorted(vals['*TABLE*'])]
    return [('UNRESOLVED:' + var, '')]

def cobol_edges(name, path, progs):
    p = ROOT / path
    if p.suffix.lower() == '.asm':
        return
    vals = literal_values(p, progs)
    for ln, t in code_lines(p):
        t0, t = t, NOLIT.sub(lambda m: m.group(0) if re.search(r"(CALL|PROGRAM\s*\()\s*$", t[:m.start()], re.I) else ' ', t)
        for m in re.finditer(r"(?<![A-Z0-9-])CALL\s+(['\"])([A-Z0-9@#$]+)\1", t, re.I):
            yield name, m.group(2).upper(), 'CALL_STATIC', f'{path}:{ln}'
        for m in re.finditer(r"(?<![A-Z0-9-])CALL\s+([A-Z][A-Z0-9-]*)\b(?!\s*['\"])", t, re.I):
            var = m.group(1).upper()
            for v in sorted(vals.get(var, [])) or ['UNRESOLVED:' + var]:
                yield name, v, 'CALL_DYNAMIC', f'{path}:{ln}'
        for m in re.finditer(r"\b(XCTL|LINK)\s+PROGRAM\s*\(\s*(['\"]?)([A-Z0-9@#$-]+)\2", t, re.I):
            kind, q, tgt = 'CICS_' + m.group(1).upper(), m.group(2), m.group(3).upper()
            if q:
                yield name, tgt, kind, f'{path}:{ln}'
            else:
                for v, sfx in resolve(tgt, vals):
                    yield name, v, kind + sfx, f'{path}:{ln}'

def jcl_edges():
    files = [p for d in ('jcl', 'proc') for p in APP.rglob('*') if p.is_file() and p.parent.name == d]
    procs = {p.stem.upper() for p in files if p.parent.name == 'proc'}
    for p in sorted(files):
        src = ('PROC:' if p.parent.name == 'proc' else 'JOB:') + p.stem.upper()
        for i, line in enumerate(p.read_text(errors='replace').splitlines(), 1):
            if line.startswith('//*'):
                continue
            m = re.search(r"\bEXEC\s+PGM=([A-Z0-9@#$]+)", line)
            if m:
                yield src, m.group(1), 'JCL_EXEC', f'{rel(p)}:{i}'
            m = re.search(r"\bEXEC\s+(?:PROC=)?([A-Z0-9@#$]+)\b(?!=)", line)
            if m and m.group(1) in procs:
                yield src, 'PROC:' + m.group(1), 'JCL_PROC', f'{rel(p)}:{i}'
            m = re.search(r"RUN\s+PROGRAM\s*\(\s*([A-Z0-9@#$]+)", line, re.I)
            if m:
                yield src, m.group(1).upper(), 'JCL_DSN_RUN', f'{rel(p)}:{i}'
            m = re.search(r"PARM='?(?:DLI|BMP|DBB),([A-Z0-9@#$]+)", line)
            if m:
                yield src, m.group(1), 'JCL_IMS', f'{rel(p)}:{i}'

def csd_edges():
    for p in sorted(APP.rglob('*')):
        if not (p.is_file() and p.parent.name == 'csd'):
            continue
        cur, start = None, 0
        for i, line in enumerate(p.read_text(errors='replace').splitlines(), 1):
            m = re.search(r"DEFINE\s+TRANSACTION\(([A-Z0-9]+)\)", line)
            if m:
                cur, start = m.group(1), i
            elif re.search(r"DEFINE\s+\w+\(", line):
                cur = None
            m = re.search(r"\bPROGRAM\(([A-Z0-9@#$]+)\)", line)
            if cur and m and 'DEFINE' not in line.split('PROGRAM(')[0][-8:]:
                yield 'TRAN:' + cur, m.group(1), 'CSD_TRAN', f'{rel(p)}:{start}'
                cur = None

def sched_edges():
    """CA-7 LJOB listing: 'JOB=X LIST=ALL' opens job X; 'JOB=Y SCHID=' under TRIGGERED JOBS = X triggers Y."""
    ca7 = APP / 'scheduler' / 'CardDemo.ca7'
    if not ca7.exists():
        return
    job = None
    for i, line in enumerate(ca7.read_text(errors='replace').splitlines(), 1):
        m = re.match(r"\s*JOB=([A-Z0-9@#$]+)\s+LIST=ALL", line)
        if m:
            job = m.group(1)
            yield 'SCHED:CA7', 'JOB:' + job, 'SCHED_DEF', f'{rel(ca7)}:{i}'
        m = re.match(r"\s*JOB=([A-Z0-9@#$]+)\s+SCHID=", line)
        if m and job:
            yield 'JOB:' + job, 'JOB:' + m.group(1), 'SCHED_TRIGGER', f'{rel(ca7)}:{i}'

def all_edges():
    progs = programs()
    edges = []
    for n, path in progs.items():
        edges += list(cobol_edges(n, path, progs))
    edges += list(jcl_edges()) + list(csd_edges()) + list(sched_edges())
    return progs, edges

if __name__ == '__main__':
    progs, edges = all_edges()
    print('src\tdst\tkind\tcite')
    for e in sorted(set(edges)):
        print('\t'.join(e))
