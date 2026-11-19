#!/usr/bin/env python3
"""Build LegacyLens data from the CardDemo source tree, the exported migration-docs
scan findings (raw/findings.txt), the business-rules catalog branch and the repo's PRs.
Everything emitted here is derived from those inputs; nothing is hand-entered."""
import json, os, re, subprocess, sys, collections, xml.etree.ElementTree as ET

HERE = os.path.dirname(os.path.abspath(__file__))
APP = os.path.dirname(HERE)
REPO = os.path.dirname(APP)
OUT = os.path.join(APP, "data")
os.makedirs(OUT, exist_ok=True)

def sh(*a):
    return subprocess.run(a, cwd=REPO, capture_output=True, text=True).stdout

files = [f for f in sh("git", "ls-files").splitlines()
         if not f.startswith(("legacylens/", "samples/", "modernization/"))]

def read(p):
    with open(os.path.join(REPO, p), "rb") as fh:
        b = fh.read()
    try:
        return b.decode("utf-8")
    except UnicodeDecodeError:
        return b.decode("latin-1")

def stem(p):
    return os.path.splitext(os.path.basename(p))[0].upper()

def ext(p):
    return os.path.splitext(p)[1].lower().lstrip(".")

nodes, edges = {}, {}

def node(id_, type_, label, **kw):
    n = nodes.get(id_)
    if not n:
        n = nodes[id_] = {"id": id_, "type": type_, "label": label}
    for k, v in kw.items():
        if v is not None and k not in n:
            n[k] = v
    return n

def edge(s, t, kind, file=None, line=None, detail=None):
    if s == t:
        return
    k = (s, t, kind)
    e = edges.get(k)
    if not e:
        e = edges[k] = {"source": s, "target": t, "kind": kind, "refs": []}
    if file and len(e["refs"]) < 60:
        r = {"file": file, "line": line}
        if detail:
            r["detail"] = detail
        if r not in e["refs"]:
            e["refs"].append(r)

def module(p):
    m = re.match(r"app/(app-[^/]+)/", p)
    return {"app-authorization-ims-db2-mq": "Authorization (IMS/DB2/MQ)",
            "app-transaction-type-db2": "Transaction type (DB2)",
            "app-vsam-mq": "Account/date services (VSAM/MQ)"}.get(m.group(1), m.group(1)) if m else "Core (VSAM/CICS/batch)"

def loc(text):
    return len(text.splitlines())

def code_lines(p, text):
    """(1-based line no, code text) with comment lines removed, fixed-format aware."""
    out = []
    e = ext(p)
    for i, l in enumerate(text.splitlines(), 1):
        if e in ("cbl", "cpy", "cob"):
            if len(l) > 6 and l[6] in "*/":
                continue
            l = l[:72]
        elif e in ("jcl", "prc"):
            if l.startswith("//*"):
                continue
            l = l[:72]
        elif e in ("bms", "asm", "csd"):
            if l.startswith("*"):
                continue
        out.append((i, l))
    return out

# ---- pass 1: declare nodes for source artifacts --------------------------------
by_ext = collections.defaultdict(list)
for f in files:
    by_ext[ext(f)].append(f)

programs = {}
for f in by_ext["cbl"]:
    t = read(f)
    online = bool(re.search(r"EXEC\s+CICS", t))
    pid = "pgm:" + stem(f)
    programs[stem(f)] = f
    kind = "online" if online else "batch"
    node(pid, "program", stem(f), file=f, loc=loc(t), module=module(f), sub=kind,
         tech=sorted({k for k, rx in {"CICS": r"EXEC\s+CICS", "DB2": r"EXEC\s+SQL", "IMS": r"CBLTDLI|EXEC\s+DLI|DLI\b", "MQ": r"'MQ(OPEN|GET|PUT|CONN)"}.items() if re.search(rx, t)}))
for f in by_ext["asm"]:
    programs[stem(f)] = f
    node("pgm:" + stem(f), "program", stem(f), file=f, loc=loc(read(f)), module=module(f), sub="assembler", tech=["HLASM"])

copybooks = {}
for f in files:
    if ext(f) in ("cpy", "dcl") or "/cpy" in f and ext(f) in ("cpy",):
        copybooks.setdefault(stem(f), f)
for name, f in copybooks.items():
    sub = "screen map" if "/cpy-bms/" in f else ("DB2 DCLGEN" if ext(f) == "dcl" else "record/common")
    node("cpy:" + name, "copybook", name, file=f, loc=loc(read(f)), module=module(f), sub=sub)

mapsets = {}
for f in by_ext["bms"]:
    mapsets[stem(f)] = f
    t = read(f)
    maps = re.findall(r"^(\w+)\s+DFHMDI", t, re.M)
    fields = len(re.findall(r"DFHMDF", t))
    node("map:" + stem(f), "screen", stem(f), file=f, loc=loc(t), module=module(f), sub="BMS mapset", maps=maps, fields=fields)
    if stem(f) in copybooks:
        edge("map:" + stem(f), "cpy:" + stem(f), "generates", f, 1, "symbolic map copybook")

jobs = {}
for f in by_ext["jcl"]:
    jobs[stem(f)] = f
    node("job:" + stem(f), "job", stem(f), file=f, loc=loc(read(f)), module=module(f), sub="JCL job")
procs = {}
for f in by_ext["prc"]:
    procs[stem(f)] = f
    node("proc:" + stem(f), "job", stem(f), file=f, loc=loc(read(f)), module=module(f), sub="JCL procedure")

# ---- datasets -----------------------------------------------------------------
def ds_norm(dsn):
    d = dsn.strip().strip("'").upper()
    d = re.sub(r"\((\+|-)?\d+\)$", "", d)
    d = re.sub(r"\.(DATA|INDEX|DAT|IDX)$", "", d)
    member = None
    m = re.match(r"(.+)\(([A-Z0-9#@$]+)\)$", d)
    if m:
        d, member = m.group(1), m.group(2)
    return d, member

SKIP_DS = re.compile(r"LOADLIB|^SYS\d|^CEE\.|^DSN\w*\.|^DFH|^CICS|^IMS\w*\.|PROCLIB|\.JCL$|\.CNTL$|^&&|DBDLIB|PSBLIB|ACBLIB|RESLIB|MACLIB|\.DCLGEN|\.LST|^SYS1|\.DBRMLIB|\.PROC\b|EXITLIB|SDSNLOAD|LINKLIB|RUNLIB|\.CBL$|\.CPY$|\.BMS$|\.LISTING$|\.SYSDEBUG$|SCEE|SDFH|SCSQ|^MQ|^CSQ|^OEM|^NULLFILE|^LBD\.|SDFSRESL")
def ds_node(dsn):
    d, member = ds_norm(dsn)
    if not d or "&" in d and not d.startswith("AWS") or SKIP_DS.search(d):
        return None
    label = re.sub(r"^AWS\.M2\.CARDDEMO\.", "", d)
    label = re.sub(r"^AWS\.M2\.", "", label)
    sub = "VSAM KSDS" if "KSDS" in d else ("VSAM alternate index" if "AIX" in d else ("GDG / sequential" if True else ""))
    if member:
        return None
    node("ds:" + d, "dataset", label, dsn=d, sub=sub)
    return "ds:" + d

# ---- CSD: transactions, CICS files -------------------------------------------
cics_files = {}
for f in by_ext["csd"]:
    lines = read(f).splitlines()
    cur = None
    for i, l in enumerate(lines, 1):
        if l.startswith("*"):
            continue
        m = re.search(r"DEFINE\s+(\w+)\((\w+)\)", l)
        if m:
            cur = (m.group(1).upper(), m.group(2).upper(), i)
            if cur[0] == "TRANSACTION":
                node("txn:" + cur[1], "transaction", cur[1], file=f, line=i, module=module(f), sub="CICS transaction")
        if not cur:
            continue
        if cur[0] == "FILE":
            m = re.search(r"DSNAME\(([^)]+)\)", l)
            if m:
                did = ds_node(m.group(1))
                if did:
                    cics_files[cur[1]] = did
                    nodes[did].setdefault("cicsFile", cur[1])
                    nodes[did].setdefault("file", f); nodes[did].setdefault("line", cur[2])
        if cur[0] == "TRANSACTION":
            m = re.search(r"PROGRAM\((\w+)\)", l)
            if m and m.group(1).upper() in programs:
                edge("txn:" + cur[1], "pgm:" + m.group(1).upper(), "starts", f, i, "CSD DEFINE TRANSACTION")

# ---- COBOL programs -----------------------------------------------------------
cics_ops = {"READ": "reads", "READNEXT": "reads", "READPREV": "reads", "STARTBR": "reads",
            "WRITE": "writes", "REWRITE": "writes", "DELETE": "writes"}
prog_dd = collections.defaultdict(dict)   # program -> ddname -> {mode, line}
txn_names = {n["label"] for n in nodes.values() if n["type"] == "transaction"}

def external(name, kind, label_sub):
    id_ = kind + ":" + name
    node(id_, "external", name, sub=label_sub)
    return id_

for name, f in programs.items():
    pid = "pgm:" + name
    text = read(f)
    cl = code_lines(f, text)
    if ext(f) == "asm":
        continue
    values = {}
    flat = "\n".join(l[7:] if len(l) > 7 else "" for _, l in cl)
    for m in re.finditer(r"^\s*\d+\s+([\w-]+)\s[^.']*?VALUE\s+'([^']*)'", flat, re.M):
        values[m.group(1).upper()] = (m.group(2).strip().upper(), cl[flat.count("\n", 0, m.start())][0])
    selects = {}
    joined = "\n".join(l for _, l in cl)
    for m in re.finditer(r"SELECT\s+([\w-]+)\s+ASSIGN\s+(?:TO\s+)?([\w-]+)", joined):
        selects[m.group(1).upper()] = m.group(2).upper().split("-")[-1]
    in_sql = None
    for idx, (i, l) in enumerate(cl):
        u = l.upper()
        m = re.search(r"^\s{0,12}COPY\s+'?([\w-]+)'?", u[6:] if len(u) > 6 else u) or re.search(r"EXEC\s+SQL\s+INCLUDE\s+(\w+)", u) or re.search(r"^.{6}\s+INCLUDE\s+(\w+)\s*$", u.rstrip())
        if m and m.group(1) in copybooks:
            edge(pid, "cpy:" + m.group(1), "includes", f, i, "COPY " + m.group(1))
        elif m and m.group(1) not in ("SQLCA", "SQLDA") and not m.group(1).startswith(("DFH", "CMQ")) and re.search(r"COPY", u):
            edge(pid, external(m.group(1), "sys", "system copybook (not in repo)"), "includes", f, i, "COPY " + m.group(1))
        m = re.search(r"CALL\s+['\"]([\w$#@-]+)['\"]", u)
        if m:
            c = m.group(1)
            if c in programs:
                edge(pid, "pgm:" + c, "calls", f, i, "CALL '%s'" % c)
            elif c.startswith("MQ"):
                edge(pid, external("IBM MQ", "mq", "message queue API"), "calls", f, i, "CALL '%s'" % c)
            elif c in ("CBLTDLI", "AIBTDLI"):
                edge(pid, external("IMS DL/I", "ims", "IMS database API"), "calls", f, i, "CALL '%s'" % c)
            else:
                edge(pid, external(c, "sys", "runtime service (not in repo)"), "calls", f, i, "CALL '%s'" % c)
        m = re.search(r"CALL\s+([A-Z][\w-]+)\s+USING", u)
        if m and m.group(1) in values and values[m.group(1)][0] in programs:
            edge(pid, "pgm:" + values[m.group(1)][0], "calls", f, i, "CALL " + m.group(1))
        if re.search(r"\bOPEN\s+(INPUT|OUTPUT|I-O|EXTEND)\b", u):
            # one OPEN can name several files and several modes, across lines
            toks = []
            for j in range(idx, min(idx + 8, len(cl))):
                seg = cl[j][1].upper()[7:]
                toks += re.findall(r"[\w-]+|\.", seg.split("OPEN", 1)[1] if j == idx else seg)
            mode = None
            for tk in toks:
                if tk in ("INPUT", "OUTPUT", "I-O", "EXTEND"):
                    mode = tk
                elif tk in selects and mode:
                    prog_dd[name].setdefault(selects[tk], {"mode": mode, "line": i})
                else:
                    break
        if re.search(r"EXEC\s+DLI|EXEC\s+DL/I", u):
            edge(pid, external("IMS DL/I", "ims", "IMS database API"), "calls", f, i, "EXEC DLI")
        # CICS command blocks
        if re.search(r"EXEC\s+CICS", u):
            blk = []
            for j in range(idx, min(idx + 14, len(cl))):
                blk.append(cl[j][1].upper())
                if "END-EXEC" in cl[j][1].upper():
                    break
            b = " ".join(blk)
            verb = re.search(r"EXEC\s+CICS\s+(\w+)", b)
            verb = verb.group(1) if verb else ""
            def resolve(arg):
                arg = arg.strip()
                if arg.startswith("'"):
                    return arg.strip("'").strip()
                return values.get(arg, (None,))[0]
            m = re.search(r"(?:DATASET|FILE)\s*\(\s*([^)]+)\)", b)
            if m and verb in cics_ops:
                fn = resolve(m.group(1))
                if fn in cics_files:
                    k = cics_ops[verb]
                    if k == "reads":
                        edge(cics_files[fn], pid, "reads", f, i, "EXEC CICS %s FILE(%s)" % (verb, fn))
                    else:
                        edge(pid, cics_files[fn], "writes", f, i, "EXEC CICS %s FILE(%s)" % (verb, fn))
            m = re.search(r"MAPSET\s*\(\s*([^)]+)\)", b)
            if m:
                ms = resolve(m.group(1))
                if ms in mapsets:
                    edge(pid, "map:" + ms, "displays", f, i, "EXEC CICS %s MAPSET(%s)" % (verb, ms))
            m = re.search(r"PROGRAM\s*\(\s*([^)]+)\)", b)
            if m and verb in ("XCTL", "LINK"):
                tp = resolve(m.group(1))
                if tp in programs:
                    edge(pid, "pgm:" + tp, "transfers", f, i, "EXEC CICS %s PROGRAM(%s)" % (verb, tp))
            if verb in ("WRITEQ", "READQ") :
                q = "TDQ" if " TD " in b else "TSQ"
                m = re.search(r"QUEUE\s*\(\s*([^)]+)\)", b)
                qn = resolve(m.group(1)) if m else None
                if qn:
                    qid = external(qn, "q", "CICS %s queue" % ("transient data" if q == "TDQ" else "temp storage"))
                    if verb == "WRITEQ":
                        edge(pid, qid, "writes", f, i, "EXEC CICS WRITEQ %s" % q)
                    else:
                        edge(qid, pid, "reads", f, i, "EXEC CICS READQ %s" % q)
        # embedded SQL
        if re.search(r"EXEC\s+SQL", u):
            blk = []
            for j in range(idx, min(idx + 40, len(cl))):
                blk.append(cl[j][1].upper())
                if "END-EXEC" in cl[j][1].upper():
                    break
            b = " ".join(blk)
            for rx, k in ((r"\bFROM\s+([A-Z][\w.]+)", "reads"), (r"\bINSERT\s+INTO\s+([A-Z][\w.]+)", "writes"),
                          (r"\bUPDATE\s+([A-Z][\w.]+)", "writes"), (r"\bDELETE\s+FROM\s+([A-Z][\w.]+)", "writes")):
                for m in re.finditer(rx, b):
                    tname = m.group(1).rstrip(".")
                    if tname in ("SYSIBM.SYSDUMMY1",) or "." not in tname and len(tname) < 4:
                        continue
                    tid = "db2:" + tname
                    node(tid, "dataset", tname.split(".")[-1], dsn=tname, sub="DB2 table")
                    if k == "reads" and not re.search(r"DELETE\s+FROM\s+" + re.escape(tname), b):
                        edge(tid, pid, "reads", f, i, "EXEC SQL")
                    elif k == "writes":
                        edge(pid, tid, "writes", f, i, "EXEC SQL")
    # literal-driven transfers: program names held in WORKING-STORAGE VALUE clauses or MOVE literals
    if nodes[pid]["sub"] == "online":
        for i, l in cl:
            for m in re.finditer(r"'([A-Z0-9]{6,8})\s*'", l.upper()):
                tp = m.group(1)
                if tp in programs and tp != name and (pid, "pgm:" + tp, "transfers") not in edges and (pid, "pgm:" + tp, "calls") not in edges:
                    edge(pid, "pgm:" + tp, "transfers", f, i, "program literal '%s' (XCTL target)" % tp)
            for m in re.finditer(r"'([A-Z0-9]{4,8})\s*'", l.upper()):
                lit = m.group(1)
                if lit in cics_files:
                    k1 = (cics_files[lit], pid, "reads"); k2 = (pid, cics_files[lit], "writes")
                    if k1 not in edges and k2 not in edges:
                        edge(cics_files[lit], pid, "reads", f, i, "file literal '%s'" % lit)

# ---- JCL ----------------------------------------------------------------------
UTIL = {"IDCAMS": "VSAM utility", "SORT": "DFSORT", "IEFBR14": "allocation utility", "IEBGENER": "copy utility",
        "SDSF": "operator commands", "IKJEFT01": "TSO/DB2 batch", "IKJEFT1B": "TSO/DB2 batch", "DFSRRC00": "IMS batch region",
        "DSNUTILB": "DB2 utility", "FTP": "file transfer", "IEBCOPY": "copy utility", "DFHCSDUP": "CICS CSD utility", "BPXBATCH": "USS batch"}
def parse_jcl(kind, name, f):
    jid = kind + ":" + name
    text = read(f)
    step = None; pgm = None; steps = []
    cl = code_lines(f, text)
    cur_dd = None
    RUNRX = r"RUN\s+PROGRAM\((\w+)\)|PARM='?\(?(?:BMP|DLI|DBB),(\w+)"
    # whole DD statement (with its continuation lines) for every line that belongs to one
    stmt_of = {}
    k = 0
    while k < len(cl):
        if re.match(r"//\S+\s+DD\s", cl[k][1]):
            j = k + 1
            while j < len(cl) and re.match(r"//\s+\S", cl[j][1]) and not re.search(r"\sEXEC\s", cl[j][1]):
                j += 1
            joined = " ".join(x[1] for x in cl[k:j])
            for q in range(k, j):
                stmt_of[q] = joined
            k = j
        else:
            k += 1
    # the application program a utility step runs (IMS region PARM, DB2 RUN PROGRAM)
    step_prog = {}
    exec_idx = [q for q, (_, x) in enumerate(cl) if re.match(r"//\S*\s+EXEC\s", x)]
    for n_, q in enumerate(exec_idx):
        end = exec_idx[n_ + 1] if n_ + 1 < len(exec_idx) else len(cl)
        for _, x in cl[q:end]:
            m2 = re.search(RUNRX, x.upper())
            if m2 and (m2.group(1) or m2.group(2)) in programs:
                step_prog[q] = m2.group(1) or m2.group(2)
                break
    for idx, (i, l) in enumerate(cl):
        m = re.match(r"//(\S*)\s+EXEC\s+(?:PGM=([\w&]+)|(?:PROC=)?(\w+))", l)
        if m:
            step = m.group(1); pgm = (m.group(2) or "").upper(); pr = (m.group(3) or "").upper()
            run_as = pgm if pgm in programs else step_prog.get(idx)
            steps.append({"step": step, "pgm": pgm or pr, "line": i})
            if pgm in programs:
                edge(jid, "pgm:" + pgm, "runs", f, i, "EXEC PGM=" + pgm)
            elif pgm in UTIL:
                edge(jid, external(pgm, "util", UTIL[pgm]), "runs", f, i, "EXEC PGM=" + pgm)
            elif pr in procs:
                edge(jid, "proc:" + pr, "runs", f, i, "EXEC " + pr)
            continue
        m = re.match(r"//(\S+)\s+DD\s", l)
        if m:
            cur_dd = m.group(1).split(".")[-1]
        # in-stream RUN PROGRAM(x) for DB2 / IMS region PARMs naming a program
        for m2 in re.finditer(r"PROGRAM\((\w+)\)|PARM='?\(?(?:BMP|DLI|DBB),(\w+)", l.upper()):
            p = m2.group(1) or m2.group(2)
            if p in programs:
                isrun = "RUN" in l.upper() or "PARM=" in l.upper()
                edge(jid, "pgm:" + p, "runs" if isrun else "defines", f, i, ("runs " if isrun else "CSD definition for ") + p)
        m = re.search(r"DSN=([A-Z0-9.&#@$()+\-]+)", l.upper())
        if m and step is not None:
            did = ds_node(m.group(1))
            if not did:
                continue
            dm = re.search(r"DISP=\(?(\w+)(?:,(\w+))?", stmt_of.get(idx, l).upper())
            disp = dm.group(1) if dm else "SHR"
            if dm and dm.group(2) == "DELETE" and not run_as:
                continue  # a cleanup step that deletes the dataset neither reads nor writes it
            mode = None
            if run_as and cur_dd in prog_dd.get(run_as, {}):
                mode = prog_dd[run_as][cur_dd]["mode"]
            tgt_prog = "pgm:" + run_as if run_as else None
            detail = "%s DD %s DISP=%s" % (step or "", cur_dd, disp)
            writes = mode in ("OUTPUT", "EXTEND", "I-O") if mode else disp in ("NEW", "MOD") or (cur_dd or "").startswith(("SORTOUT", "SYSUT2", "OUT"))
            reads = mode in ("INPUT", "I-O") if mode else not writes
            actor = tgt_prog or jid
            if writes:
                edge(actor, did, "writes", f, i, detail)
            if reads:
                edge(did, actor, "reads", f, i, detail)
        # IDCAMS control statements naming datasets
        m = re.search(r"(?:NAME|INDATASET|OUTDATASET|RELATE|PATHENTRY)\s*\(\s*([A-Z0-9.]+)", l.upper())
        if m and not l.startswith("//") and "." in m.group(1):
            did = ds_node(m.group(1))
            if did:
                edge(jid, did, "defines", f, i, "IDCAMS control statement")
    nodes[jid]["steps"] = steps
for name, f in jobs.items():
    parse_jcl("job", name, f)
for name, f in procs.items():
    parse_jcl("proc", name, f)

# programs with DD assignments never resolved through JCL: keep the DD name as evidence
for name, dds in prog_dd.items():
    nodes["pgm:" + name]["dds"] = [{"dd": d, **v} for d, v in dds.items()]

# ---- Scheduler (Control-M) ----------------------------------------------------
sched = []
for f in by_ext.get("controlm", []):
    try:
        root = ET.fromstring(read(f))
    except ET.ParseError:
        continue
    text_lines = read(f).splitlines()
    def line_of(s):
        for i, l in enumerate(text_lines, 1):
            if s in l:
                return i
    for folder in root.iter("FOLDER"):
        out_of = {}
        fj = []
        for j in folder.iter("JOB"):
            jn = j.get("JOBNAME", "").upper()
            fj.append(jn)
            for oc in j.findall("OUTCOND"):
                if oc.get("SIGN") == "+":
                    out_of[oc.get("NAME")] = jn
        for j in folder.iter("JOB"):
            jn = j.get("JOBNAME", "").upper()
            for ic in j.findall("INCOND"):
                src = out_of.get(ic.get("NAME"))
                if src and "job:" + src in nodes and "job:" + jn in nodes:
                    edge("job:" + src, "job:" + jn, "triggers", f, line_of('JOBNAME="%s"' % j.get("JOBNAME")), "Control-M condition " + ic.get("NAME"))
        sched.append({"folder": folder.get("FOLDER_NAME"), "jobs": fj})
        for jn in fj:
            if "job:" + jn in nodes:
                nodes["job:" + jn].setdefault("schedule", folder.get("FOLDER_NAME"))

# ---- copybook nesting ---------------------------------------------------------
for name, f in copybooks.items():
    for i, l in code_lines(f, read(f)):
        m = re.search(r"^\s*COPY\s+'?([\w-]+)", l[6:].upper() if len(l) > 6 else "")
        if m and m.group(1) in copybooks and m.group(1) != name:
            edge("cpy:" + name, "cpy:" + m.group(1), "includes", f, i, "COPY " + m.group(1))

# drop externals/datasets with no edges
used = set()
for (s, t, k) in edges:
    used.add(s); used.add(t)
for k in [k for k, n in nodes.items() if k not in used and n["type"] in ("external", "dataset")]:
    del nodes[k]

# ---- file inventory -----------------------------------------------------------
LANG = {"cbl": "COBOL", "cpy": "Copybook", "jcl": "JCL", "bms": "BMS map", "asm": "Assembler", "csd": "CICS CSD", "prc": "JCL proc",
        "ctl": "Control card", "ddl": "DB2 DDL", "dcl": "DB2 DCLGEN", "dbd": "IMS DBD", "psb": "IMS PSB", "controlm": "Control-M", "ca7": "CA-7",
        "mac": "Macro", "sh": "Shell", "md": "Markdown", "txt": "Text", "ps": "Data", "awk": "AWK"}
inv = []
node_by_file = {n["file"]: n["id"] for n in nodes.values() if n.get("file") and n["type"] not in ("transaction", "dataset")}
for f in files:
    p = os.path.join(REPO, f)
    size = os.path.getsize(p)
    with open(p, "rb") as fh:
        head = fh.read(4096)
    binary = b"\0" in head or f.startswith("app/data/EBCDIC") or "/data/EBCDIC/" in f or ext(f) in ("png", "jpg", "zip", "drawio", "gif")
    lines = 0 if binary else read(f).count("\n") + 1
    inv.append({"path": f, "size": size, "lines": lines, "binary": binary, "lang": LANG.get(ext(f), ext(f).upper() or "Text"), "node": node_by_file.get(f)})

# ---- findings -----------------------------------------------------------------
raw = open(os.path.join(APP, "raw", "findings.txt")).read()
findings = []
name_index = {}
for n in nodes.values():
    if n["type"] in ("program", "job", "screen", "copybook", "transaction"):
        name_index.setdefault(n["label"], []).append(n["id"])
file_set = set(files)
hdr = re.compile(r"^- (sfind-\w+): (.*) \[(\w+)/(\w+)\] repo: (\S+), scan: (\S+) created ([\d\- :]+) UTC, category: (\S+)")
for part in re.split(r"\n(?=- sfind-)", raw):
    m = hdr.match(part)
    if not m:
        continue
    body = part[m.end():]
    body = "\n".join(l[4:] if l.startswith("    ") else l for l in body.split("\n"))
    fields = {}
    cur = None
    for l in body.split("\n"):
        fm = re.match(r"^  (Description|Recommendation|Note|Code owners|Scan session|Snippet|File|Location|Line|Path): ?(.*)$", l)
        if fm:
            cur = fm.group(1); fields[cur] = fm.group(2)
        elif cur:
            fields[cur] += "\n" + l
    text = part
    paths = [p for p in dict.fromkeys(re.findall(r"(?:app|scripts|diagrams)/[\w./\-]+\.\w+", text)) if p in file_set]
    title = m.group(2)
    nids = []
    for p in paths:
        if p in node_by_file and node_by_file[p] not in nids:
            nids.append(node_by_file[p])
    primary = []
    for w in re.findall(r"[A-Z][A-Z0-9]{3,7}", title):
        for nid in name_index.get(w, []):
            if nid not in primary:
                primary.append(nid)
    if not primary and not nids:
        for w in dict.fromkeys(re.findall(r"`([A-Z][A-Z0-9]{3,7})`", fields.get("Description", "")[:500])):
            for nid in name_index.get(w, []):
                if nid not in primary:
                    primary.append(nid)
    merged = re.findall(r"sfind-\w+", fields.get("Note", ""))
    findings.append({
        "id": m.group(1), "title": title, "severity": m.group(3), "status": m.group(4), "created": m.group(7).strip(),
        "category": m.group(8), "description": fields.get("Description", "").strip(), "recommendation": fields.get("Recommendation", "").strip(),
        "note": fields.get("Note", "").strip(), "owners": fields.get("Code owners", "").strip(), "snippet": fields.get("Snippet", "").strip("\n"),
        "paths": paths, "nodes": list(dict.fromkeys(primary + nids)), "primary": primary or nids[:1], "related": [x for x in dict.fromkeys(merged) if x != m.group(1)],
    })

# ---- compliance scan (a different COBOL repository; shown separately) ----------
compliance = []
craw = open(os.path.join(APP, "raw", "compliance.txt")).read()
for part in re.split(r"\n(?=- sfind-)", craw):
    m = hdr.match(part)
    if not m:
        continue
    body = "\n".join(l[4:] if l.startswith("    ") else l for l in part[m.end():].split("\n"))
    fields = collections.defaultdict(list); cur = None
    for l in body.split("\n"):
        fm = re.match(r"^  (Description|Recommendation|Note|Code owners|Scan session|Snippet): ?(.*)$", l)
        if fm:
            cur = fm.group(1); fields[cur].append(fm.group(2))
        elif cur:
            fields[cur][-1] += "\n" + l
    desc = (fields["Description"] or [""])[0].strip()
    sec = re.search(r"##### (?:Applicability|Bears on)\n(.*?)(?=\n##### |\Z)", desc, re.S)
    ctl = sorted(set(re.findall(r"\b((?:AC|AU|CM|IA|SA|SC|SI|SR)-\d{1,2}(?:\(\d+\))?)", sec.group(1) if sec else desc)))
    snips = []
    for sn in fields["Snippet"]:
        loc_ = sn.split("\n")[0].strip()
        cm = re.search(r"Commentary: (.*)", sn)
        code = sn.split("Code:\n", 1)[1] if "Code:\n" in sn else ""
        snips.append({"loc": loc_, "commentary": cm.group(1).strip() if cm else "", "code": code.rstrip()})
    compliance.append({"id": m.group(1), "title": m.group(2), "severity": m.group(3), "status": m.group(4), "category": m.group(8),
                       "controls": ctl, "families": sorted({c.split("-")[0] for c in ctl}), "description": desc,
                       "recommendation": (fields["Recommendation"] or [""])[0].strip(), "note": (fields["Note"] or [""])[0].strip(), "snippets": snips})
json.dump({"scan": "scan-ac0861710f1d401ca6912252566f41bb", "repo": "COG-GTM/COBOL-Legacy-Benchmark-Suite",
           "profile": "COBOL Legacy Benchmark Suite - NIST 800-53 Rev 5 High (code-owned)", "catalogControls": 80,
           "url": "https://devin-gtm.devinenterprise.com/code-scan/ac0861710f1d401ca6912252566f41bb/summary", "findings": compliance},
          open(os.path.join(OUT, "compliance.json"), "w"))
print("compliance", len(compliance), collections.Counter(f for c in compliance for f in c["families"]), "no-control", sum(1 for c in compliance if not c["controls"]))

# ---- business rules catalog (branch cd-107) -----------------------------------
rules = []
BR = "origin/devin/cd-107-business-rules-catalog"
rule_docs = sh("git", "ls-tree", "-r", "--name-only", BR, "modernization/java21/docs/business-rules/").splitlines()
if not rule_docs:
    sys.exit("rules branch %s is not available; fetch it before rebuilding (see README)" % BR)
for doc in rule_docs:
    if not doc.endswith(".md") or os.path.basename(doc) in ("README.md", "SME-CHECKLIST.md"):
        continue
    md = sh("git", "show", BR + ":" + doc)
    prog = stem(doc)
    for l in md.splitlines():
        cells = [c.strip() for c in l.strip().strip("|").split("|")]
        if len(cells) >= 4 and re.match(r"^`?[A-Z]+-\d+`?$", cells[0]):
            rules.append({"id": cells[0].strip("`"), "program": prog, "cells": cells[1:], "doc": doc, "branch": BR.replace("origin/", "")})
    hdrs = [[c.strip() for c in l.strip().strip("|").split("|")] for l in md.splitlines() if re.match(r"^\|\s*(ID|Rule)", l)]
    for r in rules:
        if r["program"] == prog and hdrs:
            r["columns"] = hdrs[0][1:]

# ---- PRs ----------------------------------------------------------------------
prs = json.load(open(os.path.join(APP, "raw", "prs.json")))
prog_names = sorted(list(programs) + list(jobs), key=len, reverse=True)
pr_out = []
for p in prs:
    def expand(t):
        # "COUSR00C-03C" and "COUSR00C/01C/02C" name every program in the range or list
        def rng(rm):
            lo, hi = int(rm.group(2)), int(rm.group(4))
            if not 0 < hi - lo < 20:
                return rm.group(0)
            return " ".join("%s%0*d%s" % (rm.group(1), len(rm.group(2)), v, rm.group(3)) for v in range(lo, hi + 1))
        t = re.sub(r"(?<![A-Z0-9])([A-Z]+)(\d+)([A-Z]?)-(\d+)\3(?![A-Z0-9])", rng, t)
        return re.sub(r"(?<![A-Z0-9])([A-Z]+)(\d+[A-Z]?)((?:/\d+[A-Z]?)+)(?![A-Z0-9])",
                      lambda rm: " ".join(rm.group(1) + v for v in [rm.group(2)] + rm.group(3).strip("/").split("/")), t)
    def named(t):
        found = [("pgm:" if n in programs else "job:") + n for n in prog_names if re.search(r"(?<![A-Z0-9])" + re.escape(n) + r"(?![A-Z0-9])", t)]
        # a PR about a job is evidence for the programs that job runs, and the subprograms those call
        via = [t2 for h_ in found if h_.startswith("job:") for (s_, t2, k_) in edges if s_ == h_ and k_ == "runs" and t2.startswith("pgm:")]
        via += [t2 for h_ in via for (s_, t2, k_) in edges if s_ == h_ and k_ == "calls" and t2.startswith("pgm:")]
        return found + via
    # each program is attributed to the clause of the title that names it ("A parity; B submission")
    segs = p["title"].split(";")
    about = {}
    for seg in segs:
        for h_ in named(expand(seg.upper())):
            about.setdefault(h_, seg.strip())
    hits = list(dict.fromkeys(list(about) + named(expand(p["headRefName"].upper()))))
    if len(segs) == 1:
        about = {}
    pr_out.append({"number": p["number"], "title": p["title"], "state": p["state"], "branch": p["headRefName"], "url": p["url"],
                   "base": p.get("baseRefName"), "about": about, "created": p["createdAt"], "merged": p.get("mergedAt"), "additions": p.get("additions"), "deletions": p.get("deletions"), "nodes": sorted(set(hits))})

# ---- metrics & approaches -----------------------------------------------------
deg_in = collections.Counter(); deg_out = collections.Counter()
for (s, t, k) in edges:
    deg_out[s] += 1; deg_in[t] += 1
f_by_node = collections.defaultdict(list)
for fd in findings:
    for nid in fd["nodes"]:
        f_by_node[nid].append(fd)
pr_by_node = collections.defaultdict(list)
for p in pr_out:
    for nid in p["nodes"]:
        pr_by_node[nid].append(p)
rules_by_prog = collections.Counter(r["program"] for r in rules)

SEC = {"COSGN00C", "COUSR00C", "COUSR01C", "COUSR02C", "COUSR03C"}
UTILP = {"CSUTLDTC", "COBDATFT", "MVSWAIT", "COBSWAIT", "CODATE01"}
def approach(n):
    name = n["label"]; tech = n.get("tech", []); sub = n.get("sub")
    out_k = collections.Counter(k for (s, t, k) in edges if s == n["id"])
    in_k = collections.Counter(k for (s, t, k) in edges if t == n["id"])
    why = []
    if name in SEC:
        a = "replace"
        why.append("Sign-on and user administration are commodity identity functions; the program keeps its own user file rather than an enterprise directory.")
        tgt = "Agency or commercial identity provider (COTS/GOTS IAM), with a thin adapter for existing user types"
    elif name in UTILP or sub == "assembler":
        a = "replace"
        why.append("Utility wrapper around a runtime service (date conversion, date validation or a timed wait); a standard library call covers it.")
        tgt = "Platform library call (java.time, scheduler delay); no bespoke code carried forward"
    elif "IMS" in tech or "MQ" in tech:
        a = "rewrite"
        why.append("Depends on %s, so the data access and messaging model changes with the target platform rather than translating line for line." % " and ".join(t for t in tech if t in ("IMS", "MQ", "DB2")))
        tgt = "Event-driven service on the target messaging and relational store, behaviour pinned by characterization tests"
    elif sub == "online":
        a = "refactor"
        why.append("Pseudo-conversational CICS program: screen handling (%d BMS mapset link%s) is separable from the validation and file logic." % (out_k["displays"], "" if out_k["displays"] == 1 else "s"))
        tgt = "Service + API holding the business rules, with a web UI replacing the 3270 map"
    else:
        a = "refactor"
        why.append("Batch program with explicit file contracts (%d dataset read%s, %d write%s), which makes byte-level equivalence testing against the COBOL practical." % (in_k["reads"], "" if in_k["reads"] == 1 else "s", out_k["writes"], "" if out_k["writes"] == 1 else "s"))
        tgt = "Java 21 batch step (Spring Batch) with copybook codecs, verified against a GnuCOBOL golden master"
    fs = f_by_node.get(n["id"], [])
    crit = sum(1 for x in fs if x["severity"] in ("critical", "high"))
    coupling = deg_in[n["id"]] + deg_out[n["id"]]
    score = min(100, round(n.get("loc", 0) / 45 + coupling * 1.5 + crit * 4 + (15 if ("IMS" in tech or "MQ" in tech) else 0)))
    effort = "low" if score < 30 else ("medium" if score < 60 else "high")
    why.append("%d lines, %d connections in the estate graph, %d scan finding%s (%d high or critical)." % (n.get("loc", 0), coupling, len(fs), "" if len(fs) == 1 else "s", crit))
    alt = {"refactor": "Replatform as-is (rehost the COBOL on an emulation runtime) if the deadline outweighs the long-term cost of keeping COBOL.",
           "rewrite": "Replatform the module unchanged first, then rewrite behind a stable interface.",
           "replace": "Refactor to a small in-house service if no suitable product fits the security or accreditation boundary."}[a]
    return {"approach": a, "target": tgt, "why": why, "alternative": alt, "complexity": score, "effort": effort}

for n in nodes.values():
    n["in"] = deg_in[n["id"]]; n["out"] = deg_out[n["id"]]
    n["findings"] = len(f_by_node.get(n["id"], []))
    if n["type"] == "program":
        n["approach"] = approach(n)
        n["rules"] = rules_by_prog.get(n["label"], 0)
        n["prs"] = len(pr_by_node.get(n["id"], []))
    elif n["type"] == "job":
        n["prs"] = len(pr_by_node.get(n["id"], []))
        runs = [t for (s, t, k) in edges if s == n["id"] and k == "runs"]
        if runs and all(t.startswith("util:") for t in runs):
            n["approach"] = {"approach": "retire", "target": "Platform operations: schema migrations, data loads and backups replace VSAM define/REPRO/sort steps",
                             "why": ["Runs only system utilities (%s); it manages storage that does not exist on the target platform." % ", ".join(sorted({t.split(':')[1] for t in runs}))],
                             "alternative": "Keep as-is under a replatform, where the emulation runtime provides the same utilities.", "complexity": min(40, n.get("loc", 0) // 6), "effort": "low"}
        else:
            n["approach"] = {"approach": "refactor", "target": "Orchestrated batch job (job definition + scheduler entry) invoking the migrated program steps",
                             "why": ["Job control wraps %d program step%s; the step order and dataset hand-offs are the contract to preserve." % (len([t for t in runs if t.startswith('pgm:')]), "" if len([t for t in runs if t.startswith('pgm:')]) == 1 else "s")],
                             "alternative": "Replatform the JCL unchanged on an emulation runtime.", "complexity": min(60, n.get("loc", 0) // 4), "effort": "low" if n.get("loc", 0) < 120 else "medium"}

comp3 = sum(len(re.findall(r"COMP-3", read(f).upper())) for f in files if ext(f) in ("cpy", "cbl"))
lang_loc = collections.Counter()
for x in inv:
    if x["path"].startswith("app/") and not x["binary"] and "/data/" not in x["path"]:
        lang_loc[x["lang"]] += x["lines"]
estate = {"stats": {"comp3": comp3, "linesByLang": dict(lang_loc)}, "nodes": list(nodes.values()), "edges": list(edges.values()), "files": inv, "schedule": sched,
          "meta": {"repo": "COG-GTM/aws-mainframe-modernization-carddemo", "commit": sh("git", "log", "-1", "--format=%h", "--", "app").strip(),
                   "scan": "scan-dcf9da1645664298850b3547a8b50fca", "scanProfile": "CardDemo COBOL-to-Java 21 factory discovery",
                   "rulesBranch": BR.replace("origin/", "")}}
json.dump(estate, open(os.path.join(OUT, "estate.json"), "w"))
json.dump(findings, open(os.path.join(OUT, "findings.json"), "w"))
json.dump(rules, open(os.path.join(OUT, "rules.json"), "w"))
json.dump(pr_out, open(os.path.join(OUT, "prs.json"), "w"))

tc = collections.Counter(n["type"] for n in nodes.values()); ec = collections.Counter(e["kind"] for e in edges.values())
print("nodes", dict(tc)); print("edges", dict(ec), sum(ec.values()))
print("files", len(inv), "findings", len(findings), "linked", sum(1 for f in findings if f["nodes"]), "rules", len(rules), "prs", len(pr_out), "pr-linked", sum(1 for p in pr_out if p["nodes"]))
iso = [n["id"] for n in nodes.values() if n["in"] + n["out"] == 0]
print("isolated", len(iso), iso[:40])
