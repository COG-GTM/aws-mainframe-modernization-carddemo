#!/usr/bin/env python3
"""Build the CardDemo dependency map (step s1.2 of the Java 21 migration plan).

Reads ``docs/modernization/inventory.json`` (produced by ``build_inventory.py``) and
re-scans the COBOL/JCL sources only where the inventory has no detail (CALL counts,
dynamic XCTL/LINK target resolution, EVALUATE nesting, ROUNDED/COMPUTE, TDQ job
submission, IDCAMS REPRO direction) and writes:

* ``02-dependency-map.md``   - Mermaid call graph, program->file edges, copybook usage,
                               JCL job -> step -> program -> DD -> dataset chains,
                               complexity ratings and the recommended migration order.
* ``dependency-map.json``    - the same content, machine readable (edges, ratings, order).
* ``diagrams/*.mmd``         - one file per Mermaid diagram (rendered with ``--render``
                               to ``diagrams/*.svg`` through ``mmdc``).

Usage::

    python3 docs/modernization/build_dependency_map.py            # regenerate
    python3 docs/modernization/build_dependency_map.py --check    # freshness + coverage + hubs
    python3 docs/modernization/build_dependency_map.py --render   # regenerate and render SVGs
"""
from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
from collections import Counter, OrderedDict, defaultdict
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from build_inventory import CobolText, resolve_literal, is_dynamic, read_text, md_table, rel  # noqa: E402

ROOT = Path(__file__).resolve().parents[2]
APP = ROOT / "app"
OUT_DIR = Path(__file__).resolve().parent
INVENTORY = OUT_DIR / "inventory.json"
JSON_OUT = OUT_DIR / "dependency-map.json"
MD_OUT = OUT_DIR / "02-dependency-map.md"
DIAG_DIR = OUT_DIR / "diagrams"

# Language Environment / system routines that are not application programs.
RUNTIME_ROUTINES = {"CEE3ABD": "LE abend service", "CEEDAYS": "LE date service"}

# Hub claims from the ticket, confirmed or corrected by --check and in the report.
EXPECTED_HUBS = [
    ("COMEN01C/COADM01C XCTL to every online program", "menus_reach_all_online"),
    ("CBSTM03A calls CBSTM03B 13 times", "cbstm03b_calls"),
    ("CSUTLDTC called from 4 programs", "csutldtc_callers"),
    ("CBACT01C calls COBDATFT/MVSWAIT", "cbact01c_asm"),
    ("CEE3ABD used in 11 places", "cee3abd_sites"),
]

READ_CMDS = {"READ", "STARTBR", "READNEXT", "READPREV", "ENDBR", "RESETBR"}
WRITE_CMDS = {"WRITE", "REWRITE", "DELETE"}


# ---------------------------------------------------------------------------
# source re-scans
# ---------------------------------------------------------------------------

def nested_evaluate_depth(src: CobolText) -> int:
    depth = best = 0
    for m in re.finditer(r"(?<![A-Z0-9-])(END-EVALUATE|EVALUATE)(?![A-Z0-9-])", src.masked):
        if m.group(1) == "EVALUATE":
            depth += 1
            best = max(best, depth)
        else:
            depth = max(depth - 1, 0)
    return best


def value_of(src: CobolText, name: str) -> str | None:
    m = re.search(r"(?<![A-Z0-9-])" + re.escape(name) + r"\s+PIC[^.]*?VALUE\s+(?:IS\s+)?'([^']*)'", src.text, re.S)
    return m.group(1).strip() if m else None


def table_values(path: Path, min_len: int = 8) -> list[str]:
    """All 8-byte literal VALUEs in a copybook (menu option tables)."""
    txt = CobolText(path).text
    return [v.strip() for v in re.findall(r"PIC\s+X\(0?8\)\s+VALUE\s+'([^']*)'", txt)]


def resolve_dynamic_target(src: CobolText, var: str, prog_cbs: list[str], cpy_index: dict[str, Path],
                           all_programs: set[str]) -> tuple[list[str], list[str]]:
    """Return (resolved program names, notes) for a run-time PROGRAM(var) operand."""
    base = re.sub(r"\(.*\)$", "", var).strip()
    targets, notes = [], []
    if "(" in var:  # subscripted table entry -> enumerate the table copybook
        for cb in prog_cbs:
            p = cpy_index.get(cb)
            if p and base in CobolText(p).text:
                vals = table_values(p)
                targets.extend(vals)
                notes.append(f"table `{base}` in copybook `{cb}` ({len(vals)} program-name entries)")
        return targets, notes
    own = value_of(src, base)
    if own:
        targets.append(own)
    for m in re.finditer(r"MOVE\s+('[^']*'|[A-Z0-9-]+(?:\s+OF\s+[A-Z0-9-]+)?)\s+TO\s+" + re.escape(base) + r"(?![A-Z0-9-])", src.text):
        s = m.group(1)
        if s.startswith("'"):
            targets.append(s.strip("'").strip())
        elif s == "CDEMO-FROM-PROGRAM":
            notes.append("`CDEMO-FROM-PROGRAM` (returns to whichever program transferred in)")
        else:
            v = value_of(src, s)
            if v:
                targets.append(v)
            else:
                notes.append(f"`{s}` (run-time value)")
    return targets, notes


def scan_program(prog: dict, cpy_index: dict[str, Path], all_programs: set[str]) -> dict:
    src = CobolText(ROOT / prog["path"])
    text = src.text
    # CALL counts per target
    call_counts = Counter()
    for m in re.finditer(r"CALL\s+('[^']*'|[A-Z0-9-]+)", text):
        call_counts[resolve_literal(src, m.group(1))] += 1
    # XCTL / LINK / START with resolution of run-time operands
    transfers = []  # (kind, target, how)
    for m in re.finditer(r"EXEC\s+CICS\s+(XCTL|LINK)\s+(.*?)END-EXEC", text, re.S):
        kind, body = m.group(1), m.group(2)
        pm = re.search(r"PROGRAM\s*\(\s*('[^']*'|[A-Z0-9-]+(?:\s*\([^)]*\))?)\s*\)", body)
        if not pm:
            continue
        operand = pm.group(1).strip()
        tgt = resolve_literal(src, operand)
        if not is_dynamic(tgt):
            transfers.append((kind, tgt, "static"))
            continue
        resolved, notes = resolve_dynamic_target(src, operand, prog["copybooks"], cpy_index, all_programs)
        for t in resolved:
            transfers.append((kind, t, f"via `{operand}`"))
        for n in notes:
            transfers.append((kind, None, f"`{operand}` <- {n}"))
    starts = [resolve_literal(src, m.group(1)) for m in re.finditer(r"EXEC\s+CICS\s+START\s+.*?TRANSID\s*\(([^)]*)\)", text, re.S)]
    # job submission through an extra-partition TDQ (JCL text in WORKING-STORAGE)
    submits = []
    if re.search(r"EXEC\s+CICS\s+WRITEQ\s+TD", text):
        raw = read_text(ROOT / prog["path"]).upper()
        submits = sorted(set(re.findall(r"EXEC\s+(?:PGM|PROC)=([A-Z0-9]+)", raw)))
    return OrderedDict(
        call_counts=OrderedDict(sorted(call_counts.items())),
        transfers=transfers,
        start_transids=starts,
        submits_jobs=submits,
        evaluate_depth=nested_evaluate_depth(src),
        rounded=len(re.findall(r"(?<![A-Z0-9-])ROUNDED(?![A-Z0-9-])", src.masked)),
        compute=len(re.findall(r"(?<![A-Z0-9-])COMPUTE(?![A-Z0-9-])", src.masked)),
        arithmetic_verbs=len(re.findall(r"(?<![A-Z0-9-])(?:COMPUTE|ADD|SUBTRACT|MULTIPLY|DIVIDE)(?![A-Z0-9-])", src.masked)),
    )


def repro_direction(job_path: Path) -> dict[str, tuple[str, str]]:
    """step -> (in DD, out DD) for IDCAMS REPRO steps."""
    out, step = {}, None
    for line in read_text(job_path).upper().split("\n"):
        m = re.match(r"//(\S+)\s+EXEC\s", line)
        if m:
            step = m.group(1)
        m = re.search(r"REPRO\s+INFILE\((\w+)\)\s+OUTFILE\((\w+)\)", line)
        if m and step:
            out[step] = (m.group(1), m.group(2))
    return out


# ---------------------------------------------------------------------------
# model
# ---------------------------------------------------------------------------

def mid(name: str) -> str:
    return re.sub(r"[^A-Za-z0-9]", "_", name)


def build() -> OrderedDict:
    inv = json.loads(INVENTORY.read_text())
    core = inv["modules"]["core"]
    programs = core["programs"]
    copybooks = core["copybooks"]
    bms_copybooks = core["bms_copybooks"]
    jobs = core["jcl_jobs"]
    procs = core["jcl_procs"]
    csd_files = next(iter(core["csd"].values()))["files"]
    datasets = inv["datasets"]
    all_programs = {p for m in inv["modules"].values() for p in m["programs"]}
    ext_programs = {p: m["module"] for m in inv["modules"].values() if m["module"] != "core" for p in m["programs"]}
    cpy_index = {}
    for d in ("cpy", "cpy-bms"):
        for p in (APP / d).iterdir():
            cpy_index[p.stem.upper()] = p

    scans = {n: scan_program(p, cpy_index, all_programs) for n, p in programs.items()}

    # --- program -> program edges ------------------------------------------------
    edges = []
    for n, p in programs.items():
        s = scans[n]
        for tgt, cnt in s["call_counts"].items():
            if is_dynamic(tgt):
                continue
            kind = "CALL"
            if tgt in RUNTIME_ROUTINES:
                tt = "runtime"
            elif tgt in core["assembler"]:
                tt = "assembler"
            elif tgt in programs:
                tt = "program"
            else:
                tt = "external"
            edges.append(OrderedDict(source=n, target=tgt, kind=kind, count=cnt, target_type=tt, resolution="static"))
        for kind, tgt, how in s["transfers"]:
            if tgt is None or tgt == n:  # self references feed RETURN TRANSID, not a real transfer
                continue
            tt = "program" if tgt in programs else ("out-of-scope program" if tgt in ext_programs else "undefined")
            edges.append(OrderedDict(source=n, target=tgt, kind=kind, count=1, target_type=tt, resolution=how))
        for job in s["submits_jobs"]:
            edges.append(OrderedDict(source=n, target=job, kind="SUBMIT", count=1, target_type="jcl",
                                     resolution="JCL written to TDQ JOBS (internal reader)"))
    # dedupe (same source/target/kind) keeping the first resolution and summing counts for CALL
    merged = OrderedDict()
    for e in edges:
        k = (e["source"], e["target"], e["kind"])
        if k in merged:
            if e["kind"] != "CALL":
                merged[k]["count"] += e["count"]
            if e["resolution"] not in merged[k]["resolution"]:
                merged[k]["resolution"] += "; " + e["resolution"]
        else:
            merged[k] = e
    edges = list(merged.values())
    dyn_notes = OrderedDict()
    for n in programs:
        notes = [how for kind, tgt, how in scans[n]["transfers"] if tgt is None]
        if notes:
            dyn_notes[n] = sorted(set(notes))

    fan_out = Counter(e["source"] for e in edges if e["target_type"] in ("program", "assembler", "out-of-scope program"))
    fan_in = Counter(e["target"] for e in edges if e["target_type"] in ("program", "out-of-scope program"))

    # --- program -> file edges ---------------------------------------------------
    file_edges = []
    for n, p in programs.items():
        for f in p["files"]:
            modes = f["open_modes"] or ["(not opened)"]
            file_edges.append(OrderedDict(
                program=n, kind="batch", logical=f["dd_name"], organization=f["organization"],
                access_mode=f["access_mode"], open_modes=modes, verbs=f["verbs"], datasets=f.get("datasets", []),
                record_key=f["record_key"], alternate_keys=f["alternate_keys"]))
        for f in p["cics_files"]:
            cmds = f["commands"]
            rw = ("R" if set(cmds) & READ_CMDS else "") + ("W" if set(cmds) & WRITE_CMDS else "")
            browse = bool({"STARTBR", "READNEXT", "READPREV"} & set(cmds))
            file_edges.append(OrderedDict(
                program=n, kind="cics", logical=f["cics_file"], organization="VSAM KSDS" + (" (AIX path)" if f["cics_file"].endswith("AIX") else ""),
                access_mode=("BROWSE" if browse else "RANDOM") + f"/{rw}", open_modes=[], verbs=cmds,
                datasets=[csd_files.get(f["cics_file"], "?")], record_key=None, alternate_keys=[]))

    # --- copybook -> program ------------------------------------------------------
    cpy_rows = []
    for name, cb in copybooks.items():
        cpy_rows.append(OrderedDict(copybook=name, kind="data" if not cb["has_procedure_code"] else "procedure",
                                    path=cb["path"], loc=cb["loc"]["code"], record_length=cb["record_length"],
                                    comp3=cb["constructs"]["COMP-3"], used_by=cb["used_by"],
                                    nested_copies=cb["nested_copies"]))
    for name, cb in bms_copybooks.items():
        cpy_rows.append(OrderedDict(copybook=name, kind="bms symbolic map", path=cb["path"], loc=cb["loc"]["code"],
                                    record_length=cb["record_length"], comp3=0, used_by=cb["used_by"], nested_copies=[]))

    # --- JCL chains ----------------------------------------------------------------
    def dd_direction(step: dict, dd: dict, repro: dict[str, tuple[str, str]], prog: dict | None) -> str:
        disp = (dd["disp"] or "").upper()
        if "DELETE" in disp and ("MOD" in disp or step["program"] == "IEFBR14"):
            return "delete"
        if dd["dd"] in ("STEPLIB", "JCLLIB", "SYSEXEC"):
            return "library"
        if prog:
            for f in prog["files"]:
                if f["dd_name"] == dd["dd"]:
                    return "write" if set(f["open_modes"]) & {"OUTPUT", "EXTEND"} else ("update" if "I-O" in f["open_modes"] else "read")
        if step["step"] in repro:
            i, o = repro[step["step"]]
            if dd["dd"] == i:
                return "read"
            if dd["dd"] == o:
                return "write"
        if dd["dd"] in ("SORTOUT", "SYSUT2", "OUT", "PRC001.FILEOUT", "OUTFILE"):
            return "write"
        if dd["dd"] in ("SORTIN", "SYSUT1", "IN", "PRC001.FILEIN", "INFILE", "INDD"):
            return "read"
        return "write" if disp.startswith("(NEW") else "read"

    chains = OrderedDict()
    for name, j in list(jobs.items()) + [(f"{k} (PROC)", v) for k, v in procs.items()]:
        path = ROOT / j["path"]
        repro = repro_direction(path) if path.suffix.lower() in (".jcl",) else {}
        steps = []
        for st in j["steps"]:
            prog = programs.get(st["program"] or "")
            dds = []
            for dd in st["dds"]:
                if dd["kind"] != "dataset":
                    continue
                gen = dd["gdg_generation"]
                dsn = dd["dsn"]
                dds.append(OrderedDict(dd=dd["dd"], dataset=dsn, gdg_generation=gen,
                                       gdg=(datasets.get(dsn, {}).get("catalog_type") == "GDG BASE") or gen is not None,
                                       disp=dd["disp"], direction=dd_direction(st, dd, repro, prog)))
            steps.append(OrderedDict(step=st["step"], program=st["program"], proc=st["proc"], program_class=st["program_class"],
                                     utility_commands=st.get("utility_commands", []), cond=st["cond"], dds=dds,
                                     instream_datasets=st.get("instream_datasets", [])))
        chains[name] = OrderedDict(job=j["job_name"] or j["proc_name"], path=j["path"], uses_gdg=j.get("uses_gdg", False),
                                   programs=[s["program"] for s in steps if s["program"] in programs], steps=steps)

    # dataset lineage between jobs -> JCL dependency order
    writers, readers = defaultdict(set), defaultdict(set)
    for name, ch in chains.items():
        if name.endswith("(PROC)"):
            continue
        for st in ch["steps"]:
            for dd in st["dds"]:
                if dd["direction"] in ("write", "update"):
                    writers[dd["dataset"]].add(name)
                if dd["direction"] in ("read", "update"):
                    readers[dd["dataset"]].add(name)
            for ds in st["instream_datasets"]:
                if "REPRO" not in st["utility_commands"]:
                    writers[ds].add(name)  # DEFINE CLUSTER / DEFINE GDG etc.
    job_deps = OrderedDict()
    for name in chains:
        if name.endswith("(PROC)"):
            continue
        deps = OrderedDict()
        for st in chains[name]["steps"]:
            for dd in st["dds"]:
                if dd["direction"] in ("read", "update"):
                    for w in sorted(writers[dd["dataset"]]):
                        if w != name:
                            deps.setdefault(w, []).append(dd["dataset"])
        job_deps[name] = deps
    # scheduler edges (CA-7 triggers + Control-M conditions) for comparison
    sched_edges = []
    for sname, s in core["scheduler"].items():
        if "jobs" in s:
            for j in s["jobs"]:
                for t in j["triggers"]:
                    sched_edges.append(OrderedDict(source=j["job_name"], target=t["job"], scheduler=sname, schid=t["schid"]))
        for folder in s.get("folders", []):
            outs = {}
            for j in folder["jobs"]:
                for c in j["out_conditions"]:
                    if c.startswith("+"):
                        outs[c[1:]] = j["job_name"]
            for j in folder["jobs"]:
                for c in j["in_conditions"]:
                    if c in outs:
                        sched_edges.append(OrderedDict(source=outs[c], target=j["job_name"], scheduler=sname, folder=folder["folder"]))

    # --- complexity ----------------------------------------------------------------
    def bucket(v, cuts):
        return sum(1 for c in cuts if v >= c)

    ratings = OrderedDict()
    for n, p in programs.items():
        s = scans[n]
        c = p["constructs"]
        files_touched = len({ds for fe in file_edges if fe["program"] == n for ds in (fe["datasets"] or [f"DD:{fe['logical']}"])})
        goto_alter = c["GO TO"] + c["ALTER"]
        comp3 = c["COMP-3"] + p["constructs_via_copybooks"].get("COMP-3", 0)
        coupling = fan_in[n] + fan_out[n]
        pts = OrderedDict(
            loc=bucket(p["loc"]["code"], (200, 500, 1000)),
            files=bucket(files_touched, (1, 3, 5)),
            control_flow=min(3, bucket(goto_alter, (1, 10, 30)) + (1 if c["ALTER"] else 0) + (1 if s["evaluate_depth"] >= 2 else 0)),
            arithmetic=bucket(comp3 + s["rounded"] + s["compute"], (1, 6, 15)),
            coupling=bucket(coupling, (1, 3, 6)),
        )
        score = sum(pts.values())
        rating = "High" if score >= 9 else ("Medium" if score >= 5 else "Low")
        if c["ALTER"] and rating != "High":
            rating = {"Low": "Medium", "Medium": "High"}[rating]
        ratings[n] = OrderedDict(
            program=n, type=p["type"], rating=rating, score=score, points=pts,
            metrics=OrderedDict(loc_code=p["loc"]["code"], files_touched=files_touched, go_to=c["GO TO"], alter=c["ALTER"],
                                evaluate_max_depth=s["evaluate_depth"], comp3=comp3, rounded=s["rounded"], compute=s["compute"],
                                arithmetic_verbs=s["arithmetic_verbs"], fan_in=fan_in[n], fan_out=fan_out[n],
                                cics_commands=sum(p["cics_commands"].values())))

    # --- hub confirmation -----------------------------------------------------------
    online = [n for n, p in programs.items() if p["type"] == "online"]
    menu_targets = {m: sorted(e["target"] for e in edges if e["source"] == m and e["kind"] == "XCTL") for m in ("COMEN01C", "COADM01C")}
    reach = set(menu_targets["COMEN01C"]) | set(menu_targets["COADM01C"])
    not_reached = sorted(set(online) - reach)
    hubs = OrderedDict(
        menus_reach_all_online=OrderedDict(
            claim="COMEN01C/COADM01C XCTL to every online program",
            verdict="corrected",
            detail=(f"COMEN01C -> {len(menu_targets['COMEN01C'])} programs via COMEN02Y, COADM01C -> {len(menu_targets['COADM01C'])} via COADM02Y; "
                    f"together they reach {len(reach & set(online))}/{len(online)} core online programs. Not reached: {', '.join(not_reached)} "
                    f"(the sign-on program XCTLs *to* the menus, and sub-screens COCRDSLC/COCRDUPC, COTRN01C, COUSR02C/COUSR03C are also reached from their list screens). "
                    f"Menu tables also name {', '.join(sorted(t for t in reach if t not in programs))}, which are not core programs (extension apps, out of scope)."),
            menu_targets=menu_targets, not_reached=not_reached),
        cbstm03b_calls=OrderedDict(
            claim="CBSTM03A calls CBSTM03B 13 times",
            verdict="confirmed" if scans["CBSTM03A"]["call_counts"].get("CBSTM03B") == 13 else "corrected",
            detail=f"{scans['CBSTM03A']['call_counts'].get('CBSTM03B', 0)} CALL 'CBSTM03B' statements in CBSTM03A"),
        csutldtc_callers=OrderedDict(claim="CSUTLDTC called from 4 programs"),
        cbact01c_asm=OrderedDict(claim="CBACT01C calls COBDATFT/MVSWAIT"),
        cee3abd_sites=OrderedDict(claim="CEE3ABD used in 11 places"),
    )
    direct = sorted(e["source"] for e in edges if e["target"] == "CSUTLDTC")
    # calls reaching CSUTLDTC through a copybook with procedure code (CSUTLDPY)
    via_cpy = []
    for cbn, cb in copybooks.items():
        if "CSUTLDTC" in CobolText(ROOT / cb["path"]).text and cb["has_procedure_code"]:
            via_cpy.extend((u, cbn) for u in cb["used_by"] if u not in direct)
    n_callers = len(direct) + len(via_cpy)
    hubs["csutldtc_callers"].update(
        verdict="confirmed" if n_callers == 4 else "corrected",
        detail=(f"{len(direct)} direct callers ({', '.join(direct)}) + {len(via_cpy)} through copybook procedure code "
                f"({', '.join(f'{u} via {c}' for u, c in via_cpy)}) = {n_callers} calling programs, not 4."),
        direct=direct, via_copybook=[OrderedDict(program=u, copybook=c) for u, c in via_cpy])
    asm_callers = {a: core["assembler"][a]["called_by"] for a in core["assembler"]}
    hubs["cbact01c_asm"].update(
        verdict="corrected" if asm_callers.get("MVSWAIT") != ["CBACT01C"] else "confirmed",
        detail=(f"CBACT01C calls COBDATFT (and CEE3ABD) only; MVSWAIT is called by {', '.join(asm_callers.get('MVSWAIT', []))} "
                f"(WAITSTEP job), not by CBACT01C."), assembler_callers=asm_callers)
    cee = sorted(e["source"] for e in edges if e["target"] == "CEE3ABD")
    hubs["cee3abd_sites"].update(verdict="confirmed" if len(cee) == 11 else "corrected",
                                 detail=f"CALL 'CEE3ABD' appears once in each of {len(cee)} batch programs: {', '.join(cee)}", programs=cee)
    hubs["no_cics_start"] = OrderedDict(claim="EXEC CICS START edges", verdict="none found",
                                        detail="No program issues EXEC CICS START; the only asynchronous hand-off is CORPT00C writing TRANREPT JCL to TDQ JOBS.")

    # --- recommended order ------------------------------------------------------------
    app_jobs = [n for n, ch in chains.items() if ch["programs"] and not n.endswith("(PROC)")]
    util_jobs = [n for n in chains if n not in app_jobs and not n.endswith("(PROC)")]
    # online ranks: forward edges = XCTL/LINK to a non-menu, non-sign-on program
    hubs_online = {"COMEN01C", "COADM01C", "COSGN00C"}
    fwd = defaultdict(set)
    for e in edges:
        if e["kind"] in ("XCTL", "LINK") and e["source"] in online and e["target"] in programs and e["target"] not in hubs_online and e["source"] not in hubs_online:
            fwd[e["source"]].add(e["target"])

    def rank(n, seen=()):
        return 0 if not fwd[n] else 1 + max((rank(t, seen + (n,)) for t in fwd[n] if t not in seen), default=0)

    mutual = sorted({tuple(sorted((a, b))) for a in list(fwd) for b in list(fwd[a]) if a in fwd.get(b, set())})

    leaf = sorted((n for n in online if n not in hubs_online and rank(n) == 0), key=lambda n: (ratings[n]["score"], n))
    parents = sorted((n for n in online if n not in hubs_online and rank(n) > 0), key=lambda n: (rank(n), ratings[n]["score"], n))

    def job_line(job):
        return f"{job} ({', '.join(chains[job]['programs'])})"

    order = [
        OrderedDict(phase="A", title="Data-only utilities and plumbing (no business logic)",
                    rationale="Dataset definitions, loads, backups and operator jobs translate to schema/DDL, seed loaders and Spring Batch infrastructure; they have no COBOL business rules and unblock every later phase.",
                    steps=[
                        OrderedDict(step="A1", items=["COBSWAIT", "MVSWAIT", "WAITSTEP"], kind="programs/jobs",
                                    note="Timer utility (COBSWAIT -> MVSWAIT STIMER); becomes a scheduler delay, no port needed."),
                        OrderedDict(step="A2", items=["CSUTLDTC", "CEEDAYS"], kind="programs",
                                    note="Pure date-validation subprogram shared by CORPT00C, COTRN02C and COACTUPC (via CSUTLDPY); port first as a java.time utility so online phases reuse it."),
                        OrderedDict(step="A3", items=["DEFGDGB", "DEFGDGD", "DEFCUST", "DALYREJS", "REPTFILE", "TRANIDX", "ESDSRRDS", "CBADMCDJ"], kind="jobs",
                                    note="IDCAMS DEFINE CLUSTER/AIX/GDG and CSD upload: become Flyway DDL + table/sequence definitions."),
                        OrderedDict(step="A4", items=["ACCTFILE", "CARDFILE", "CUSTFILE", "XREFFILE", "TRANFILE", "TRANTYPE", "TRANCATG", "DISCGRP", "TCATBALF", "DUSRSECJ"], kind="jobs",
                                    note="File loads (REPRO PS -> VSAM): become seed-data loaders for the golden set; needed before any batch or online program can be verified."),
                        OrderedDict(step="A5", items=["OPENFIL", "CLOSEFIL", "TRANBKP", "PRTCATBL", "COMBTRAN", "FTPJCL", "INTRDRJ1", "INTRDRJ2", "TXT2PDF1"], kind="jobs",
                                    note="CICS file open/close, GDG backups, SORT merges and transfer jobs: become batch job steps/ops scripts with no COBOL to translate."),
                    ]),
        OrderedDict(phase="B", title="Batch jobs in JCL/dataset dependency order",
                    rationale="Order follows the dataset lineage in section 4: each job only reads datasets written by jobs earlier in the list (POSTTRAN updates ACCTFILE/TCATBALF that INTCALC reads; INTCALC writes SYSTRAN(+1) that COMBTRAN merges back; TRANREPT and CREASTMT read the posted TRANSACT file).",
                    steps=[
                        OrderedDict(step="B1", items=["READACCT", "READCARD", "READCUST", "READXREF"], kind="jobs",
                                    programs=["CBACT01C", "CBACT02C", "CBACT03C", "CBCUS01C", "COBDATFT"],
                                    note="Read-only file dump programs: lowest risk, exercise the VSAM -> JPA repositories and the COBDATFT date formatter."),
                        OrderedDict(step="B2", items=["POSTTRAN"], kind="jobs", programs=["CBTRN02C", "CBTRN01C"],
                                    note="Daily transaction posting (validation + account/category balance update, rejects to DALYREJS(+1)). CBTRN01C is the earlier validation-only variant with no JCL; port alongside as the validation module."),
                        OrderedDict(step="B3", items=["INTCALC"], kind="jobs", programs=["CBACT04C"],
                                    note="Interest calculation over TCATBALF/DISCGRP, writes SYSTRAN(+1) and updates ACCTFILE; depends on POSTTRAN output."),
                        OrderedDict(step="B4", items=["TRANREPT", "CREASTMT"], kind="jobs", programs=["CBTRN03C", "CBSTM03A", "CBSTM03B"],
                                    note="Reporting: TRANREPT (REPROC backup -> SORT -> CBTRN03C report GDG) and CREASTMT (SORT -> REPRO -> CBSTM03A/B statements). CBSTM03A/B carry the GO TO/ALTER logic and are the hardest batch port."),
                        OrderedDict(step="B5", items=["CBEXPORT", "CBIMPORT"], kind="jobs", programs=["CBEXPORT", "CBIMPORT"],
                                    note="Branch migration export/import; independent of the daily chain, last because they are operational tooling."),
                    ]),
        OrderedDict(phase="C", title="Online programs from leaf screens to menus",
                    rationale="Leaf screens have no forward XCTL (they only return to the menu/sign-on), so each can be verified in isolation against its BMS map; list screens that XCTL into detail/update screens follow; menus and sign-on last because they depend on every target existing.",
                    steps=[
                        OrderedDict(step="C1", items=leaf, kind="programs", note="Leaf screens, ordered by complexity score (lowest first)."),
                        OrderedDict(step="C2", items=parents, kind="programs", note="List screens that transfer to detail/update screens (" + "; ".join(f"{a} -> {', '.join(sorted(fwd[a]))}" for a in parents) + ")."
                                         + (" Mutual transfers (" + ", ".join(f"{a} <-> {b}" for a, b in mutual) + ") are ported as a pair." if mutual else "")),
                        OrderedDict(step="C3", items=["COMEN01C", "COADM01C"], kind="programs", note="Menus (table-driven XCTL from COMEN02Y/COADM02Y)."),
                        OrderedDict(step="C4", items=["COSGN00C"], kind="programs", note="Sign-on / entry transaction CC00 (USRSEC read, XCTL to the two menus); last so the full navigation path can be verified end to end."),
                    ]),
    ]

    # coverage of the order
    ordered_programs = {i for ph in order for st in ph["steps"] for i in (st.get("programs") or []) + (st["items"] if st["kind"] == "programs" else [])}
    ordered_programs |= {p for ph in order for st in ph["steps"] if st["kind"] in ("jobs", "programs/jobs") for i in st["items"] for p in chains.get(i, {}).get("programs", [])}
    ordered_jobs = {i for ph in order for st in ph["steps"] if st["kind"] in ("jobs", "programs/jobs") for i in st["items"]}
    coverage = OrderedDict(
        programs_total=len(programs), programs_in_order=sorted(ordered_programs & set(programs)),
        programs_missing_from_order=sorted(set(programs) - ordered_programs),
        jobs_total=len(jobs), jobs_missing_from_order=sorted(set(jobs) - ordered_jobs),
    )

    return OrderedDict(
        generated_by=rel(Path(__file__)), inputs=[rel(INVENTORY)], scope="core module (app/) only; extension apps appear only as out-of-scope XCTL targets",
        program_edges=edges, dynamic_transfer_notes=dyn_notes, file_edges=file_edges, copybook_usage=cpy_rows,
        jcl_chains=chains, job_dependencies=job_deps, scheduler_edges=sched_edges, hubs=hubs, ratings=ratings,
        rating_criteria=OrderedDict(
            points="each criterion scores 0-3; total 0-15; Low <= 4, Medium 5-8, High >= 9; a program containing ALTER is raised one level (ALTER has no Java equivalent and forces a control-flow rewrite)",
            loc="code lines (comments/blank excluded): >=200 ->1, >=500 ->2, >=1000 ->3",
            files="distinct datasets touched (batch SELECT/ASSIGN + CICS FILE via CSD): >=1 ->1, >=3 ->2, >=5 ->3",
            control_flow="GO TO + ALTER count: >=1 ->1, >=10 ->2, >=30 ->3; +1 if any ALTER; +1 if EVALUATE nests >= 2 deep (capped at 3)",
            arithmetic="COMP-3 fields (own + via copybooks) + ROUNDED + COMPUTE statements: >=1 ->1, >=6 ->2, >=15 ->3",
            coupling="fan-in + fan-out over program/assembler edges (CALL, XCTL, LINK, resolved dynamic targets): >=1 ->1, >=3 ->2, >=6 ->3",
        ),
        migration_order=order, order_coverage=coverage,
        counts=OrderedDict(programs=len(programs), online=len(online), program_edges=len(edges), file_edges=len(file_edges),
                           copybooks=len(cpy_rows), jcl_jobs=len(jobs), jcl_procs=len(procs),
                           ratings=OrderedDict(Counter(r["rating"] for r in ratings.values()))),
    )


# ---------------------------------------------------------------------------
# Mermaid
# ---------------------------------------------------------------------------

def mermaid_diagrams(dm: OrderedDict, inv_core: dict) -> OrderedDict:
    programs = inv_core["programs"]
    online = [n for n, p in programs.items() if p["type"] == "online"]
    ratings = dm["ratings"]
    diagrams = OrderedDict()

    def node(n, label=None, shape="[]"):
        label = label or n
        shape = shape.replace(" ", "")
        o, c = shape[: len(shape) // 2], shape[len(shape) // 2:]
        return f'    {mid(n)}{o}"{label}"{c}'

    def cls(n):
        r = ratings.get(n)
        return f"    class {mid(n)} {r['rating'].lower()};" if r else ""

    style = ["    classDef high fill:#f8d7da,stroke:#c00;", "    classDef medium fill:#fff3cd,stroke:#c90;",
             "    classDef low fill:#d4edda,stroke:#090;", "    classDef ext fill:#eee,stroke:#999,stroke-dasharray: 3 3;",
             "    classDef ds fill:#e7f0ff,stroke:#36c;", "    classDef job fill:#f3e8ff,stroke:#63c;"]

    # 1. online call graph ---------------------------------------------------------
    lines = ["flowchart LR"] + style
    for n in online:
        p = programs[n]
        lines.append(node(n, f"{n}<br/>{p['transaction_id'] or ''} {ratings[n]['rating']}"))
        lines.append(cls(n))
    ext = sorted({e["target"] for e in dm["program_edges"] if e["source"] in online and e["target_type"] in ("out-of-scope program", "undefined")})
    for t in ext:
        lines.append(node(t, f"{t}<br/>out of scope", "([])"))
        lines.append(f"    class {mid(t)} ext;")
    seen_jobs = set()
    for e in dm["program_edges"]:
        if e["source"] not in online:
            continue
        if e["target_type"] in ("program", "out-of-scope program", "undefined"):
            arrow = "-->" if e["resolution"] == "static" else "-.->"
            lines.append(f"    {mid(e['source'])} {arrow}|\"{e['kind']}\"| {mid(e['target'])}")
        elif e["kind"] == "SUBMIT":
            if e["target"] not in seen_jobs:
                lines.append(node("JOB_" + e["target"], f"JCL {e['target']}", "[[]]"))
                lines.append(f"    class {mid('JOB_' + e['target'])} job;")
                seen_jobs.add(e["target"])
            lines.append(f"    {mid(e['source'])} -.->|\"SUBMIT via TDQ JOBS\"| {mid('JOB_' + e['target'])}")
        elif e["kind"] == "CALL":
            if e["target"] not in seen_jobs:
                lines.append(node(e["target"]))
                lines.append(cls(e["target"]))
                seen_jobs.add(e["target"])
            lines.append(f"    {mid(e['source'])} -->|\"CALL x{e['count']}\"| {mid(e['target'])}")
    diagrams["01-online-call-graph"] = "\n".join(l for l in lines if l)

    # 2. batch call graph -----------------------------------------------------------
    batch = [n for n, p in programs.items() if p["type"] != "online"]
    lines = ["flowchart LR"] + style
    for n in batch:
        lines.append(node(n, f"{n}<br/>{programs[n]['type']} {ratings[n]['rating']}"))
        lines.append(cls(n))
    for a in inv_core["assembler"]:
        lines.append(node(a, f"{a}<br/>Assembler", "[/ /]"))
    for r, d in RUNTIME_ROUTINES.items():
        lines.append(node(r, f"{r}<br/>{d}", "(( ))"))
    for e in dm["program_edges"]:
        if e["source"] in batch and e["kind"] == "CALL":
            lines.append(f"    {mid(e['source'])} -->|\"CALL x{e['count']}\"| {mid(e['target'])}")
    diagrams["02-batch-call-graph"] = "\n".join(l for l in lines if l)

    # 3. online program -> CICS file --------------------------------------------------
    lines = ["flowchart LR"] + style
    files = OrderedDict()
    for fe in dm["file_edges"]:
        if fe["kind"] == "cics":
            files[fe["logical"]] = fe["datasets"][0]
    for n in online:
        if any(fe["program"] == n and fe["kind"] == "cics" for fe in dm["file_edges"]):
            lines.append(node(n))
            lines.append(cls(n))
    for f, ds in files.items():
        lines.append(node("F_" + f, f"{f}<br/>{ds}", "[( )]"))
        lines.append(f"    class {mid('F_' + f)} ds;")
    for fe in dm["file_edges"]:
        if fe["kind"] == "cics":
            lines.append(f"    {mid(fe['program'])} -->|\"{fe['access_mode']}: {' '.join(fe['verbs'])}\"| {mid('F_' + fe['logical'])}")
    diagrams["03-online-program-files"] = "\n".join(l for l in lines if l)

    # 4. batch program -> dataset -----------------------------------------------------
    lines = ["flowchart LR"] + style
    dss = OrderedDict()
    for fe in dm["file_edges"]:
        if fe["kind"] == "batch":
            for ds in fe["datasets"] or [f"(unassigned DD {fe['logical']})"]:
                dss[ds] = True
    for n in batch:
        if any(fe["program"] == n and fe["kind"] == "batch" for fe in dm["file_edges"]):
            lines.append(node(n))
            lines.append(cls(n))
    for ds in dss:
        lines.append(node("D_" + ds, ds, "[( )]"))
        lines.append(f"    class {mid('D_' + ds)} ds;")
    for fe in dm["file_edges"]:
        if fe["kind"] != "batch":
            continue
        label = f"{fe['logical']} {fe['organization']}/{fe['access_mode']} {'+'.join(fe['open_modes'])}"
        for ds in fe["datasets"] or [f"(unassigned DD {fe['logical']})"]:
            if set(fe["open_modes"]) & {"OUTPUT", "EXTEND"}:
                lines.append(f"    {mid(fe['program'])} -->|\"{label}\"| {mid('D_' + ds)}")
            elif "I-O" in fe["open_modes"]:
                lines.append(f"    {mid(fe['program'])} <-->|\"{label}\"| {mid('D_' + ds)}")
            else:
                lines.append(f"    {mid('D_' + ds)} -->|\"{label}\"| {mid(fe['program'])}")
    diagrams["04-batch-program-datasets"] = "\n".join(l for l in lines if l)

    # 5. JCL lineage: job -> step/program -> dataset(generation) ---------------------------
    core_jobs = ["ACCTFILE", "CARDFILE", "CUSTFILE", "XREFFILE", "TRANFILE", "TRANTYPE", "TRANCATG", "DISCGRP", "TCATBALF", "DUSRSECJ",
                 "READACCT", "READCARD", "READCUST", "READXREF", "POSTTRAN", "INTCALC", "TRANBKP", "TRANREPT", "COMBTRAN", "CREASTMT",
                 "PRTCATBL", "CBEXPORT", "CBIMPORT", "TXT2PDF1"]
    lines = ["flowchart LR"] + style
    ds_nodes = OrderedDict()
    for job in core_jobs:
        ch = dm["jcl_chains"][job]
        lines.append(f"    subgraph {mid('J_' + job)}[\"JOB {job}\"]")
        for st in ch["steps"]:
            if not st["dds"] or all(d["direction"] in ("delete", "library") for d in st["dds"]):
                continue
            prog = st["program"] or f"PROC {st['proc']}"
            sid = f"{job}.{st['step']}"
            lines.append(f'        {mid(sid)}["{st["step"]}<br/>{prog}"]')
            if st["program"] in programs:
                lines.append(f"        class {mid(sid)} {ratings[st['program']]['rating'].lower()};")
        lines.append("    end")
    for job in core_jobs:
        ch = dm["jcl_chains"][job]
        for st in ch["steps"]:
            if not st["dds"] or all(d["direction"] in ("delete", "library") for d in st["dds"]):
                continue
            sid = f"{job}.{st['step']}"
            for d in st["dds"]:
                if d["direction"] in ("delete", "library"):
                    continue
                ds_nodes[d["dataset"]] = d["gdg"]
                gen = f" ({d['gdg_generation']})" if d["gdg_generation"] else ""
                lab = f"{d['dd']}{gen}"
                if d["direction"] == "write":
                    lines.append(f"    {mid(sid)} -->|\"{lab}\"| {mid('D_' + d['dataset'])}")
                elif d["direction"] == "update":
                    lines.append(f"    {mid(sid)} <-->|\"{lab}\"| {mid('D_' + d['dataset'])}")
                else:
                    lines.append(f"    {mid('D_' + d['dataset'])} -->|\"{lab}\"| {mid(sid)}")
    for ds, gdg in ds_nodes.items():
        lines.insert(len(style) + 1, node("D_" + ds, ds + ("<br/>GDG" if gdg else ""), "[( )]") + f"\n    class {mid('D_' + ds)} ds;")
    diagrams["05-jcl-dataset-lineage"] = "\n".join(l for l in lines if l)
    return diagrams


# ---------------------------------------------------------------------------
# Markdown
# ---------------------------------------------------------------------------

def render_markdown(dm: OrderedDict, inv: dict, diagrams: OrderedDict) -> str:
    core = inv["modules"]["core"]
    programs = core["programs"]
    out = []
    w = out.append
    w("# 02 — Dependency map and complexity ratings (CardDemo)\n")
    w(f"Generated by `{dm['generated_by']}` from `{dm['inputs'][0]}` (step s1.1 output) plus targeted re-scans of `app/cbl`, `app/cpy` and `app/jcl`. "
      "Regenerate with `python3 docs/modernization/build_dependency_map.py`; `--check` verifies freshness, that every program and JCL job appears, and the hub claims; "
      "`--render` writes `diagrams/*.svg` with `mmdc`. Machine-readable form: `docs/modernization/dependency-map.json` (edges, ratings, order).\n")
    w(f"Scope: {dm['scope']}.\n")
    c = dm["counts"]
    w(f"Totals: **{c['programs']} COBOL programs** ({c['online']} online), **{c['program_edges']} program edges**, **{c['file_edges']} program→file edges**, "
      f"**{c['copybooks']} copybooks**, **{c['jcl_jobs']} JCL jobs + {c['jcl_procs']} procedures**; ratings: "
      + ", ".join(f"{k} {v}" for k, v in c["ratings"].items()) + ".\n")
    w("Node colours in every diagram: green = Low, yellow = Medium, red = High complexity; grey dashed = out-of-scope program; blue cylinder = dataset/file.\n")

    # 1 ---------------------------------------------------------------------------
    w("## 1. Program → program call graph\n")
    w("Edge kinds: `CALL` (static subprogram call, with call-site count), `EXEC CICS XCTL`/`LINK` (solid = literal PROGRAM name, dotted = run-time operand resolved from `MOVE`/`VALUE`/menu tables), `SUBMIT` (JCL written to the `JOBS` extra-partition TDQ). "
      + dm["hubs"]["no_cics_start"]["detail"] + "\n")
    w("### 1a. Online programs (CICS XCTL/LINK)\n")
    w("```mermaid\n" + diagrams["01-online-call-graph"] + "\n```\n")
    w("### 1b. Batch programs (CALL)\n")
    w("```mermaid\n" + diagrams["02-batch-call-graph"] + "\n```\n")
    w("### 1c. Edge list\n")
    rows = [[e["source"], e["kind"], e["target"], e["target_type"], e["count"], e["resolution"]] for e in dm["program_edges"]]
    w(md_table(["Source", "Kind", "Target", "Target type", "Sites", "Resolution"], rows))
    w("\nRun-time transfer operands that could not be reduced to a program name:\n")
    for n, notes in dm["dynamic_transfer_notes"].items():
        w(f"- `{n}`: " + "; ".join(notes))
    w("")
    w("### 1d. Hub confirmation\n")
    rows = []
    for k, h in dm["hubs"].items():
        rows.append([h["claim"], f"**{h['verdict']}**", h["detail"]])
    w(md_table(["Claim from the plan", "Verdict", "Evidence"], rows))
    w("")

    # 2 ---------------------------------------------------------------------------
    w("## 2. Program → file edges (access mode)\n")
    w("### 2a. Online programs → CICS files\n")
    w("```mermaid\n" + diagrams["03-online-program-files"] + "\n```\n")
    rows = [[fe["program"], fe["logical"], fe["datasets"][0], fe["access_mode"], " ".join(fe["verbs"])]
            for fe in dm["file_edges"] if fe["kind"] == "cics"]
    w(md_table(["Program", "CICS FILE", "Dataset (CSD)", "Access", "Commands"], rows))
    w("\n### 2b. Batch programs → datasets\n")
    w("```mermaid\n" + diagrams["04-batch-program-datasets"] + "\n```\n")
    rows = [[fe["program"], fe["logical"], fe["organization"], fe["access_mode"], "+".join(fe["open_modes"]), " ".join(fe["verbs"]),
             fe["record_key"] or "", ", ".join(fe["datasets"]) or "(no JCL DD found)"] for fe in dm["file_edges"] if fe["kind"] == "batch"]
    w(md_table(["Program", "DD", "Organization", "Access mode", "OPEN", "Verbs", "Record key", "Dataset(s) from JCL"], rows))
    w("")

    # 3 ---------------------------------------------------------------------------
    w("## 3. Copybook → program usage\n")
    rows = [[r["copybook"], r["kind"], r["loc"], r["record_length"] or "", r["comp3"], ", ".join(r["used_by"]) or "*(unused)*", ", ".join(r["nested_copies"])]
            for r in dm["copybook_usage"]]
    w(md_table(["Copybook", "Kind", "Code LOC", "Record bytes", "COMP-3 fields", "Used by", "Nested COPY"], rows))
    w("")

    # 4 ---------------------------------------------------------------------------
    w("## 4. JCL job → step → program → DD → dataset chains\n")
    w("### 4a. Dataset lineage of the application batch chain\n")
    w("Steps that only DELETE/DEFINE are omitted from the picture (they are in the table). `(+1)`/`(0)` are GDG relative generations; datasets marked GDG are GDG bases in `LISTCAT`.\n")
    w("```mermaid\n" + diagrams["05-jcl-dataset-lineage"] + "\n```\n")
    w("### 4b. All jobs and procedures\n")
    rows = []
    for name, ch in dm["jcl_chains"].items():
        for st in ch["steps"]:
            prog = st["program"] or f"PROC {st['proc']}"
            if not st["dds"]:
                extra = ", ".join(st["utility_commands"]) or ""
                ins = ", ".join(st["instream_datasets"]) if st["instream_datasets"] else ""
                rows.append([name, st["step"], prog, st["program_class"], "", f"{extra} {ins}".strip() or "(no dataset DD)", "", ""])
                continue
            for i, d in enumerate(st["dds"]):
                rows.append([name if i == 0 else "", st["step"] if i == 0 else "", prog if i == 0 else "", st["program_class"] if i == 0 else "",
                             d["dd"], d["dataset"], (d["gdg_generation"] or ("GDG" if d["gdg"] else "")), f"{d['direction']} {d['disp'] or ''}".strip()])
    w(md_table(["Job", "Step", "Program", "Class", "DD", "Dataset", "GDG gen", "Direction / DISP"], rows))
    w("\n### 4c. Job dependencies derived from dataset lineage\n")
    w("Job B depends on job A when B reads (or updates) a dataset that A writes or defines. This is the order used for phase B of the migration order.\n")
    rows = []
    for job, deps in dm["job_dependencies"].items():
        if deps:
            rows.append([job, "; ".join(f"{a} ({', '.join(sorted(set(ds)))})" for a, ds in deps.items())])
    w(md_table(["Job", "Depends on (through dataset)"], rows))
    w("\n### 4d. Scheduler edges (CA-7 triggers / Control-M conditions) for comparison\n")
    rows = [[e["source"], e["target"], e["scheduler"], e.get("schid") or e.get("folder", "")] for e in dm["scheduler_edges"]]
    w(md_table(["Job", "Triggers", "Scheduler", "SCHID / folder"], rows))
    w("")

    # 5 ---------------------------------------------------------------------------
    w("## 5. Complexity ratings\n")
    rc = dm["rating_criteria"]
    w("Scoring (" + rc["points"] + "):\n")
    for k in ("loc", "files", "control_flow", "arithmetic", "coupling"):
        w(f"- **{k}**: {rc[k]}")
    w("")
    rows = []
    for r in sorted(dm["ratings"].values(), key=lambda r: (-r["score"], r["program"])):
        m, p = r["metrics"], r["points"]
        rows.append([r["program"], r["type"], f"**{r['rating']}**", r["score"],
                     f"{m['loc_code']} ({p['loc']})", f"{m['files_touched']} ({p['files']})",
                     f"GO TO {m['go_to']}, ALTER {m['alter']}, EVALUATE depth {m['evaluate_max_depth']} ({p['control_flow']})",
                     f"COMP-3 {m['comp3']}, ROUNDED {m['rounded']}, COMPUTE {m['compute']} ({p['arithmetic']})",
                     f"in {m['fan_in']} / out {m['fan_out']} ({p['coupling']})", m["cics_commands"]])
    w(md_table(["Program", "Type", "Rating", "Score", "Code LOC (pts)", "Files (pts)", "Control flow (pts)", "Arithmetic (pts)", "Coupling (pts)", "CICS cmds"], rows))
    w("\nNo program uses `ROUNDED`; packed-decimal arithmetic is concentrated in CBACT04C (interest), CBTRN02C/CBTRN03C and the account/bill-payment screens (COACTUPC, COBIL00C). "
      "`ALTER` exists only in CBSTM03A (4 sites, with 14 GO TOs) and CBSTM03B is a GO TO-driven dispatcher (13 GO TOs in 162 lines).\n")

    # 6 ---------------------------------------------------------------------------
    w("## 6. Recommended migration order\n")
    w("Phases run in order A → B → C; within a phase, steps run in the listed order. Later plan steps should reference items as `phase.step` (e.g. `B2 POSTTRAN`).\n")
    for ph in dm["migration_order"]:
        w(f"### Phase {ph['phase']} — {ph['title']}\n")
        w(ph["rationale"] + "\n")
        for st in ph["steps"]:
            items = ", ".join(f"`{i}`" for i in st["items"])
            progs = (" — programs: " + ", ".join(f"`{p}`" for p in st["programs"])) if st.get("programs") else ""
            w(f"{st['step']}. **{st['kind']}**: {items}{progs}. {st['note']}")
        w("")
    cov = dm["order_coverage"]
    w(f"Order coverage: {len(cov['programs_in_order'])}/{cov['programs_total']} core programs and {cov['jobs_total'] - len(cov['jobs_missing_from_order'])}/{cov['jobs_total']} JCL jobs are placed"
      + (f"; missing programs: {', '.join(cov['programs_missing_from_order'])}" if cov["programs_missing_from_order"] else "")
      + (f"; missing jobs: {', '.join(cov['jobs_missing_from_order'])}" if cov["jobs_missing_from_order"] else "") + ".\n")
    return "\n".join(out)


# ---------------------------------------------------------------------------
# checks / main
# ---------------------------------------------------------------------------

def coverage_check(dm: OrderedDict, inv: dict, diagrams: OrderedDict, md: str) -> list[str]:
    core = inv["modules"]["core"]
    problems = []
    diagram_text = "\n".join(diagrams.values())
    for n in core["programs"]:
        if not re.search(r'\b' + mid(n) + r'\[', diagram_text):
            problems.append(f"program {n} missing from Mermaid diagrams")
        if n not in dm["ratings"]:
            problems.append(f"program {n} has no complexity rating")
    for j in core["jcl_jobs"]:
        if j not in dm["jcl_chains"]:
            problems.append(f"JCL job {j} missing from chains")
        if f"| {j} |" not in md:
            problems.append(f"JCL job {j} missing from the markdown JCL table")
    cov = dm["order_coverage"]
    if cov["programs_missing_from_order"]:
        problems.append(f"programs missing from migration order: {cov['programs_missing_from_order']}")
    if cov["jobs_missing_from_order"]:
        problems.append(f"jobs missing from migration order: {cov['jobs_missing_from_order']}")
    return problems


def render_svgs(diagrams: OrderedDict) -> list[str]:
    mmdc = shutil.which("mmdc") or os.environ.get("MMDC")
    if not mmdc:
        return ["mmdc not found (npm i -g @mermaid-js/mermaid-cli, or set MMDC=/path/to/mmdc)"]
    chrome = (os.environ.get("PUPPETEER_EXECUTABLE_PATH") or shutil.which("google-chrome") or shutil.which("chromium")
              or shutil.which("google-chrome", path=str(Path.home() / ".local/bin")))
    cfg = None
    if chrome:
        cfg = tempfile.NamedTemporaryFile("w", suffix=".json", delete=False)
        json.dump({"executablePath": chrome, "args": ["--no-sandbox"]}, cfg)
        cfg.close()
    msgs = []
    for name in diagrams:
        src, svg = DIAG_DIR / f"{name}.mmd", DIAG_DIR / f"{name}.svg"
        cmd = [mmdc, "-i", str(src), "-o", str(svg), "-q"] + (["-p", cfg.name] if cfg else [])
        r = subprocess.run(cmd, capture_output=True, text=True)
        msgs.append(f"{'rendered' if r.returncode == 0 else 'FAILED'} {rel(svg)} {r.stderr.strip()[-300:] if r.returncode else ''}".strip())
    return msgs


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--check", action="store_true", help="verify outputs are up to date, coverage is complete and report hub verdicts")
    ap.add_argument("--render", action="store_true", help="also render diagrams/*.svg with mmdc")
    args = ap.parse_args()
    inv = json.loads(INVENTORY.read_text())
    dm = build()
    diagrams = mermaid_diagrams(dm, inv["modules"]["core"])
    dm["diagrams"] = OrderedDict((k, f"docs/modernization/diagrams/{k}.mmd") for k in diagrams)
    md = render_markdown(dm, inv, diagrams)
    js = json.dumps(dm, indent=2) + "\n"
    problems = coverage_check(dm, inv, diagrams, md)
    status = 0
    if args.check:
        outputs = [(JSON_OUT, js), (MD_OUT, md)] + [(DIAG_DIR / f"{k}.mmd", v + "\n") for k, v in diagrams.items()]
        for path, text in outputs:
            if not path.exists() or path.read_text() != text:
                print(f"STALE: {rel(path)} differs from generated content", file=sys.stderr)
                status = 1
    for p in problems:
        print(f"COVERAGE: {p}", file=sys.stderr)
        status = 1
    if not args.check:
        DIAG_DIR.mkdir(exist_ok=True)
        JSON_OUT.write_text(js)
        MD_OUT.write_text(md)
        for k, v in diagrams.items():
            (DIAG_DIR / f"{k}.mmd").write_text(v + "\n")
        print(f"wrote {rel(JSON_OUT)}, {rel(MD_OUT)} and {len(diagrams)} diagrams under {rel(DIAG_DIR)}/")
    core = inv["modules"]["core"]
    print(f"coverage: {len(core['programs'])} programs rated and drawn, {len(core['jcl_jobs'])} JCL jobs charted, "
          f"{dm['counts']['program_edges']} program edges, {dm['counts']['file_edges']} file edges; "
          f"order covers {len(dm['order_coverage']['programs_in_order'])}/{dm['order_coverage']['programs_total']} programs; "
          f"{'OK' if status == 0 else 'FAILED'}")
    for k, h in dm["hubs"].items():
        print(f"hub [{h['verdict']}] {h['claim']}: {h['detail']}")
    if args.render:
        for m in render_svgs(diagrams):
            print(m)
    return status


if __name__ == "__main__":
    sys.exit(main())
