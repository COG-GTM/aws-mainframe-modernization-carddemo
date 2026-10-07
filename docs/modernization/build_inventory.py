#!/usr/bin/env python3
"""Build the CardDemo mainframe artifact inventory from the sources under ``app/``.

Outputs (both regenerated from scratch on every run):

* ``docs/modernization/inventory.json``  machine-readable inventory
* ``docs/modernization/01-inventory.md`` human-readable inventory

Usage::

    python3 docs/modernization/build_inventory.py          # regenerate
    python3 docs/modernization/build_inventory.py --check  # exit 1 if outputs are stale
                                                           # or any app/ file is missing

Standard library only, static analysis only.  Every file under ``app/`` gets an
entry; the script fails if any file is left out of the inventory.
"""
from __future__ import annotations

import argparse
import bisect
import json
import re
import sys
import xml.etree.ElementTree as ET
from collections import Counter, OrderedDict, defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
APP = ROOT / "app"
OUT_DIR = Path(__file__).resolve().parent
JSON_OUT = OUT_DIR / "inventory.json"
MD_OUT = OUT_DIR / "01-inventory.md"

EXTENSION_MODULES = OrderedDict(
    [
        ("app-authorization-ims-db2-mq", "Pending authorizations: IMS DB + Db2 + IBM MQ"),
        ("app-transaction-type-db2", "Transaction type maintenance on Db2"),
        ("app-vsam-mq", "VSAM inquiry over IBM MQ request/response"),
    ]
)
OUT_OF_SCOPE_REASON = (
    "Decision d-scope: core app only (online + batch under app/). Extension apps are "
    "inventoried for completeness but excluded from the Java 21 migration unless the "
    "decision is changed."
)

DIR_KINDS = {
    "cbl": "cobol_program",
    "cpy": "copybook",
    "cpy-bms": "bms_copybook",
    "bms": "bms_map",
    "jcl": "jcl_job",
    "proc": "jcl_proc",
    "ctl": "control_card",
    "asm": "assembler",
    "maclib": "asm_macro",
    "csd": "cics_csd",
    "catlg": "catalog_listing",
    "scheduler": "scheduler_def",
    "data": "data_file",
    "ddl": "sql_ddl",
    "dcl": "sql_dclgen",
    "ims": "ims_definition",
}
KIND_LABELS = OrderedDict(
    [
        ("cobol_program", "COBOL programs"),
        ("copybook", "Copybooks"),
        ("bms_copybook", "BMS symbolic-map copybooks"),
        ("bms_map", "BMS mapsets"),
        ("jcl_job", "JCL jobs"),
        ("jcl_proc", "JCL procedures"),
        ("control_card", "Utility control cards"),
        ("assembler", "Assembler programs"),
        ("asm_macro", "Assembler macros"),
        ("cics_csd", "CICS CSD definitions"),
        ("catalog_listing", "Catalog listings"),
        ("scheduler_def", "Scheduler definitions"),
        ("data_file", "Sample data files"),
        ("sql_ddl", "SQL DDL"),
        ("sql_dclgen", "SQL DCLGEN includes"),
        ("ims_definition", "IMS DBD/PSB definitions"),
        ("readme", "Module README"),
        ("placeholder", "Git placeholder (.gitkeep)"),
    ]
)

SYSTEM_UTILITIES = {
    "IDCAMS", "SORT", "ICEMAN", "DFSORT", "IEBGENER", "IEFBR14", "IKJEFT01", "IKJEFT1A",
    "IKJEFT1B", "DFHCSDUP", "DFSRRC00", "DSNTIAUL", "DSNTEP2", "DSNTEP4", "DSNTIAD",
    "DSNUTILB", "IEBCOPY", "FTP", "ADRDSSU", "DFSUDMP0", "DFSURGU0",
}

# ASCII sample files -> record layout copybook (same mapping the README gives for the
# EBCDIC originals; the ASCII files are the decoded equivalents).
ASCII_LAYOUTS = {
    "acctdata.txt": ("CVACT01Y", "AWS.M2.CARDDEMO.ACCTDATA.PS"),
    "carddata.txt": ("CVACT02Y", "AWS.M2.CARDDEMO.CARDDATA.PS"),
    "cardxref.txt": ("CVACT03Y", "AWS.M2.CARDDEMO.CARDXREF.PS"),
    "custdata.txt": ("CVCUS01Y", "AWS.M2.CARDDEMO.CUSTDATA.PS"),
    "dailytran.txt": ("CVTRA06Y", "AWS.M2.CARDDEMO.DALYTRAN.PS"),
    "discgrp.txt": ("CVTRA02Y", "AWS.M2.CARDDEMO.DISCGRP.PS"),
    "tcatbal.txt": ("CVTRA01Y", "AWS.M2.CARDDEMO.TCATBALF.PS"),
    "trancatg.txt": ("CVTRA04Y", "AWS.M2.CARDDEMO.TRANCATG.PS"),
    "trantype.txt": ("CVTRA03Y", "AWS.M2.CARDDEMO.TRANTYPE.PS"),
}
# EBCDIC files the README table does not list: layout copybook + how it was established.
EBCDIC_LAYOUT_OVERRIDES = {
    "AWS.M2.CARDDEMO.EXPORT.DATA.PS": (
        "CVEXPORT",
        "Written by CBEXPORT (EXPORT-RECORD, CVEXPORT); not in the README table",
    ),
    "AWS.M2.CARDDEMO.ACCDATA.PS": (
        "CVACT01Y",
        "Byte-identical to AWS.M2.CARDDEMO.ACCTDATA.PS; not in the README table",
    ),
}


# ---------------------------------------------------------------------------
# helpers
# ---------------------------------------------------------------------------

def read_text(path: Path) -> str:
    return path.read_bytes().decode("latin-1")


def rel(path: Path) -> str:
    return path.relative_to(ROOT).as_posix()


def md_escape(text) -> str:
    text = "" if text is None else str(text)
    return text.replace("|", "\\|").replace("\n", " ")


def md_table(headers, rows) -> str:
    if not rows:
        return "_none_"
    out = ["| " + " | ".join(headers) + " |", "|" + "|".join(" --- " for _ in headers) + "|"]
    for row in rows:
        out.append("| " + " | ".join(md_escape(c) for c in row) + " |")
    return "\n".join(out)


def kw_count(counter: Counter, keys) -> str:
    return ", ".join(f"{k}:{counter[k]}" for k in keys if counter.get(k))


def plural(n: int, word: str) -> str:
    return f"{n} {word}{'' if n == 1 else 's'}"


# ---------------------------------------------------------------------------
# COBOL
# ---------------------------------------------------------------------------

class CobolText:
    """Fixed-format COBOL: comment stripping, literal masking, offset -> line mapping."""

    def __init__(self, path: Path):
        self.path = path
        raw = read_text(path).split("\n")
        if raw and raw[-1] == "":
            raw.pop()
        self.total_lines = len(raw)
        self.comment_lines = 0
        self.blank_lines = 0
        self.code: list[tuple[int, str]] = []
        for no, line in enumerate(raw, 1):
            line = line.rstrip("\r")
            if not line.strip():
                self.blank_lines += 1
                continue
            if len(line) < 7:
                continue
            ind = line[6]
            body = line[7:72]
            if ind in "*/":
                self.comment_lines += 1
                continue
            if not body.strip():
                self.blank_lines += 1
                continue
            if ind == "-" and self.code:
                cont = body.lstrip()
                if cont[:1] in "'\"":
                    cont = cont[1:]
                prev_no, prev = self.code[-1]
                self.code[-1] = (prev_no, prev.rstrip() + cont)
                continue
            self.code.append((no, body))
        self.code_lines = len(self.code)
        parts, masked, self.offsets, pos = [], [], [], 0
        for _, body in self.code:
            self.offsets.append(pos)
            parts.append(body)
            masked.append(mask_literals(body))
            pos += len(body) + 1
        self.text = "\n".join(parts).upper()
        self.masked = "\n".join(masked).upper()

    def line_of(self, offset: int) -> int:
        idx = bisect.bisect_right(self.offsets, offset) - 1
        return self.code[max(idx, 0)][0] if self.code else 0


def mask_literals(text: str) -> str:
    out, i, n = list(text), 0, len(text)
    while i < n:
        c = text[i]
        if c in "'\"":
            j = i + 1
            while j < n and text[j] != c:
                out[j] = " "
                j += 1
            i = j + 1
        else:
            i += 1
    return "".join(out)


def word_re(word: str) -> re.Pattern:
    return re.compile(r"(?<![A-Z0-9-])" + word + r"(?![A-Z0-9-])")


CONSTRUCT_RES = OrderedDict(
    [
        ("GO TO", word_re(r"GO\s+TO")),
        ("ALTER", word_re(r"ALTER")),
        ("COMP-3", word_re(r"(?:COMP(?:UTATIONAL)?-3|PACKED-DECIMAL)")),
        ("COMP", word_re(r"(?:COMP(?:UTATIONAL)?(?:-[45])?|BINARY)")),
        ("REDEFINES", word_re(r"REDEFINES")),
        ("OCCURS", word_re(r"OCCURS")),
        ("OCCURS DEPENDING", word_re(r"OCCURS[^.]*?DEPENDING\s+ON")),
        ("PERFORM THRU", word_re(r"PERFORM\s+[A-Z0-9-]+\s+(?:THRU|THROUGH)")),
        ("STRING/UNSTRING", word_re(r"(?:UNSTRING|STRING)\s+[A-Z0-9-]+")),
        ("INSPECT", word_re(r"INSPECT")),
        ("EXEC SQL", word_re(r"EXEC\s+SQL")),
        ("CALL", word_re(r"CALL\s+")),
    ]
)
CONSTRUCTS_WITH_LINES = {"GO TO", "ALTER", "EXEC SQL", "CALL"}


def count_constructs(src: CobolText) -> tuple[Counter, dict]:
    counts, lines = Counter(), {}
    for name, rx in CONSTRUCT_RES.items():
        hits = [m.start() for m in rx.finditer(src.masked)]
        counts[name] = len(hits)
        if name in CONSTRUCTS_WITH_LINES and hits:
            lines[name] = sorted({src.line_of(h) for h in hits})
    return counts, lines


PIC_RE = re.compile(r"\bPIC(?:TURE)?\s+(?:IS\s+)?(\S+)")
USAGE_RE = re.compile(
    r"(?<![A-Z0-9-])(COMP-3|COMPUTATIONAL-3|PACKED-DECIMAL|COMP-5|COMPUTATIONAL-5|COMP-4|"
    r"COMPUTATIONAL-4|COMP-1|COMPUTATIONAL-1|COMP-2|COMPUTATIONAL-2|COMP|COMPUTATIONAL|"
    r"BINARY|DISPLAY|INDEX|POINTER)(?![A-Z0-9-])"
)
OCCURS_RE = re.compile(r"\bOCCURS\s+(\d+)(?:\s+TO\s+(\d+))?")
REDEF_RE = re.compile(r"\bREDEFINES\s+([A-Z0-9-]+)")
ITEM_RE = re.compile(r"^\s*(\d{1,2})\s+(\S+)?(.*)$", re.S)
DATA_KEYWORDS = {"PIC", "PICTURE", "REDEFINES", "OCCURS", "VALUE", "VALUES", "COMP", "COMP-3",
                 "COMPUTATIONAL", "COMPUTATIONAL-3", "BINARY", "DISPLAY", "USAGE", "SIGN",
                 "JUSTIFIED", "JUST", "SYNC", "SYNCHRONIZED", "BLANK", "EXTERNAL", "GLOBAL"}


def picture_bytes(pic: str, usage: str | None) -> int:
    pic = pic.rstrip(".").upper()
    expanded = re.sub(r"(.)\((\d+)\)", lambda m: m.group(1) * int(m.group(2)), pic)
    expanded = re.sub(r"CR|DB", "..", expanded)
    digits = expanded.count("9")
    if usage in ("COMP-3", "COMPUTATIONAL-3", "PACKED-DECIMAL"):
        return (digits + 2) // 2
    if usage in ("COMP", "COMPUTATIONAL", "COMP-4", "COMPUTATIONAL-4", "COMP-5",
                 "COMPUTATIONAL-5", "BINARY"):
        return 2 if digits <= 4 else 4 if digits <= 9 else 8
    if usage in ("COMP-1", "COMPUTATIONAL-1"):
        return 4
    if usage in ("COMP-2", "COMPUTATIONAL-2", "POINTER"):
        return 8
    return len(re.sub(r"[SVP]", "", expanded))


def sentences(masked: str):
    start = 0
    for m in re.finditer(r"\.(?=\s|$)", masked):
        yield masked[start:m.start()]
        start = m.end()
    if start < len(masked):
        yield masked[start:]


def parse_data_items(masked: str) -> list[dict]:
    items = []
    for sent in sentences(masked):
        m = ITEM_RE.match(sent)
        if not m:
            continue
        level = int(m.group(1))
        if level == 0 or (level > 49 and level not in (66, 77, 88)):
            continue
        name, rest = m.group(2) or "FILLER", m.group(3) or ""
        if name in DATA_KEYWORDS or name.startswith("PIC"):
            rest, name = (name + " " + rest), "FILLER"
        pic = PIC_RE.search(rest)
        usage = USAGE_RE.search(rest)
        occ = OCCURS_RE.search(rest)
        red = REDEF_RE.search(rest)
        items.append(
            {
                "level": level,
                "name": name,
                "pic": pic.group(1).rstrip(".") if pic else None,
                "usage": usage.group(1) if usage else None,
                "occurs": int(occ.group(2) or occ.group(1)) if occ else None,
                "redefines": red.group(1) if red else None,
            }
        )
    return items


def record_sizes(items: list[dict]) -> list[dict]:
    """Byte size of every level-01 record described by ``items``."""
    body = [it for it in items if it["level"] not in (66, 88)]

    def size(i: int, inherited: str | None) -> tuple[int, int]:
        it = body[i]
        usage = it["usage"] or inherited
        if it["pic"]:
            s, j = picture_bytes(it["pic"], usage), i + 1
        else:
            s, j = 0, i + 1
            while j < len(body) and body[j]["level"] > it["level"] and body[j]["level"] != 77:
                cs, nj = size(j, usage)
                if not body[j]["redefines"]:
                    s += cs
                j = nj
        return s * (it["occurs"] or 1), j

    out, i = [], 0
    while i < len(body):
        it = body[i]
        if it["level"] in (1, 77):
            s, j = size(i, None)
            out.append({"name": it["name"], "level": it["level"], "bytes": s,
                        "redefines": it["redefines"]})
            i = j
        else:
            i += 1
    return out


SELECT_RE = re.compile(r"\bSELECT\s+(?:OPTIONAL\s+)?([A-Z0-9-]+)\s+ASSIGN\s+TO\s+([A-Z0-9'\"-]+)([^.]*)")
FD_RE = re.compile(r"\bFD\s+([A-Z0-9-]+)")
CICS_RE = re.compile(r"EXEC\s+CICS\s+(.*?)\s*END-EXEC", re.S)
CICS_FILE_VERBS = {"READ", "WRITE", "REWRITE", "DELETE", "STARTBR", "READNEXT", "READPREV",
                   "ENDBR", "RESETBR", "UNLOCK"}
CICS_TWO_WORD = {("SEND", "MAP"), ("SEND", "TEXT"), ("SEND", "CONTROL"), ("SEND", "PAGE"),
                 ("RECEIVE", "MAP"), ("HANDLE", "ABEND"), ("HANDLE", "AID"),
                 ("HANDLE", "CONDITION"), ("IGNORE", "CONDITION"), ("WRITEQ", "TS"),
                 ("WRITEQ", "TD"), ("READQ", "TS"), ("READQ", "TD"), ("DELETEQ", "TS"),
                 ("DELETEQ", "TD"), ("SYNCPOINT", "ROLLBACK"), ("PUSH", "HANDLE"),
                 ("POP", "HANDLE"), ("SEND", "FROM")}
OPEN_RE = re.compile(r"\bOPEN\s+((?:(?:INPUT|OUTPUT|I-O|EXTEND)\s+(?:[A-Z0-9-]+\s*)+)+)")
BATCH_VERB_RE = re.compile(r"(?<![A-Z0-9-])(READ|WRITE|REWRITE|DELETE|START)\s+([A-Z0-9-]+)")
TRANID_RE = re.compile(r"[A-Z0-9-]*TRANID[A-Z0-9-]*\s+PIC\s+X\(0?4\)\s+VALUE\s+'([A-Z0-9]{4})'")
CALL_RE = re.compile(r"(?<![A-Z0-9-])CALL\s+(['\"]([A-Z0-9-]+)['\"]|[A-Z0-9-]+)")
LITERAL_VALUE_RE_TMPL = r"\b{name}\s+PIC\s+[^.]*?VALUE\s+(?:IS\s+)?'([^']*)'"


PAREN_ARG = r"\(\s*((?:[^()]|\([^()]*\))+?)\s*\)"


def resolve_literal(src: CobolText, token: str) -> str:
    """Literal -> its value; data-name with a constant VALUE -> that value; else ``<data-name>``
    (a value only known at run time, e.g. a program name taken from the COMMAREA)."""
    token = token.strip()
    if token[:1] in "'\"":
        return token.strip("'\"").strip()
    m = re.search(LITERAL_VALUE_RE_TMPL.format(name=re.escape(token)), src.text, re.S)
    return m.group(1).strip() if m else f"<{token}>"


def is_dynamic(name: str) -> bool:
    return name.startswith("<")


def analyze_cobol(path: Path, copybook_index: dict[str, Path]) -> dict:
    src = CobolText(path)
    text, masked = src.text, src.masked
    pid = re.search(r"PROGRAM-ID\.\s+([A-Z0-9-]+)", text)
    program_id = pid.group(1) if pid else path.stem.upper()

    copybooks = []
    for m in re.finditer(r"(?<![A-Z0-9-])COPY\s+([A-Z0-9-]+)", masked):
        name = m.group(1)
        if name not in copybooks:
            copybooks.append(name)
    sql_includes = sorted({m.group(1) for m in re.finditer(r"EXEC\s+SQL\s+INCLUDE\s+([A-Z0-9-]+)", masked)})

    uses_cics = bool(re.search(r"EXEC\s+CICS", masked))
    uses_sql = bool(re.search(r"EXEC\s+SQL", masked))
    uses_dli = bool(re.search(r"CALL\s+'(?:CBLTDLI|AIBTDLI)'", text))
    uses_mq = bool(re.search(r"CALL\s+'MQ[A-Z]+'", text))

    # batch file control -------------------------------------------------
    files = OrderedDict()
    proc_off = masked.find("PROCEDURE DIVISION")
    proc_off = proc_off if proc_off >= 0 else len(masked)
    env_masked = masked[:proc_off]
    for m in SELECT_RE.finditer(env_masked):
        fname, dd, clause = m.group(1), m.group(2), m.group(3)
        dd = dd.strip("'\"").split("-")[-1] if "-" in dd and not dd.startswith("'") else dd.strip("'\"")
        org = re.search(r"ORGANIZATION\s+(?:IS\s+)?(INDEXED|SEQUENTIAL|RELATIVE|LINE\s+SEQUENTIAL)", clause)
        acc = re.search(r"ACCESS\s+(?:MODE\s+)?(?:IS\s+)?(SEQUENTIAL|RANDOM|DYNAMIC)", clause)
        key = re.search(r"RECORD\s+KEY\s+(?:IS\s+)?([A-Z0-9-]+)", clause)
        akey = re.findall(r"ALTERNATE\s+(?:RECORD\s+)?KEY\s+(?:IS\s+)?([A-Z0-9-]+)", clause)
        files[fname] = OrderedDict(
            dd_name=dd,
            organization=(org.group(1).replace("  ", " ") if org else "SEQUENTIAL"),
            access_mode=(acc.group(1) if acc else "SEQUENTIAL"),
            record_key=(key.group(1) if key else None),
            alternate_keys=akey,
            open_modes=[],
            verbs=[],
            records=[],
        )
    # FD -> record names
    rec_to_file = {}
    for fm in FD_RE.finditer(env_masked):
        fname = fm.group(1)
        if fname not in files:
            continue
        tail = env_masked[fm.end():]
        nxt = re.search(r"\bFD\s+[A-Z0-9-]+|WORKING-STORAGE\s+SECTION|LINKAGE\s+SECTION", tail)
        chunk = tail[: nxt.start()] if nxt else tail
        for rm in re.finditer(r"(?:^|\n)\s*01\s+([A-Z0-9-]+)", chunk):
            files[fname]["records"].append(rm.group(1))
            rec_to_file[rm.group(1)] = fname
        for cm in re.finditer(r"(?<![A-Z0-9-])COPY\s+([A-Z0-9-]+)", chunk):
            cp = copybook_index.get(cm.group(1))
            if cp:
                for it in parse_data_items(CobolText(cp).masked):
                    if it["level"] == 1:
                        files[fname]["records"].append(it["name"])
                        rec_to_file[it["name"]] = fname
    proc_masked = masked[proc_off:]
    for m in OPEN_RE.finditer(proc_masked):
        mode = None
        for tok in m.group(1).split():
            if tok in ("INPUT", "OUTPUT", "I-O", "EXTEND"):
                mode = tok
            elif tok in files and mode:
                if mode not in files[tok]["open_modes"]:
                    files[tok]["open_modes"].append(mode)
    for m in BATCH_VERB_RE.finditer(proc_masked):
        verb, target = m.group(1), m.group(2)
        fname = target if target in files else rec_to_file.get(target)
        if fname and verb not in files[fname]["verbs"]:
            files[fname]["verbs"].append(verb)

    # CICS -------------------------------------------------------------------
    cics_commands = Counter()
    cics_files: dict[str, dict] = OrderedDict()
    mapsets, maps = set(), set()
    for m in CICS_RE.finditer(masked):
        block = m.group(1)
        toks = re.findall(r"[A-Z][A-Z0-9-]*", block.split("(")[0]) or block.split()[:1]
        if not toks:
            continue
        cmd = toks[0]
        second = re.match(r"\s*" + re.escape(cmd) + r"\s+([A-Z]+)", block)
        if second and (cmd, second.group(1)) in CICS_TWO_WORD and second.group(1) != "FROM":
            cmd = f"{cmd} {second.group(1)}"
        cics_commands[cmd] += 1
        raw_block = text[m.start(1): m.end(1)]
        if toks[0] in CICS_FILE_VERBS:
            fm = re.search(r"(?:FILE|DATASET)\s*" + PAREN_ARG, raw_block)
            if fm:
                fname = resolve_literal(src, fm.group(1))
                entry = cics_files.setdefault(fname, OrderedDict(cics_file=fname, commands=[]))
                if toks[0] not in entry["commands"]:
                    entry["commands"].append(toks[0])
        for mm in re.finditer(r"MAPSET\s*" + PAREN_ARG, raw_block):
            mapsets.add(resolve_literal(src, mm.group(1)))
        for mm in re.finditer(r"(?<![A-Z])MAP\s*" + PAREN_ARG, raw_block):
            maps.add(resolve_literal(src, mm.group(1)))

    # transaction id, calls ----------------------------------------------------
    tranid = TRANID_RE.search(text)
    calls = []
    for m in CALL_RE.finditer(text):
        target = m.group(2) or resolve_literal(src, m.group(1))
        if target not in calls:
            calls.append(target)
    xctl_links = []
    for m in re.finditer(r"EXEC\s+CICS\s+(XCTL|LINK)\s+PROGRAM\s*" + PAREN_ARG, text, re.S):
        tgt = resolve_literal(src, m.group(2))
        if tgt not in xctl_links:
            xctl_links.append(tgt)

    counts, lines = count_constructs(src)
    via_copy = Counter()
    for cb in copybooks:
        cp = copybook_index.get(cb)
        if cp:
            c, _ = count_constructs(CobolText(cp))
            for k in ("COMP-3", "COMP", "REDEFINES", "OCCURS", "OCCURS DEPENDING"):
                via_copy[k] += c[k]

    if uses_cics:
        ptype = "online"
    elif not files and not uses_sql and not uses_dli:
        ptype = "utility"
    else:
        ptype = "batch"
    using = re.search(r"PROCEDURE\s+DIVISION\s+USING", masked)

    return OrderedDict(
        program_id=program_id,
        type=ptype,
        subprogram=bool(using),
        transaction_id=(tranid.group(1) if tranid else None),
        mapsets=sorted(m for m in mapsets if not is_dynamic(m)),
        maps=sorted(m for m in maps if not is_dynamic(m)),
        dynamic_map_refs=sorted(m.strip("<>") for m in mapsets | maps if is_dynamic(m)),
        loc=OrderedDict(total=src.total_lines, code=src.code_lines, comment=src.comment_lines,
                        blank=src.blank_lines),
        copybooks=copybooks,
        sql_includes=sql_includes,
        files=[OrderedDict(file_name=k, **v) for k, v in files.items()],
        cics_files=list(cics_files.values()),
        cics_commands=OrderedDict(sorted(cics_commands.items())),
        calls=calls,
        xctl_link_targets=[t for t in xctl_links if not is_dynamic(t)],
        dynamic_xctl_link_vars=[t.strip("<>") for t in xctl_links if is_dynamic(t)],
        uses=OrderedDict(cics=uses_cics, sql=uses_sql, dli=uses_dli, mq=uses_mq),
        constructs=OrderedDict((k, counts[k]) for k in CONSTRUCT_RES),
        construct_lines=lines,
        constructs_via_copybooks=OrderedDict(sorted(via_copy.items())),
    )


def analyze_copybook(path: Path) -> dict:
    src = CobolText(path)
    items = parse_data_items(src.masked)
    recs = record_sizes(items)
    counts, _ = count_constructs(src)
    nested = sorted({m.group(1) for m in re.finditer(r"(?<![A-Z0-9-])COPY\s+([A-Z0-9-]+)", src.masked)})
    return OrderedDict(
        loc=OrderedDict(total=src.total_lines, code=src.code_lines, comment=src.comment_lines),
        records=recs,
        record_length=(max((r["bytes"] for r in recs if not r["redefines"]), default=0) or None),
        elementary_items=sum(1 for it in items if it["pic"]),
        level_88_count=sum(1 for it in items if it["level"] == 88),
        constructs=OrderedDict((k, counts[k]) for k in ("COMP-3", "COMP", "REDEFINES", "OCCURS", "OCCURS DEPENDING")),
        nested_copies=nested,
        has_procedure_code=bool(re.search(r"\bPERFORM\b|\bMOVE\b|\bEVALUATE\b", src.masked)),
    )


# ---------------------------------------------------------------------------
# BMS
# ---------------------------------------------------------------------------

def analyze_bms(path: Path) -> dict:
    lines = read_text(path).split("\n")
    mapset, maps, fields, named, cur = None, [], 0, 0, None
    joined = []
    for line in lines:
        line = line.rstrip("\r")
        if line.startswith("*"):
            continue
        joined.append(line[:71])
    text = "\n".join(joined)
    for m in re.finditer(r"(?m)^(\S+)?\s+(DFHMSD|DFHMDI|DFHMDF)\b(.*)$", text):
        label, macro = m.group(1), m.group(2)
        if macro == "DFHMSD" and label:
            mapset = label
        elif macro == "DFHMDI":
            # grab continuation lines to find SIZE
            tail = text[m.end():]
            nxt = re.search(r"(?m)^\S*\s+DFHMD[IF]\b", tail)
            block = m.group(3) + " " + (tail[: nxt.start()] if nxt else tail)
            size = re.search(r"SIZE=\((\d+),(\d+)\)", block)
            maps.append(OrderedDict(map=label, size=(f"{size.group(1)}x{size.group(2)}" if size else None)))
        elif macro == "DFHMDF":
            fields += 1
            if label:
                named += 1
    return OrderedDict(
        mapset=mapset or path.stem.upper(),
        maps=maps,
        fields=fields,
        named_fields=named,
        loc=len([l for l in lines if l.strip()]),
    )


# ---------------------------------------------------------------------------
# JCL / PROC
# ---------------------------------------------------------------------------

def split_params(s: str) -> list[str]:
    out, depth, cur, quote = [], 0, "", None
    for ch in s:
        if quote:
            cur += ch
            if ch == quote:
                quote = None
            continue
        if ch in "'\"":
            quote = ch
            cur += ch
        elif ch == "(":
            depth += 1
            cur += ch
        elif ch == ")":
            depth -= 1
            cur += ch
        elif ch == "," and depth == 0:
            out.append(cur)
            cur = ""
        else:
            cur += ch
    if cur:
        out.append(cur)
    return [p.strip() for p in out if p.strip()]


def param_dict(params: list[str]) -> OrderedDict:
    d = OrderedDict()
    positional = []
    for p in params:
        if "=" in p and not p.startswith("'"):
            k, v = p.split("=", 1)
            d[k.strip()] = v.strip()
        else:
            positional.append(p)
    if positional:
        d["_positional"] = positional
    return d


GDG_REL_RE = re.compile(r"^(.*)\(([+-]?\d+)\)$")


def operand_field(text: str) -> str:
    """JCL operands end at the first blank outside quotes/parens; the rest is a comment."""
    depth, quote = 0, None
    for i, ch in enumerate(text):
        if quote:
            if ch == quote:
                quote = None
        elif ch in "'\"":
            quote = ch
        elif ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
        elif ch == " " and depth <= 0:
            return text[:i]
    return text


def analyze_jcl(path: Path) -> dict:
    lines = [l.rstrip("\r") for l in read_text(path).split("\n")]
    statements = []  # (name, op, params_text, instream_lines)
    i = 0
    n = len(lines)
    while i < n:
        line = lines[i]
        i += 1
        if not line.startswith("//") or line.startswith("//*"):
            continue
        body = line[2:72]
        if not body.strip():
            statements.append(("", "NULL", "", []))
            continue
        m = re.match(r"^(\S*)\s+(\S+)\s*(.*)$", body)
        if not m:
            m2 = re.match(r"^(\S*)\s*$", body)
            statements.append(((m2.group(1) if m2 else body.strip()), "", "", []))
            continue
        name, op, params = m.group(1), m.group(2), operand_field(m.group(3))
        if op not in ("JOB", "EXEC", "DD", "PROC", "PEND", "JCLLIB", "SET", "IF", "ELSE",
                      "ENDIF", "INCLUDE", "OUTPUT", "CNTL", "ENDCNTL"):
            # e.g. "//STEP1 EXEC" already handled; label-only lines / overrides w/o op
            params, op = (op + " " + params).strip(), "?"
        # continuation
        while params.endswith(",") and i < n and lines[i].startswith("//") and not lines[i].startswith("//*"):
            cont = lines[i][2:72].strip()
            if cont.startswith("//") or re.match(r"^\S+\s+(DD|EXEC|JOB)\b", cont):
                break
            params += operand_field(cont)
            i += 1
        instream = []
        if op == "DD" and re.match(r"^(\*|DATA)(,|$)", params):
            while i < n and not lines[i].startswith("//") and not lines[i].startswith("/*"):
                instream.append(lines[i][:80])
                i += 1
            if i < n and lines[i].startswith("/*"):
                i += 1
        statements.append((name, op, params, instream))

    job = OrderedDict(job_name=None, job_params=None, jcllib=None, proc_name=None, steps=[])
    steps = job["steps"]
    cur_step = None
    last_dd = None
    for name, op, params, instream in statements:
        pd = param_dict(split_params(params))
        if op == "JOB":
            job["job_name"] = name
            job["job_params"] = OrderedDict((k, v) for k, v in pd.items() if k in ("CLASS", "MSGCLASS", "COND", "REGION", "TIME", "NOTIFY"))
        elif op == "PROC":
            job["proc_name"] = name
        elif op == "JCLLIB":
            job["jcllib"] = pd.get("ORDER")
        elif op == "EXEC":
            pgm = pd.get("PGM")
            proc = pd.get("PROC") or (pd["_positional"][0] if "_positional" in pd and not pgm else None)
            cur_step = OrderedDict(
                step=name, program=pgm, proc=proc, cond=pd.get("COND"), parm=pd.get("PARM"),
                program_class=("system utility" if pgm in SYSTEM_UTILITIES else "application" if pgm else "procedure"),
                dds=[], utility_commands=[],
            )
            steps.append(cur_step)
            last_dd = None
        elif op == "DD":
            if cur_step is None:
                cur_step = OrderedDict(step="(job level)", program=None, proc=None, cond=None, parm=None,
                                       program_class="job-level DD", dds=[], utility_commands=[])
                steps.append(cur_step)
            dsn = pd.get("DSN") or pd.get("DSNAME")
            disp = pd.get("DISP")
            dcb = pd.get("DCB", "")
            lrecl = pd.get("LRECL") or (re.search(r"LRECL=(\d+)", dcb or "") or [None, None])[1] if (pd.get("LRECL") or re.search(r"LRECL=(\d+)", dcb or "")) else None
            recfm = pd.get("RECFM") or (re.search(r"RECFM=([A-Z]+)", dcb or "").group(1) if re.search(r"RECFM=([A-Z]+)", dcb or "") else None)
            gdg = None
            if dsn:
                dsn = dsn.strip("'")
                gm = GDG_REL_RE.match(dsn)
                if gm and not gm.group(1).endswith("."):
                    dsn, gdg = gm.group(1), gm.group(2)
            kind = ("instream" if re.match(r"^(\*|DATA)(,|$)", params) else "sysout" if "SYSOUT" in pd
                    else "dummy" if "DUMMY" in pd.get("_positional", []) else "dataset" if dsn else "other")
            entry = OrderedDict(dd=name, kind=kind, dsn=dsn, disp=disp, lrecl=(int(lrecl) if lrecl else None),
                                recfm=recfm, gdg_generation=gdg, sysout=pd.get("SYSOUT"),
                                instream_lines=len(instream))
            if not name and last_dd is not None:
                entry["dd"] = last_dd["dd"]
                entry["concatenated"] = True
            cur_step["dds"].append(entry)
            if name:
                last_dd = entry
            if instream and name in ("SYSIN", "SYSTSIN", "SYSUT1", "INPUT") or (instream and cur_step["program"] in SYSTEM_UTILITIES):
                for il in instream:
                    s = il.strip()
                    vm = re.match(r"^(DEFINE|DELETE|REPRO|ALTER|BLDINDEX|PRINT|LISTCAT|VERIFY|SET|SORT|MERGE|INCLUDE|OMIT|OPTION|OUTREC|INREC|GENERATE|COPY|SELECT|RUN|DSN|END)\b\s*(\S*)", s)
                    if vm:
                        verb = vm.group(1)
                        if verb == "DEFINE":
                            verb = f"DEFINE {vm.group(2).strip('(').split('(')[0]}"
                        elif verb == "DELETE":
                            verb = "DELETE"
                        cur_step["utility_commands"].append(verb)
                    for dm in re.finditer(r"(?:NAME|INDATASET|OUTDATASET|IDS|ODS|RELATE|PATHENTRY)\s*\(\s*([A-Z0-9.@#$-]+(?:\([+-]?\d+\))?)\s*\)", s):
                        ds = dm.group(1)
                        if "." in ds:
                            cur_step.setdefault("instream_datasets", [])
                            if ds not in cur_step["instream_datasets"]:
                                cur_step["instream_datasets"].append(ds)
    # roll-ups
    datasets = OrderedDict()
    gdg_use = False
    programs = []
    procs = []
    conds = []
    for st in steps:
        if st["program"] and st["program"] not in programs:
            programs.append(st["program"])
        if st["proc"] and st["proc"] not in procs:
            procs.append(st["proc"])
        if st["cond"]:
            conds.append(f"{st['step']}: COND={st['cond']}")
        for dd in st["dds"]:
            if dd["dsn"]:
                datasets.setdefault(dd["dsn"], []).append(f"{st['step']}/{dd['dd']}")
            if dd["gdg_generation"] is not None:
                gdg_use = True
        for ds in st.get("instream_datasets", []):
            base = GDG_REL_RE.match(ds)
            if base:
                gdg_use = True
                ds = base.group(1)
            datasets.setdefault(ds, []).append(f"{st['step']}/instream")
    if job["job_params"] and job["job_params"].get("COND"):
        conds.insert(0, f"JOB: COND={job['job_params']['COND']}")
    job["programs"] = programs
    job["procs_called"] = procs
    job["datasets"] = datasets
    job["uses_gdg"] = gdg_use
    job["cond_codes"] = conds
    job["step_count"] = len([s for s in steps if s["step"] != "(job level)"])
    job["loc"] = len([l for l in lines if l.strip()])
    return job


# ---------------------------------------------------------------------------
# CSD, catalog, scheduler, assembler, misc
# ---------------------------------------------------------------------------

def analyze_csd(path: Path) -> dict:
    text = read_text(path)
    defs = []
    for m in re.finditer(r"(?m)^\s*DEFINE\s+(\w+)\s*\(\s*([^)]+)\)\s*GROUP\s*\(\s*([^)]+)\)(.*?)(?=^\s*(?:DEFINE|ADD|LIST|REMOVE|DELETE)\b|\Z)", text, re.S):
        rtype, rname, group, attrs = m.group(1).upper(), m.group(2).strip(), m.group(3).strip(), m.group(4)
        a = OrderedDict()
        for am in re.finditer(r"\b(PROGRAM|DSNAME|LANGUAGE|TRANSACTION|RECORDFORMAT|ADD|BROWSE|DELETE|READ|UPDATE|TASKDATALOC|RESIDENT|STATUS|DESCRIPTION|TWASIZE)\s*\(\s*([^)]*)\)", attrs):
            a[am.group(1)] = am.group(2).strip()
        defs.append(OrderedDict(type=rtype, name=rname, group=group, attributes=a))
    groups_in_lists = [OrderedDict(group=m.group(1), list=m.group(2)) for m in re.finditer(r"ADD\s+GROUP\s*\(\s*([^)]+)\)\s*LIST\s*\(\s*([^)]+)\)", text)]
    by_type = Counter(d["type"] for d in defs)
    return OrderedDict(
        definitions_by_type=OrderedDict(sorted(by_type.items())),
        transactions=OrderedDict((d["name"], d["attributes"].get("PROGRAM")) for d in defs if d["type"] == "TRANSACTION"),
        files=OrderedDict((d["name"], d["attributes"].get("DSNAME")) for d in defs if d["type"] == "FILE"),
        programs=[d["name"] for d in defs if d["type"] == "PROGRAM"],
        mapsets=[d["name"] for d in defs if d["type"] == "MAPSET"],
        other=[OrderedDict(type=d["type"], name=d["name"]) for d in defs if d["type"] not in ("TRANSACTION", "FILE", "PROGRAM", "MAPSET")],
        groups_in_lists=groups_in_lists,
        definitions=defs,
    )


CATLG_RE = re.compile(r"^.(NONVSAM|CLUSTER|DATA|INDEX|GDG BASE|AIX|PATH|ALIAS|PAGESPACE|USERCATALOG)\s+-+\s+(\S+)")


def analyze_catalog(path: Path) -> dict:
    entries = []
    lines = read_text(path).split("\n")
    for idx, line in enumerate(lines):
        m = CATLG_RE.match(line.rstrip("\r"))
        if m:
            ent = OrderedDict(type=m.group(1), name=m.group(2))
            # pull a few attributes from the following block
            block = "\n".join(lines[idx: idx + 40])
            for key, rx in (("maxlrecl", r"MAXLRECL-+(\d+)"), ("keylen", r"KEYLEN-+(\d+)"), ("rkp", r"RKP-+(\d+)"),
                            ("rec_total", r"REC-TOTAL-+(\d+)"), ("association", r"ASSOCIATIONS\s*\n\s*(?:AIX|CLUSTER|DATA|INDEX|PATH)-+(\S+)")):
                mm = re.search(rx, block)
                if mm and (ent["type"] in ("DATA", "INDEX", "AIX", "PATH", "CLUSTER") or key == "association"):
                    ent[key] = int(mm.group(1)) if mm.group(1).isdigit() else mm.group(1)
            entries.append(ent)
    by_type = Counter(e["type"] for e in entries)
    return OrderedDict(
        listcat_command=next((l.strip() for l in lines if "LISTCAT" in l and "LEVEL" in l), None),
        entries_by_type=OrderedDict(sorted(by_type.items())),
        clusters=[e["name"] for e in entries if e["type"] == "CLUSTER"],
        aix=[e["name"] for e in entries if e["type"] == "AIX"],
        paths=[e["name"] for e in entries if e["type"] == "PATH"],
        gdg_bases=[e["name"] for e in entries if e["type"] == "GDG BASE"],
        nonvsam=[e["name"] for e in entries if e["type"] == "NONVSAM"],
        entries=entries,
    )


def analyze_controlm(path: Path) -> dict:
    root = ET.fromstring(read_text(path).encode("latin-1"))
    folders = []
    for f in root.iter():
        if f.tag not in ("FOLDER", "SMART_FOLDER"):
            continue
        jobs = []
        for j in f.findall("JOB"):
            jobs.append(
                OrderedDict(
                    job_name=j.get("JOBNAME"), member=j.get("MEMNAME"), description=j.get("DESCRIPTION"),
                    task_type=j.get("TASKTYPE"), days=j.get("DAYS"), time_to=j.get("TIMETO"),
                    max_rerun=j.get("MAXRERUN"), cyclic=j.get("CYCLIC"),
                    in_conditions=[c.get("NAME") for c in j.findall("INCOND")],
                    out_conditions=[f"{c.get('SIGN')}{c.get('NAME')}" for c in j.findall("OUTCOND")],
                )
            )
        folders.append(
            OrderedDict(
                folder=f.get("FOLDER_NAME"), type=("smart" if f.tag == "SMART_FOLDER" else "regular"),
                days=f.get("DAYS"), interval=f.get("INTERVAL"), application=(jobs[0]["job_name"] and f.find("JOB").get("APPLICATION")) if jobs else None,
                jobs=jobs,
            )
        )
    return OrderedDict(format="Control-M XML (DEFTABLE)", folders=folders,
                       job_count=sum(len(f["jobs"]) for f in folders),
                       jcl_members=sorted({j["member"] for f in folders for j in f["jobs"]}))


def analyze_ca7(path: Path) -> dict:
    text = read_text(path)
    blocks = re.split(r"(?m)^\s*1LJOB,JOB=", text)[1:]
    jobs = OrderedDict()
    for b in blocks:
        name = re.match(r"([A-Z0-9]+)", b).group(1)
        job = jobs.setdefault(name, OrderedDict(job_name=name, jcl_member=None, system=None, triggers=[], schids=[]))
        hm = re.search(rf"(?m)^\s*{name}\s+(\d+)\s+(\S+)\s+(\S+)", b)
        if hm:
            job["jcl_member"], job["system"] = hm.group(2), hm.group(3)
        for tm in re.finditer(r"JOB=([A-Z0-9]+)\s+SCHID=(\d+)", b):
            t = OrderedDict(job=tm.group(1), schid=tm.group(2))
            if t not in job["triggers"]:
                job["triggers"].append(t)
        for sm in re.finditer(r"SCHID=(\d+)", b):
            if sm.group(1) not in job["schids"]:
                job["schids"].append(sm.group(1))
    return OrderedDict(format="CA-7 LJOB listing", job_count=len(jobs), jobs=list(jobs.values()),
                       jcl_members=sorted({j["jcl_member"] for j in jobs.values() if j["jcl_member"]}))


def analyze_asm(path: Path) -> dict:
    lines = [l.rstrip("\r") for l in read_text(path).split("\n")]
    code = [l for l in lines if l.strip() and not l.startswith("*")]
    csects = re.findall(r"(?m)^(\S+)\s+(?:CSECT|START|RSECT)\b", "\n".join(code))
    macros = re.findall(r"(?m)^\s+MACRO\s*\n\s*(?:\S+\s+)?(\S+)", "\n".join(code))
    entries = re.findall(r"(?m)^\s+ENTRY\s+(\S+)", "\n".join(code))
    svc = sorted(set(re.findall(r"(?m)^\S*\s+(STIMER|WAIT|WTO|TIME|STCK|GETMAIN|FREEMAIN|LINK|LOAD|ABEND|CONVTOD|STCKCONV)\b", "\n".join(code))))
    return OrderedDict(loc=OrderedDict(total=len([l for l in lines if l.strip()]), code=len(code)),
                       csects=csects, macro_definitions=macros, entries=entries, system_macros_used=svc)


def analyze_ddl(path: Path) -> dict:
    text = read_text(path).upper()
    return OrderedDict(tables=re.findall(r"CREATE\s+TABLE\s+([A-Z0-9_.]+)", text),
                       indexes=re.findall(r"CREATE\s+(?:UNIQUE\s+)?INDEX\s+([A-Z0-9_.]+)", text),
                       tablespaces=re.findall(r"CREATE\s+TABLESPACE\s+([A-Z0-9_.]+)", text),
                       loc=len([l for l in text.split("\n") if l.strip()]))


def analyze_ims(path: Path) -> dict:
    text = read_text(path)
    return OrderedDict(kind=("DBD" if re.search(r"\bDBD\s", text) else "PSB" if re.search(r"\bPSBGEN\b|\bPCB\s", text) else "unknown"),
                       names=re.findall(r"\b(?:DBD|PSBGEN)\s+[^\n]*?NAME=([A-Z0-9]+)", text) or re.findall(r"DBDNAME=([A-Z0-9]+)", text),
                       segments=re.findall(r"SEGM\s+NAME=([A-Z0-9]+)", text),
                       pcbs=len(re.findall(r"(?m)^\S*\s+PCB\s", text)),
                       loc=len([l for l in text.split("\n") if l.strip()]))


# ---------------------------------------------------------------------------
# data files
# ---------------------------------------------------------------------------

def readme_dataset_table() -> OrderedDict:
    table = OrderedDict()
    for line in read_text(ROOT / "README.md").split("\n"):
        m = re.match(r"\s*\|\s*(AWS\.M2\.CARDDEMO\.[A-Z0-9.]+)\s*\|\s*([^|]*?)\s*\|\s*([A-Z0-9]+)\s*\|\s*([A-Z]+)\s*\|\s*(\d+)\s*\|", line)
        if m:
            table[m.group(1)] = OrderedDict(description=m.group(2), copybook=m.group(3), recfm=m.group(4), lrecl=int(m.group(5)))
    return table


def analyze_data(path: Path, readme: OrderedDict, copybooks: dict) -> dict:
    data = path.read_bytes()
    size = len(data)
    name = path.name
    ascii_dir = path.parent.name == "ASCII"
    entry = OrderedDict(encoding=("ASCII" if ascii_dir else "EBCDIC"), bytes=size)
    if ascii_dir:
        lines = data.split(b"\n")
        if lines and lines[-1] == b"":
            lines.pop()
        lens = sorted({len(l.rstrip(b"\r")) for l in lines})
        cb, ds = ASCII_LAYOUTS.get(name, (None, None))
        entry.update(records=len(lines), line_length=(lens[0] if len(lens) == 1 else f"{lens[0]}-{lens[-1]}"),
                     layout_copybook=cb, ebcdic_equivalent=ds, lrecl=(copybooks[cb]["record_length"] if cb in copybooks else None),
                     lrecl_source=("record length computed from layout copybook" if cb else None))
        if cb in copybooks and lens and lens[-1] != copybooks[cb]["record_length"]:
            entry["note"] = (f"line length {entry['line_length']} differs from copybook record length "
                             f"{copybooks[cb]['record_length']} (trailing blanks trimmed / text export)")
    else:
        dsn = name
        info = readme.get(dsn)
        if info:
            cb, lrecl, src = info["copybook"], info["lrecl"], "README dataset table"
            entry["description"] = info["description"]
            entry["recfm"] = info["recfm"]
        elif dsn in EBCDIC_LAYOUT_OVERRIDES:
            cb, src = EBCDIC_LAYOUT_OVERRIDES[dsn]
            lrecl = copybooks[cb]["record_length"]
            entry["recfm"] = "FB"
        else:
            cb, lrecl, src = None, None, "unknown"
        entry.update(dataset=dsn, layout_copybook=cb, lrecl=lrecl, lrecl_source=src)
        if lrecl:
            entry["records"] = size // lrecl
            entry["size_divisible_by_lrecl"] = (size % lrecl == 0)
        if cb and cb in copybooks:
            entry["copybook_record_length"] = copybooks[cb]["record_length"]
            entry["lrecl_matches_copybook"] = (copybooks[cb]["record_length"] == lrecl)
    return entry


# ---------------------------------------------------------------------------
# build
# ---------------------------------------------------------------------------

def classify(path: Path) -> tuple[str, str]:
    parts = path.relative_to(APP).parts
    module = parts[0] if parts[0] in EXTENSION_MODULES else "core"
    sub = parts[1:] if module != "core" else parts
    if path.name == ".gitkeep":
        return module, "placeholder"
    if path.name.lower() == "readme.md":
        return module, "readme"
    return module, DIR_KINDS.get(sub[0], "other")


def build() -> OrderedDict:
    all_files = sorted(p for p in APP.rglob("*") if p.is_file())
    by_kind: dict[tuple[str, str], list[Path]] = defaultdict(list)
    for p in all_files:
        by_kind[classify(p)].append(p)

    copybook_index: dict[str, Path] = {}
    for (module, kind), paths in by_kind.items():
        if kind in ("copybook", "bms_copybook"):
            for p in paths:
                copybook_index.setdefault(p.stem.upper(), p)
    readme = readme_dataset_table()

    inventory = OrderedDict()
    inventory["generated_by"] = "docs/modernization/build_inventory.py"
    inventory["source_root"] = "app/"
    inventory["scope"] = OrderedDict(
        in_scope_modules=["core"],
        out_of_scope_modules=OrderedDict(EXTENSION_MODULES),
        out_of_scope_reason=OUT_OF_SCOPE_REASON,
    )

    modules = OrderedDict()
    artifacts = []  # flat list: every file
    for module in ["core"] + list(EXTENSION_MODULES):
        mod = OrderedDict(
            module=module, in_scope=(module == "core"),
            description=("CardDemo core online (CICS/VSAM) + batch application" if module == "core" else EXTENSION_MODULES[module]),
            programs=OrderedDict(), copybooks=OrderedDict(), bms_copybooks=OrderedDict(), bms_maps=OrderedDict(),
            jcl_jobs=OrderedDict(), jcl_procs=OrderedDict(), control_cards=OrderedDict(), assembler=OrderedDict(),
            asm_macros=OrderedDict(), csd=OrderedDict(), catalog=OrderedDict(), scheduler=OrderedDict(),
            data_files=OrderedDict(), sql_ddl=OrderedDict(), sql_dclgen=OrderedDict(), ims=OrderedDict(), other_files=[],
        )
        cbs = mod["copybooks"]
        for p in by_kind.get((module, "copybook"), []) + by_kind.get((module, "bms_copybook"), []):
            info = analyze_copybook(p)
            info["path"] = rel(p)
            info["used_by"] = []
            (cbs if classify(p)[1] == "copybook" else mod["bms_copybooks"])[p.stem.upper()] = info
        # programs
        for p in by_kind.get((module, "cobol_program"), []):
            info = analyze_cobol(p, copybook_index)
            info["path"] = rel(p)
            info["jcl_jobs"] = []
            info["called_by"] = []
            mod["programs"][info["program_id"]] = info
        for p in by_kind.get((module, "bms_map"), []):
            info = analyze_bms(p)
            info["path"] = rel(p)
            info["used_by"] = []
            info["symbolic_copybook"] = (rel(copybook_index[info["mapset"]]) if info["mapset"] in copybook_index else None)
            mod["bms_maps"][info["mapset"]] = info
        for p in by_kind.get((module, "jcl_job"), []):
            info = analyze_jcl(p)
            info["path"] = rel(p)
            info["scheduled_by"] = []
            mod["jcl_jobs"][p.stem.upper()] = info
        for p in by_kind.get((module, "jcl_proc"), []):
            info = analyze_jcl(p)
            info["path"] = rel(p)
            mod["jcl_procs"][p.stem.upper()] = info
        for p in by_kind.get((module, "control_card"), []):
            lines = [l.rstrip() for l in read_text(p).split("\n") if l.strip() and not l.strip().startswith("/*")]
            mod["control_cards"][p.stem.upper()] = OrderedDict(path=rel(p), statements=lines[:20], loc=len(lines))
        for p in by_kind.get((module, "assembler"), []):
            info = analyze_asm(p)
            info["path"] = rel(p)
            info["called_by"] = []
            mod["assembler"][p.stem.upper()] = info
        for p in by_kind.get((module, "asm_macro"), []):
            info = analyze_asm(p)
            info["path"] = rel(p)
            mod["asm_macros"][p.stem.upper()] = info
        for p in by_kind.get((module, "cics_csd"), []):
            info = analyze_csd(p)
            info["path"] = rel(p)
            mod["csd"][p.name] = info
        for p in by_kind.get((module, "catalog_listing"), []):
            info = analyze_catalog(p)
            info["path"] = rel(p)
            mod["catalog"][p.name] = info
        for p in by_kind.get((module, "scheduler_def"), []):
            info = analyze_controlm(p) if p.suffix.lower() == ".controlm" else analyze_ca7(p)
            info["path"] = rel(p)
            mod["scheduler"][p.name] = info
        for p in by_kind.get((module, "data_file"), []):
            if p.name == ".gitkeep":
                continue
            info = analyze_data(p, readme, cbs if module == "core" else modules["core"]["copybooks"])
            info["path"] = rel(p)
            mod["data_files"][rel(p).split("app/", 1)[1]] = info
        for p in by_kind.get((module, "sql_ddl"), []):
            info = analyze_ddl(p)
            info["path"] = rel(p)
            mod["sql_ddl"][p.stem.upper()] = info
        for p in by_kind.get((module, "sql_dclgen"), []):
            text = read_text(p).upper()
            mod["sql_dclgen"][p.stem.upper()] = OrderedDict(path=rel(p), table=(re.search(r"TABLE\s+([A-Z0-9_.]+)", text) or [None, None])[1],
                                                           host_structure=(re.search(r"(?m)^\s*01\s+([A-Z0-9-]+)", text) or [None, None])[1],
                                                           loc=len([l for l in text.split("\n") if l.strip()]))
        for p in by_kind.get((module, "ims_definition"), []):
            info = analyze_ims(p)
            info["path"] = rel(p)
            mod["ims"][p.name] = info
        for kind in ("readme", "placeholder", "other"):
            for p in by_kind.get((module, kind), []):
                mod["other_files"].append(OrderedDict(path=rel(p), kind=kind))
        for p in by_kind.get((module, "data_file"), []):
            if p.name == ".gitkeep":
                mod["other_files"].append(OrderedDict(path=rel(p), kind="placeholder"))
        modules[module] = mod

    # cross references ----------------------------------------------------------
    core = modules["core"]
    all_programs = {pid: (m, info) for m in modules.values() for pid, info in m["programs"].items()}
    for mname, mod in modules.items():
        for pid, prog in mod["programs"].items():
            for cb in prog["copybooks"]:
                for m2 in modules.values():
                    for coll in ("copybooks", "bms_copybooks"):
                        if cb in m2[coll]:
                            m2[coll][cb]["used_by"].append(pid)
            for ms in prog["mapsets"]:
                for m2 in modules.values():
                    if ms in m2["bms_maps"] and pid not in m2["bms_maps"][ms]["used_by"]:
                        m2["bms_maps"][ms]["used_by"].append(pid)
            for cb in prog["copybooks"]:  # map via symbolic copybook too
                for m2 in modules.values():
                    if cb in m2["bms_maps"] and pid not in m2["bms_maps"][cb]["used_by"]:
                        m2["bms_maps"][cb]["used_by"].append(pid)
            for tgt in prog["calls"] + prog["xctl_link_targets"]:
                if tgt in all_programs:
                    all_programs[tgt][1]["called_by"].append(pid)
                for m2 in modules.values():
                    if tgt in m2["assembler"]:
                        m2["assembler"][tgt]["called_by"].append(pid)
        for jname, job in mod["jcl_jobs"].items():
            for st in job["steps"]:
                if st["program"] in all_programs:
                    all_programs[st["program"]][1]["jcl_jobs"].append(f"{jname}/{st['step']}")
                    # attach datasets to program file entries by DD name
                    prog = all_programs[st["program"]][1]
                    for f in prog["files"]:
                        for dd in st["dds"]:
                            if dd["dd"] == f["dd_name"] and dd["dsn"]:
                                f.setdefault("datasets", [])
                                ds = dd["dsn"] + (f"({dd['gdg_generation']})" if dd["gdg_generation"] is not None else "")
                                if ds not in f["datasets"]:
                                    f["datasets"].append(ds)
    # CSD transaction -> program and CICS file -> dataset
    csd_tx = OrderedDict()
    csd_files = OrderedDict()
    for mod in modules.values():
        for csd in mod["csd"].values():
            csd_tx.update(csd["transactions"])
            csd_files.update(csd["files"])
    for pid, (mod, prog) in all_programs.items():
        tx = [t for t, p in csd_tx.items() if p == pid]
        prog["csd_transactions"] = tx
        if not prog["transaction_id"] and tx:
            prog["transaction_id"] = tx[0]
        for cf in prog["cics_files"]:
            cf["dataset"] = csd_files.get(cf["cics_file"])
    # scheduler -> jcl
    for mod in modules.values():
        for sname, sched in mod["scheduler"].items():
            for member in sched["jcl_members"]:
                for m2 in modules.values():
                    if member in m2["jcl_jobs"]:
                        m2["jcl_jobs"][member]["scheduled_by"].append(sname)

    # dataset cross-reference (core) ---------------------------------------------------
    datasets = OrderedDict()
    def ds_entry(dsn):
        return datasets.setdefault(dsn, OrderedDict(catalog_type=None, cics_files=[], jcl_references=[], sample_data=None))
    for cat in core["catalog"].values():
        for e in cat["entries"]:
            if e["type"] in ("CLUSTER", "NONVSAM", "GDG BASE", "AIX", "PATH"):
                ds_entry(e["name"])["catalog_type"] = e["type"]
    for cf, dsn in csd_files.items():
        if dsn:
            ds_entry(dsn)["cics_files"].append(cf)
    for coll in ("jcl_jobs", "jcl_procs"):
        for jname, job in core[coll].items():
            for dsn, refs in job["datasets"].items():
                ds_entry(dsn)["jcl_references"].extend(f"{jname}:{r}" for r in refs)
    for key, df in core["data_files"].items():
        if df.get("dataset"):
            ds_entry(df["dataset"])["sample_data"] = df["path"]
    inventory["datasets"] = OrderedDict(sorted(datasets.items()))

    # counts -------------------------------------------------------------------------------
    def count_module(mod):
        ptypes = Counter(p["type"] for p in mod["programs"].values())
        return OrderedDict(
            cobol_programs=len(mod["programs"]),
            cobol_programs_by_type=OrderedDict(online=ptypes["online"], batch=ptypes["batch"], utility=ptypes["utility"]),
            cobol_loc_total=sum(p["loc"]["total"] for p in mod["programs"].values()),
            cobol_loc_code=sum(p["loc"]["code"] for p in mod["programs"].values()),
            copybooks=len(mod["copybooks"]), bms_copybooks=len(mod["bms_copybooks"]), bms_maps=len(mod["bms_maps"]),
            jcl_jobs=len(mod["jcl_jobs"]), jcl_procs=len(mod["jcl_procs"]), control_cards=len(mod["control_cards"]),
            assembler=len(mod["assembler"]), asm_macros=len(mod["asm_macros"]), csd=len(mod["csd"]),
            catalog_listings=len(mod["catalog"]), scheduler_defs=len(mod["scheduler"]), data_files=len(mod["data_files"]),
            sql_ddl=len(mod["sql_ddl"]), sql_dclgen=len(mod["sql_dclgen"]), ims_definitions=len(mod["ims"]),
            other_files=len(mod["other_files"]),
        )
    inventory["counts"] = OrderedDict((m, count_module(mod)) for m, mod in modules.items())
    inventory["modules"] = modules

    # coverage ---------------------------------------------------------------------------
    covered = set()
    for mod in modules.values():
        for key in ("programs", "copybooks", "bms_copybooks", "bms_maps", "jcl_jobs", "jcl_procs", "control_cards",
                    "assembler", "asm_macros", "csd", "catalog", "scheduler", "data_files", "sql_ddl", "sql_dclgen", "ims"):
            for info in mod[key].values():
                covered.add(info["path"])
        for o in mod["other_files"]:
            covered.add(o["path"])
    all_rel = {rel(p) for p in all_files}
    missing = sorted(all_rel - covered)
    extra = sorted(covered - all_rel)
    inventory["coverage"] = OrderedDict(files_under_app=len(all_rel), files_in_inventory=len(covered & all_rel),
                                        missing=missing, not_on_disk=extra, complete=(not missing and not extra))
    inventory["files"] = sorted(all_rel)
    return inventory


# ---------------------------------------------------------------------------
# markdown
# ---------------------------------------------------------------------------

def fmt_files(prog) -> str:
    parts = []
    for f in prog["files"]:
        modes = "/".join(f["open_modes"]) or "-"
        verbs = ",".join(f["verbs"]) or "-"
        parts.append(f"`{f['dd_name']}` ({f['organization'][:3]}; OPEN {modes}; {verbs})")
    for cf in prog["cics_files"]:
        parts.append(f"`{cf['cics_file']}` (CICS; {','.join(cf['commands'])})")
    return "<br>".join(parts) if parts else "-"


def render_markdown(inv: OrderedDict) -> str:
    core = inv["modules"]["core"]
    cnt = inv["counts"]
    cov = inv["coverage"]
    L = []
    L.append("# 01 — Mainframe artifact inventory (CardDemo)")
    L.append("")
    L.append("Generated by `docs/modernization/build_inventory.py` from the sources under `app/` on `main`. "
             "Regenerate with `python3 docs/modernization/build_inventory.py`; verify freshness and coverage with `--check`. "
             "The machine-readable form (every field below plus per-step JCL detail, per-record copybook sizes, "
             "CSD/catalog/scheduler detail) is `docs/modernization/inventory.json`.")
    L.append("")
    L.append("## Scope")
    L.append("")
    L.append("- **In scope:** the core CardDemo application (`app/cbl`, `app/cpy`, `app/cpy-bms`, `app/bms`, `app/jcl`, `app/proc`, "
             "`app/ctl`, `app/asm`, `app/maclib`, `app/csd`, `app/catlg`, `app/scheduler`, `app/data`).")
    L.append(f"- **Out of scope (inventoried only):** {', '.join(f'`app/{m}`' for m in EXTENSION_MODULES)}. {OUT_OF_SCOPE_REASON}")
    L.append("")
    L.append("## Coverage check")
    L.append("")
    L.append(f"- Files under `app/`: **{cov['files_under_app']}**; files in this inventory: **{cov['files_in_inventory']}**; "
             f"missing: **{len(cov['missing'])}**; complete: **{cov['complete']}**.")
    if cov["missing"]:
        L.append("- Missing: " + ", ".join(f"`{m}`" for m in cov["missing"]))
    L.append("")
    L.append("## Summary counts")
    L.append("")
    rows = []
    labels = [("cobol_programs", "COBOL programs"), ("cobol_loc_total", "COBOL lines (total)"), ("cobol_loc_code", "COBOL lines (code only)"),
              ("copybooks", "Copybooks"), ("bms_copybooks", "BMS symbolic copybooks"), ("bms_maps", "BMS mapsets"), ("jcl_jobs", "JCL jobs"),
              ("jcl_procs", "JCL procedures"), ("control_cards", "Control cards"), ("assembler", "Assembler programs"), ("asm_macros", "Assembler macros"),
              ("csd", "CICS CSD files"), ("catalog_listings", "Catalog listings"), ("scheduler_defs", "Scheduler definitions"), ("data_files", "Sample data files"),
              ("sql_ddl", "SQL DDL"), ("sql_dclgen", "SQL DCLGEN"), ("ims_definitions", "IMS DBD/PSB"), ("other_files", "README / .gitkeep")]
    mods = list(inv["modules"])
    for key, label in labels:
        rows.append([label] + [cnt[m][key] for m in mods])
    bt = cnt["core"]["cobol_programs_by_type"]
    rows.insert(1, ["— of which online / batch / utility"] + [
        f"{cnt[m]['cobol_programs_by_type']['online']} / {cnt[m]['cobol_programs_by_type']['batch']} / {cnt[m]['cobol_programs_by_type']['utility']}" for m in mods])
    L.append(md_table(["Artifact", "core (in scope)"] + [f"{m} (out of scope)" for m in mods[1:]], rows))
    L.append("")

    # programs ---------------------------------------------------------------------
    L.append("## COBOL programs (core)")
    L.append("")
    L.append("Type: `online` = contains `EXEC CICS`; `batch` = file I/O without CICS; `utility` = called subroutine with no file I/O. "
             "LOC = total lines / code lines (comments and blanks excluded). Construct counts are occurrences in the program source itself; "
             "the `via copybooks` column adds COMP-3 / REDEFINES / OCCURS occurrences pulled in by `COPY`.")
    L.append("")
    rows = []
    for pid, p in sorted(core["programs"].items(), key=lambda kv: ({"online": 0, "batch": 1, "utility": 2}[kv[1]["type"]], kv[0])):
        c, v = p["constructs"], p["constructs_via_copybooks"]
        rows.append([
            f"`{pid}`", p["type"] + (" (sub)" if p["subprogram"] else ""), p["transaction_id"] or "-",
            ", ".join(p["mapsets"]) or "-", f"{p['loc']['total']} / {p['loc']['code']}",
            ", ".join(p["copybooks"]) or "-", fmt_files(p),
            ", ".join(f"{k}×{n}" for k, n in p["cics_commands"].items()) or "-",
            c["GO TO"], c["ALTER"], c["COMP-3"], c["REDEFINES"], c["OCCURS"],
            f"COMP-3 {v.get('COMP-3', 0)}, REDEF {v.get('REDEFINES', 0)}, OCCURS {v.get('OCCURS', 0)}",
            ", ".join(p["calls"] + p["xctl_link_targets"] + [f"dynamic:{v}" for v in p["dynamic_xctl_link_vars"]]) or "-",
            ", ".join(p["jcl_jobs"]) or "-",
        ])
    L.append(md_table(["Program", "Type", "TranID", "Mapset", "LOC", "Copybooks", "Files / datasets (access)", "CICS commands",
                       "GO TO", "ALTER", "COMP-3", "REDEFINES", "OCCURS", "via copybooks", "Calls / XCTL / LINK", "Run by JCL"], rows))
    L.append("")
    L.append("### Batch file access detail (core)")
    L.append("")
    rows = []
    for pid, p in sorted(core["programs"].items()):
        for f in p["files"]:
            rows.append([f"`{pid}`", f["file_name"], f"`{f['dd_name']}`", f["organization"], f["access_mode"], f["record_key"] or "-",
                         "/".join(f["open_modes"]) or "-", ", ".join(f["verbs"]) or "-", "<br>".join(f.get("datasets", [])) or "-"])
    L.append(md_table(["Program", "COBOL file", "DD", "Organization", "Access", "Record key", "OPEN", "Verbs", "Dataset(s) from JCL"], rows))
    L.append("")
    L.append("### CICS file access detail (core)")
    L.append("")
    rows = []
    for pid, p in sorted(core["programs"].items()):
        for cf in p["cics_files"]:
            rows.append([f"`{pid}`", f"`{cf['cics_file']}`", ", ".join(cf["commands"]), cf.get("dataset") or "-"])
    L.append(md_table(["Program", "CICS FILE", "Commands", "Dataset (CSD)"], rows))
    L.append("")
    L.append("### GO TO / ALTER locations (core)")
    L.append("")
    rows = []
    for pid, p in sorted(core["programs"].items()):
        for k in ("GO TO", "ALTER"):
            if p["construct_lines"].get(k):
                rows.append([f"`{pid}`", k, len(p["construct_lines"][k]), ", ".join(str(n) for n in p["construct_lines"][k])])
    L.append(md_table(["Program", "Construct", "Count", "Source lines"], rows))
    L.append("")

    # copybooks ----------------------------------------------------------------------
    L.append("## Copybooks (core, `app/cpy`)")
    L.append("")
    L.append("Record length = computed storage size of the largest non-REDEFINES level-01 item (bytes).")
    L.append("")
    rows = []
    for name, cb in sorted(core["copybooks"].items()):
        recs = ", ".join(f"{r['name']} ({r['bytes']})" + (" redef" if r["redefines"] else "") for r in cb["records"]) or "-"
        c = cb["constructs"]
        rows.append([f"`{name}`", f"{cb['loc']['total']} / {cb['loc']['code']}", cb["record_length"] or "-", recs, c["COMP-3"], c["COMP"], c["REDEFINES"], c["OCCURS"],
                     ", ".join(sorted(set(cb["used_by"]))) or "**unused**"])
    L.append(md_table(["Copybook", "LOC", "Record len", "Level-01 items (bytes)", "COMP-3", "COMP", "REDEFINES", "OCCURS", "Used by"], rows))
    L.append("")
    L.append("## BMS mapsets and symbolic copybooks (core)")
    L.append("")
    rows = []
    for name, bm in sorted(core["bms_maps"].items()):
        tx = sorted({core["programs"][u]["transaction_id"] for u in bm["used_by"] if u in core["programs"] and core["programs"][u]["transaction_id"]})
        rows.append([f"`{name}`", ", ".join(f"{m['map']} ({m['size']})" for m in bm["maps"]), bm["fields"], bm["named_fields"],
                     f"`{Path(bm['symbolic_copybook']).name}`" if bm["symbolic_copybook"] else "-", ", ".join(bm["used_by"]) or "-", ", ".join(tx) or "-"])
    L.append(md_table(["Mapset", "Maps (rows x cols)", "Fields", "Named fields", "Symbolic copybook", "Programs", "TranID"], rows))
    L.append("")

    # JCL -------------------------------------------------------------------------------------
    L.append("## JCL jobs (core, `app/jcl`)")
    L.append("")
    L.append("`GDG` marks jobs that reference relative generations `(+1)`/`(0)`/`(-1)` on a DD or in IDCAMS control cards. "
             "COND codes list every `COND=` on a JOB or EXEC statement. Instream datasets are those named inside IDCAMS/utility control cards.")
    L.append("")
    rows = []
    for name, j in sorted(core["jcl_jobs"].items()):
        steps = "<br>".join(f"{s['step']}: {('PGM=' + s['program']) if s['program'] else ('PROC=' + (s['proc'] or '?'))}"
                            + (f" [{', '.join(s['utility_commands'][:6])}{'…' if len(s['utility_commands']) > 6 else ''}]" if s["utility_commands"] else "")
                            for s in j["steps"] if s["step"] != "(job level)")
        dds = []
        for s in j["steps"]:
            for d in s["dds"]:
                if d["dsn"]:
                    g = f"({d['gdg_generation']})" if d["gdg_generation"] is not None else ""
                    extra = f" LRECL={d['lrecl']}" if d["lrecl"] else ""
                    dds.append(f"`{d['dd']}`→{d['dsn']}{g}{extra}")
            for ds in s.get("instream_datasets", []):
                dds.append(f"(instream) {ds}")
        rows.append([f"`{name}`", j["step_count"], steps, "<br>".join(dds) or "-", "yes" if j["uses_gdg"] else "-",
                     "<br>".join(j["cond_codes"]) or "-", ", ".join(j["scheduled_by"]) or "-"])
    L.append(md_table(["Job", "Steps", "Step → program", "DD → dataset", "GDG", "COND codes", "Scheduler"], rows))
    L.append("")
    L.append("## JCL procedures (`app/proc`) and control cards (`app/ctl`)")
    L.append("")
    rows = []
    for name, j in sorted(core["jcl_procs"].items()):
        steps = "<br>".join(f"{s['step']}: {('PGM=' + s['program']) if s['program'] else ('PROC=' + (s['proc'] or '?'))}" for s in j["steps"])
        dds = "<br>".join(f"`{d['dd']}`→{d['dsn']}" + (f"({d['gdg_generation']})" if d["gdg_generation"] is not None else "") for s in j["steps"] for d in s["dds"] if d["dsn"])
        used = [jn for jn, jj in core["jcl_jobs"].items() if name in jj["procs_called"]]
        rows.append([f"`{name}`", j["path"], steps, dds or "-", ", ".join(used) or "-"])
    L.append(md_table(["Proc", "Path", "Steps", "DD → dataset", "Called by job"], rows))
    L.append("")
    rows = [[f"`{n}`", c["path"], " ".join(c["statements"])] for n, c in core["control_cards"].items()]
    L.append(md_table(["Control card", "Path", "Statements"], rows))
    L.append("")

    # asm ------------------------------------------------------------------------------------------
    L.append("## Assembler programs and macros")
    L.append("")
    rows = []
    for n, a in sorted(core["assembler"].items()):
        rows.append([f"`{n}`", "program", a["path"], a["loc"]["code"], ", ".join(a["csects"]) or "-", ", ".join(a["system_macros_used"]) or "-", ", ".join(a["called_by"]) or "-"])
    for n, a in sorted(core["asm_macros"].items()):
        rows.append([f"`{n}`", "macro", a["path"], a["loc"]["code"], ", ".join(a["macro_definitions"]) or "-", ", ".join(a["system_macros_used"]) or "-", "-"])
    L.append(md_table(["Member", "Kind", "Path", "Code lines", "CSECT / macro", "System macros", "Called by"], rows))
    L.append("")

    # CSD -----------------------------------------------------------------------------------------
    L.append("## CICS resource definitions (`app/csd`)")
    L.append("")
    for n, csd in core["csd"].items():
        L.append(f"`{csd['path']}` — definitions by type: " + ", ".join(f"{t} {c}" for t, c in csd["definitions_by_type"].items())
                 + (f"; groups in lists: {', '.join(g['group'] + '→' + g['list'] for g in csd['groups_in_lists'])}" if csd["groups_in_lists"] else ""))
        L.append("")
        rows = [[f"`{t}`", p or "-", "yes" if p in core["programs"] else "**no**"] for t, p in csd["transactions"].items()]
        L.append(md_table(["Transaction", "Program", "Program source in app/cbl"], rows))
        L.append("")
        rows = [[f"`{f}`", d or "-"] for f, d in csd["files"].items()]
        L.append(md_table(["CICS FILE", "DSNAME"], rows))
        L.append("")
        L.append("Programs defined: " + ", ".join(f"`{p}`" for p in csd["programs"]) + ".  Mapsets defined: " + ", ".join(f"`{m}`" for m in csd["mapsets"]) + ".")
        missing_src = [p for p in csd["programs"] if p not in core["programs"]]
        if missing_src:
            L.append("")
            L.append("Programs defined in the CSD with no source under `app/cbl`: " + ", ".join(f"`{p}`" for p in missing_src) + ".")
        L.append("")

    # catalog ------------------------------------------------------------------------------------
    L.append("## Catalog listing (`app/catlg`)")
    L.append("")
    for n, cat in core["catalog"].items():
        L.append(f"`{cat['path']}` — `{cat['listcat_command']}` — entries by type: " + ", ".join(f"{t} {c}" for t, c in cat["entries_by_type"].items()))
        L.append("")
        L.append("VSAM clusters: " + ", ".join(f"`{c}`" for c in cat["clusters"]))
        L.append("")
        L.append("Alternate indexes / paths: " + ", ".join(f"`{c}`" for c in cat["aix"] + cat["paths"]))
        L.append("")
        L.append("GDG bases: " + ", ".join(f"`{c}`" for c in cat["gdg_bases"]))
        L.append("")
        L.append(f"Non-VSAM entries ({len(cat['nonvsam'])}, mostly GDG generations and sequential files): see `inventory.json`.")
        L.append("")

    # scheduler ---------------------------------------------------------------------------------
    L.append("## Scheduler definitions (`app/scheduler`)")
    L.append("")
    for n, s in core["scheduler"].items():
        L.append(f"### `{s['path']}` — {s['format']} — {s['job_count']} job definitions")
        L.append("")
        if "folders" in s:
            rows = []
            for f in s["folders"]:
                for j in f["jobs"]:
                    rows.append([f["folder"], f["type"], f"`{j['job_name']}`", j["member"], j["days"] or f["days"] or "-", j["time_to"] or "-",
                                 ", ".join(j["in_conditions"]) or "-", ", ".join(j["out_conditions"]) or "-", "yes" if j["member"] in core["jcl_jobs"] else "**no**"])
            L.append(md_table(["Folder", "Folder type", "Job", "JCL member", "Days", "Time to", "In conditions", "Out conditions", "JCL in app/jcl"], rows))
        else:
            rows = [[f"`{j['job_name']}`", j["jcl_member"] or "-", j["system"] or "-", ", ".join(j["schids"]) or "-",
                     ", ".join(f"{t['job']} (SCHID {t['schid']})" for t in j["triggers"]) or "-",
                     "yes" if j["jcl_member"] in core["jcl_jobs"] else "**no**"] for j in s["jobs"]]
            L.append(md_table(["Job", "JCL member", "System", "SCHIDs", "Triggers", "JCL in app/jcl"], rows))
        L.append("")

    # data ---------------------------------------------------------------------------------------
    L.append("## Sample data files (`app/data`)")
    L.append("")
    L.append("LRECL for EBCDIC files comes from the README dataset table (verified against the computed copybook record length and the file size); "
             "for ASCII files it is the copybook record length, with the observed line length shown for comparison.")
    L.append("")
    rows = []
    for key, d in core["data_files"].items():
        if d["encoding"] == "EBCDIC":
            check = ("ok" if d.get("lrecl_matches_copybook") and d.get("size_divisible_by_lrecl") else "**check**") if d.get("lrecl") else "-"
            rows.append([f"`{d['path']}`", "EBCDIC", d.get("dataset") or "-", d.get("layout_copybook") or "-", d.get("lrecl") or "-", d.get("recfm") or "-",
                         d["bytes"], d.get("records", "-"), f"{d['lrecl_source']}; copybook={d.get('copybook_record_length', '-')}; {check}" + (f"; {d['note']}" if d.get("note") else "")])
        else:
            rows.append([f"`{d['path']}`", "ASCII", d.get("ebcdic_equivalent") or "-", d.get("layout_copybook") or "-", d.get("lrecl") or "-", "text",
                         d["bytes"], d.get("records", "-"), f"line length {d.get('line_length')}" + (f"; {d['note']}" if d.get("note") else "")])
    L.append(md_table(["File", "Encoding", "Dataset", "Layout copybook", "LRECL", "RECFM", "Bytes", "Records", "Notes"], rows))
    L.append("")

    # datasets --------------------------------------------------------------------------------
    L.append("## Dataset cross-reference (core)")
    L.append("")
    L.append("Every `AWS.M2.CARDDEMO.*` dataset seen in the catalog listing (clusters, AIX/paths, GDG bases), the CSD, JCL DDs / IDCAMS cards, or the sample data.")
    L.append("")
    rows = []
    for dsn, d in inv["datasets"].items():
        if not dsn.startswith("AWS.M2.CARDDEMO"):
            continue
        jobs = sorted({r.split(":")[0] for r in d["jcl_references"]})
        rows.append([f"`{dsn}`", d["catalog_type"] or "-", ", ".join(d["cics_files"]) or "-", ", ".join(jobs) or "-", f"`{Path(d['sample_data']).name}`" if d["sample_data"] else "-"])
    L.append(md_table(["Dataset", "Catalog type", "CICS FILE", "JCL jobs/procs", "Sample data"], rows))
    L.append("")

    # extension apps -------------------------------------------------------------------------
    L.append("## Extension applications (inventoried, out of scope)")
    L.append("")
    for mname in EXTENSION_MODULES:
        mod = inv["modules"][mname]
        L.append(f"### `app/{mname}` — {mod['description']}")
        L.append("")
        rows = []
        for pid, p in mod["programs"].items():
            u = p["uses"]
            tech = ", ".join(k.upper() for k, v in u.items() if v) or "-"
            rows.append([p["path"], "COBOL " + p["type"], f"{p['loc']['total']} / {p['loc']['code']}", tech,
                         p["transaction_id"] or "-", ", ".join(p["copybooks"] + p["sql_includes"]) or "-",
                         f"GO TO {p['constructs']['GO TO']}, COMP-3 {p['constructs']['COMP-3']}, REDEF {p['constructs']['REDEFINES']}, OCCURS {p['constructs']['OCCURS']}"])
        for coll, label in (("copybooks", "copybook"), ("bms_copybooks", "BMS copybook")):
            for n, cb in mod[coll].items():
                rows.append([cb["path"], label, f"{cb['loc']['total']} / {cb['loc']['code']}", "-", "-", f"record len {cb['record_length'] or '-'}", ", ".join(sorted(set(cb["used_by"]))) or "-"])
        for n, bm in mod["bms_maps"].items():
            rows.append([bm["path"], "BMS mapset", bm["loc"], "-", "-", f"{len(bm['maps'])} map(s), {bm['fields']} fields", ", ".join(bm["used_by"]) or "-"])
        for n, j in mod["jcl_jobs"].items():
            rows.append([j["path"], "JCL job", j["loc"], "GDG" if j["uses_gdg"] else "-", "-", "; ".join(f"{s['step']}={s['program'] or 'PROC ' + str(s['proc'])}" for s in j["steps"]), ", ".join(j["cond_codes"]) or "-"])
        for n, c in mod["control_cards"].items():
            rows.append([c["path"], "control card", c["loc"], "-", "-", " ".join(c["statements"])[:120], "-"])
        for n, c in mod["csd"].items():
            rows.append([c["path"], "CICS CSD", "-", "-", ", ".join(c["transactions"]), ", ".join(f"{t} {k}" for t, k in c["definitions_by_type"].items()), "-"])
        for n, d in mod["sql_ddl"].items():
            rows.append([d["path"], "SQL DDL", d["loc"], "DB2", "-", "tables " + (", ".join(d["tables"]) or "-") + "; indexes " + (", ".join(d["indexes"]) or "-"), "-"])
        for n, d in mod["sql_dclgen"].items():
            rows.append([d["path"], "DCLGEN", d["loc"], "DB2", "-", f"table {d['table'] or '-'}; host {d['host_structure'] or '-'}", "-"])
        for n, d in mod["ims"].items():
            rows.append([d["path"], f"IMS {d['kind']}", d["loc"], "IMS", "-", "names " + (", ".join(d["names"]) or "-") + (f"; segments {', '.join(d['segments'])}" if d["segments"] else "") + (f"; PCBs {d['pcbs']}" if d["pcbs"] else ""), "-"])
        for key, d in mod["data_files"].items():
            rows.append([d["path"], f"data ({d['encoding']})", d["bytes"], "-", "-", f"LRECL {d.get('lrecl') or 'unknown'}", d.get("lrecl_source") or "-"])
        for o in mod["other_files"]:
            rows.append([o["path"], o["kind"], "-", "-", "-", "-", "-"])
        L.append(md_table(["File", "Kind", "LOC / size", "Tech", "TranID", "Detail", "Used by / notes"], rows))
        L.append("")

    # other core files ------------------------------------------------------------------------------
    if core["other_files"]:
        L.append("## Other files under `app/` (core)")
        L.append("")
        L.append(md_table(["File", "Kind"], [[f"`{o['path']}`", o["kind"]] for o in core["other_files"]]))
        L.append("")

    # observations -----------------------------------------------------------------------------------
    L.append("## Observations for the migration")
    L.append("")
    progs = core["programs"]
    goto = sorted(((p["constructs"]["GO TO"], pid) for pid, p in progs.items() if p["constructs"]["GO TO"]), reverse=True)
    alter = [pid for pid, p in progs.items() if p["constructs"]["ALTER"]]
    L.append(f"- `GO TO` appears in {len(goto)} of {len(progs)} core programs ({', '.join(f'{pid}×{n}' for n, pid in goto)}); "
             f"`ALTER` appears in {len(alter) or 'none'}{' (' + ', '.join(alter) + ')' if alter else ''}.")
    c3 = sum(p["constructs"]["COMP-3"] for p in progs.values())
    c3c = sum(cb["constructs"]["COMP-3"] for cb in core["copybooks"].values())
    L.append(f"- Packed decimal (`COMP-3`): {c3} occurrences in program source plus {c3c} in copybooks; "
             f"`REDEFINES`: {sum(p['constructs']['REDEFINES'] for p in progs.values())} in programs, {sum(cb['constructs']['REDEFINES'] for cb in core['copybooks'].values())} in copybooks; "
             f"`OCCURS`: {sum(p['constructs']['OCCURS'] for p in progs.values())} in programs, {sum(cb['constructs']['OCCURS'] for cb in core['copybooks'].values())} in copybooks.")
    unused = [n for n, cb in core["copybooks"].items() if not cb["used_by"]]
    L.append(f"- Copybooks not referenced by any program under `app/`: {', '.join(f'`{u}`' for u in unused) or 'none'}.")
    csd = next(iter(core["csd"].values()), None)
    if csd:
        no_src = [p for p in csd["programs"] if p not in progs]
        no_tx = [pid for pid, p in progs.items() if p["type"] == "online" and not p["csd_transactions"]]
        L.append(f"- CSD programs with no source in `app/cbl`: {', '.join(f'`{p}`' for p in no_src) or 'none'}; "
                 f"online programs with no CSD transaction: {', '.join(f'`{p}`' for p in no_tx) or 'none'} "
                 f"(reached via XCTL from other programs).")
    gdg_jobs = [n for n, j in core["jcl_jobs"].items() if j["uses_gdg"]]
    L.append(f"- GDG use in JCL: {len(gdg_jobs)} jobs ({', '.join(f'`{j}`' for j in gdg_jobs)}).")
    cond_jobs = [n for n, j in core["jcl_jobs"].items() if j["cond_codes"]]
    L.append(f"- Jobs with `COND=` logic: {', '.join(f'`{j}`' for j in cond_jobs) or 'none'}.")
    not_sched = [n for n in core["jcl_jobs"] if not core["jcl_jobs"][n]["scheduled_by"]]
    L.append(f"- JCL members not referenced by either scheduler definition: {len(not_sched)} ({', '.join(f'`{j}`' for j in not_sched)}).")
    asm_callers = {n: a["called_by"] for n, a in core["assembler"].items()}
    L.append("- Assembler dependencies: " + "; ".join(f"`{n}` called by {', '.join(c) or 'nobody'}" for n, c in asm_callers.items()) + ".")
    no_dd = [(pid, f["dd_name"], ", ".join(p["jcl_jobs"])) for pid, p in progs.items() for f in p["files"] if p["jcl_jobs"] and not f.get("datasets")]
    if no_dd:
        L.append("- COBOL files whose DD is not supplied by the JCL that runs the program: "
                 + "; ".join(f"`{pid}` DD `{dd}` (run by {jobs})" for pid, dd, jobs in no_dd) + ".")
    not_run = [pid for pid, p in progs.items() if p["type"] == "batch" and not p["jcl_jobs"] and not p["called_by"]]
    L.append(f"- Batch programs not executed by any JCL under `app/jcl` and not called by another program: {', '.join(f'`{p}`' for p in not_run) or 'none'}.")
    L.append("")
    L.append("## Verification notes and limitations")
    L.append("")
    L.append(f"- **JCL count:** the ticket quotes 39 JCL members; `app/jcl` on `main` holds **{len(core['jcl_jobs'])}** files "
             f"(`ls app/jcl | wc -l`), all inventoried above. Repository history (`git log --diff-filter=D -- app/jcl`) shows no deleted member, "
             f"so 38 is taken as authoritative; 39 is reached only if the two `app/proc` members or a CA-7/Control-M-only job are counted.")
    ca7 = core["scheduler"].get("CardDemo.ca7")
    if ca7:
        raw = len(re.findall(r"(?m)^\s*1LJOB,JOB=", read_text(ROOT / ca7["path"])))
        L.append(f"- **CA-7 listing:** {raw} `1LJOB` report sections describe {ca7['job_count']} distinct jobs (several jobs are listed twice, "
                 f"once with the header and once with only the TRIGGERED JOBS block); the inventory merges them by job name.")
    L.append("- **Program type** is inferred statically (CICS → online; SELECT/ASSIGN, EXEC SQL or DL/I → batch; otherwise utility). "
             "`CBSTM03B` is a batch subroutine (called by `CBSTM03A`) and `CSUTLDTC`/`COBSWAIT` are utilities called from online/batch code.")
    L.append("- **Transaction IDs** come from the `WS-TRANID` literal in each program, cross-checked against `DEFINE TRANSACTION` in the CSD (column `csd_transactions` in the JSON). "
             "Both sources agree for every core online program.")
    L.append("- **LOC** counts physical lines; `code` excludes `*`/`/` comment lines in column 7 and blank lines. Continuation lines (`-` in column 7) are merged into the preceding line before pattern matching.")
    L.append("- **Construct counts** are matched on comment-stripped, literal-masked source, so `GO TO` inside a comment or string is not counted. `COMP-3` includes `PACKED-DECIMAL`; the separate `COMP` column covers binary `COMP`/`COMP-4`/`COMP-5`/`BINARY`.")
    L.append("- **Copybook record lengths** are computed from PIC/USAGE/OCCURS (REDEFINES excluded) and match the README LRECL for every data file in the README table (see the `check` column in the data table).")
    L.append("- **Dynamic references** (`XCTL PROGRAM(CDEMO-TO-PROGRAM)`, `SEND MAP(CCARD-NEXT-MAP)`) cannot be resolved statically; they are listed as `dynamic:<data-name>` and in `dynamic_xctl_link_vars` / `dynamic_map_refs` in the JSON.")
    L.append("- **JCL operands** are parsed per the JCL rule that the operand field ends at the first blank; e.g. the stray text on the `STMTFILE` continuation card in `CREASTMT.JCL` is treated as a comment and the DSN on the following card is still picked up.")
    L.append("- Extension-app programs are analysed with the same rules but their copybooks (`CMQ*`, `DCL*`, `IMSFUNCS`, …) are only resolved when present under `app/`; MQ/Db2/IMS library copybooks are external.")
    L.append("")
    return "\n".join(L) + "\n"


# ---------------------------------------------------------------------------
# main
# ---------------------------------------------------------------------------

def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--check", action="store_true", help="verify outputs are up to date and coverage is complete")
    args = ap.parse_args()
    inv = build()
    json_text = json.dumps(inv, indent=2) + "\n"
    md_text = render_markdown(inv)
    cov = inv["coverage"]
    status = 0
    if not cov["complete"]:
        print(f"COVERAGE INCOMPLETE: missing={cov['missing']} not_on_disk={cov['not_on_disk']}", file=sys.stderr)
        status = 1
    if args.check:
        for path, text in ((JSON_OUT, json_text), (MD_OUT, md_text)):
            if not path.exists() or path.read_text() != text:
                print(f"STALE: {rel(path)} differs from generated content", file=sys.stderr)
                status = 1
        print(f"coverage: {cov['files_in_inventory']}/{cov['files_under_app']} files under app/ inventoried; "
              f"{'OK' if status == 0 else 'FAILED'}")
        return status
    JSON_OUT.write_text(json_text)
    MD_OUT.write_text(md_text)
    c = inv["counts"]["core"]
    print(f"wrote {rel(JSON_OUT)} and {rel(MD_OUT)}")
    print(f"coverage: {cov['files_in_inventory']}/{cov['files_under_app']} files under app/ inventoried (missing {len(cov['missing'])})")
    print(f"core: {c['cobol_programs']} programs {dict(c['cobol_programs_by_type'])}, {c['copybooks']} copybooks, "
          f"{c['bms_copybooks']} BMS copybooks, {c['bms_maps']} mapsets, {c['jcl_jobs']} JCL, {c['jcl_procs']} procs, {c['data_files']} data files")
    return status


if __name__ == "__main__":
    sys.exit(main())
