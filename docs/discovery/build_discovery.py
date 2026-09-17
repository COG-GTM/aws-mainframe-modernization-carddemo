#!/usr/bin/env python3
"""Static estate discovery generator.

Reads the mainframe application sources under ``app/`` and regenerates the
discovery dossier under ``docs/discovery/``:

* ``inventory.json``                     machine-readable source of truth
* ``01-inventory.md``                    artifact counts and inventory table
* ``02-dependency-map.md``               dependency graphs, reverse dataset view, orphans
* ``03-conversion-construct-register.md`` conversion-relevant construct register
* ``04-field-lineage.md``                two worked field lineages (data lives in this file,
                                         every cited line is verified against source)
* ``05-government-decisions.md``         decisions only the system owner can make
* ``README.md``                          one-page summary with the headline numbers

Usage::

    python3 docs/discovery/build_discovery.py          # regenerate
    python3 docs/discovery/build_discovery.py --check  # exit 1 if anything is stale

Standard library only.  Static analysis only: nothing is executed, no external
system is contacted, and no data file content is interpreted beyond counting.
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
OUT_DIR = Path(__file__).resolve().parent

SOURCE_DIRS = [
    "app/cbl",
    "app/cpy",
    "app/cpy-bms",
    "app/bms",
    "app/jcl",
    "app/proc",
    "app/csd",
    "app/asm",
    "app/maclib",
    "app/catlg",
    "app/ctl",
    "app/data",
    "app/scheduler",
    "app/app-authorization-ims-db2-mq",
    "app/app-transaction-type-db2",
    "app/app-vsam-mq",
]

GENERATED_FILES = [
    "inventory.json",
    "01-inventory.md",
    "02-dependency-map.md",
    "03-conversion-construct-register.md",
    "04-field-lineage.md",
    "05-government-decisions.md",
    "README.md",
]

# Directory-name -> artifact type.  Module sub-directories reuse the same names.
DIR_TYPES = {
    "cbl": "cobol_program",
    "cpy": "copybook",
    "cpy-bms": "bms_copybook",
    "bms": "bms_map",
    "jcl": "jcl_job",
    "proc": "jcl_proc",
    "csd": "csd",
    "asm": "assembler",
    "maclib": "asm_macro",
    "catlg": "catalog_listing",
    "ctl": "control_card",
    "data": "data_sample",
    "scheduler": "scheduler_def",
    "ddl": "sql_ddl",
    "dcl": "sql_dclgen",
    "ims": "ims_definition",
}

TYPE_LABELS = OrderedDict(
    [
        ("cobol_program", "COBOL program"),
        ("copybook", "Copybook"),
        ("bms_copybook", "BMS symbolic-map copybook"),
        ("bms_map", "BMS mapset source"),
        ("jcl_job", "JCL job"),
        ("jcl_proc", "JCL procedure"),
        ("csd", "CICS CSD definition file"),
        ("assembler", "Assembler program"),
        ("asm_macro", "Assembler macro"),
        ("catalog_listing", "Catalog listing"),
        ("control_card", "Utility control card"),
        ("data_sample", "Data file (sample)"),
        ("scheduler_def", "Scheduler definition file"),
        ("sql_ddl", "SQL DDL"),
        ("sql_dclgen", "SQL DCLGEN include"),
        ("ims_definition", "IMS DBD/PSB definition"),
        ("module_readme", "Module documentation"),
        ("other", "Other"),
    ]
)

# Programs supplied by the operating system, subsystems or run-time libraries.
SYSTEM_UTILITIES = {
    "IDCAMS", "SORT", "ICEMAN", "DFSORT", "SYNCSORT", "IEBGENER", "IEFBR14", "IKJEFT01",
    "IKJEFT1A", "IKJEFT1B", "DFHCSDUP", "SDSF", "DFSRRC00", "DFSURGU0", "DFSURGL0",
    "DSNTIAUL", "DSNTEP2", "DSNTEP4", "DSNTIAD", "IEBCOPY", "IEHLIST", "IEBPTPCH",
    "ADRDSSU", "FTP", "IEHPROGM", "DFSUDMP0", "DFSURRL0", "DFSURPR0", "DFSUPRT0",
    "DFSURUL0", "DSNUTILB", "DFSRRC00", "DFSDDLT0", "DFSPSBLD", "DFSUACB0",
}

# Run-time / subsystem entry points reached through CALL.
EXTERNAL_CALL_FAMILIES = [
    (re.compile(r"^CEE"), "Language Environment"),
    (re.compile(r"^MQ"), "Message queuing API"),
    (re.compile(r"^CBLTDLI$|^AIBTDLI$|^PLITDLI$|^ASMTDLI$"), "IMS DL/I interface"),
    (re.compile(r"^DSNTIA"), "Relational database sample interface"),
    (re.compile(r"^DFH"), "CICS"),
]

CICS_FILE_VERBS = {
    "READ": "read",
    "READNEXT": "read",
    "READPREV": "read",
    "STARTBR": "read",
    "RESETBR": "read",
    "ENDBR": None,
    "WRITE": "write",
    "REWRITE": "rewrite",
    "DELETE": "delete",
    "UNLOCK": None,
}

COBOL_RESERVED = {
    "TO", "FROM", "BY", "GIVING", "INTO", "OF", "IN", "IF", "ELSE", "END-IF", "THEN",
    "MOVE", "PERFORM", "EXEC", "END-EXEC", "EVALUATE", "WHEN", "END-EVALUATE", "SET",
    "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "COMPUTE", "GO", "CALL", "DISPLAY", "STRING",
    "UNSTRING", "INITIALIZE", "INSPECT", "READ", "WRITE", "REWRITE", "DELETE", "OPEN",
    "CLOSE", "RETURN", "STOP", "EXIT", "CONTINUE", "NOT", "AT", "INVALID", "ON", "END",
    "UNTIL", "VARYING", "AND", "OR", "ALSO", "TRUE", "FALSE", "OTHER", "SPACE", "SPACES",
    "ZERO", "ZEROS", "ZEROES", "LOW-VALUE", "LOW-VALUES", "HIGH-VALUE", "HIGH-VALUES",
    "END-PERFORM", "END-READ", "END-WRITE", "END-CALL", "END-STRING", "END-UNSTRING",
    "THRU", "THROUGH", "WITH", "NO", "ADVANCING", "UPON", "ACCEPT", "SEARCH", "ALL",
    "END-SEARCH", "SENTENCE", "NEXT", "ROUNDED", "SIZE", "ERROR", "END-COMPUTE", "END-ADD",
    "GOBACK", "FUNCTION", "LENGTH", "CORRESPONDING", "CORR", "DELIMITED", "POINTER",
    "TALLYING", "REPLACING", "CONVERTING", "COUNT", "USING", "RETURNING", "END-DELETE",
    "END-REWRITE", "END-START", "START", "KEY", "EQUAL", "IS", "GREATER", "LESS", "THAN",
    "NUMERIC", "ALPHABETIC", "POSITIVE", "NEGATIVE", "DEPENDING", "TIMES", "END-MULTIPLY",
    "END-DIVIDE", "END-SUBTRACT", "REMAINDER",
}


# ---------------------------------------------------------------------------
# Generic helpers
# ---------------------------------------------------------------------------

def read_bytes(path: Path) -> bytes:
    return path.read_bytes()


def decode_text(data: bytes) -> str:
    return data.decode("latin-1")


def rel(path: Path) -> str:
    return path.relative_to(ROOT).as_posix()


def md_escape(text) -> str:
    text = "" if text is None else str(text)
    return text.replace("|", "\\|").replace("\n", " ").replace("\r", "")


def md_table(headers, rows) -> str:
    if not rows:
        return "_None found in this source tree._"
    out = ["| " + " | ".join(md_escape(h) for h in headers) + " |",
           "|" + "|".join(" --- " for _ in headers) + "|"]
    for row in rows:
        out.append("| " + " | ".join(md_escape(c) for c in row) + " |")
    return "\n".join(out)


def cite(path: str, line: int) -> str:
    return f"`{path}:{line}`"


def mermaid_id(name: str) -> str:
    return "n_" + re.sub(r"[^A-Za-z0-9]", "_", name)


def mermaid_label(name: str) -> str:
    return name.replace('"', "'")


GDG_RELATIVE_RE = re.compile(r"\(([+-]?\d+)\)$")
GDG_ABSOLUTE_RE = re.compile(r"\.G\d{4}V\d{2}$")


def strip_gdg(dsn: str) -> str:
    return GDG_RELATIVE_RE.sub("", dsn)


def split_gdg(dsn: str) -> tuple[str, str | None]:
    """``AWS.X.BKUP(+1)`` -> (``AWS.X.BKUP``, ``+1``); a plain name has no generation."""
    dsn = dsn.strip().strip("'\"").upper()
    m = GDG_RELATIVE_RE.search(dsn)
    return (dsn[: m.start()], m.group(1)) if m else (dsn, None)


def normalize_dsn(dsn: str) -> str:
    return split_gdg(dsn)[0]


DSN_QUALIFIER_RE = re.compile(r"^[A-Z@#$][A-Z0-9@#$-]{0,7}$")


def sample_file_dsn(filename: str) -> str:
    """Dataset name carried by a sample data file name: trailing components that are
    not valid (upper-case, 1-8 character) qualifiers are filesystem extensions
    (``.dat``, ``.txt``) and are dropped; ``.PS`` / ``.INIT`` are kept."""
    parts = filename.split(".")
    while len(parts) > 1 and not DSN_QUALIFIER_RE.match(parts[-1]):
        parts.pop()
    return ".".join(parts).upper()


# ---------------------------------------------------------------------------
# COBOL source model
# ---------------------------------------------------------------------------

def mask_literals(text: str) -> str:
    """Replace the inside of quoted literals with blanks, keeping length."""
    out = list(text)
    i, n = 0, len(text)
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


class CobolSource:
    """Fixed-format COBOL text with comment removal, literal masking and line mapping."""

    def __init__(self, path: Path, text: str):
        self.path = path
        self.rel = rel(path)
        self.raw_lines = text.split("\n")
        if self.raw_lines and self.raw_lines[-1] == "":
            self.raw_lines.pop()
        self.code: list[tuple[int, str]] = []  # (line number, code text)
        for lineno, raw in enumerate(self.raw_lines, 1):
            line = raw.rstrip("\r").expandtabs(8)
            if len(line) < 7:
                continue
            indicator = line[6]
            if indicator in "*/":
                continue
            body = line[7:72]
            if indicator == "-" and self.code:
                cont = body.lstrip()
                if cont[:1] in "'\"":
                    cont = cont[1:]
                prev_no, prev = self.code[-1]
                self.code[-1] = (prev_no, prev.rstrip() + cont)
                continue
            self.code.append((lineno, body))
        raw_parts = []
        masked_parts = []
        self.offsets: list[int] = []
        pos = 0
        for _, body in self.code:
            self.offsets.append(pos)
            raw_parts.append(body)
            masked_parts.append(mask_literals(body))
            pos += len(body) + 1
        self.text = " ".join(raw_parts)
        self.masked = " ".join(masked_parts)
        self.upper = self.masked.upper()
        self.line_count = len(self.raw_lines)

    def line_of(self, offset: int) -> int:
        idx = bisect.bisect_right(self.offsets, offset) - 1
        if idx < 0:
            idx = 0
        return self.code[idx][0] if self.code else 0

    def snippet(self, offset: int, width: int = 70) -> str:
        lineno = self.line_of(offset)
        idx = bisect.bisect_right(self.offsets, offset) - 1
        body = self.code[idx][1].strip() if 0 <= idx < len(self.code) else ""
        return (body[:width] + "...") if len(body) > width else body

    def finditer(self, pattern, flags=0):
        return re.finditer(pattern, self.upper, flags)

    def raw_slice(self, start: int, end: int) -> str:
        return self.text[start:end]

    def division_offset(self, name: str) -> int:
        m = re.search(r"\b" + name + r"\s+DIVISION\b", self.upper)
        return m.start() if m else -1

    def statements(self):
        """Yield (start_offset, masked_text) split on sentence-ending periods."""
        start = 0
        for m in re.finditer(r"\.(?=\s|$)", self.upper):
            yield start, self.upper[start:m.start()]
            start = m.end()
        if start < len(self.upper):
            yield start, self.upper[start:]


PIC_RE = re.compile(r"\bPIC(?:TURE)?\s+(?:IS\s+)?(\S+)")
USAGE_RE = re.compile(
    r"\b(COMP-3|COMPUTATIONAL-3|PACKED-DECIMAL|COMP-5|COMPUTATIONAL-5|COMP-4|COMPUTATIONAL-4|"
    r"COMP-1|COMPUTATIONAL-1|COMP-2|COMPUTATIONAL-2|COMP|COMPUTATIONAL|BINARY|DISPLAY|INDEX|POINTER)\b"
)
DATA_ITEM_RE = re.compile(r"^\s*(0?[1-9]|[1-4][0-9]|66|77|88)\s+([A-Z0-9][A-Z0-9-]*|FILLER)\b(.*)$", re.S)
VALUE_RE = re.compile(
    r"\bVALUES?\s+(?:IS\s+|ARE\s+)?('[^']*'|\"[^\"]*\"|[-+]?\d+(?:\.\d+)?|SPACES?|ZEROS?|ZEROES|"
    r"LOW-VALUES?|HIGH-VALUES?|NULLS?|QUOTES?)",
    re.I,
)


def picture_is_edited(pic: str) -> bool:
    pic = pic.rstrip(".").upper()
    if not pic or "X" in pic or "A" in pic:
        return False
    if not re.search(r"[9Z*]", pic):
        return False
    return bool(re.search(r"[Z*+\-$,./B]|CR|DB", pic.replace("V", "")))


def picture_is_signed_zoned(pic: str, usage: str | None) -> bool:
    pic = pic.rstrip(".").upper()
    if not pic.startswith("S"):
        return False
    if usage and usage not in ("DISPLAY",):
        return False
    return not picture_is_edited(pic)


def picture_bytes(pic: str, usage: str | None) -> int:
    """Storage bytes of an elementary item: PICTURE symbols expanded (``X(08)``),
    ``S``/``V``/``P`` take no storage, packed and binary items use their digit count."""
    pic = pic.rstrip(".").upper()
    expanded = re.sub(r"(.)\((\d+)\)", lambda m: m.group(1) * int(m.group(2)), pic)
    expanded = re.sub(r"CR|DB", "..", expanded)
    positions = len(re.sub(r"[SVP]", "", expanded))
    digits = expanded.count("9")
    if usage in ("COMP-3", "COMPUTATIONAL-3", "PACKED-DECIMAL"):
        return (digits + 2) // 2
    if usage in ("COMP", "COMPUTATIONAL", "COMP-4", "COMPUTATIONAL-4", "COMP-5", "COMPUTATIONAL-5", "BINARY"):
        return 2 if digits <= 4 else 4 if digits <= 9 else 8
    if usage in ("COMP-1", "COMPUTATIONAL-1"):
        return 4
    if usage in ("COMP-2", "COMPUTATIONAL-2"):
        return 8
    return positions


FIGURATIVE_CONSTANTS = {"SPACE", "SPACES", "ZERO", "ZEROS", "ZEROES", "LOW-VALUE", "LOW-VALUES",
                        "HIGH-VALUE", "HIGH-VALUES", "NULL", "NULLS", "QUOTE", "QUOTES"}


def data_layout(items: list[dict], gi: int) -> tuple[list[dict], int]:
    """Elementary items below ``items[gi]`` with their byte offset from the start of that
    group, and the group's total size.  ``REDEFINES`` overlays and level-88 conditions
    take no storage; an elementary ``OCCURS`` counts once per repetition."""
    group_level = items[gi]["level"]
    out, pos, i, overlay = [], 0, gi + 1, None
    while i < len(items) and items[i]["level"] > group_level and items[i]["level"] not in (66, 77):
        it = items[i]
        i += 1
        if it["level"] == 88 or (overlay is not None and it["level"] > overlay):
            continue
        overlay = None
        if it["redefines"]:
            overlay = it["level"]
            continue
        if it["pic"]:
            size = picture_bytes(it["pic"], it["usage"])
            out.append({"item": it, "offset": pos, "size": size})
            pos += size * (it["occurs"] or 1)
    return out, pos


class CobolProgram:
    """Facts extracted from one COBOL program or copybook."""

    def __init__(self, src: CobolSource):
        self.src = src
        self.program_id: str | None = None
        self.copies: list[dict] = []
        self.calls: list[dict] = []
        self.cics: list[dict] = []
        self.sql: list[dict] = []
        self.selects: list[dict] = []     # SELECT file ASSIGN TO ddname
        self.fd_records: dict[str, str] = {}  # record name -> file name
        self.file_modes: dict[str, set] = defaultdict(set)
        self.file_mode_lines: dict[str, list] = defaultdict(list)
        self.data_items: list[dict] = []
        self.values: dict[str, list] = defaultdict(list)   # var -> [(literal, line)]
        self.moves: dict[str, list] = defaultdict(list)    # dst -> [(src, line, offset)]
        self.paragraphs: list[dict] = []                   # {name, start, end} in source order
        self.perform_sites: list[dict] = []                # {targets: [names], offset}
        self.constructs: list[dict] = []
        self.has_cics = False
        self.has_sql = False
        self.has_file_control = False
        self.parse()

    # -- helpers ---------------------------------------------------------
    def add_construct(self, kind: str, offset: int, detail: str = ""):
        self.constructs.append(
            {"construct": kind, "path": self.src.rel, "line": self.src.line_of(offset),
             "detail": detail, "snippet": self.src.snippet(offset)}
        )

    def literal_at(self, m, group) -> str | None:
        """Return the raw literal text (without quotes) for a masked match group."""
        s, e = m.start(group), m.end(group)
        if s < 0:
            return None
        raw = self.src.raw_slice(s, e)
        if raw[:1] in "'\"" and raw[-1:] == raw[:1]:
            return raw[1:-1]
        return raw

    # -- parse -----------------------------------------------------------
    def parse(self):
        src = self.src
        up = src.upper
        m = re.search(r"\bPROGRAM-ID\s*\.?\s+([A-Z0-9][A-Z0-9-]*)", up)
        if m:
            self.program_id = m.group(1).rstrip(".")
        self.has_file_control = bool(re.search(r"\bFILE-CONTROL\b", up))
        proc_off = src.division_offset("PROCEDURE")
        data_off = src.division_offset("DATA")
        self._parse_copies()
        self._parse_data_items(proc_off)
        self._parse_selects()
        self._parse_calls()
        self._parse_cics()
        self._parse_sql()
        self._parse_paragraphs(proc_off)
        self._parse_moves(proc_off)
        self._parse_file_verbs(proc_off)
        self._parse_constructs(proc_off, data_off)

    def _parse_copies(self):
        for m in self.src.finditer(r"(?<![A-Z0-9-])COPY\s+('[^']*'|\"[^\"]*\"|[A-Z0-9][A-Z0-9-]*)"):
            name = self.literal_at(m, 1)
            if not name:
                continue
            name = name.strip().rstrip(".").upper()
            replacing = bool(re.match(r"\s+REPLACING\b", self.src.upper[m.end():m.end() + 40]))
            self.copies.append({"name": name, "line": self.src.line_of(m.start()), "replacing": replacing})

    def _parse_data_items(self, proc_off: int):
        for start, stmt in self.src.statements():
            if proc_off >= 0 and start >= proc_off:
                break
            dm = DATA_ITEM_RE.match(stmt)
            if not dm:
                continue
            level, name, rest = dm.group(1), dm.group(2), dm.group(3)
            pic_m = PIC_RE.search(rest)
            pic = pic_m.group(1).rstrip(".") if pic_m else None
            usage_m = USAGE_RE.search(rest)
            usage = usage_m.group(1) if usage_m else None
            raw_stmt = self.src.raw_slice(start, start + len(stmt))
            val_m = VALUE_RE.search(raw_stmt)
            value = None
            if val_m:
                value = val_m.group(1)
                if value[:1] in "'\"":
                    value = value[1:-1]
            item_off = start + (len(stmt) - len(stmt.lstrip()))
            line = self.src.line_of(item_off)
            redef_m = re.search(r"\bREDEFINES\s+([A-Z0-9][A-Z0-9-]*)", rest)
            occurs_m = re.search(r"\bOCCURS\s+(\d+)", rest)
            item = {"level": int(level), "name": name, "pic": pic, "usage": usage,
                    "value": value, "line": line, "offset": item_off,
                    "redefines": redef_m.group(1) if redef_m else None,
                    "occurs": int(occurs_m.group(1)) if occurs_m else None,
                    "occurs_depending": bool(re.search(r"\bOCCURS\b.*\bDEPENDING\s+ON\b", rest, re.S))}
            self.data_items.append(item)
            if value is not None and name != "FILLER":
                self.values[name].append((value, line))
            if name == "FILLER" and value is not None:
                self.values["FILLER"].append((value, line))

    def _parse_selects(self):
        for m in self.src.finditer(r"(?<![A-Z0-9-])SELECT\s+(?:OPTIONAL\s+)?([A-Z0-9][A-Z0-9-]*)\s+ASSIGN\s+TO\s+([A-Z0-9][A-Z0-9-]*)"):
            ddname = m.group(2)
            ddname = ddname.split("-")[-1] if "-" in ddname and ddname.upper().startswith(("UT-", "DA-", "S-")) else ddname
            self.selects.append({"file": m.group(1), "ddname": ddname, "line": self.src.line_of(m.start())})
        # FD file -> 01 record names
        for m in self.src.finditer(r"\bFD\s+([A-Z0-9][A-Z0-9-]*)(.*?)(?=\bFD\s|\bSD\s|\bWORKING-STORAGE\b|\bLINKAGE\b|\bPROCEDURE\b|$)", re.S):
            fname = m.group(1)
            for r in re.finditer(r"(?:^|\s)01\s+([A-Z0-9][A-Z0-9-]*)", m.group(2)):
                self.fd_records[r.group(1)] = fname

    def _parse_calls(self):
        for m in self.src.finditer(r"(?<![A-Z0-9-])CALL\s+('[^']*'|\"[^\"]*\"|[A-Z0-9][A-Z0-9-]*)"):
            tok = self.src.upper[m.start(1):m.end(1)]
            line = self.src.line_of(m.start())
            if tok[:1] in "'\"":
                name = self.literal_at(m, 1).strip().upper()
                self.calls.append({"target": name, "kind": "static", "line": line, "via": None, "offset": m.start()})
            else:
                self.calls.append({"target": None, "kind": "dynamic", "line": line, "via": tok, "offset": m.start()})

    def _parse_cics(self):
        for m in self.src.finditer(r"\bEXEC\s+CICS\s+(.*?)\bEND-EXEC\b", re.S):
            body = m.group(1)
            self.has_cics = True
            words = re.sub(r"\(", " (", body).split()
            if not words:
                continue
            verb = words[0]
            if verb in ("SEND", "RECEIVE") and len(words) > 1 and words[1] in ("MAP", "TEXT", "CONTROL", "PAGE"):
                verb = verb + " " + words[1]
            if verb == "HANDLE" and len(words) > 1:
                verb = verb + " " + words[1]
            entry = {"verb": verb, "line": self.src.line_of(m.start()), "options": {}, "offset": m.start()}
            for om in re.finditer(r"\b(PROGRAM|DATASET|FILE|MAP|MAPSET|TRANSID|QUEUE|CHANNEL|CONTAINER|RIDFLD|INTO|FROM|COMMAREA|ABCODE)"
                                  r"\s*\(\s*((?:[^()]|\([^()]*\))*?)\s*\)", body):
                key = om.group(1)
                raw = self.src.raw_slice(m.start(1) + om.start(2), m.start(1) + om.end(2)).strip()
                masked = om.group(2).strip()
                if raw[:1] in "'\"":
                    entry["options"][key] = {"literal": raw.strip("'\"").strip(), "var": None}
                else:
                    entry["options"][key] = {"literal": None, "var": masked}
            self.cics.append(entry)

    def _parse_sql(self):
        for m in self.src.finditer(r"\bEXEC\s+SQL\s+(.*?)\bEND-EXEC\b", re.S):
            self.has_sql = True
            words = m.group(1).split()
            stmt = words[0] if words else "?"
            if stmt in ("DECLARE", "INCLUDE", "OPEN", "CLOSE", "FETCH", "COMMIT", "ROLLBACK", "WHENEVER") and len(words) > 1:
                detail = stmt + " " + words[1]
            else:
                detail = stmt
            self.sql.append({"statement": stmt, "detail": detail, "line": self.src.line_of(m.start()), "offset": m.start()})

    def _parse_moves(self, proc_off: int):
        """Record MOVE statements as dst -> (src, line, offset).  The destination list
        is read token by token and stops at the first reserved word or period, so the
        next statement is not swallowed into this one."""
        if proc_off < 0:
            return
        text = self.src.upper
        head = re.compile(r"(?<![A-Z0-9-])MOVE\s+('[^']*'|\"[^\"]*\"|[A-Z0-9][A-Z0-9-]*(?:\s*\([^)]*\))?)\s+TO\s+")
        dst_tok = re.compile(r"\s*([A-Z0-9][A-Z0-9-]*)(?:\s*\([^)]*\))?(?:\s+(?:OF|IN)\s+[A-Z0-9][A-Z0-9-]*(?:\s*\([^)]*\))?)*")
        pos = proc_off
        while True:
            m = head.search(text, pos)
            if not m:
                break
            start = m.start()
            tok = m.group(1)
            if tok[:1] in "'\"":
                src_val = ("lit", self.src.raw_slice(m.start(1) + 1, m.end(1) - 1))
            else:
                src_val = ("var", re.sub(r"\s*\(.*", "", tok))
            line = self.src.line_of(start)
            pos = m.end()
            while True:
                d = dst_tok.match(text, pos)
                if not d:
                    break
                dst = d.group(1)
                if dst in COBOL_RESERVED or dst.isdigit():
                    break
                self.moves[dst].append((src_val, line, start))
                pos = d.end()

    def _parse_paragraphs(self, proc_off: int):
        """Index paragraph/section headers (Area A names) and PERFORM / GO TO sites."""
        if proc_off < 0:
            return
        up = self.src.upper
        heads = []
        for idx, (_, body) in enumerate(self.src.code):
            off = self.src.offsets[idx]
            if off < proc_off or not body[:1].strip():
                continue
            hm = re.match(r"^([A-Z0-9][A-Z0-9-]*)\s*(?:SECTION\s*)?\.", body.strip().upper())
            if hm and hm.group(1) not in ("PROCEDURE", "DECLARATIVES", "END"):
                heads.append((hm.group(1), off))
        for i, (name, start) in enumerate(heads):
            end = heads[i + 1][1] if i + 1 < len(heads) else len(up)
            self.paragraphs.append({"name": name, "start": start, "end": end})
        names = [p["name"] for p in self.paragraphs]
        for m in re.finditer(r"(?<![A-Z0-9-])(?:PERFORM|GO\s+TO)\s+([A-Z0-9][A-Z0-9-]*)(?:\s+(?:THRU|THROUGH)\s+([A-Z0-9][A-Z0-9-]*))?", up[proc_off:]):
            first, last = m.group(1), m.group(2)
            if first in COBOL_RESERVED or first not in names:
                continue
            targets = [first]
            if last and last in names and names.index(last) > names.index(first):
                targets = names[names.index(first):names.index(last) + 1]
            self.perform_sites.append({"targets": targets, "offset": proc_off + m.start()})

    def _parse_file_verbs(self, proc_off: int):
        if proc_off < 0:
            return
        text = self.src.upper[proc_off:]

        def rec(mode, fname, off):
            self.file_modes[fname].add(mode)
            self.file_mode_lines[fname].append((mode, self.src.line_of(proc_off + off)))

        for m in re.finditer(r"(?<![A-Z0-9-])OPEN\s+((?:(?:INPUT|OUTPUT|I-O|EXTEND)\s+(?:[A-Z0-9][A-Z0-9-]*\s*)+)+)", text):
            mode = None
            for tok in m.group(1).split():
                if tok in ("INPUT", "OUTPUT", "I-O", "EXTEND"):
                    mode = "open " + tok.lower()
                elif tok in COBOL_RESERVED:
                    break
                elif mode:
                    rec(mode, tok, m.start())
        for m in re.finditer(r"(?<![A-Z0-9-])READ\s+([A-Z0-9][A-Z0-9-]*)", text):
            rec("read", m.group(1), m.start())
        for m in re.finditer(r"(?<![A-Z0-9-])(WRITE|REWRITE)\s+([A-Z0-9][A-Z0-9-]*)", text):
            fname = self.fd_records.get(m.group(2), m.group(2))
            rec(m.group(1).lower(), fname, m.start())
        for m in re.finditer(r"(?<![A-Z0-9-])DELETE\s+([A-Z0-9][A-Z0-9-]*)(?!\s*\()", text):
            if m.group(1) in self.fd_records.values() or m.group(1) in {s["file"] for s in self.selects}:
                rec("delete", m.group(1), m.start())
        for m in re.finditer(r"(?<![A-Z0-9-])START\s+([A-Z0-9][A-Z0-9-]*)", text):
            if m.group(1) in {s["file"] for s in self.selects}:
                rec("read", m.group(1), m.start())

    def _parse_constructs(self, proc_off: int, data_off: int):
        src = self.src
        up = src.upper
        for m in src.finditer(r"(?<![A-Z0-9-])REDEFINES\s+([A-Z0-9][A-Z0-9-]*)"):
            self.add_construct("REDEFINES", m.start(), m.group(1))
        for item in self.data_items:
            if item["occurs_depending"]:
                self.add_construct("OCCURS ... DEPENDING ON", item["offset"], item["name"])
            pic, usage = item["pic"], item["usage"]
            if usage in ("COMP-3", "COMPUTATIONAL-3", "PACKED-DECIMAL"):
                self.add_construct("COMP-3 / packed decimal", item["offset"], f"{item['name']} PIC {pic}")
            elif usage in ("COMP", "COMPUTATIONAL", "COMP-4", "COMPUTATIONAL-4", "COMP-5", "COMPUTATIONAL-5", "BINARY"):
                self.add_construct("COMP / binary", item["offset"], f"{item['name']} PIC {pic}")
            if pic and picture_is_signed_zoned(pic, usage):
                self.add_construct("Signed zoned decimal (PIC S9)", item["offset"], f"{item['name']} PIC {pic}")
            if pic and picture_is_edited(pic):
                self.add_construct("Edited numeric picture", item["offset"], f"{item['name']} PIC {pic}")
        if proc_off >= 0:
            for m in re.finditer(r"(?<![A-Z0-9-])GO\s+TO(?![A-Z0-9-])\s*([A-Z0-9][A-Z0-9-]*)?", up[proc_off:]):
                self.add_construct("GO TO", proc_off + m.start(), m.group(1) or "")
            for m in re.finditer(r"(?<![A-Z0-9-])ALTER\s+([A-Z0-9][A-Z0-9-]*)", up[proc_off:]):
                self.add_construct("ALTER", proc_off + m.start(), m.group(1))
            for m in re.finditer(r"(?<![A-Z0-9-])PERFORM\s+([A-Z0-9][A-Z0-9-]*)\s+(?:THRU|THROUGH)\s+([A-Z0-9][A-Z0-9-]*)", up[proc_off:]):
                self.add_construct("PERFORM ... THRU", proc_off + m.start(), f"{m.group(1)} THRU {m.group(2)}")
            for m in re.finditer(r"(?<![A-Z0-9-])NEXT\s+SENTENCE(?![A-Z0-9-])", up[proc_off:]):
                self.add_construct("NEXT SENTENCE", proc_off + m.start(), "")
        for c in self.cics:
            self.add_construct("EXEC CICS", c["offset"], c["verb"])
        for s in self.sql:
            self.add_construct("EXEC SQL", s["offset"], s["detail"])
        # date handling
        date_intrinsics = r"\bFUNCTION\s+(CURRENT-DATE|INTEGER-OF-DATE|DATE-OF-INTEGER|INTEGER-OF-DAY|DAY-OF-INTEGER|WHEN-COMPILED)\b"
        for m in src.finditer(date_intrinsics):
            self.add_construct("Date handling", m.start(), "FUNCTION " + m.group(1))
        for m in src.finditer(r"(?<![A-Z0-9-])ACCEPT\s+[A-Z0-9][A-Z0-9-]*\s+FROM\s+(DATE|DAY|TIME|DAY-OF-WEEK)(\s+YYYYMMDD|\s+YYYYDDD)?\b"):
            self.add_construct("Date handling", m.start(), "ACCEPT FROM " + m.group(1) + (m.group(2) or ""))
        for c in self.cics:
            if c["verb"] in ("ASKTIME", "FORMATTIME"):
                self.add_construct("Date handling", c["offset"], "EXEC CICS " + c["verb"])
        date_name = re.compile(r"(DATE|-DT\b|-TS\b|YYYY|-MM\b|-DD\b|DAYS?\b|YEAR|MONTH|LILLIAN|LILIAN|JULIAN|CENTURY|-YY\b|-CC\b)")
        if proc_off >= 0:
            for m in re.finditer(r"(?<![A-Z0-9-])(COMPUTE|ADD|SUBTRACT|MULTIPLY|DIVIDE)(?![A-Z0-9-])(.*?)(?=\b(?:COMPUTE|ADD|SUBTRACT|MULTIPLY|DIVIDE|MOVE|IF|ELSE|END-IF|PERFORM|EXEC|EVALUATE|WHEN|SET|GO|CALL|DISPLAY|STRING|INITIALIZE|READ|WRITE|REWRITE|DELETE|OPEN|CLOSE|GOBACK|STOP|EXIT|CONTINUE|END-EVALUATE|END-PERFORM|INSPECT|UNSTRING|ACCEPT)\b|\.\s|$)", up[proc_off:], re.S):
                body = m.group(2)
                operands = [t for t in re.findall(r"[A-Z][A-Z0-9-]*", body) if t not in COBOL_RESERVED]
                if any(date_name.search(t) for t in operands):
                    self.add_construct("Date arithmetic", proc_off + m.start(),
                                       m.group(1) + " " + " ".join(operands[:4]))
                literals = [t for t in re.findall(r"(?<![A-Z0-9-])[-+]?\d+(?:\.\d+)?\b", body)]
                for lit in literals:
                    if lit.lstrip("+-") in ("0", "1", "00", "01"):
                        continue
                    self.add_construct("Hard-coded amount / numeric literal", proc_off + m.start(),
                                       f"{m.group(1)} literal {lit}")
            for m in re.finditer(r"(?<![A-Z0-9-])MOVE\s+([-+]?\d+\.\d+)\s+TO\s+([A-Z0-9][A-Z0-9-]*)", up[proc_off:]):
                self.add_construct("Hard-coded amount / numeric literal", proc_off + m.start(),
                                   f"MOVE {m.group(1)} TO {m.group(2)}")
        for item in self.data_items:
            v = item["value"]
            if v is None:
                continue
            if re.fullmatch(r"\d{4}-\d{2}-\d{2}(.*)?|\d{4}/\d{2}/\d{2}|\d{2}/\d{2}/\d{4}|\d{4}-\d{2}-\d{2}-\d{2}\.\d{2}\.\d{2}(\.\d+)?", v):
                self.add_construct("Hard-coded date", item["offset"], f"{item['name']} VALUE '{v}'")
            elif re.fullmatch(r"[-+]?\d+\.\d+", v) and item["name"] != "FILLER":
                self.add_construct("Hard-coded amount / numeric literal", item["offset"], f"{item['name']} VALUE {v}")
        for m in re.finditer(r"'(\d{4}-\d{2}-\d{2}(?:-\d{2}\.\d{2}\.\d{2}(?:\.\d+)?)?|\d{4}/\d{2}/\d{2}|\d{2}/\d{2}/\d{4})'", src.text):
            if proc_off >= 0 and m.start() >= proc_off:
                self.add_construct("Hard-coded date", m.start(), f"literal '{m.group(1)}'")
        # external / LE / assembler calls
        for c in self.calls:
            self.add_construct("CALL (static or dynamic)", c["offset"], c["target"] or f"dynamic via {c['via']}")

    # -- resolution ------------------------------------------------------
    def paragraph_at(self, offset: int) -> dict | None:
        for p in self.paragraphs:
            if p["start"] <= offset < p["end"]:
                return p
        return None

    def _def_is_conditional(self, def_off: int, ref_off: int) -> bool:
        """True when a block (IF / EVALUATE / WHEN / ELSE) opened before the MOVE closes
        before the reference, i.e. the MOVE does not dominate the reference."""
        depth = 0
        for t in re.finditer(r"(?<![A-Z0-9-])(IF|EVALUATE|END-IF|END-EVALUATE|ELSE|WHEN)(?![A-Z0-9-])", self.src.upper[def_off:ref_off]):
            tok = t.group(1)
            if tok in ("IF", "EVALUATE"):
                depth += 1
            elif tok in ("END-IF", "END-EVALUATE"):
                depth -= 1
                if depth < 0:
                    return True
            elif depth == 0:
                return True
        return False

    def reaching_defs(self, var: str, ref_off: int, depth: int = 0, seen=None) -> tuple[list, bool]:
        """MOVEs to ``var`` that can reach ``ref_off``: the MOVEs earlier in the same
        paragraph, and, unless one of those dominates the reference, the MOVEs that
        reach each PERFORM / GO TO of that paragraph (or its fall-through entry).
        Returns (defs, dominated) where dominated means every path carries a MOVE."""
        seen = seen if seen is not None else set()
        key = (var, ref_off)
        if key in seen or depth > 8:
            return [], False
        seen.add(key)
        para = self.paragraph_at(ref_off)
        if para is None:
            return [d for d in self.moves.get(var, []) if d[2] < ref_off], False
        local = [d for d in self.moves.get(var, []) if para["start"] <= d[2] < ref_off]
        dominating = [d[2] for d in local if not self._def_is_conditional(d[2], ref_off)]
        if dominating:
            last = max(dominating)          # kills every earlier MOVE to the same name
            return [d for d in local if d[2] >= last], True
        defs = list(local)
        entry_points = [s["offset"] for s in self.perform_sites if para["name"] in s["targets"]]
        idx = self.paragraphs.index(para)
        if idx > 0:
            entry_points.append(self.paragraphs[idx - 1]["end"] - 1)   # fall-through
        all_dominated = bool(entry_points)
        for off in entry_points:
            sub, dom = self.reaching_defs(var, off, depth + 1, seen)
            defs.extend(sub)
            all_dominated = all_dominated and dom
        return defs, all_dominated

    def resolve_var(self, var: str, extra_values: dict, ref_off: int | None = None,
                    depth: int = 0, seen=None) -> set:
        """Literal values ``var`` can hold at ``ref_off``: VALUE clauses (own and copybook)
        unless a MOVE dominates the reference, plus the reaching MOVEs.  Without
        ``ref_off`` every MOVE in the program is taken (flow-insensitive)."""
        return self.resolve_var_ex(var, extra_values, ref_off, depth, seen)[0]

    def resolve_var_ex(self, var: str, extra_values: dict, ref_off: int | None = None,
                       depth: int = 0, seen=None) -> tuple[set, bool]:
        """As ``resolve_var`` but also returns whether every path to ``ref_off`` gives
        ``var`` a literal.  False means some path leaves it with a value not visible in
        this program (caller commarea, terminal input, a file record, ...)."""
        seen = seen if seen is not None else set()
        var = re.sub(r"\s*\(.*", "", var).strip().upper()
        if not var or (var, ref_off) in seen or depth > 4:
            return set(), True
        seen.add((var, ref_off))
        out = set()
        if ref_off is None:
            defs, dominated = self.moves.get(var, []), False
        else:
            defs, dominated = self.reaching_defs(var, ref_off)
        has_initial = False
        if not dominated:
            for lit, _ in self.values.get(var, []) + extra_values.get(var, []):
                if isinstance(lit, str):
                    out.add(lit.strip())
                    has_initial = True
        complete = dominated or has_initial
        for (kind, val), _, off in defs:
            if kind == "lit":
                out.add(val.strip())
            else:
                sub, sub_complete = self.resolve_var_ex(val, extra_values, None if ref_off is None else off,
                                                        depth + 1, seen)
                out |= sub
                complete = complete and sub_complete
        return out, complete


# ---------------------------------------------------------------------------
# JCL
# ---------------------------------------------------------------------------

def split_jcl_params(params: str) -> list[str]:
    out, depth, cur, quote = [], 0, "", None
    for ch in params:
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
    return out


def jcl_param_field(rest: str) -> str:
    """Operand field ends at the first blank outside quotes/parentheses."""
    depth, quote = 0, None
    for i, ch in enumerate(rest):
        if quote:
            if ch == quote:
                quote = None
            continue
        if ch in "'\"":
            quote = ch
        elif ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
        elif ch == " " and depth == 0:
            return rest[:i]
    return rest


class JclStatement:
    def __init__(self, name, op, params, line):
        self.name, self.op, self.params, self.line = name, op, params, line
        self.instream: list[tuple[int, str]] = []

    def kv(self) -> dict:
        out = {}
        pos = []
        for p in split_jcl_params(self.params):
            if "=" in p and not p.startswith("'"):
                k, v = p.split("=", 1)
                out[k.upper()] = v
            else:
                pos.append(p)
        out["_positional"] = pos
        return out


def parse_jcl(text: str) -> list[JclStatement]:
    stmts: list[JclStatement] = []
    lines = text.split("\n")
    pending: JclStatement | None = None  # statement expecting continuation
    in_stream: JclStatement | None = None
    for lineno, raw in enumerate(lines, 1):
        line = raw.rstrip("\r")[:72]
        if in_stream is not None:
            if line.startswith("/*") or line.startswith("//"):
                in_stream = None
            else:
                in_stream.instream.append((lineno, line))
                continue
        if line.startswith("//*") or not line.strip():
            continue
        if line.startswith("/*"):
            continue
        if not line.startswith("//"):
            continue
        body = line[2:]
        if pending is not None and body[:1] == " ":
            field = jcl_param_field(body.strip())
            pending.params = pending.params.rstrip(",") + "," + field if pending.params else field
            if not field.endswith(","):
                pending = None
            continue
        pending = None
        if body.strip() == "":
            continue
        parts = body.split(None, 1)
        if body[:1] == " ":
            name = ""
            rest = body.strip()
        else:
            name = parts[0]
            rest = parts[1] if len(parts) > 1 else ""
        op_parts = rest.split(None, 1)
        op = op_parts[0].upper() if op_parts else ""
        params = jcl_param_field(op_parts[1].strip()) if len(op_parts) > 1 else ""
        st = JclStatement(name, op, params, lineno)
        stmts.append(st)
        if params.endswith(","):
            pending = st
        if op == "DD" and (params.strip() in ("*", "DATA") or params.startswith("*,") or params.startswith("DATA,")):
            in_stream = st
    return stmts


class JclMember:
    """A JCL job or procedure with its steps, DDs and in-stream procedures."""

    def __init__(self, path: Path, text: str):
        self.path = path
        self.rel = rel(path)
        self.stmts = parse_jcl(text)
        self.line_count = len(text.split("\n")) - (1 if text.endswith("\n") else 0)
        self.job_name = None
        self.job_line = None
        self.proc_name = None
        self.proc_line = None
        self.proc_defaults: dict[str, str] = {}
        self.steps: list[dict] = []
        self.instream_procs: dict[str, list[dict]] = {}
        self.sets: dict[str, str] = {}
        self._build()

    def _build(self):
        cur_step = None
        cur_proc = None
        proc_steps: list[dict] = []
        for st in self.stmts:
            if st.op == "JOB":
                self.job_name, self.job_line = st.name, st.line
            elif st.op == "SET":
                for k, v in st.kv().items():
                    if k != "_positional":
                        self.sets[k] = v
            elif st.op == "PROC":
                cur_proc = st.name
                proc_steps = []
                if self.proc_name is None and self.job_name is None:
                    self.proc_name = st.name
                    self.proc_line = st.line
                    self.proc_defaults = {k: v for k, v in st.kv().items() if k != "_positional"}
            elif st.op == "PEND":
                if cur_proc:
                    self.instream_procs[cur_proc] = proc_steps
                cur_proc = None
                cur_step = None
            elif st.op == "EXEC":
                kv = st.kv()
                pgm = kv.get("PGM")
                proc = kv.get("PROC")
                if pgm is None and proc is None and kv["_positional"]:
                    proc = kv["_positional"][0]
                cur_step = {"step": st.name, "line": st.line, "pgm": pgm.upper() if pgm else None,
                            "proc": proc.upper() if proc else None, "parm": kv.get("PARM"),
                            "symbols": {k: v for k, v in kv.items() if k not in ("_positional", "PGM", "PROC", "PARM", "COND", "REGION", "TIME")},
                            "dds": [], "instream": []}
                if cur_proc is not None:
                    proc_steps.append(cur_step)
                else:
                    self.steps.append(cur_step)
            elif st.op == "DD" and cur_step is not None:
                kv = st.kv()
                dsn = kv.get("DSN") or kv.get("DSNAME")
                dd = {"ddname": st.name, "line": st.line, "dsn": dsn, "disp": kv.get("DISP"),
                      "sysout": "SYSOUT" in kv,
                      "dummy": "DUMMY" in kv["_positional"] or (dsn or "").upper() == "NULLFILE",
                      "instream": bool(st.instream)}
                if st.instream:
                    cur_step["instream"].extend(st.instream)
                cur_step["dds"].append(dd)
        if cur_proc is not None and cur_proc not in self.instream_procs:
            self.instream_procs[cur_proc] = proc_steps  # cataloged procedure without PEND
        if self.proc_name and self.proc_name in self.instream_procs and not self.steps:
            self.steps = self.instream_procs[self.proc_name]


def resolve_symbolics(value: str, sets: dict) -> str:
    """Substitute &SYMBOL references that a SET statement in the same member defines.
    A SET value may itself contain symbols (``SET LBNM=&CODER..M2``), so substitution
    repeats until nothing changes, bounded so a self-referencing SET cannot loop."""
    out = value or ""
    for _ in range(8):
        new = re.sub(r"&([A-Z0-9@#$]+)\.?", lambda m: sets.get(m.group(1), m.group(0)), out)
        if new == out:
            break
        out = new
    return out


# ---------------------------------------------------------------------------
# CSD (DFHCSDUP command syntax)
# ---------------------------------------------------------------------------

CSD_CMDS = ("DEFINE", "DELETE", "ADD", "REMOVE", "LIST", "ALTER", "COPY", "APPEND", "INITIALIZE",
            "UPGRADE", "EXTRACT", "VERIFY", "SERVICE", "SCAN", "USERDEFINE")


def parse_csd(lines: list[tuple[int, str]]) -> list[dict]:
    """lines: (lineno, text).  Returns DEFINE entries with attributes."""
    entries = []
    cur = None
    for lineno, raw in lines:
        text = raw.rstrip("\r")
        if len(text) > 72 and text[72:].strip().isdigit():
            text = text[:72]
        stripped = text.strip()
        if not stripped or stripped.startswith("*"):
            continue
        first = stripped.split()[0].upper()
        if first in CSD_CMDS:
            if cur:
                entries.append(cur)
            cur = {"command": first, "line": lineno, "text": stripped[len(first):].strip()}
            continue
        if cur:
            cur["text"] += " " + stripped
    if cur:
        entries.append(cur)
    for e in entries:
        attrs = {}
        kind = None
        for m in re.finditer(r"([A-Z][A-Z0-9]*)\s*\(([^()]*(?:\([^()]*\)[^()]*)*)\)", e["text"]):
            k = m.group(1).upper()
            if kind is None:
                kind = k
                e["name"] = m.group(2).strip().upper()
            attrs[k] = m.group(2).strip()
        e["kind"] = kind or "?"
        e["kind"] = {"TRAN": "TRANSACTION", "PROG": "PROGRAM", "DB2TRAN": "DB2TRAN", "DB2ENTRY": "DB2ENTRY"}.get(e["kind"], e["kind"])
        e.setdefault("name", "?")
        e["attrs"] = attrs
    return [e for e in entries if e["command"] == "DEFINE"]


# ---------------------------------------------------------------------------
# BMS, Assembler, catalog, scheduler
# ---------------------------------------------------------------------------

def parse_bms(text: str) -> dict:
    mapsets, maps, fields = [], [], 0
    for lineno, raw in enumerate(text.split("\n"), 1):
        line = raw.rstrip("\r")[:71]
        if line.startswith("*") or not line.strip():
            continue
        parts = line.split()
        if line[0] != " " and len(parts) > 1:
            label, op = parts[0], parts[1].upper()
        else:
            label, op = None, parts[0].upper()
        if op == "DFHMSD" and label:
            mapsets.append({"name": label.upper(), "line": lineno})
        elif op == "DFHMDI" and label:
            maps.append({"name": label.upper(), "line": lineno})
        elif op == "DFHMDF":
            fields += 1
    return {"mapsets": mapsets, "maps": maps, "field_count": fields}


def parse_asm(text: str) -> dict:
    csects, copies, ops = [], [], []
    for lineno, raw in enumerate(text.split("\n"), 1):
        line = raw.rstrip("\r")[:71]
        if line.startswith("*") or not line.strip():
            continue
        parts = line.split()
        if line[0] != " " and len(parts) > 1 and parts[1].upper() in ("CSECT", "START", "RSECT"):
            csects.append({"name": parts[0].upper(), "line": lineno})
        if line[0] != " ":
            if len(parts) < 2:
                continue
            op, operands = parts[1].upper(), parts[2:]
        else:
            op, operands = parts[0].upper(), parts[1:]
        if op == "COPY" and operands:
            copies.append({"name": operands[0].upper().rstrip(","), "line": lineno})
        elif op not in ("MACRO", "MEND", "DSECT", "CSECT", "START", "RSECT", "END"):
            ops.append({"op": op, "line": lineno})
    return {"csects": csects, "copies": copies, "ops": ops}


def parse_listcat(text: str) -> list[dict]:
    out = []
    for lineno, raw in enumerate(text.split("\n"), 1):
        m = re.match(r"^[0 ]?(CLUSTER|NONVSAM|AIX|PATH|DATA|INDEX|GDG BASE|ALIAS)\s*-+\s*([A-Z0-9.@#$]+)", raw.rstrip("\r"))
        if m:
            out.append({"entry_type": m.group(1), "dsn": m.group(2), "line": lineno})
    return out


def parse_controlm(text: str, path_rel: str) -> dict:
    jobs, edges = [], []
    try:
        root = ET.fromstring(text)
    except ET.ParseError as exc:
        return {"jobs": [], "edges": [], "error": str(exc)}
    lines = text.split("\n")

    def find_line(needle: str, start: int) -> int:
        for i in range(start, len(lines)):
            if needle in lines[i]:
                return i + 1
        return 0

    cursor = 0
    for folder in root:
        folder_name = folder.get("FOLDER_NAME") or folder.get("JOBNAME") or folder.tag
        for job in folder.iter("JOB"):
            jobname, memname = job.get("JOBNAME"), job.get("MEMNAME")
            line = find_line(f'JOBNAME="{jobname}" MEMNAME="{memname}"', cursor)
            cursor = max(cursor, line)
            inconds = [c.get("NAME") for c in job.findall("INCOND")]
            outconds = [c.get("NAME") for c in job.findall("OUTCOND") if c.get("SIGN") == "+"]
            jobs.append({"scheduler": "Control-M", "folder": folder_name, "job": jobname, "member": memname,
                         "line": line, "in_conditions": inconds, "out_conditions": outconds, "path": path_rel})
    cond_owner = {}
    for j in jobs:
        for c in j["out_conditions"]:
            cond_owner[c] = j
    for j in jobs:
        for c in j["in_conditions"]:
            if c in cond_owner:
                edges.append({"scheduler": "Control-M", "from": cond_owner[c]["job"], "to": j["job"], "folder": j["folder"],
                              "kind": f"IN condition {c}", "line": j["line"], "path": path_rel})
    return {"jobs": jobs, "edges": edges}


def parse_ca7(text: str, path_rel: str) -> dict:
    jobs, edges = [], []
    cur = None
    for lineno, raw in enumerate(text.split("\n"), 1):
        line = raw.rstrip("\r")
        m = re.match(r"^\s*1?LJOB,JOB=([A-Z0-9]+)", line)
        if m:
            cur = {"scheduler": "CA-7", "folder": None, "job": m.group(1), "member": None, "line": lineno,
                   "in_conditions": [], "out_conditions": [], "path": path_rel}
            jobs.append(cur)
            continue
        if cur is None:
            continue
        m = re.match(r"^\s*([A-Z0-9]+)\s+(\d{3})\s+([A-Z0-9]+)\s+[A-Z0-9]+\s+\d{3}", line)
        if m and m.group(1) == cur["job"] and cur["member"] is None:
            cur["member"] = m.group(3)
            continue
        m = re.match(r"^\s*JOB=([A-Z0-9]+)\s+SCHID=", line)
        if m:
            cur["out_conditions"].append(m.group(1))
            edges.append({"scheduler": "CA-7", "from": cur["job"], "to": m.group(1), "folder": None,
                          "kind": "triggered job (SCHID entry)", "line": lineno, "path": path_rel})
    seen, unique = set(), []
    for j in jobs:
        if j["job"] in seen:
            continue
        seen.add(j["job"])
        unique.append(j)
    return {"jobs": unique, "edges": edges}


# ---------------------------------------------------------------------------
# Estate model
# ---------------------------------------------------------------------------

CSD_FREE_TEXT_ATTRS = {"DESCRIPTION", "DESC"}  # free-text attributes carry no dependency information

HEADLINE_CATEGORIES = [
    "program->copybook",
    "program->dataset",
    "jclstep->program",
    "transaction->program",
    "program->program",
]
EXTRA_CATEGORIES = [
    "program->bmsmap",
    "assembler->macro",
    "jclstep->proc",
    "scheduler->job",
    "csdfile->dataset",
    "csdlibrary->dataset",
    "copybook->copybook",
]


class Estate:
    def __init__(self, root: Path):
        self.root = root
        self.artifacts: list[dict] = []
        self.by_path: dict[str, dict] = {}
        self.programs: dict[str, dict] = {}     # program name -> artifact
        self.copybooks: dict[str, dict] = {}    # copybook name -> artifact
        self.cobol: dict[str, CobolProgram] = {}  # path -> parsed
        self.jcl: dict[str, JclMember] = {}
        self.csd_entries: list[dict] = []
        self.bms_by_mapset: dict[str, dict] = {}
        self.edges: list[dict] = []
        self.datasets: dict[str, dict] = {}
        self.scheduler_jobs: list[dict] = []
        self.scheduler_edges: list[dict] = []
        self.listcat: list[dict] = []
        self.notes: list[str] = []

    # -- discovery -------------------------------------------------------
    def discover(self):
        for d in SOURCE_DIRS:
            base = self.root / d
            if not base.exists():
                self.notes.append(f"source directory missing: {d}")
                continue
            for path in sorted(p for p in base.rglob("*") if p.is_file()):
                if path.name.startswith("."):
                    continue
                self._add_artifact(path, d)
        self.artifacts.sort(key=lambda a: a["path"])
        for a in self.artifacts:
            self.by_path[a["path"]] = a

    def _classify(self, path: Path, source_dir: str) -> tuple[str, str]:
        parts = path.relative_to(self.root / source_dir).parts
        top = Path(source_dir).name
        module = "core"
        if top.startswith("app-"):
            module = top
            sub = parts[0] if len(parts) > 1 else None
        else:
            sub = top
        if sub is None:
            if path.suffix.lower() == ".md":
                return "module_readme", module
            return "other", module
        return DIR_TYPES.get(sub, "other"), module

    def _add_artifact(self, path: Path, source_dir: str):
        atype, module = self._classify(path, source_dir)
        data = read_bytes(path)
        text = decode_text(data)
        line_count = text.count("\n") + (1 if text and not text.endswith("\n") else 0)
        art = OrderedDict()
        art["path"] = rel(path)
        art["type"] = atype
        art["subtype"] = None
        art["module"] = module
        art["name"] = path.stem.upper()
        art["line_count"] = line_count
        art["byte_size"] = len(data)
        art["copy_targets"] = []
        art["call_targets"] = []
        art["cics_verbs"] = {}
        art["exec_sql"] = False
        art["files"] = []
        art["transaction_ids"] = []
        art["jcl_steps"] = []
        art["csd_definitions"] = []
        art["bms"] = None
        art["scheduler_jobs"] = []
        art["notes"] = []
        if atype == "cobol_program" or atype in ("copybook", "bms_copybook", "sql_dclgen") and path.suffix.lower() in (".cpy", ".cbl", ".dcl"):
            prog = CobolProgram(CobolSource(path, text))
            self.cobol[art["path"]] = prog
            if atype == "cobol_program":
                art["name"] = prog.program_id or path.stem.upper()
                if prog.has_cics:
                    art["subtype"] = "online"
                elif prog.has_file_control or art["name"].startswith("CB"):
                    art["subtype"] = "batch"
                else:
                    art["subtype"] = "utility"
                art["name_prefix_class"] = ("online (CO*)" if art["name"].startswith("CO")
                                            else "batch (CB*)" if art["name"].startswith("CB") else "other prefix")
                if art["name_prefix_class"].startswith("online") and art["subtype"] != "online":
                    art["notes"].append(f"name prefix suggests online but no EXEC CICS found; classified {art['subtype']}")
                if art["name_prefix_class"].startswith("batch") and art["subtype"] != "batch":
                    art["notes"].append(f"name prefix suggests batch; classified {art['subtype']}")
                self.programs[art["name"]] = art
                self.programs.setdefault(path.stem.upper(), art)
            else:
                self.copybooks[path.stem.upper()] = art
            art["copy_targets"] = [{"name": c["name"], "line": c["line"], "resolved": None, "path": None}
                                   for c in prog.copies]
            for s in prog.sql:
                if s["statement"] == "INCLUDE":
                    inc = s["detail"].split()[-1].upper()
                    art["copy_targets"].append({"name": inc, "line": s["line"], "resolved": None, "path": None,
                                                "via": "EXEC SQL INCLUDE"})
            art["call_targets"] = [{"target": c["target"], "kind": c["kind"], "via": c["via"], "line": c["line"],
                                    "offset": c["offset"], "resolved": None, "resolved_to": []} for c in prog.calls]
            art["cics_verbs"] = dict(sorted(Counter(c["verb"] for c in prog.cics).items()))
            art["exec_sql"] = prog.has_sql
            art["exec_sql_statements"] = len(prog.sql)
            art["files"] = [{"select": s["file"], "ddname": s["ddname"], "line": s["line"],
                             "modes": sorted(m for m in prog.file_modes.get(s["file"], set()) if not m.startswith("open")),
                             "open_modes": sorted(m for m in prog.file_modes.get(s["file"], set()) if m.startswith("open")),
                             "datasets": []} for s in prog.selects]
            art["cics_files"] = []
        elif atype in ("copybook", "bms_copybook"):
            self.copybooks[path.stem.upper()] = art
        elif atype in ("jcl_job", "jcl_proc"):
            member = JclMember(path, text)
            self.jcl[art["path"]] = member
            if member.job_name:
                art["name"] = member.job_name.upper()
                if atype == "jcl_proc":
                    art["notes"].append("JOB statement found in proc directory")
            elif member.proc_name:
                art["name"] = path.stem.upper()
                art["proc_statement_name"] = member.proc_name.upper()
                if atype == "jcl_job":
                    art["type"] = "jcl_proc"
                    art["notes"].append("PROC statement found in jcl directory; classified as procedure")
                if member.proc_name.upper() != path.stem.upper():
                    art["notes"].append(f"PROC statement name {member.proc_name.upper()} differs from member name {path.stem.upper()}")
            for step in member.steps:
                art["jcl_steps"].append({"step": step["step"], "line": step["line"], "pgm": step["pgm"],
                                         "proc": step["proc"], "parm": step["parm"], "driving_program": None,
                                         "driving_program_kind": None, "proc_bindings": [],
                                         "dds": [{"ddname": d["ddname"], "dsn": d["dsn"], "line": d["line"], "disp": d["disp"]}
                                                 for d in step["dds"] if d["dsn"] or d["sysout"] or d["instream"] or d["dummy"]]})
            for pname, psteps in member.instream_procs.items():
                if pname == member.proc_name and member.steps is psteps:
                    continue
                art["notes"].append(f"in-stream procedure {pname} with {len(psteps)} step(s)")
            # CSD definitions and utility control statements embedded in in-stream data
            for step in member.steps:
                if step["pgm"] == "DFHCSDUP" and step["instream"]:
                    ents = parse_csd(step["instream"])
                    for e in ents:
                        e["source"] = art["path"]
                        e["module"] = module
                        e["sets"] = member.sets
                    art["csd_definitions"] = [{"kind": e["kind"], "name": e["name"], "line": e["line"],
                                               "group": e["attrs"].get("GROUP"), "program": e["attrs"].get("PROGRAM"),
                                               "transid": e["attrs"].get("TRANSID"), "dsname": e["attrs"].get("DSNAME")}
                                              for e in ents]
                    self.csd_entries.extend(ents)
        elif atype == "csd":
            ents = parse_csd([(i, l) for i, l in enumerate(text.split("\n"), 1)])
            for e in ents:
                e["source"] = art["path"]
                e["module"] = module
            art["csd_definitions"] = [{"kind": e["kind"], "name": e["name"], "line": e["line"],
                                       "group": e["attrs"].get("GROUP"), "program": e["attrs"].get("PROGRAM"),
                                       "transid": e["attrs"].get("TRANSID"), "dsname": e["attrs"].get("DSNAME")}
                                      for e in ents]
            self.csd_entries.extend(ents)
        elif atype == "bms_map":
            bms = parse_bms(text)
            art["bms"] = bms
            for ms in bms["mapsets"]:
                self.bms_by_mapset[ms["name"]] = art
            self.bms_by_mapset.setdefault(path.stem.upper(), art)
        elif atype == "assembler":
            asm = parse_asm(text)
            art["asm"] = asm
            if asm["csects"]:
                art["name"] = asm["csects"][0]["name"]
            self.programs[art["name"]] = art
        elif atype == "catalog_listing":
            self.listcat = parse_listcat(text)
            art["catalog_entries"] = len(self.listcat)
        elif atype == "scheduler_def":
            if text.lstrip().startswith("<"):
                sched = parse_controlm(text, art["path"])
            else:
                sched = parse_ca7(text, art["path"])
            art["scheduler_jobs"] = sched["jobs"]
            self.scheduler_jobs.extend(sched["jobs"])
            self.scheduler_edges.extend(sched["edges"])
        elif atype == "control_card":
            m = re.search(r"RUN\s+PROGRAM\s*\(\s*([A-Z0-9]+)\s*\)", text, re.I)
            if m:
                art["notes"].append(f"RUN PROGRAM({m.group(1).upper()})")
        elif atype == "data_sample":
            if "." in path.name and DSN_QUALIFIER_RE.match(path.name.split(".")[0]):
                art["name"] = sample_file_dsn(path.name)
            if b"\n" not in data:
                art["line_count"] = None
                art["notes"].append("no line terminators (fixed-length records); line count not applicable")
        self.artifacts.append(art)

    # -- edges -----------------------------------------------------------
    def add_edge(self, category, frm, to, path, line, status, detail="", **extra):
        e = {"category": category, "from": frm, "to": to, "path": path, "line": line,
             "status": status, "detail": detail}
        e.update(extra)
        self.edges.append(e)
        return e

    def dataset(self, dsn: str) -> dict:
        key, generation = split_gdg(dsn)
        d = self.datasets.get(key)
        if d is None:
            d = {"dsn": key, "catalog_types": set(), "jcl_refs": [], "csd_files": [], "program_access": [],
                 "csd_libraries": [], "sample_files": [], "symbolic": "&" in key, "relative_generations": set()}
            self.datasets[key] = d
        if generation is not None:
            d["relative_generations"].add(generation)
        return d

    def find_program(self, name: str | None):
        if not name:
            return None
        return self.programs.get(name.strip().upper())

    def find_copybook(self, name: str | None):
        if not name:
            return None
        return self.copybooks.get(name.strip().upper())

    def included_copybooks(self, art: dict) -> list[dict]:
        """Copybook artifacts a program (or copybook) brings in through COPY / EXEC SQL
        INCLUDE, followed transitively, in first-seen order."""
        out, seen, queue = [], {art["path"]}, list(art["copy_targets"])
        while queue:
            cb = self.find_copybook(queue.pop(0)["name"])
            if cb and cb["path"] not in seen:
                seen.add(cb["path"])
                out.append(cb)
                queue.extend(cb.get("copy_targets", []))
        return out

    def program_values(self, art: dict) -> dict:
        """VALUE clauses a program sees from its copybooks (its own live on its parser)."""
        out: dict = defaultdict(list)
        for cb in self.included_copybooks(art):
            if cb["path"] in self.cobol:
                for k, v in self.cobol[cb["path"]].values.items():
                    out[k].extend(v)
        return out

    def statement_sites(self, art: dict) -> list[dict]:
        """Where a program's executable statements live: its own source and every
        included copybook that carries procedure code.  Each site pairs the parsed
        source that owns the statement offsets (``prog``) with the VALUE clauses of the
        rest of the compilation unit (``values``), so a CALL inside a copybook resolves
        against the including program's literals."""
        own = self.cobol[art["path"]]
        sites = [{"prog": own, "path": art["path"], "values": self.program_values(art), "copybook": None}]
        for cb in self.included_copybooks(art):
            prog = self.cobol.get(cb["path"])
            if not prog or not (prog.calls or prog.cics):
                continue
            values: dict = defaultdict(list)
            for k, v in own.values.items():
                values[k].extend(v)
            for other in self.included_copybooks(art):
                if other["path"] != cb["path"] and other["path"] in self.cobol:
                    for k, v in self.cobol[other["path"]].values.items():
                        values[k].extend(v)
            sites.append({"prog": prog, "path": cb["path"], "values": values, "copybook": cb["name"]})
        return sites

    def table_values(self, art: dict, var: str) -> dict | None:
        """Literals a subscripted reference such as ``OPT-PGMNAME(WS-OPTION)`` can take
        when the table is a ``REDEFINES`` of a group initialised by ``VALUE`` clauses:
        the element field is located by byte offset inside one OCCURS entry and every
        VALUE literal at that offset in the redefined data is one possible value."""
        field = re.sub(r"\s*\(.*", "", var).strip().upper()
        for path in [art["path"]] + [cb["path"] for cb in self.included_copybooks(art)]:
            prog = self.cobol.get(path)
            if not prog:
                continue
            items = prog.data_items
            for fi, it in enumerate(items):
                if it["name"] != field or not it["pic"]:
                    continue
                ancestors, level = [], it["level"]
                for j in range(fi - 1, -1, -1):
                    if items[j]["level"] < level:
                        ancestors.append(j)
                        level = items[j]["level"]
                        if level == 1:
                            break
                occ = next((j for j in ancestors if items[j]["occurs"]), None)
                if occ is None:
                    continue
                redef = next((j for j in [occ] + ancestors[ancestors.index(occ):] if items[j]["redefines"]), None)
                if redef is None:
                    continue
                entry_fields, entry_size = data_layout(items, occ)
                fld = next((f for f in entry_fields if f["item"] is it), None)
                if fld is None or not entry_size:
                    continue
                data_name = items[redef]["redefines"]
                di = next((j for j in range(redef - 1, -1, -1) if items[j]["name"] == data_name), None)
                if di is None:
                    continue
                data_fields, _ = data_layout(items, di)
                literals = [(f["item"]["value"], f["item"]["line"]) for f in data_fields
                            if f["offset"] % entry_size == fld["offset"] and f["size"] == fld["size"]
                            and isinstance(f["item"]["value"], str)
                            and f["item"]["value"].upper() not in FIGURATIVE_CONSTANTS]
                if not literals:
                    continue
                occurs = items[occ]["occurs"]
                top = ancestors[-1]
                count_field = None
                for f in data_layout(items, top)[0]:
                    v = f["item"]["value"]
                    if f["item"]["name"] != "FILLER" and "9" in (f["item"]["pic"] or "") \
                            and isinstance(v, str) and v.isdigit() and int(v) == len(literals):
                        count_field = f["item"]["name"]
                        break
                return {"field": field, "table": items[occ]["name"], "data": data_name, "source": path,
                        "line": items[occ]["line"], "literals": literals, "initialised": len(literals),
                        "occurs": occurs, "count_field": count_field,
                        "complete": len(literals) == occurs or count_field is not None}
        return None

    def build(self):
        self._link_copybooks()
        self._link_asm_macros()
        self._link_calls()
        self._link_csd()
        self._link_jcl()
        self._link_online_files()
        self._link_scheduler()
        self._link_catalog()
        self._finish_datasets()

    def _link_copybooks(self):
        for art in self.artifacts:
            if art["type"] not in ("cobol_program", "copybook", "bms_copybook", "sql_dclgen"):
                continue
            cat = "program->copybook" if art["type"] == "cobol_program" else "copybook->copybook"
            for c in art["copy_targets"]:
                target = self.find_copybook(c["name"])
                if target:
                    c["resolved"], c["path"] = True, target["path"]
                    self.add_edge(cat, art["name"], target["name"], art["path"], c["line"], "resolved",
                                  c.get("via", "COPY"))
                else:
                    c["resolved"] = False
                    fam = "system-supplied (CICS/MQ/SQL) include" if re.match(r"^(DFH|CMQ|SQLCA|SQLDA|DSN)", c["name"]) else "not found in repository"
                    self.add_edge(cat, art["name"], c["name"], art["path"], c["line"], "unresolved", fam)

    def _link_asm_macros(self):
        """Assembler source -> macro library member: explicit ``COPY member`` and macro
        instructions whose opcode is a member of ``app/maclib``.  Opcodes that are not a
        repository macro are treated as machine or system instructions and not reported."""
        macros = {a["name"]: a for a in self.artifacts if a["type"] == "asm_macro"}
        for art in self.artifacts:
            if art["type"] != "assembler":
                continue
            asm = art["asm"]
            art["copy_targets"] = []
            for c in asm["copies"]:
                target = macros.get(c["name"])
                art["copy_targets"].append({"name": c["name"], "line": c["line"], "resolved": bool(target),
                                            "path": target["path"] if target else None, "via": "COPY"})
                if target:
                    self.add_edge("assembler->macro", art["name"], target["name"], art["path"], c["line"], "resolved", "COPY")
                else:
                    self.add_edge("assembler->macro", art["name"], c["name"], art["path"], c["line"], "unresolved",
                                  "not found in repository")
            for o in asm["ops"]:
                target = macros.get(o["op"])
                if target:
                    art["copy_targets"].append({"name": o["op"], "line": o["line"], "resolved": True,
                                                "path": target["path"], "via": "macro instruction"})
                    self.add_edge("assembler->macro", art["name"], target["name"], art["path"], o["line"], "resolved",
                                  "macro instruction")

    def _resolve_program_ref(self, art: dict, site: dict, literal, var, offset):
        """Return (list of (name, kind, detail), complete) for a program reference at
        ``offset`` in ``site``.  ``complete`` is False when some control-flow path
        reaches the statement with a value the program never assigns from a literal."""
        if literal:
            return [(literal.strip().upper(), "static", "literal")], True
        vals, complete = site["prog"].resolve_var_ex(var, site["values"], offset)
        how = f"resolved through VALUE/MOVE of {var} reaching this statement"
        if not complete:
            how += "; on another path the value is not set in this program"
        if site["copybook"]:
            how += f"; statement included from copybook {site['copybook']}"
        out = [(v.upper(), "dynamic", how)
               for v in sorted(vals) if v.strip()]
        if not out and "(" in var:
            tbl = self.table_values(art, var)
            if tbl:
                how = (f"table-driven: {tbl['field']} is an element of {tbl['table']} OCCURS {tbl['occurs']} "
                       f"({tbl['source']}:{tbl['line']}); {tbl['initialised']} entries initialised by VALUE literals in "
                       f"{tbl['data']}")
                how += (f"; populated-entry count held in {tbl['count_field']} VALUE {tbl['initialised']}"
                        if tbl["count_field"] else
                        f"; entries beyond {tbl['initialised']} are not initialised by VALUE")
                out = [(lit.strip().upper(), "dynamic", how) for lit in sorted({l for l, _ in tbl["literals"]})]
                complete = tbl["complete"]
        return out, complete

    def _unreached_detail(self, site: dict, var: str, what: str) -> str:
        """Explain an unresolved variable reference, listing literals assigned to the
        variable elsewhere in the program that control flow does not carry to it."""
        elsewhere = sorted(v for v in site["prog"].resolve_var(var, site["values"]) if v.strip())
        if elsewhere:
            return (f"{what} through {var}: no VALUE/MOVE literal reaches this statement; "
                    f"assigned elsewhere in the program (not established by control flow): {', '.join(elsewhere)}")
        return f"{what} through {var}: no VALUE/MOVE literal assigns it"

    def _add_runtime_value_edge(self, art: dict, path: str, var: str, what: str, line: int):
        self.add_edge("program->program", art["name"], f"{what} via {var} (value from outside this program)",
                      path, line, "unresolved",
                      f"{what} through {var}: on at least one path the value comes from data this program does not set "
                      f"(caller commarea, terminal input or a record); the resolved targets at this line are the literals "
                      f"visible on the other paths", kind="dynamic")

    def _link_calls(self):
        for art in self.artifacts:
            if art["type"] != "cobol_program":
                continue
            for site in self.statement_sites(art):
                self._link_site_calls(art, site)

    def _link_site_calls(self, art: dict, site: dict):
        prog, path, cb = site["prog"], site["path"], site["copybook"]
        if cb is None:
            calls = art["call_targets"]
        else:
            calls = [{"target": c["target"], "kind": c["kind"], "via": c["via"], "line": c["line"], "offset": c["offset"],
                      "resolved": None, "resolved_to": [], "included_from": cb, "path": path} for c in prog.calls]
            art["call_targets"].extend(calls)
        for c in calls:
            refs, complete = self._resolve_program_ref(art, site, c["target"], c["via"], c["offset"])
            if not refs:
                c["resolved"] = False
                self.add_edge("program->program", art["name"], f"dynamic via {c['via']}", path, c["line"],
                              "unresolved", self._unreached_detail(site, c["via"], "CALL"), kind="dynamic")
                continue
            if not complete:
                self._add_runtime_value_edge(art, path, c["via"], "CALL", c["line"])
            for name, kind, how in refs:
                target = self.find_program(name)
                if target:
                    c["resolved"] = True
                    c["resolved_to"].append(target["name"])
                    self.add_edge("program->program", art["name"], target["name"], path, c["line"],
                                  "resolved", f"CALL {kind} ({how}); target type {target['type']}", kind=kind)
                else:
                    fam = next((f for rx, f in EXTERNAL_CALL_FAMILIES if rx.match(name)), "not in repository")
                    c["resolved"] = False
                    self.add_edge("program->program", art["name"], name, path, c["line"], "unresolved",
                                  f"CALL {kind}: {fam}", kind=kind)
        for cx in prog.cics:
            if cx["verb"] not in ("XCTL", "LINK"):
                continue
            opt = cx["options"].get("PROGRAM")
            if not opt:
                self.add_edge("program->program", art["name"], f"{cx['verb']} without PROGRAM option", path,
                              cx["line"], "unresolved", "PROGRAM option not parsed", kind="dynamic")
                continue
            refs, complete = self._resolve_program_ref(art, site, opt["literal"], opt["var"], cx["offset"])
            if not refs:
                self.add_edge("program->program", art["name"], f"{cx['verb']} via {opt['var']}", path, cx["line"],
                              "unresolved", self._unreached_detail(site, opt["var"], f"EXEC CICS {cx['verb']}"), kind="dynamic")
                continue
            if not complete:
                self._add_runtime_value_edge(art, path, opt["var"], f"EXEC CICS {cx['verb']}", cx["line"])
            for name, kind, how in refs:
                target = self.find_program(name)
                if target:
                    self.add_edge("program->program", art["name"], target["name"], path, cx["line"], "resolved",
                                  f"EXEC CICS {cx['verb']} {kind} ({how})", kind=kind)
                else:
                    self.add_edge("program->program", art["name"], name, path, cx["line"], "unresolved",
                                  f"EXEC CICS {cx['verb']} {kind} ({how}): program source not in repository", kind=kind)

    def _link_csd(self):
        for e in self.csd_entries:
            kind, attrs = e["kind"], e["attrs"]
            if kind == "TRANSACTION" and attrs.get("PROGRAM"):
                pname = attrs["PROGRAM"].upper()
                target = self.find_program(pname)
                if target:
                    target["transaction_ids"].append(e["name"])
                    self.add_edge("transaction->program", e["name"], target["name"], e["source"], e["line"], "resolved",
                                  f"DEFINE TRANSACTION in group {attrs.get('GROUP', '?')}")
                else:
                    self.add_edge("transaction->program", e["name"], pname, e["source"], e["line"], "unresolved",
                                  "program named in CSD has no source in repository")
            if kind == "PROGRAM" and attrs.get("TRANSID"):
                target = self.find_program(e["name"])
                if target:
                    if e["attrs"]["TRANSID"] not in target["transaction_ids"]:
                        target["transaction_ids"].append(e["attrs"]["TRANSID"])
                    self.add_edge("transaction->program", attrs["TRANSID"], target["name"], e["source"], e["line"], "resolved",
                                  "TRANSID attribute on DEFINE PROGRAM")
                else:
                    self.add_edge("transaction->program", attrs["TRANSID"], e["name"], e["source"], e["line"], "unresolved",
                                  "program named in CSD has no source in repository")
            sets = e.get("sets") or {}
            if kind == "FILE" and attrs.get("DSNAME"):
                d = self.dataset(resolve_symbolics(attrs["DSNAME"], sets))
                d["csd_files"].append({"file": e["name"], "path": e["source"], "line": e["line"]})
                self.add_edge("csdfile->dataset", e["name"], d["dsn"], e["source"], e["line"], "resolved", "DSNAME attribute")
            if kind == "LIBRARY":
                for attr in sorted(a for a in attrs if re.fullmatch(r"DSNAME\d*", a)):
                    d = self.dataset(resolve_symbolics(attrs[attr], sets))
                    d["csd_libraries"].append({"library": e["name"], "path": e["source"], "line": e["line"]})
                    self.add_edge("csdlibrary->dataset", e["name"], d["dsn"], e["source"], e["line"], "resolved",
                                  f"{attr} attribute")
            if kind == "PROGRAM":
                target = self.find_program(e["name"])
                if target:
                    target.setdefault("csd_program_refs", []).append(f"{e['source']}:{e['line']}")
                else:
                    self.notes.append(f"CSD DEFINE PROGRAM({e['name']}) at {e['source']}:{e['line']} has no source in repository")

    def _proc_lookup(self, member: JclMember, name: str) -> list[tuple[str, list[dict], str]]:
        """Return [(label, steps, path)] for a procedure name: in-stream first, then the
        procedure-library member of that name (JCL resolves PROC= by member name)."""
        if name in member.instream_procs and not (member.proc_name == name and member.steps is member.instream_procs[name]):
            return [(f"in-stream {name}", member.instream_procs[name], member.rel)]
        out = []
        for path, m in self.jcl.items():
            art = self.by_path[path]
            if art["type"] != "jcl_proc":
                continue
            if art["name"] == name:
                out.append((art["name"], m.steps, path))
        return out

    @staticmethod
    def _mlabel(art: dict) -> str:
        """Job or procedure label used in step edges (procedures are marked to avoid clashing with a job of the same member name)."""
        return art["name"] + (" (PROC)" if art["type"] == "jcl_proc" else "")

    def _step_programs(self, step: dict) -> list[tuple[str, str]]:
        """(program name, how) pairs a step drives, including utility-hosted programs."""
        out = []
        pgm = step["pgm"]
        if pgm:
            out.append((pgm, "PGM="))
            parm = (step.get("parm") or "").strip("'\"()")
            if pgm in ("DFSRRC00",) and parm:
                parts = [p.strip() for p in parm.split(",")]
                if len(parts) >= 2 and parts[1]:
                    out.append((parts[1].upper(), "DFSRRC00 PARM region program"))
            if pgm in ("IKJEFT01", "IKJEFT1A", "IKJEFT1B"):
                for _, l in step.get("instream", []):
                    m = re.search(r"RUN\s+PROGRAM\s*\(\s*([A-Z0-9]+)\s*\)", l, re.I)
                    if m:
                        out.append((m.group(1).upper(), "TSO batch RUN PROGRAM"))
        return out

    def _classify_pgm(self, name: str) -> tuple[str | None, str]:
        target = self.find_program(name)
        if target:
            return target["name"], "application"
        if name in SYSTEM_UTILITIES or name.startswith(("DFS", "DSN", "IEB", "IEH", "IEF", "IKJ", "ICE", "DFH", "ADR", "IDC")):
            return name, "utility"
        return None, "unresolved"

    def _link_jcl(self):
        # Pass 1: step -> program edges for every member (jobs and procedures).
        for path, member in self.jcl.items():
            art = self.by_path[path]
            for step, sinfo in zip(member.steps, art["jcl_steps"]):
                label = f"{self._mlabel(art)}/{step['step'] or '?'}"
                drivers = []
                if step["proc"]:
                    procs = self._proc_lookup(member, step["proc"])
                    if not procs:
                        self.add_edge("jclstep->proc", label, step["proc"], path, step["line"], "unresolved",
                                      "procedure not found in repository")
                    for plabel, _, ppath in procs:
                        self.add_edge("jclstep->proc", label, plabel, path, step["line"],
                                      "resolved" if len(procs) == 1 else "ambiguous",
                                      f"EXEC PROC={step['proc']} -> {ppath}" + ("" if len(procs) == 1 else " (several members define this PROC name)"))
                    sinfo["driving_program"] = f"PROC {step['proc']}"
                    sinfo["driving_program_kind"] = "procedure"
                    if len(procs) > 1:
                        art["notes"].append(f"step {step['step']} EXEC PROC={step['proc']} is ambiguous: {', '.join(p for p, _, _ in procs)}")
                else:
                    for pgm, how in self._step_programs(step):
                        name, kind = self._classify_pgm(pgm)
                        if kind == "unresolved":
                            self.add_edge("jclstep->program", label, pgm, path, step["line"], "unresolved",
                                          f"{how}{pgm}: neither application source nor a recognised system utility")
                        else:
                            self.add_edge("jclstep->program", label, name, path, step["line"], "resolved", f"{how}{pgm} ({kind})",
                                          program_kind=kind)
                        drivers.append((pgm, kind))
                    sinfo["driving_program"] = drivers[0][0] if drivers else None
                    sinfo["driving_program_kind"] = drivers[0][1] if drivers else None
                    if len(drivers) > 1:
                        sinfo["hosted_programs"] = [{"program": p, "kind": k} for p, k in drivers[1:]]
                # datasets referenced by the step's DD statements (for an EXEC PROC step these are the
                # invocation-level overrides such as PRC001.FILEIN)
                for dd in step["dds"]:
                    if dd["dsn"] and not dd["dummy"]:
                        dsn = resolve_symbolics(dd["dsn"], member.sets)
                        d = self.dataset(dsn)
                        d["jcl_refs"].append({"member": self._mlabel(art), "step": step["step"], "step_line": step["line"], "ddname": dd["ddname"],
                                              "path": path, "line": dd["line"], "disp": dd["disp"],
                                              "program": drivers[0][0] if drivers else (f"PROC {step['proc']}" if step["proc"] else None)})
                # IDCAMS control statements name datasets that appear nowhere else
                if step["pgm"] == "IDCAMS":
                    for lineno, l in step["instream"]:
                        for m in re.finditer(r"\b(?:NAME|INDATASET|OUTDATASET|IDS|ODS)\s*\(\s*([A-Z0-9.&@#$]+(?:\([^)]*\))?)\s*\)", l, re.I):
                            d = self.dataset(resolve_symbolics(m.group(1), member.sets))
                            d["jcl_refs"].append({"member": self._mlabel(art), "step": step["step"], "step_line": step["line"], "ddname": "(IDCAMS control statement)",
                                                  "path": path, "line": lineno, "disp": None, "program": "IDCAMS"})
                        m = re.match(r"^\s*(?:DELETE|DEL)\s+([A-Z0-9.&@#$]+)", l, re.I)
                        if m:
                            d = self.dataset(resolve_symbolics(m.group(1), member.sets))
                            d["jcl_refs"].append({"member": self._mlabel(art), "step": step["step"], "step_line": step["line"], "ddname": "(IDCAMS DELETE)",
                                                  "path": path, "line": lineno, "disp": None, "program": "IDCAMS"})
        # Pass 2: program -> dataset through the DD statements of every step that runs the program.
        runs: dict[str, list] = defaultdict(list)  # program -> [(member art, step, effective dds, via)]
        for path, member in self.jcl.items():
            art = self.by_path[path]
            if art["type"] != "jcl_job":
                continue
            for step, sinfo in zip(member.steps, art["jcl_steps"]):
                self._collect_runs(runs, member, art, step, depth=0, overrides={}, via="",
                                   bindings=sinfo["proc_bindings"] if step["proc"] else None)
        # A batch subprogram reached only by CALL runs inside its caller's step and shares its DD statements.
        static_callers = defaultdict(set)
        for e in self.edges:
            if e["category"] == "program->program" and e["status"] == "resolved" and "CALL" in e.get("detail", ""):
                static_callers[e["to"]].add(e["from"])
        for _ in range(3):
            for callee, callers in static_callers.items():
                if runs.get(callee):
                    continue
                for caller in sorted(callers):
                    for job_art, step, dds, via in runs.get(caller, []):
                        runs[callee].append((job_art, step, dds, f"{via} (called from {caller})"))
        for art in self.artifacts:
            if art["type"] != "cobol_program":
                continue
            prog = self.cobol[art["path"]]
            if not art["files"]:
                continue
            program_runs = runs.get(art["name"], [])
            for f in art["files"]:
                modes = f["modes"] or ["unknown"]
                if not program_runs:
                    self.add_edge("program->dataset", art["name"], f"DD {f['ddname']} (no JCL step executes this program)",
                                  art["path"], f["line"], "unresolved",
                                  "SELECT ... ASSIGN found but no JCL job in the repository executes the program", modes=modes)
                    continue
                seen = set()
                for job_art, step, dds, via in program_runs:
                    dd = dds.get(f["ddname"])
                    if dd is None:
                        self.add_edge("program->dataset", art["name"], f"DD {f['ddname']} in {self._mlabel(job_art)}/{step['step']}",
                                      art["path"], f["line"], "unresolved",
                                      f"no DD statement for {f['ddname']} in step {self._mlabel(job_art)}/{step['step']}{via}", modes=modes)
                        continue
                    if dd["dummy"] or dd["sysout"] or (dd["instream"] and not dd["dsn"]):
                        f["datasets"].append({"job": self._mlabel(job_art), "step": step["step"], "dsn": None,
                                              "dd_kind": "DUMMY" if dd["dummy"] else "SYSOUT" if dd["sysout"] else "in-stream",
                                              "path": dd["_path"], "line": dd["line"]})
                        continue
                    if not dd["dsn"]:
                        self.add_edge("program->dataset", art["name"], f"DD {f['ddname']} in {self._mlabel(job_art)}/{step['step']}",
                                      art["path"], f["line"], "unresolved", "DD statement without DSN", modes=modes)
                        continue
                    dsn = resolve_symbolics(dd["dsn"], dd["_sets"])
                    d = self.dataset(dsn)
                    key = (d["dsn"], self._mlabel(job_art), step["step"])
                    if key in seen:
                        continue
                    seen.add(key)
                    f["datasets"].append({"job": self._mlabel(job_art), "step": step["step"], "dsn": d["dsn"], "dd_kind": "DSN",
                                          "path": dd["_path"], "line": dd["line"]})
                    status = "unresolved" if d["symbolic"] else "resolved"
                    d["program_access"].append({"program": art["name"], "modes": modes, "via": f"{self._mlabel(job_art)}/{step['step']}{via}",
                                                "path": art["path"], "line": f["line"], "dd_path": dd["_path"], "dd_line": dd["line"]})
                    self.add_edge("program->dataset", art["name"], d["dsn"], art["path"], f["line"], status,
                                  f"DD {f['ddname']} in {self._mlabel(job_art)}/{step['step']}{via} at {dd['_path']}:{dd['line']}" +
                                  ("; unresolved JCL symbolic" if d["symbolic"] else ""), modes=modes)

    def _collect_runs(self, runs, member: JclMember, job_art: dict, step: dict, depth: int, overrides: dict, via: str,
                      symbols=None, bindings: list | None = None):
        """Walk a job step (expanding EXEC PROC) and record which program runs with which effective DD statements.
        ``bindings`` (an EXEC PROC step's ``proc_bindings`` list) receives every dataset the expanded procedure
        steps bind after invocation overrides and symbolic substitution."""
        if depth > 3:
            return
        if step["proc"]:
            procs = self._proc_lookup(member, step["proc"])
            ov = dict(overrides)
            for dd in step["dds"]:
                if "." in dd["ddname"]:
                    ov[dd["ddname"]] = dict(dd, _path=job_art["path"], _sets=member.sets)
            for plabel, psteps, ppath in procs:
                pmember = self.jcl.get(ppath, member)
                syms = dict(pmember.proc_defaults)
                syms.update(pmember.sets)
                syms.update(member.sets)
                syms.update(step.get("symbols", {}))
                for ps in psteps:
                    self._collect_runs(runs, pmember, job_art, ps, depth + 1, ov, f" (via PROC {step['proc']})", syms, bindings)
            return
        dds = {}
        for dd in step["dds"]:
            if dd["ddname"]:
                dds[dd["ddname"]] = dict(dd, _path=member.rel, _sets=(symbols if symbols is not None else member.sets))
        for key, dd in overrides.items():
            stepname, ddname = key.split(".", 1)
            if stepname == step["step"]:
                dds[ddname] = dd
        if bindings is not None:
            for ddname, dd in dds.items():
                if dd.get("dsn") and not dd.get("dummy"):
                    bindings.append({"proc_step": step["step"], "program": step["pgm"], "ddname": ddname,
                                     "dsn": split_gdg(resolve_symbolics(dd["dsn"], dd["_sets"]))[0],
                                     "path": dd["_path"], "line": dd["line"], "disp": dd.get("disp")})
        for pgm, _how in self._step_programs(step):
            target = self.find_program(pgm)
            if target:
                runs[target["name"]].append((job_art, step, dds, via))
                continue
            name, kind = self._classify_pgm(pgm)
            if kind == "utility":
                for ddname, dd in dds.items():
                    if dd.get("dsn") and not dd.get("dummy"):
                        d = self.dataset(resolve_symbolics(dd["dsn"], dd["_sets"]))
                        d["program_access"].append({"program": pgm, "modes": ["unknown"], "utility": True,
                                                    "via": f"{self._mlabel(job_art)}/{step['step']}{via} DD {ddname} DISP={dd.get('disp')}",
                                                    "path": dd["_path"], "line": dd["line"], "dd_path": dd["_path"], "dd_line": dd["line"]})

    def _link_online_files(self):
        csd_files = {}
        for e in self.csd_entries:
            if e["kind"] == "FILE":
                csd_files.setdefault(e["name"], e)
        for art in self.artifacts:
            if art["type"] != "cobol_program":
                continue
            for site in self.statement_sites(art):
                self._link_site_online_files(art, site, csd_files)

    def _link_site_online_files(self, art: dict, site: dict, csd_files: dict):
        prog, path, values = site["prog"], site["path"], site["values"]
        for cx in prog.cics:
            verb = cx["verb"]
            if verb in CICS_FILE_VERBS:
                opt = cx["options"].get("DATASET") or cx["options"].get("FILE")
                if not opt:
                    continue
                mode = CICS_FILE_VERBS[verb]
                if mode is None:
                    continue
                names = sorted({opt["literal"].strip().upper()} if opt["literal"] else {v.strip().upper() for v in prog.resolve_var(opt["var"], values, cx["offset"])})
                if not names:
                    art["cics_files"].append({"file": None, "var": opt["var"], "verb": verb, "mode": mode, "line": cx["line"], "dsn": None})
                    self.add_edge("program->dataset", art["name"], f"CICS file via {opt['var']}", path, cx["line"], "unresolved",
                                  self._unreached_detail(site, opt["var"], f"EXEC CICS {verb} file name"), modes=[mode])
                    continue
                for fname in names:
                    entry = csd_files.get(fname)
                    rec = {"file": fname, "var": opt["var"], "verb": verb, "mode": mode, "line": cx["line"], "dsn": None}
                    if site["copybook"]:
                        rec["included_from"], rec["path"] = site["copybook"], path
                    art["cics_files"].append(rec)
                    if entry and entry["attrs"].get("DSNAME"):
                        d = self.dataset(resolve_symbolics(entry["attrs"]["DSNAME"], entry.get("sets") or {}))
                        rec["dsn"] = d["dsn"]
                        d["program_access"].append({"program": art["name"], "modes": [mode], "via": f"CICS FILE {fname} ({entry['source']}:{entry['line']})",
                                                    "path": path, "line": cx["line"], "dd_path": entry["source"], "dd_line": entry["line"]})
                        self.add_edge("program->dataset", art["name"], d["dsn"], path, cx["line"], "resolved",
                                      f"EXEC CICS {verb} FILE {fname} -> CSD DSNAME at {entry['source']}:{entry['line']}", modes=[mode])
                    else:
                        self.add_edge("program->dataset", art["name"], f"CICS file {fname}", path, cx["line"], "unresolved",
                                      "no DEFINE FILE with DSNAME for this file name in any CSD source", modes=[mode])
            if verb in ("SEND MAP", "RECEIVE MAP") and "MAPSET" in cx["options"]:
                opt = cx["options"]["MAPSET"]
                if not opt:
                    continue
                names = sorted({opt["literal"].strip().upper()} if opt["literal"] else {v.strip().upper() for v in prog.resolve_var(opt["var"], values, cx["offset"])})
                if not names:
                    self.add_edge("program->bmsmap", art["name"], f"mapset via {opt['var']}", path, cx["line"], "unresolved",
                                  self._unreached_detail(site, opt["var"], "MAPSET"))
                for ms in names:
                    target = self.bms_by_mapset.get(ms)
                    if target:
                        self.add_edge("program->bmsmap", art["name"], target["name"], path, cx["line"], "resolved", f"{verb} MAPSET {ms}")
                    else:
                        self.add_edge("program->bmsmap", art["name"], ms, path, cx["line"], "unresolved", "mapset source not in repository")

    def _link_scheduler(self):
        jobs_by_name = {a["name"]: a for a in self.artifacts if a["type"] == "jcl_job"}
        stems = {Path(a["path"]).stem.upper(): a for a in self.artifacts if a["type"] == "jcl_job"}
        for j in self.scheduler_jobs:
            member = (j.get("member") or j["job"] or "").upper()
            target = jobs_by_name.get(member) or stems.get(member)
            label = f"{j['scheduler']}:{j['job']}" + (f" [{j['folder']}]" if j.get("folder") else "")
            if target:
                self.add_edge("scheduler->job", label, target["name"], j["path"], j["line"], "resolved", f"member {member}")
            else:
                self.add_edge("scheduler->job", label, member or "?", j["path"], j["line"], "unresolved",
                              "scheduler references a JCL member that is not in the repository")

    def _link_catalog(self):
        for e in self.listcat:
            d = self.dataset(e["dsn"])
            d["catalog_types"].add(e["entry_type"])
        for art in self.artifacts:
            if art["type"] == "data_sample" and "." in art["name"]:
                d = self.dataset(art["name"])
                d["sample_files"].append(art["path"])

    def _finish_datasets(self):
        for d in self.datasets.values():
            types = d["catalog_types"]
            dsn = d["dsn"]
            if types & {"DATA", "INDEX"} and not types & {"CLUSTER", "AIX", "PATH"} or re.search(r"\.(DATA|INDEX)$", dsn):
                d["kind"] = "VSAM component (DATA/INDEX)"
            elif types & {"CLUSTER", "AIX", "PATH"} or re.search(r"\.VSAM\b|KSDS|ESDS|RRDS|\.AIX\b|\.PATH\b", dsn):
                d["kind"] = "VSAM"
            elif "GDG BASE" in types or d["relative_generations"] or GDG_ABSOLUTE_RE.search(dsn):
                d["kind"] = "GDG / sequential"
            elif re.search(r"LOADLIB|LOAD$|CNTL|PROC$|JCL$|COBOL|COPY|CPY|BMS$|ASM$|MACLIB|DBRMLIB|SDSN|RUNLIB|SDFH|SCEE|LINKLIB|SRCLIB|LISTING|BIND$|DBRM|PROCLIB|PARMLIB|MACLIB|SIGY|SISP|SCSQ|SDFS|PSBLIB|DBDLIB|ACBLIB|RESLIB|SDFSRESL|\(", dsn):
                d["kind"] = "library (PDS/PDSE)"
            elif d["symbolic"]:
                d["kind"] = "unresolved symbolic"
            elif re.search(r"\.PS$|\.PS\.|SEQ|BKUP|DALY|REPT|\.TXT$|PARM$|EXPORT|IMPORT|UNLOAD|REJS|SYSIN|SYSOUT|OUTPUT|REPORT|SORTED|COPY", dsn):
                d["kind"] = "sequential"
            else:
                d["kind"] = "unknown"
            d["catalog_types"] = sorted(types)
            d["relative_generations"] = sorted(d["relative_generations"], key=lambda g: (int(g), g))
            sources = []
            if d["program_access"]:
                sources.append("application program" if any(not a.get("utility") for a in d["program_access"]) else "utility step only")
            if d["jcl_refs"]:
                sources.append("JCL DD/control statement")
            if d["csd_files"]:
                sources.append("CSD FILE")
            if d["csd_libraries"]:
                sources.append("CSD LIBRARY")
            if d["catalog_types"]:
                sources.append("catalog listing")
            if d["sample_files"]:
                sources.append("sample data file")
            d["reference_sources"] = sources

    DATA_BEARING_KINDS = ("VSAM", "GDG / sequential", "sequential", "unknown")

    # -- derived views --------------------------------------------------
    def distinct_edges(self, categories, status):
        seen = OrderedDict()
        for e in self.edges:
            if e["category"] in categories and e["status"] == status:
                seen.setdefault((e["category"], e["from"], e["to"]), e)
        return list(seen.values())

    def referenced_programs(self) -> dict[str, set]:
        refs: dict[str, set] = defaultdict(set)
        for e in self.edges:
            if e["status"] != "resolved":
                continue
            if e["category"] in ("jclstep->program", "transaction->program", "program->program"):
                refs[e["to"]].add(e["category"])
        for art in self.artifacts:
            if art.get("csd_program_refs"):
                refs[art["name"]].add("csd DEFINE PROGRAM")
        return refs

    def orphans(self) -> dict:
        refs = self.referenced_programs()
        programs = []
        for art in self.artifacts:
            if art["type"] in ("cobol_program", "assembler") and art["name"] not in refs:
                programs.append(art)
        copied = {e["to"] for e in self.edges
                  if e["category"] in ("program->copybook", "copybook->copybook", "assembler->macro") and e["status"] == "resolved"}
        copybooks = [a for a in self.artifacts
                     if a["type"] in ("copybook", "bms_copybook", "sql_dclgen", "asm_macro") and a["name"] not in copied]
        datasets = [d for d in self.datasets.values()
                    if d["kind"] in self.DATA_BEARING_KINDS and not any(not a.get("utility") for a in d["program_access"])]
        return {"programs": programs, "copybooks": copybooks, "datasets": datasets}

    def construct_rows(self) -> list[dict]:
        rows = []
        for path, prog in self.cobol.items():
            rows.extend(prog.constructs)
        # JCL-level hard-coded values
        for path, member in self.jcl.items():
            for step in member.steps:
                parm = step.get("parm")
                if parm and re.search(r"\d{8}", parm):
                    rows.append({"construct": "Hard-coded date", "path": path, "line": step["line"],
                                 "detail": f"PARM={parm}", "snippet": f"EXEC PGM={step['pgm']},PARM={parm}"})
                for lineno, l in step["instream"]:
                    for m in re.finditer(r"C'(\d{4}-\d{2}-\d{2})'", l):
                        rows.append({"construct": "Hard-coded date", "path": path, "line": lineno,
                                     "detail": f"SORT symbol {m.group(1)}", "snippet": l.strip()[:70]})
        rows.sort(key=lambda r: (r["construct"], r["path"], r["line"], r["detail"]))
        return rows


# ---------------------------------------------------------------------------
# Summary numbers
# ---------------------------------------------------------------------------

TYPE_LABELS = OrderedDict([
    ("cobol_program", "COBOL program"),
    ("copybook", "Copybook (COBOL)"),
    ("bms_copybook", "Copybook (BMS symbolic map)"),
    ("sql_dclgen", "Copybook (SQL DCLGEN)"),
    ("bms_map", "BMS map source"),
    ("jcl_job", "JCL job"),
    ("jcl_proc", "JCL procedure"),
    ("csd", "CICS CSD definition file"),
    ("assembler", "Assembler program"),
    ("asm_macro", "Assembler macro"),
    ("control_card", "Control card / parameter member"),
    ("sql_ddl", "SQL DDL member"),
    ("ims_definition", "IMS DBD/PSB definition"),
    ("data_sample", "Sample data file"),
    ("catalog_listing", "Catalog listing"),
    ("scheduler_def", "Scheduler definition"),
    ("module_readme", "Module documentation"),
])

LINEAGE_FILE = "04-field-lineage.md"
DECISIONS_FILE = "05-government-decisions.md"
MARK_BEGIN = "<!-- generated:{tag} -->"
MARK_END = "<!-- /generated:{tag} -->"


def lineage_stats(path: Path) -> dict:
    """Count hop rows (a table row whose last cell is Confirmed or Inferred) per lineage section."""
    stats = OrderedDict()
    section = None
    if not path.exists():
        return stats
    for line in path.read_text(encoding="utf-8").splitlines():
        if line.startswith("## "):
            section = line[3:].strip()
            continue
        m = re.match(r"^\|.*\|\s*\*{0,2}(Confirmed|Inferred)\*{0,2}\s*\|\s*$", line)
        if m and section:
            s = stats.setdefault(section, {"hops": 0, "Confirmed": 0, "Inferred": 0})
            s["hops"] += 1
            s[m.group(1)] += 1
    return stats


def decision_count(path: Path) -> int:
    if not path.exists():
        return 0
    return sum(1 for l in path.read_text(encoding="utf-8").splitlines() if re.match(r"^\|\s*D\d+\s*\|", l))


def summarize(estate: Estate) -> dict:
    s = OrderedDict()
    type_counts = Counter(a["type"] for a in estate.artifacts)
    s["artifact_total"] = len(estate.artifacts)
    s["artifact_counts"] = OrderedDict((t, type_counts[t]) for t in TYPE_LABELS if type_counts.get(t))
    for t in type_counts:
        if t not in s["artifact_counts"]:
            s["artifact_counts"][t] = type_counts[t]
    s["cobol_subtypes"] = OrderedDict(sorted(Counter(a["subtype"] for a in estate.artifacts if a["type"] == "cobol_program").items()))
    s["cobol_prefix_classes"] = OrderedDict(sorted(Counter(a["name_prefix_class"] for a in estate.artifacts if a["type"] == "cobol_program").items()))
    s["module_counts"] = OrderedDict(sorted(Counter(a["module"] for a in estate.artifacts).items()))
    resolved = estate.distinct_edges(HEADLINE_CATEGORIES, "resolved")
    unresolved = estate.distinct_edges(HEADLINE_CATEGORIES, "unresolved")
    s["headline"] = OrderedDict([
        ("resolved_edges", len(resolved)),
        ("unresolved_edges", len(unresolved)),
        ("categories", HEADLINE_CATEGORIES),
        ("by_category", OrderedDict((c, OrderedDict([("resolved", sum(1 for e in resolved if e["category"] == c)),
                                                       ("unresolved", sum(1 for e in unresolved if e["category"] == c))]))
                                    for c in HEADLINE_CATEGORIES)),
    ])
    s["other_edges"] = OrderedDict((c, OrderedDict([("resolved", len(estate.distinct_edges([c], "resolved"))),
                                                    ("unresolved", len(estate.distinct_edges([c], "unresolved"))),
                                                    ("ambiguous", len(estate.distinct_edges([c], "ambiguous")))]))
                                   for c in EXTRA_CATEGORIES)
    rows = estate.construct_rows()
    s["construct_total"] = len(rows)
    s["construct_counts"] = OrderedDict(sorted(Counter(r["construct"] for r in rows).items(), key=lambda kv: (-kv[1], kv[0])))
    o = estate.orphans()
    s["orphans"] = OrderedDict([("programs", len(o["programs"])), ("copybooks", len(o["copybooks"])),
                                ("datasets", len(o["datasets"])),
                                ("total", len(o["programs"]) + len(o["copybooks"]) + len(o["datasets"]))])
    s["dataset_total"] = len(estate.datasets)
    s["dataset_kinds"] = OrderedDict(sorted(Counter(d["kind"] for d in estate.datasets.values()).items()))
    s["csd_entry_counts"] = OrderedDict(sorted(Counter(e["kind"] for e in estate.csd_entries).items()))
    s["csd_entry_total"] = len(estate.csd_entries)
    s["scheduler_job_total"] = len(estate.scheduler_jobs)
    s["dynamic_calls_unresolved"] = sum(1 for e in estate.edges if e["category"] == "program->program"
                                        and e["status"] == "unresolved" and e.get("kind") == "dynamic")
    s["datasets_jcl_only"] = sum(1 for d in estate.datasets.values() if d["jcl_refs"] and not d["program_access"]
                                 and d["kind"] in Estate.DATA_BEARING_KINDS)
    ls = lineage_stats(OUT_DIR / LINEAGE_FILE)
    s["lineage"] = OrderedDict([
        ("hops", sum(v["hops"] for v in ls.values())),
        ("confirmed", sum(v["Confirmed"] for v in ls.values())),
        ("inferred", sum(v["Inferred"] for v in ls.values())),
        ("by_lineage", ls),
    ])
    s["government_decisions"] = decision_count(OUT_DIR / DECISIONS_FILE)
    return s


def serialize(estate: Estate, summary: dict) -> dict:
    arts = []
    for a in estate.artifacts:
        o = OrderedDict()
        for k, v in a.items():
            if k == "bms" and v:
                o[k] = {"mapsets": v["mapsets"], "maps": v["maps"]}
            elif k == "call_targets":
                o[k] = [{kk: vv for kk, vv in c.items() if kk != "offset"} for c in v]
            else:
                o[k] = v
        arts.append(o)
    datasets = []
    for d in sorted(estate.datasets.values(), key=lambda d: d["dsn"]):
        datasets.append(OrderedDict([
            ("dsn", d["dsn"]), ("kind", d["kind"]), ("catalog_types", d["catalog_types"]),
            ("relative_generations", d["relative_generations"]),
            ("reference_sources", d["reference_sources"]),
            ("program_access", d["program_access"]), ("jcl_refs", d["jcl_refs"]),
            ("csd_files", d["csd_files"]), ("csd_libraries", d["csd_libraries"]), ("sample_files", d["sample_files"]),
        ]))
    return OrderedDict([
        ("schema", "estate-discovery/1"),
        ("generator", "docs/discovery/build_discovery.py"),
        ("source_dirs", SOURCE_DIRS),
        ("summary", summary),
        ("artifacts", arts),
        ("edges", estate.edges),
        ("datasets", datasets),
        ("csd_entries", [OrderedDict([("kind", e["kind"]), ("name", e["name"]),
                                      ("attrs", {k: v for k, v in e["attrs"].items() if k not in CSD_FREE_TEXT_ATTRS}),
                                      ("path", e["source"]), ("line", e["line"]), ("module", e["module"])])
                         for e in estate.csd_entries]),
        ("scheduler_jobs", estate.scheduler_jobs),
        ("scheduler_edges", estate.scheduler_edges),
        ("constructs", estate.construct_rows()),
        ("notes", estate.notes),
    ])


# ---------------------------------------------------------------------------
# Markdown renderers
# ---------------------------------------------------------------------------

GENERATED_BANNER = ("<!-- Generated by docs/discovery/build_discovery.py. Do not edit by hand; "
                    "run `python3 docs/discovery/build_discovery.py` to regenerate. -->\n\n")


def fmt_int(n) -> str:
    return f"{n:,}"


def render_inventory(estate: Estate, s: dict) -> str:
    out = [GENERATED_BANNER, "# 01 - Artifact inventory\n"]
    out.append(f"Static inventory of `{'`, `'.join(SOURCE_DIRS)}` — **{fmt_int(s['artifact_total'])} artifacts**. "
               "Every row below is derived from the source tree by the generator; nothing is hand-typed.\n")
    out.append("## Counts by artifact type\n")
    rows = [(TYPE_LABELS.get(t, t), t, fmt_int(n)) for t, n in s["artifact_counts"].items()]
    rows.append(("**Total**", "", f"**{fmt_int(s['artifact_total'])}**"))
    out.append(md_table(["Artifact type", "`type` in inventory.json", "Count"], rows) + "\n")

    out.append("### COBOL programs by classification\n")
    out.append("Classification rule: `online` = program contains `EXEC CICS`; `batch` = no CICS but has a "
               "`FILE-CONTROL`/`SELECT` or a `CB*` name; `utility` = neither (called subroutines such as date utilities).\n")
    rows = [(k, fmt_int(v)) for k, v in s["cobol_subtypes"].items()]
    out.append(md_table(["Classification (by content)", "Count"], rows) + "\n")
    rows = [(k, fmt_int(v)) for k, v in s["cobol_prefix_classes"].items()]
    out.append(md_table(["Name-prefix class", "Count"], rows) + "\n")
    mism = [(a["name"], a["path"], a["subtype"], a["name_prefix_class"], "; ".join(n for n in a["notes"] if "prefix" in n))
            for a in estate.artifacts if a["type"] == "cobol_program" and any("prefix" in n for n in a["notes"])]
    if mism:
        out.append("Programs whose name prefix disagrees with their content classification:\n")
        out.append(md_table(["Program", "Path", "Content class", "Prefix class", "Note"], mism) + "\n")

    out.append("### Artifacts by module\n")
    rows = [(m, fmt_int(n)) for m, n in s["module_counts"].items()]
    out.append(md_table(["Module (source subtree)", "Artifacts"], rows) + "\n")

    out.append("### CICS CSD definitions\n")
    rows = [(k, fmt_int(v)) for k, v in s["csd_entry_counts"].items()]
    rows.append(("**Total DEFINE statements**", f"**{fmt_int(s['csd_entry_total'])}**"))
    out.append(md_table(["Resource kind", "DEFINE statements"], rows) + "\n")

    out.append("### Datasets (distinct DSNs seen anywhere in source)\n")
    out.append("A dataset is counted once per normalised DSN (GDG relative generations and quotes stripped). "
               "The `kind` is a heuristic from the catalog listing entry type and the DSN's naming; see `02-dependency-map.md` "
               "for who reads and writes each one.\n")
    rows = [(k, fmt_int(v)) for k, v in s["dataset_kinds"].items()]
    rows.append(("**Total**", f"**{fmt_int(s['dataset_total'])}**"))
    out.append(md_table(["Dataset kind", "Count"], rows) + "\n")

    out.append("### Scheduler definitions\n")
    out.append(f"{fmt_int(s['scheduler_job_total'])} scheduled job definitions across "
               f"{fmt_int(s['artifact_counts'].get('scheduler_def', 0))} scheduler files.\n")
    rows = [(j["scheduler"], j["job"], j.get("folder") or "", j.get("member") or j["job"], cite(j["path"], j["line"]))
            for j in estate.scheduler_jobs]
    out.append(md_table(["Scheduler", "Job", "Folder / schedule id", "JCL member", "Source"], rows) + "\n")

    out.append("## Full inventory\n")
    out.append("One row per artifact. `Depends on` lists `COPY`/`INCLUDE` targets for COBOL, `COPY` members and repository macros for "
               "Assembler, the driving program per step for JCL, "
               "and `DEFINE` counts for CSD. `Calls` marks each `CALL`/`LINK`/`XCTL` target as static (literal) or dynamic (variable); "
               "`(from COPY x)` marks a call that a procedural copybook carries into the program.\n")
    rows = []
    for a in estate.artifacts:
        dep = ""
        calls = ""
        if a["type"] in ("cobol_program", "copybook", "bms_copybook", "sql_dclgen"):
            dep = ", ".join(c["name"] + ("" if c["resolved"] else " (unresolved)") for c in a["copy_targets"])
            calls = ", ".join((c["target"] or f"via {c['via']}") + f" [{c['kind']}]"
                              + (f" (from COPY {c['included_from']})" if c.get("included_from") else "")
                              for c in a["call_targets"])
        elif a["type"] == "assembler":
            dep = ", ".join(f"{c['name']} ({c['via']})" + ("" if c["resolved"] else " (unresolved)") for c in a["copy_targets"])
        elif a["type"] in ("jcl_job", "jcl_proc"):
            dep = "; ".join(f"{st['step'] or '?'}→{st.get('driving_program') or '?'}" for st in a["jcl_steps"])
        elif a["type"] == "csd":
            dep = f"{len(a['csd_definitions'])} DEFINE statements"
        elif a["type"] == "bms_map" and a["bms"]:
            dep = "mapset " + ", ".join(m["name"] for m in a["bms"]["mapsets"]) + "; maps " + ", ".join(m["name"] for m in a["bms"]["maps"])
        elif a["type"] == "scheduler_def":
            dep = f"{len(a['scheduler_jobs'])} job definitions"
        elif a["type"] == "catalog_listing":
            dep = f"{a.get('catalog_entries', 0)} catalog entries"
        cics = ", ".join(f"{k}×{v}" for k, v in a["cics_verbs"].items()) if a["cics_verbs"] else ""
        rows.append((f"`{a['path']}`", a["type"] + (f" / {a['subtype']}" if a["subtype"] else ""), a["name"],
                     "n/a" if a["line_count"] is None else fmt_int(a["line_count"]), dep, calls, cics,
                     "yes" if a["exec_sql"] else "", ", ".join(a["transaction_ids"])))
    out.append(md_table(["Path", "Type", "Program-id / member", "Lines", "Depends on", "Calls", "EXEC CICS verbs", "EXEC SQL", "CICS tran id"], rows) + "\n")

    out.append("## What the script could not resolve\n")
    unres_dyn = [e for e in estate.edges if e["category"] == "program->program" and e["status"] == "unresolved" and e.get("kind") == "dynamic"]
    out.append(f"### Dynamic `CALL` / `LINK` / `XCTL` through a variable whose value is not fully established in the program ({fmt_int(len(unres_dyn))})\n")
    out.append("The generator follows the `MOVE`/`VALUE` definitions that reach each statement (earlier in the same paragraph, "
               "or through the `PERFORM`/`GO TO` sites and fall-through that enter it) and the menu tables in `COMEN02Y`/`COADM02Y`; "
               "a statement is listed here when no literal reaches it, or when at least one path reaches it with a value the program "
               "never sets (typically the caller's commarea).  Literal targets found on the other paths are reported as resolved edges.\n")
    out.append(md_table(["Program", "Statement", "Source", "Reason"],
                        [(e["from"], e["to"], cite(e["path"], e["line"]), e["detail"]) for e in unres_dyn]) + "\n")
    ext = [e for e in estate.distinct_edges(["program->program"], "unresolved") if e.get("kind") != "dynamic"]
    out.append(f"### Call targets with no source in the repository ({fmt_int(len(ext))} distinct)\n")
    out.append("System, Language Environment, DL/I and message-queue APIs, or application programs the repository does not contain.\n")
    out.append(md_table(["Program", "Target", "Source", "Classification"],
                        [(e["from"], e["to"], cite(e["path"], e["line"]), e["detail"]) for e in ext]) + "\n")
    jcl_only = [d for d in sorted(estate.datasets.values(), key=lambda d: d["dsn"])
                if d["jcl_refs"] and not d["program_access"] and d["kind"] in Estate.DATA_BEARING_KINDS]
    out.append(f"### Datasets referenced only in JCL ({fmt_int(len(jcl_only))})\n")
    out.append("Named on a `DD` or utility control statement but never opened by a COBOL program in this repository "
               "(some are consumed by utilities such as IDCAMS/SORT whose behaviour is not analysed).\n")
    out.append(md_table(["DSN", "Kind", "First JCL reference"],
                        [(d["dsn"], d["kind"], cite(d["jcl_refs"][0]["path"], d["jcl_refs"][0]["line"])) for d in jcl_only]) + "\n")
    o = estate.orphans()
    out.append(f"### Programs with no caller ({fmt_int(len(o['programs']))})\n")
    out.append("No JCL `EXEC PGM=`, no CSD `DEFINE PROGRAM`/`TRANSACTION`, and no resolved `CALL`/`LINK`/`XCTL` names them.\n")
    out.append(md_table(["Program", "Path", "Lines"], [(a["name"], f"`{a['path']}`", a["line_count"]) for a in o["programs"]]) + "\n")
    out.append(f"### Copybooks nobody copies ({fmt_int(len(o['copybooks']))})\n")
    out.append(md_table(["Copybook", "Path", "Lines"], [(a["name"], f"`{a['path']}`", a["line_count"]) for a in o["copybooks"]]) + "\n")
    unres_cics = [e for e in estate.edges if e["category"] == "transaction->program" and e["status"] == "unresolved"]
    out.append(f"### CSD transactions whose program has no source ({fmt_int(len(unres_cics))})\n")
    out.append(md_table(["Transaction", "Program", "Source", "Reason"],
                        [(e["from"], e["to"], cite(e["path"], e["line"]), e["detail"]) for e in unres_cics]) + "\n")
    if estate.notes:
        out.append("### Generator notes\n")
        out.append("\n".join(f"- {n}" for n in estate.notes) + "\n")
    return "\n".join(out)


def _copybooks_of(estate: Estate, program: str) -> list[str]:
    return sorted({e["to"] for e in estate.edges if e["category"] == "program->copybook" and e["from"] == program and e["status"] == "resolved"})


def _datasets_of(estate: Estate, program: str) -> list[tuple[str, str, str]]:
    seen = OrderedDict()
    for e in estate.edges:
        if e["category"] == "program->dataset" and e["from"] == program and e["status"] == "resolved":
            seen.setdefault(e["to"], set()).update(e.get("modes") or ["unknown"])
    return [(dsn, "/".join(sorted(m)), "") for dsn, m in sorted(seen.items())]


def _step_rows(estate: Estate) -> list[dict]:
    """Flattened job -> step -> program rows for every JCL job (procedures expanded)."""
    rows = []
    for art in estate.artifacts:
        if art["type"] not in ("jcl_job", "jcl_proc"):
            continue
        for st in art["jcl_steps"]:
            rows.append({"job": art["name"] + (" (PROC)" if art["type"] == "jcl_proc" else ""), "job_path": art["path"],
                         "step": st["step"] or "?", "line": st["line"], "program": st.get("driving_program"),
                         "kind": st.get("driving_program_kind"), "hosted": st.get("hosted_programs", []), "parm": st.get("parm"),
                         "proc_bindings": st.get("proc_bindings", [])})
    return rows


def _mermaid_graph(estate: Estate) -> tuple[str, int]:
    """Focused graph: the batch posting/interest/report chain plus the online programs that share its datasets."""
    lines = ["graph LR"]
    nodes = OrderedDict()
    edges = OrderedDict()

    def node(key, label, shape="[", close="]"):
        if key not in nodes:
            nodes[key] = f'{mermaid_id(key)}{shape}"{mermaid_label(label)}"{close}'
        return mermaid_id(key)

    def edge(a, b, label=""):
        edges[(a, b, label)] = None

    focus_jobs = ["POSTTRAN", "INTCALC", "TRANREPT"]
    batch_programs = []
    for e in estate.edges:
        if e["category"] == "jclstep->program" and e["status"] == "resolved" and e.get("program_kind") == "application":
            job = e["from"].split("/")[0]
            if job in focus_jobs and e["to"] not in batch_programs:
                batch_programs.append(e["to"])
                edge(node(f"job:{job}", job, "([", "])"), node(f"pgm:{e['to']}", e["to"]), e["from"].split("/")[1])
    focus_dsns = set()
    for p in batch_programs:
        cps = _copybooks_of(estate, p)
        if cps:
            edge(node(f"pgm:{p}", p), node(f"cpy:{p}", f"{len(cps)} copybook" + ("s" if len(cps) != 1 else ""), "[/", "/]"), "COPY")
        for dsn, modes, _ in _datasets_of(estate, p):
            short = strip_gdg(dsn)
            focus_dsns.add(short)
            edge(node(f"pgm:{p}", p), node(f"dsn:{short}", short, "[(", ")]"), modes)
    # online programs touching the same datasets, via their CSD transactions
    online = []
    for e in estate.edges:
        if e["category"] == "program->dataset" and e["status"] == "resolved" and strip_gdg(e["to"]) in focus_dsns:
            a = estate.find_program(e["from"])
            if a and a["subtype"] == "online" and e["from"] not in online:
                online.append(e["from"])
    online = sorted(online)[:8]
    for p in online:
        trans = sorted({e["from"] for e in estate.edges if e["category"] == "transaction->program" and e["to"] == p and e["status"] == "resolved"})
        for t in trans[:2]:
            edge(node(f"tran:{t}", t, "{{", "}}"), node(f"pgm:{p}", p), "CSD")
        maps = sorted({e["to"] for e in estate.edges if e["category"] == "program->bmsmap" and e["from"] == p and e["status"] == "resolved"})
        for m in maps[:1]:
            edge(node(f"pgm:{p}", p), node(f"bms:{m}", f"BMS {m}", ">", "]"), "SEND/RECEIVE MAP")
        cps = _copybooks_of(estate, p)
        if cps:
            edge(node(f"pgm:{p}", p), node(f"cpy:{p}", f"{len(cps)} copybook" + ("s" if len(cps) != 1 else ""), "[/", "/]"), "COPY")
        for dsn, modes, _ in _datasets_of(estate, p):
            short = strip_gdg(dsn)
            if short in focus_dsns:
                edge(node(f"pgm:{p}", p), node(f"dsn:{short}", short, "[(", ")]"), modes)
    lines.extend(f"    {v}" for v in nodes.values())
    for (a, b, label), _ in edges.items():
        lines.append(f"    {a} -->|{mermaid_label(label)}| {b}" if label else f"    {a} --> {b}")
    return "\n".join(lines), len(nodes)


def render_dependency_map(estate: Estate, s: dict) -> str:
    out = [GENERATED_BANNER, "# 02 - Dependency map\n"]
    h = s["headline"]
    out.append(f"**{fmt_int(h['resolved_edges'])} distinct dependency edges resolved, {fmt_int(h['unresolved_edges'])} unresolved** "
               "across the five headline categories (program→copybook, program→dataset, JCL step→program, transaction→program, program→program). "
               "An edge is *distinct* per (category, from, to); the same `COPY` in two places counts once.\n")
    rows = [(c, fmt_int(v["resolved"]), fmt_int(v["unresolved"])) for c, v in h["by_category"].items()]
    rows.append(("**Total**", f"**{fmt_int(h['resolved_edges'])}**", f"**{fmt_int(h['unresolved_edges'])}**"))
    out.append(md_table(["Edge category", "Resolved", "Unresolved"], rows) + "\n")
    out.append("Supporting edge categories (not in the headline count):\n")
    rows = [(c, fmt_int(v["resolved"]), fmt_int(v["unresolved"]), fmt_int(v["ambiguous"])) for c, v in s["other_edges"].items()]
    out.append(md_table(["Edge category", "Resolved", "Unresolved", "Ambiguous"], rows) + "\n")

    graph, n = _mermaid_graph(estate)
    out.append(f"## Focused graph ({n} nodes)\n")
    out.append("Batch chain for the three jobs that carry the money lineage (`POSTTRAN`, `INTCALC`, `TRANREPT`) and the online "
               "programs that touch the same datasets. Copybooks are collapsed to one node per program; GDG generation suffixes are stripped. "
               "The full estate is in the tables that follow.\n")
    out.append("```mermaid\n" + graph + "\n```\n")

    out.append("## Batch: JCL job → step → program → copybooks → datasets\n")
    out.append("Every step of every JCL member. Utility steps (IDCAMS, SORT, IEBGENER, …) show the datasets named on their `DD`s "
               "with mode `unknown` because utility control statements are not interpreted. Application steps list the program's "
               "resolved copybooks and the datasets its `SELECT ... ASSIGN` names bind to through the step's `DD` statements. "
               "`EXEC PROC` steps show every dataset the expanded procedure steps bind after the invocation's `DD` overrides and "
               "symbolic substitution, labelled `procstep.DDNAME`.\n")
    rows = []
    invoked_procs = {e["to"] for e in estate.edges if e["category"] == "jclstep->proc" and e["status"] == "resolved"}
    for r in _step_rows(estate):
        pgm = r["program"] or "?"
        hosted = ", ".join(f"{hp['program']} ({hp['kind']})" for hp in r["hosted"])
        target = estate.find_program(pgm) if r["kind"] == "application" else None
        hosted_apps = [hp["program"] for hp in r["hosted"] if hp["kind"] == "application"]
        cps = ""
        dss = ""
        if target:
            cps = ", ".join(_copybooks_of(estate, target["name"]))
            dss = "<br>".join(f"{d} ({m})" for d, m, _ in _datasets_of_step(estate, target["name"], r["job"], r["step"]))
            if not dss and r["job"].endswith("(PROC)"):
                dss = ("(procedure step: datasets bind through the invoking job's rows)" if r["job"].split()[0] in invoked_procs
                       else "(procedure step: no job in the repository invokes this procedure)")
        elif hosted_apps:
            cps = "; ".join(f"{p}: " + ", ".join(_copybooks_of(estate, p)) for p in hosted_apps)
            dss = "<br>".join(f"{d} ({m})" for p in hosted_apps for d, m, _ in _datasets_of_step(estate, p, r["job"], r["step"]))
        elif r["kind"] == "procedure":
            dss = "<br>".join(f"{b['dsn']} ({_binding_mode(estate, b)}; {b['proc_step']}.{b['ddname']})" for b in r["proc_bindings"])
        else:
            dss = "<br>".join(f"{d} (unknown)" for d in _utility_step_datasets(estate, r["job"], r["line"]))
        rows.append((r["job"], r["step"], cite(r["job_path"], r["line"]), pgm + (f" [{r['kind']}]" if r["kind"] else ""),
                     hosted, r["parm"] or "", cps, dss))
    out.append(md_table(["Job", "Step", "Source", "Program", "Hosted program(s)", "PARM", "Copybooks", "Datasets (mode)"], rows) + "\n")

    out.append("## Online: transaction → program → BMS map → copybooks → datasets\n")
    out.append("Transactions come from `DEFINE TRANSACTION ... PROGRAM(...)` in the CSD sources; maps from `EXEC CICS SEND/RECEIVE MAP ... MAPSET(...)`; "
               "datasets from `EXEC CICS READ/WRITE/REWRITE/DELETE/STARTBR ... FILE(...)` joined to `DEFINE FILE ... DSNAME(...)`. "
               "Programs reached only by `XCTL`/`LINK` (no transaction of their own) are listed with an empty transaction cell.\n")
    rows = []
    online_programs = sorted(a["name"] for a in estate.artifacts if a["type"] == "cobol_program" and a["subtype"] == "online")
    tran_of = defaultdict(list)
    for e in estate.edges:
        if e["category"] == "transaction->program" and e["status"] == "resolved":
            tran_of[e["to"]].append((e["from"], cite(e["path"], e["line"])))
    for p in online_programs:
        a = estate.find_program(p)
        trans = sorted(set(tran_of.get(p, [])))
        maps = sorted({e["to"] for e in estate.edges if e["category"] == "program->bmsmap" and e["from"] == p and e["status"] == "resolved"})
        callers = sorted({e["from"] for e in estate.edges if e["category"] == "program->program" and e["to"] == p and e["status"] == "resolved"})
        files = OrderedDict()
        for cf in a.get("cics_files", []):
            key = cf["file"] or f"via {cf['var']}"
            files.setdefault(key, [cf["dsn"] or "(no CSD DSNAME)", set()])[1].add(cf["mode"])
        dss = "<br>".join(f"{k} → {v[0]} ({'/'.join(sorted(v[1]))})" for k, v in files.items())
        sql = "yes" if a["exec_sql"] else ""
        rows.append((", ".join(f"{t} {c}" for t, c in trans), p, f"`{a['path']}`", ", ".join(maps),
                     ", ".join(_copybooks_of(estate, p)), dss, sql, ", ".join(callers)))
    out.append(md_table(["Transaction (CSD source)", "Program", "Path", "BMS mapset(s)", "Copybooks", "CICS files → dataset (mode)", "SQL", "Reached from"], rows) + "\n")

    out.append("## Program → program calls\n")
    rows = []
    for e in estate.distinct_edges(["program->program"], "resolved") + estate.distinct_edges(["program->program"], "unresolved"):
        rows.append((e["from"], e["to"], e.get("kind", ""), e["status"], cite(e["path"], e["line"]), e["detail"]))
    out.append(md_table(["From", "To", "Kind", "Status", "Source", "Detail"], rows) + "\n")

    out.append("## Reverse view: dataset → programs (with access mode)\n")
    out.append("Mode is taken from the COBOL verbs (`READ`, `WRITE`, `REWRITE`, `DELETE`, `START`, or the `EXEC CICS` equivalent) applied to the "
               "file bound to that dataset; `unknown` where the dataset is only named on a utility step's `DD`, an IDCAMS control statement, "
               "the CSD, the catalog listing, or a sample data file. Library datasets (load, source, control, procedure) are listed separately.\n")
    data_ds = [d for d in sorted(estate.datasets.values(), key=lambda d: d["dsn"]) if d["kind"] in Estate.DATA_BEARING_KINDS]
    other_ds = [d for d in sorted(estate.datasets.values(), key=lambda d: d["dsn"]) if d["kind"] not in Estate.DATA_BEARING_KINDS]
    rows = []
    for d in data_ds:
        acc = OrderedDict()
        for a in d["program_access"]:
            acc.setdefault(a["program"], [set(), []])
            acc[a["program"]][0].update(a["modes"])
            acc[a["program"]][1].append(cite(a["path"], a["line"]))
        progs = "<br>".join(f"{p}: {'/'.join(sorted(v[0]))} {v[1][0]}" for p, v in acc.items()) or "— (no program opens it)"
        rows.append((d["dsn"], d["kind"], ", ".join(d["catalog_types"]), progs, ", ".join(d["reference_sources"])))
    out.append(md_table(["Dataset", "Kind", "Catalog entry", "Program: mode (first citation)", "Referenced from"], rows) + "\n")
    out.append(f"### Library and component datasets ({fmt_int(len(other_ds))})\n")
    out.append(md_table(["Dataset", "Kind", "Referenced from", "First reference"],
                        [(d["dsn"], d["kind"], ", ".join(d["reference_sources"]), _first_ref(d)) for d in other_ds]) + "\n")

    o = estate.orphans()
    out.append("## Orphans\n")
    out.append(f"**{fmt_int(s['orphans']['total'])} orphans**: {fmt_int(s['orphans']['programs'])} program(s) nothing references, "
               f"{fmt_int(s['orphans']['copybooks'])} copybook(s) nothing copies, {fmt_int(s['orphans']['datasets'])} data-bearing dataset(s) "
               "no application program opens. Whether an orphan is dead, seasonal, or driven from outside the repository is a system-owner "
               "question (see `05-government-decisions.md`).\n")
    out.append("### Programs no JCL, CSD, or resolved call references\n")
    out.append(md_table(["Program", "Path", "Lines", "Files it declares"],
                        [(a["name"], f"`{a['path']}`", a["line_count"], ", ".join(f["ddname"] for f in a["files"])) for a in o["programs"]]) + "\n")
    out.append("### Copybooks nobody copies\n")
    out.append(md_table(["Copybook", "Path", "Lines"], [(a["name"], f"`{a['path']}`", a["line_count"]) for a in o["copybooks"]]) + "\n")
    out.append("### Data-bearing datasets no application program opens\n")
    out.append("Includes utility-only datasets (backup generations, REPRO targets), catalog-only entries (old GDG generations), and "
               "sample data files whose DSN matches no `DD`.\n")
    out.append(md_table(["Dataset", "Kind", "Referenced from", "First reference"],
                        [(d["dsn"], d["kind"], ", ".join(d["reference_sources"]), _first_ref(d)) for d in o["datasets"]]) + "\n")

    out.append("## Scheduler → job\n")
    rows = [(e["from"], e["to"], e["status"], cite(e["path"], e["line"]), e["detail"]) for e in estate.edges if e["category"] == "scheduler->job"]
    out.append(md_table(["Scheduler entry", "JCL job", "Status", "Source", "Detail"], rows) + "\n")
    if estate.scheduler_edges:
        rows = [(x["scheduler"], x["from"], x["to"], x["kind"], cite(x["path"], x["line"])) for x in estate.scheduler_edges]
        out.append("Job-to-job ordering stated in the scheduler definitions:\n")
        out.append(md_table(["Scheduler", "Predecessor", "Successor", "Kind", "Source"], rows) + "\n")
    return "\n".join(out)


def _first_ref(d: dict) -> str:
    if d["jcl_refs"]:
        return cite(d["jcl_refs"][0]["path"], d["jcl_refs"][0]["line"])
    if d["program_access"]:
        return cite(d["program_access"][0]["path"], d["program_access"][0]["line"])
    if d["csd_files"]:
        return cite(d["csd_files"][0]["path"], d["csd_files"][0]["line"])
    if d["csd_libraries"]:
        return cite(d["csd_libraries"][0]["path"], d["csd_libraries"][0]["line"])
    if d["sample_files"]:
        return f"`{d['sample_files'][0]}`"
    if d["catalog_types"]:
        return "`app/catlg/LISTCAT.txt`"
    return ""


def _datasets_of_step(estate: Estate, program: str, job: str, step: str) -> list[tuple[str, str, str]]:
    a = estate.find_program(program)
    seen = OrderedDict()
    for f in a["files"]:
        for ds in f["datasets"]:
            if ds["job"] == job and ds["step"] == step:
                label = ds["dsn"] or f"{f['ddname']}={ds['dd_kind']}"
                seen.setdefault(label, set()).update(f["modes"] or ["unknown"])
    return [(k, "/".join(sorted(v)), "") for k, v in seen.items()]


def _binding_mode(estate: Estate, b: dict) -> str:
    """Access mode of a procedure-step DD binding: the program's COBOL verbs when an application program
    declares that DD, otherwise ``unknown`` (utility control statements are not interpreted)."""
    a = estate.find_program(b["program"])
    if a:
        for f in a["files"]:
            if f["ddname"] == b["ddname"]:
                return "/".join(sorted(f["modes"] or ["unknown"]))
    return "unknown"


def _utility_step_datasets(estate: Estate, job: str, step_line: int) -> list[str]:
    """Datasets a utility step names, keyed on the EXEC line so two steps sharing a name stay distinct."""
    out = []
    for d in sorted(estate.datasets.values(), key=lambda d: d["dsn"]):
        for r in d["jcl_refs"]:
            if r["member"] == job and r["step_line"] == step_line and d["dsn"] not in out:
                out.append(d["dsn"])
    return out


def render_constructs(estate: Estate, s: dict) -> str:
    rows = estate.construct_rows()
    out = [GENERATED_BANNER, "# 03 - Conversion-relevant construct register\n"]
    out.append(f"**{fmt_int(len(rows))} occurrences** of constructs that matter for maintenance and for any future conversion, each with "
               "`path:line`. Occurrences are counted per statement or per data item (a `REDEFINES` inside a copybook counts once in the "
               "copybook, not once per program that copies it). Detection is lexical on comment-stripped, literal-masked source; "
               "the `Detail` column shows what was matched so a reader can verify or discount it.\n")
    out.append("**Scope of the literal constructs.** *Hard-coded amount / numeric literal* covers numeric literals in arithmetic "
               "statements (`COMPUTE`, `ADD`, `SUBTRACT`, `MULTIPLY`, `DIVIDE`; `0` and `1` excluded), decimal literals in `MOVE`, and "
               "decimal `VALUE` clauses. Integer `MOVE` literals and integer `VALUE` clauses (counters, lengths, limits, wait intervals, "
               "placeholder identifiers) are not counted: they are far more numerous than the amounts and would swamp the register, "
               "so a reader looking for them must read the source. *Hard-coded date* covers `VALUE` clauses and quoted literals in the forms `YYYY-MM-DD`, "
               "`YYYY-MM-DD-HH.MM.SS`, `YYYY/MM/DD` and `MM/DD/YYYY` only.\n")
    out.append("## Counts per construct\n")
    out.append(md_table(["Construct", "Occurrences", "Programs / copybooks"],
                        [(c, fmt_int(n), fmt_int(len({r['path'] for r in rows if r['construct'] == c}))) for c, n in s["construct_counts"].items()]
                        + [("**Total**", f"**{fmt_int(len(rows))}**", "")]) + "\n")
    out.append("## Counts per program / copybook\n")
    per_path = OrderedDict()
    for r in rows:
        per_path.setdefault(r["path"], Counter())[r["construct"]] += 1
    prow = []
    for path, cnt in sorted(per_path.items(), key=lambda kv: (-sum(kv[1].values()), kv[0])):
        prow.append((f"`{path}`", fmt_int(sum(cnt.values())), ", ".join(f"{c}×{n}" for c, n in sorted(cnt.items(), key=lambda kv: (-kv[1], kv[0])))))
    out.append(md_table(["Path", "Occurrences", "Breakdown"], prow) + "\n")
    out.append("## Occurrences by construct, then by program\n")
    for construct, n in s["construct_counts"].items():
        out.append(f"### {construct} ({fmt_int(n)})\n")
        by_path = OrderedDict()
        for r in rows:
            if r["construct"] == construct:
                by_path.setdefault(r["path"], []).append(r)
        for path, items in by_path.items():
            out.append(f"#### `{path}` ({fmt_int(len(items))})\n")
            out.append(md_table(["Line", "Detail", "Source text"],
                                [(cite(path, r["line"]), r.get("detail", ""), f"`{r.get('snippet', '')}`" if r.get("snippet") else "") for r in items]) + "\n")
    return "\n".join(out)


def render_readme(estate: Estate, s: dict) -> str:
    h = s["headline"]
    out = ["# Estate discovery dossier\n"]
    out.append(f"**Headline: the generator resolved {fmt_int(h['resolved_edges'])} distinct dependency edges and could not resolve "
               f"{fmt_int(h['unresolved_edges'])}** (categories: program→copybook, program→dataset, JCL step→program, transaction→program, "
               "program→program; distinct per category/from/to). Every number in this folder is computed by `build_discovery.py` from the "
               "source under `app/`; none is hand-typed.\n")
    out.append("## What is in this folder\n")
    out.append(md_table(["File", "Contents", "Produced by"], [
        ("`build_discovery.py`", "Standard-library Python 3 generator: parses the source trees, links dependencies, writes every file below and refreshes the marked blocks in the two authored files", "—"),
        ("`inventory.json`", "Machine-readable source of truth: artifacts, edges, datasets, CSD entries, scheduler jobs, construct occurrences, summary", "generator"),
        ("`01-inventory.md`", "Counts by artifact type, the full inventory table, and what could not be resolved", "generator"),
        ("`02-dependency-map.md`", "Batch and online dependency chains, Mermaid graph plus plain tables, reverse dataset view, orphans", "generator"),
        ("`03-conversion-construct-register.md`", "Every occurrence of conversion-relevant constructs with `path:line`, counted per construct and per program", "generator"),
        (f"`{LINEAGE_FILE}`", "Two worked field lineages (money and date), hop by hop, each hop Confirmed or Inferred", "authored; hop counts refreshed by generator"),
        (f"`{DECISIONS_FILE}`", "Numbered decisions only the system owner can make, with evidence and impact if undecided", "authored; count refreshed by generator"),
        ("`../../tests/test_discovery.py`", "Tests: JSON↔Markdown count agreement both ways, control totals, stale-file detection, real dependency edges", "—"),
    ]) + "\n")
    out.append("## How to regenerate and check\n")
    out.append("```bash\npython3 docs/discovery/build_discovery.py          # rewrite every generated file\n"
               "python3 docs/discovery/build_discovery.py --check  # exit 1 if any generated file is stale\n"
               "python3 -m pytest tests/test_discovery.py\n```\n")
    out.append("## Numbers\n")
    rows = [("Dependency edges resolved (headline)", fmt_int(h["resolved_edges"])),
            ("Dependency edges unresolved (headline)", fmt_int(h["unresolved_edges"]))]
    for c, v in h["by_category"].items():
        rows.append((f"  {c}", f"{fmt_int(v['resolved'])} resolved / {fmt_int(v['unresolved'])} unresolved"))
    rows.append(("Artifacts (total)", fmt_int(s["artifact_total"])))
    for t, n in s["artifact_counts"].items():
        rows.append((f"  {TYPE_LABELS.get(t, t)}", fmt_int(n)))
    for k, v in s["cobol_subtypes"].items():
        rows.append((f"  COBOL programs classified {k}", fmt_int(v)))
    rows.append(("Construct-register occurrences", fmt_int(s["construct_total"])))
    rows.append(("Lineage hops", f"{fmt_int(s['lineage']['hops'])} ({fmt_int(s['lineage']['confirmed'])} Confirmed / {fmt_int(s['lineage']['inferred'])} Inferred)"))
    for name, v in s["lineage"]["by_lineage"].items():
        rows.append((f"  {name}", f"{fmt_int(v['hops'])} ({fmt_int(v['Confirmed'])} Confirmed / {fmt_int(v['Inferred'])} Inferred)"))
    rows.append(("Government decisions", fmt_int(s["government_decisions"])))
    rows.append(("Orphans", f"{fmt_int(s['orphans']['total'])} ({fmt_int(s['orphans']['programs'])} programs, "
                            f"{fmt_int(s['orphans']['copybooks'])} copybooks, {fmt_int(s['orphans']['datasets'])} datasets)"))
    rows.append(("Distinct datasets seen", fmt_int(s["dataset_total"])))
    rows.append(("CSD DEFINE statements", fmt_int(s["csd_entry_total"])))
    rows.append(("Scheduler job definitions", fmt_int(s["scheduler_job_total"])))
    out.append(md_table(["Measure", "Value"], rows) + "\n")
    out.append("## Confirmed / Inferred convention\n")
    out.append("- **Confirmed** — the statement is visible in source at the cited `path:line`; a reader can open the file and see it.\n"
               "- **Inferred** — derived from source but not directly visible (for example, run-time behaviour of a utility, the value a "
               "variable holds when a statement executes, or whether an orphan is dead). Inferred items need a runtime trace or a "
               "subject-matter expert before they are treated as fact.\n"
               "- Generated tables carry the citation of the statement they were derived from; `unresolved` in a generated table means "
               "the generator found the reference but could not bind it to a source artifact.\n")
    out.append("## Limits of this analysis\n")
    out.append("- **Static analysis only.** No program was executed, no runtime trace was taken, no CICS region or batch scheduler was "
               "queried. Dynamic `CALL`/`XCTL` targets are resolved only when a literal can be followed through the `MOVE`/`VALUE` "
               "definitions that reach the statement (same paragraph, or its `PERFORM`/`GO TO`/fall-through entry points) or through "
               "the menu copybooks; a value-dependent branch is not evaluated, so a resolved dynamic target is a static over-approximation.\n"
               "- **No production data.** Only the sample data files committed in `app/data` were inventoried, by name; their contents were not read.\n"
               "- **Sample repository, not a customer estate.** The source is a public sample application. Dataset qualifiers, transaction ids and "
               "program names are reproduced verbatim as identifiers; they are evidence, not endorsements.\n"
               "- **Heuristic parsing.** The COBOL, JCL, CSD, BMS and scheduler readers are purpose-built and lexical, not full compilers. "
               "Utility control statements (IDCAMS, SORT) are read only for dataset names. Where a rule was applied (for example, the "
               "`online`/`batch`/`utility` classification) it is stated next to the table it produced.\n"
               "- **Decision-neutral.** The dossier records what the code does and what only the system owner can decide. It does not "
               "recommend a target of any kind.\n")
    return "\n".join(out)


def replace_marked_block(text: str, tag: str, body: str) -> str:
    begin = MARK_BEGIN.format(tag=tag)
    end = MARK_END.format(tag=tag)
    pattern = re.compile(re.escape(begin) + r".*?" + re.escape(end), re.S)
    replacement = f"{begin}\n{body.strip()}\n{end}"
    if not pattern.search(text):
        raise SystemExit(f"marker block '{tag}' not found in authored file")
    return pattern.sub(lambda _m: replacement, text)


def lineage_summary_block(s: dict) -> str:
    L = s["lineage"]
    rows = [(name, fmt_int(v["hops"]), fmt_int(v["Confirmed"]), fmt_int(v["Inferred"])) for name, v in L["by_lineage"].items()]
    rows.append(("**Total**", f"**{fmt_int(L['hops'])}**", f"**{fmt_int(L['confirmed'])}**", f"**{fmt_int(L['inferred'])}**"))
    return md_table(["Lineage", "Hops", "Confirmed", "Inferred"], rows)


def decisions_summary_block(s: dict) -> str:
    return f"**{fmt_int(s['government_decisions'])} decisions** are listed below; none is made here."


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def generate() -> dict[str, str]:
    estate = Estate(ROOT)
    estate.discover()
    estate.build()
    s = summarize(estate)
    files = OrderedDict()
    files["inventory.json"] = json.dumps(serialize(estate, s), indent=1, ensure_ascii=False, default=_json_default) + "\n"
    files["01-inventory.md"] = render_inventory(estate, s)
    files["02-dependency-map.md"] = render_dependency_map(estate, s)
    files["03-conversion-construct-register.md"] = render_constructs(estate, s)
    files["README.md"] = render_readme(estate, s)
    for name, tag, body in ((LINEAGE_FILE, "lineage-summary", lineage_summary_block(s)),
                            (DECISIONS_FILE, "decision-count", decisions_summary_block(s))):
        p = OUT_DIR / name
        if p.exists():
            files[name] = replace_marked_block(p.read_text(encoding="utf-8"), tag, body)
    return files


def _json_default(o):
    if isinstance(o, set):
        return sorted(o)
    raise TypeError(f"not serialisable: {type(o)}")


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description="Regenerate the estate discovery dossier from source.")
    ap.add_argument("--check", action="store_true", help="exit 1 if any generated file differs from what the source produces")
    args = ap.parse_args(argv)
    files = generate()
    stale = []
    for name, content in files.items():
        p = OUT_DIR / name
        current = p.read_text(encoding="utf-8") if p.exists() else None
        if current != content:
            stale.append(name)
            if not args.check:
                p.write_text(content, encoding="utf-8")
    summary = json.loads(files["inventory.json"])["summary"]
    h = summary["headline"]
    if args.check:
        if stale:
            print("STALE: " + ", ".join(stale))
            print("run: python3 docs/discovery/build_discovery.py")
            return 1
        print(f"OK: {len(files)} generated files are current "
              f"(edges resolved {h['resolved_edges']}, unresolved {h['unresolved_edges']}; artifacts {summary['artifact_total']})")
        return 0
    print(f"wrote {len(stale)} file(s), {len(files) - len(stale)} unchanged, under {OUT_DIR.relative_to(ROOT)}")
    print(f"artifacts {summary['artifact_total']}; dependency edges resolved {h['resolved_edges']}, unresolved {h['unresolved_edges']}; "
          f"constructs {summary['construct_total']}; orphans {summary['orphans']['total']}; "
          f"lineage hops {summary['lineage']['hops']} ({summary['lineage']['confirmed']} Confirmed / {summary['lineage']['inferred']} Inferred); "
          f"decisions {summary['government_decisions']}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
