"""COBOL copybook / record-layout parser (standard library only).

Parses fixed-format COBOL data descriptions (copybooks or the DATA DIVISION
of a program) into a flat field layout:

    level numbers, names / FILLER, PIC X / 9 / S9 / V (implied decimal),
    USAGE DISPLAY / COMP-3 (PACKED-DECIMAL) / COMP / BINARY,
    OCCURS n TIMES, REDEFINES, 88-level conditions (skipped).

Every leaf field becomes a ``Field`` with ``name, offset, length, type,
scale, sign, occurs`` (plus ``digits``, ``usage``, ``redefines``,
``path``).  Group items keep their children so ``records.py`` can emit
OCCURS groups as JSON arrays.

Usage::

    from copybook import parse_file, layout
    fields = parse_file('app/cpy/CVACT01Y.cpy')           # all 01 levels
    rec    = layout(fields, 'ACCOUNT-RECORD')              # one record
    print(rec.length)                                      # 300
    for f in rec.leaves(): print(f.name, f.offset, f.length, f.type)

Command line::

    python3 copybook.py app/cpy/CVACT01Y.cpy [RECORD-NAME]
"""
from __future__ import annotations

import json
import re
import sys
from dataclasses import dataclass, field as dc_field
from typing import Iterator, List, Optional

LEVEL_RE = re.compile(r"^\d{1,2}$")
PIC_REPEAT_RE = re.compile(r"([A-Z9SVPZ*+\-$,./B0])\((\d+)\)", re.I)


class CopybookError(ValueError):
    pass


@dataclass
class Field:
    level: int
    name: str                       # data-name or FILLER
    pic: Optional[str] = None       # normalised PIC string, None for groups
    usage: str = "DISPLAY"          # DISPLAY | COMP-3 | COMP
    occurs: int = 1                 # OCCURS n TIMES (1 = scalar)
    redefines: Optional[str] = None
    offset: int = 0                 # byte offset within the 01 record
    length: int = 0                 # byte length of ONE occurrence
    type: str = "group"             # group | alnum | numeric
    digits: int = 0                 # total 9s (numeric)
    scale: int = 0                  # digits after the implied V
    sign: bool = False              # PIC S...
    children: List["Field"] = dc_field(default_factory=list)
    path: str = ""                  # qualified name A.B.C

    # --- helpers -------------------------------------------------------
    @property
    def is_group(self) -> bool:
        return self.type == "group"

    @property
    def is_filler(self) -> bool:
        return self.name == "FILLER"

    @property
    def total_length(self) -> int:
        return self.length * self.occurs

    def leaves(self, include_redefines: bool = False) -> Iterator["Field"]:
        """Yield elementary fields in storage order (one entry per OCCURS
        group, not per occurrence)."""
        if self.redefines and not include_redefines:
            return
        if self.is_group:
            for c in self.children:
                yield from c.leaves(include_redefines)
        else:
            yield self

    def find(self, name: str) -> Optional["Field"]:
        if self.name == name:
            return self
        for c in self.children:
            hit = c.find(name)
            if hit:
                return hit
        return None

    def to_dict(self) -> dict:
        d = {
            "level": self.level, "name": self.name, "offset": self.offset,
            "length": self.length, "type": self.type, "usage": self.usage,
            "pic": self.pic, "digits": self.digits, "scale": self.scale,
            "sign": self.sign, "occurs": self.occurs,
            "redefines": self.redefines, "path": self.path,
        }
        if self.children:
            d["children"] = [c.to_dict() for c in self.children]
        return d


# ----------------------------------------------------------------------
# Source reading
# ----------------------------------------------------------------------
def _code_lines(text: str) -> Iterator[str]:
    """Strip sequence area (cols 1-6), indicator (col 7) and cols 73+."""
    for raw in text.splitlines():
        line = raw.rstrip("\r\n")
        if len(line) >= 7:
            indicator = line[6]
            body = line[7:72]
        else:
            indicator = " "
            body = ""
        if indicator in "*/":
            continue
        if indicator == "-":
            # continuation lines are not used by CardDemo copybooks
            raise CopybookError("continuation lines are not supported: %r" % raw)
        yield body


def _statements(text: str) -> Iterator[List[str]]:
    """Yield token lists for each level-number statement (ends with '.')."""
    tokens: List[str] = []
    collecting = False
    for body in _code_lines(text):
        for pos, tok in enumerate(body.split()):
            if not collecting:
                # a data description starts with a level number that is
                # the first token of its line (avoids "FROM 10 TO 80 ...")
                if pos == 0 and LEVEL_RE.match(tok):
                    collecting = True
                    tokens = [tok]
                continue
            tokens.append(tok)
            if tok.endswith(".") and not _inside_quote(tokens):
                tokens[-1] = tok[:-1]
                if tokens[-1] == "":
                    tokens.pop()
                yield tokens
                tokens = []
                collecting = False
    if collecting and tokens:
        yield tokens


def _inside_quote(tokens: List[str]) -> bool:
    joined = " ".join(tokens)
    return (joined.count("'") % 2 == 1) or (joined.count('"') % 2 == 1)


# ----------------------------------------------------------------------
# PIC handling
# ----------------------------------------------------------------------
def expand_pic(pic: str) -> str:
    return PIC_REPEAT_RE.sub(lambda m: m.group(1) * int(m.group(2)), pic)


def describe_pic(pic: str, usage: str):
    """Return (type, length_bytes, digits, scale, sign) for a PIC/USAGE."""
    p = expand_pic(pic.upper())
    sign = p.startswith("S")
    if sign:
        p = p[1:]
    if "X" in p or "A" in p:
        if usage != "DISPLAY":
            raise CopybookError("alphanumeric PIC with usage %s" % usage)
        return "alnum", len(p), 0, 0, False
    if "V" in p:
        before, after = p.split("V", 1)
    else:
        before, after = p, ""
    if set(before + after) - set("9"):
        # edited pictures (Z, -, etc.) are output-only; treat as alnum text
        return "alnum", len(p.replace("V", "")), 0, 0, False
    digits = len(before) + len(after)
    scale = len(after)
    if usage == "DISPLAY":
        length = digits
    elif usage == "COMP-3":
        length = digits // 2 + 1
    elif usage == "COMP":
        length = 2 if digits <= 4 else 4 if digits <= 9 else 8
    else:
        raise CopybookError("unsupported usage %s" % usage)
    return "numeric", length, digits, scale, sign


# ----------------------------------------------------------------------
# Statement -> Field
# ----------------------------------------------------------------------
_USAGE_WORDS = {
    "COMP-3": "COMP-3", "COMPUTATIONAL-3": "COMP-3", "PACKED-DECIMAL": "COMP-3",
    "COMP": "COMP", "COMPUTATIONAL": "COMP", "BINARY": "COMP",
    "COMP-4": "COMP", "COMPUTATIONAL-4": "COMP", "COMP-5": "COMP",
    "DISPLAY": "DISPLAY",
}


def _field_from_tokens(tokens: List[str]) -> Optional[Field]:
    level = int(tokens[0])
    if level in (66, 88):
        return None
    i = 1
    name = "FILLER"
    if i < len(tokens) and tokens[i].upper() not in (
            "PIC", "PICTURE", "REDEFINES", "OCCURS", "USAGE", "VALUE",
            "COMP-3", "COMP", "BINARY", "DISPLAY") and tokens[i].upper() != "FILLER":
        name = tokens[i].upper()
        i += 1
    elif i < len(tokens) and tokens[i].upper() == "FILLER":
        i += 1
    f = Field(level=level, name=name)
    while i < len(tokens):
        t = tokens[i].upper()
        if t in ("PIC", "PICTURE"):
            i += 1
            if tokens[i].upper() == "IS":
                i += 1
            f.pic = tokens[i].upper()
        elif t == "USAGE":
            i += 1
            if tokens[i].upper() == "IS":
                i += 1
            f.usage = _USAGE_WORDS[tokens[i].upper()]
        elif t in _USAGE_WORDS:
            f.usage = _USAGE_WORDS[t]
        elif t == "OCCURS":
            i += 1
            f.occurs = int(tokens[i])
            # skip TIMES / TO n TIMES DEPENDING ON x / INDEXED BY ...
            while i + 1 < len(tokens) and tokens[i + 1].upper() in (
                    "TIMES", "TO", "DEPENDING", "ON", "INDEXED", "BY",
                    "ASCENDING", "DESCENDING", "KEY", "IS") :
                i += 1
                if tokens[i].upper() in ("TO", "DEPENDING", "INDEXED", "KEY",
                                         "ASCENDING", "DESCENDING"):
                    i += 1  # consume operand
        elif t == "REDEFINES":
            i += 1
            f.redefines = tokens[i].upper()
        elif t == "VALUE" or t == "VALUES":
            # swallow the literal(s) up to the end of the statement
            i = len(tokens)
            break
        elif t in ("SIGN", "LEADING", "TRAILING", "SEPARATE", "CHARACTER",
                   "JUSTIFIED", "JUST", "RIGHT", "SYNC", "SYNCHRONIZED",
                   "BLANK", "WHEN", "ZERO", "ZEROS", "ZEROES", "IS", "GLOBAL",
                   "EXTERNAL"):
            if t in ("SEPARATE",):
                raise CopybookError("SIGN SEPARATE is not supported: %s" % " ".join(tokens))
        else:
            raise CopybookError("unexpected token %r in %s" % (tokens[i], " ".join(tokens)))
        i += 1
    if f.pic is not None:
        f.type, f.length, f.digits, f.scale, f.sign = describe_pic(f.pic, f.usage)
    return f


# ----------------------------------------------------------------------
# Tree building and offset assignment
# ----------------------------------------------------------------------
def parse_text(text: str) -> List[Field]:
    """Parse COBOL source text and return the list of 01/77-level items."""
    flat = [f for f in (_field_from_tokens(t) for t in _statements(text)) if f]
    roots: List[Field] = []
    stack: List[Field] = []
    for f in flat:
        if f.level in (1, 77):
            roots.append(f)
            stack = [f]
            continue
        while stack and stack[-1].level >= f.level:
            stack.pop()
        if not stack:
            raise CopybookError("level %02d %s has no parent" % (f.level, f.name))
        stack[-1].children.append(f)
        stack.append(f)
    for r in roots:
        _assign_offsets(r, 0, r.name)
    return roots


def _assign_offsets(f: Field, offset: int, path: str) -> int:
    """Assign offsets depth first; return the length of ONE occurrence."""
    f.offset = offset
    f.path = path
    if not f.children:
        if f.pic is None:
            raise CopybookError("elementary item %s has no PIC" % f.name)
        return f.length
    f.type = "group"
    cur = offset
    by_name = {}
    for c in f.children:
        if c.redefines:
            target = by_name.get(c.redefines)
            if target is None:
                raise CopybookError("%s REDEFINES unknown %s" % (c.name, c.redefines))
            _assign_offsets(c, target.offset, path + "." + c.name)
        else:
            ln = _assign_offsets(c, cur, path + "." + c.name)
            cur += ln * c.occurs
        by_name[c.name] = c
    f.length = cur - offset
    return f.length


def parse_file(path: str, encoding: str = "latin-1") -> List[Field]:
    with open(path, "r", encoding=encoding) as fh:
        return parse_text(fh.read())


def layout(fields: List[Field], record_name: Optional[str] = None) -> Field:
    """Pick one 01-level record from ``parse_*`` output (default: first)."""
    if record_name is None:
        if len(fields) != 1:
            raise CopybookError("several 01 levels; specify record_name: %s"
                                % [f.name for f in fields])
        return fields[0]
    for f in fields:
        if f.name == record_name.upper():
            return f
    raise CopybookError("record %s not found in %s" % (record_name, [f.name for f in fields]))


def load_layout(path: str, record_name: Optional[str] = None) -> Field:
    return layout(parse_file(path), record_name)


def flat_layout(rec: Field) -> List[dict]:
    """Flat table of elementary fields (one row per OCCURS occurrence)."""
    rows: List[dict] = []

    def walk(f: Field, base: int, prefix: str):
        for idx in range(f.occurs):
            off = base + idx * f.length
            sfx = "(%d)" % (idx + 1) if f.occurs > 1 else ""
            if f.is_group:
                for c in f.children:
                    if c.redefines:
                        continue
                    walk(c, off + (c.offset - f.offset), prefix + f.name + sfx + ".")
            else:
                rows.append({
                    "name": prefix + f.name + sfx, "offset": off, "length": f.length,
                    "type": f.type, "usage": f.usage, "pic": f.pic, "digits": f.digits,
                    "scale": f.scale, "sign": f.sign, "occurs": f.occurs,
                })

    for c in rec.children or [rec]:
        if c is rec:
            walk(c, 0, "")
        elif not c.redefines:
            walk(c, c.offset, "")
    return rows


def main(argv: List[str]) -> int:
    if len(argv) < 2:
        print(__doc__)
        return 2
    roots = parse_file(argv[1])
    rec = layout(roots, argv[2] if len(argv) > 2 else None) if len(argv) > 2 or len(roots) == 1 else None
    if rec is None:
        for r in roots:
            print("%-30s length %d" % (r.name, r.length))
        return 0
    print(json.dumps({"record": rec.name, "length": rec.length,
                      "fields": flat_layout(rec)}, indent=2))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
