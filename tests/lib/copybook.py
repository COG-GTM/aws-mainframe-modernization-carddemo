"""Minimal COBOL data-description parser used by the test suite.

Parses fixed-format COBOL data entries (copybooks or the DATA DIVISION of a
program) into a tree of fields with byte offsets/lengths computed the way
IBM Enterprise COBOL lays out storage (DISPLAY = 1 byte per digit/char,
COMP/BINARY = 2/4/8, COMP-3 = (digits/2)+1, POINTER/INDEX = 4).
"""
import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import List, Optional

_STRING_RE = re.compile(r"'[^']*'|\"[^\"]*\"")
_SENTENCE_SPLIT_RE = re.compile(r"\.(?=\s|$)")


@dataclass(eq=False)
class Field:
    level: int
    name: str
    pic: Optional[str] = None
    usage: str = "DISPLAY"
    occurs: int = 1
    occurs_max: Optional[int] = None
    depending_on: Optional[str] = None
    redefines: Optional[str] = None
    sign_separate: bool = False
    children: List["Field"] = field(default_factory=list)
    parent: Optional["Field"] = None
    offset: int = 0
    line: int = 0

    @property
    def is_group(self):
        return self.pic is None and not self.children == [] or (
            self.pic is None and self.usage in ("DISPLAY",) and self.children
        )

    @property
    def elementary_size(self):
        return pic_storage_size(self.pic, self.usage, self.sign_separate)

    @property
    def size(self):
        """Storage size of ONE occurrence."""
        if self.children:
            total = 0
            for child in self.children:
                if child.redefines is None:
                    total += child.size * child.occurs
            return total
        if self.pic is None:
            return usage_size_without_pic(self.usage)
        return self.elementary_size

    @property
    def total_size(self):
        return self.size * self.occurs

    def walk(self):
        yield self
        for child in self.children:
            yield from child.walk()

    def find(self, name):
        for f in self.walk():
            if f.name == name:
                return f
        return None


def usage_size_without_pic(usage):
    return {"POINTER": 4, "INDEX": 4, "COMP-1": 4, "COMP-2": 8,
            "PROCEDURE-POINTER": 8, "FUNCTION-POINTER": 4}.get(usage, 0)


def expand_pic(pic):
    """Expand repetition factors: 9(3)V99 -> 999V99."""
    return re.sub(r"(.)\((\d+)\)", lambda m: m.group(1) * int(m.group(2)), pic.upper())


def pic_digits(pic):
    p = expand_pic(pic)
    return p.count("9")


def pic_storage_size(pic, usage="DISPLAY", sign_separate=False):
    if pic is None:
        return usage_size_without_pic(usage)
    p = expand_pic(pic)
    if usage in ("COMP", "COMP-4", "BINARY", "COMP-5"):
        d = pic_digits(pic)
        return 2 if d <= 4 else 4 if d <= 9 else 8
    if usage in ("COMP-3", "PACKED-DECIMAL"):
        return pic_digits(pic) // 2 + 1
    if usage in ("COMP-1",):
        return 4
    if usage in ("COMP-2",):
        return 8
    size = 0
    i = 0
    while i < len(p):
        two = p[i:i + 2]
        if two in ("CR", "DB"):
            size += 2
            i += 2
            continue
        c = p[i]
        if c in "SVP":
            pass
        else:
            size += 1
        i += 1
    if sign_separate and "S" in p:
        size += 1
    return size


def _logical_lines(text):
    """Yield (lineno, area_a_b_text) for fixed-format source, merging
    continuation lines and dropping comments / sequence areas."""
    out = []
    for no, raw in enumerate(text.splitlines(), 1):
        raw = raw.rstrip("\n\r").expandtabs(4)
        if len(raw) < 7:
            continue
        indicator = raw[6]
        body = raw[7:72]
        if indicator in "*/":
            continue
        if indicator == "-" and out:
            stripped = body.lstrip()
            if stripped[:1] in ("'", '"'):
                stripped = stripped[1:]
            prev_no, prev = out[-1]
            out[-1] = (prev_no, prev.rstrip() + stripped)
            continue
        if indicator.upper() == "D":
            continue
        out.append((no, body))
    return out


def sentences(text):
    """Split COBOL source into (lineno, sentence) with literals removed."""
    buf = []
    for no, body in _logical_lines(text):
        buf.append((no, _STRING_RE.sub("''", body)))
    result = []
    current = ""
    start = None
    for no, body in buf:
        if start is None and body.strip():
            start = no
        current += " " + body
        parts = _SENTENCE_SPLIT_RE.split(current)
        for done in parts[:-1]:
            if done.strip():
                result.append((start, " ".join(done.split())))
            start = no
        current = parts[-1]
        if not current.strip():
            start = None
    if current.strip():
        result.append((start, " ".join(current.split())))
    return result


_LEVEL_RE = re.compile(r"^(\d{1,2})\s+(.*)$")


def parse_entry(sentence):
    m = _LEVEL_RE.match(sentence)
    if not m:
        return None
    level = int(m.group(1))
    rest = m.group(2).split()
    name = "FILLER"
    if rest and not rest[0].upper() in ("PIC", "PICTURE", "REDEFINES", "OCCURS",
                                         "VALUE", "VALUES", "COMP", "COMP-3",
                                         "USAGE", "BINARY"):
        name = rest[0].upper()
        rest = rest[1:]
    f = Field(level=level, name=name)
    toks = [t.upper() for t in rest]
    i = 0
    while i < len(toks):
        t = toks[i]
        if t in ("PIC", "PICTURE"):
            i += 1
            if i < len(toks) and toks[i] == "IS":
                i += 1
            f.pic = rest[i].upper()
        elif t == "REDEFINES":
            i += 1
            f.redefines = toks[i]
        elif t == "OCCURS":
            i += 1
            f.occurs = int(toks[i])
            if i + 2 < len(toks) and toks[i + 1] == "TO":
                f.occurs_max = int(toks[i + 2])
                i += 2
        elif t == "DEPENDING":
            i += 1
            if toks[i] == "ON":
                i += 1
            f.depending_on = toks[i]
        elif t in ("COMP", "COMPUTATIONAL", "COMP-4", "COMPUTATIONAL-4", "BINARY"):
            f.usage = "COMP"
        elif t in ("COMP-5", "COMPUTATIONAL-5"):
            f.usage = "COMP-5"
        elif t in ("COMP-3", "COMPUTATIONAL-3", "PACKED-DECIMAL"):
            f.usage = "COMP-3"
        elif t in ("COMP-1", "COMPUTATIONAL-1"):
            f.usage = "COMP-1"
        elif t in ("COMP-2", "COMPUTATIONAL-2"):
            f.usage = "COMP-2"
        elif t in ("POINTER", "INDEX"):
            f.usage = t
        elif t == "SEPARATE":
            f.sign_separate = True
        elif t in ("VALUE", "VALUES"):
            break
        i += 1
    if f.occurs_max:
        # Variable-length tables occupy their maximum size.
        f.occurs = f.occurs_max
    return f


def parse_records(text):
    """Return a list of top-level (level 01/77, or the shallowest level found)
    Field trees parsed from the given COBOL text."""
    roots = []
    stack = []
    for lineno, sent in sentences(text):
        f = parse_entry(sent)
        if f is None:
            continue
        f.line = lineno
        if f.level in (66, 88):
            continue
        if f.level == 77:
            f.level = 1
        while stack and stack[-1].level >= f.level:
            stack.pop()
        if stack:
            f.parent = stack[-1]
            stack[-1].children.append(f)
        else:
            roots.append(f)
        stack.append(f)
    for r in roots:
        assign_offsets(r, 0)
    return roots


def assign_offsets(f, offset):
    f.offset = offset
    pos = offset
    by_name = {}
    for child in f.children:
        if child.redefines:
            target = by_name.get(child.redefines)
            start = target.offset if target else pos
            assign_offsets(child, start)
        else:
            assign_offsets(child, pos)
            pos += child.size * child.occurs
        by_name[child.name] = child


def parse_file(path):
    return parse_records(Path(path).read_text(errors="replace"))


_COPY_RE = re.compile(r"(?im)^(.{6}) +COPY +([A-Z0-9-]+)\.?.*$")


def expand_copies(text, cpy_dirs):
    """Inline COPY members (no REPLACING support; one level deep is
    enough for the batch programs)."""
    def repl(m):
        for d in cpy_dirs:
            for p in Path(d).iterdir():
                if p.stem.upper() == m.group(2).upper():
                    return p.read_text(errors="replace")
        return m.group(0)
    return _COPY_RE.sub(repl, text)


def data_division(text):
    """Extract FILE/WORKING-STORAGE/LINKAGE text from a full program."""
    up = text.upper()
    start = up.find("DATA DIVISION")
    end = up.find("PROCEDURE DIVISION")
    if start < 0:
        return ""
    # Back up to start of line so fixed-format columns stay intact.
    start = text.rfind("\n", 0, start) + 1
    end = text.rfind("\n", 0, end) + 1 if end > 0 else len(text)
    return text[start:end]


def leaf_fields(record):
    """Elementary, non-redefining fields of a record (first occurrence)."""
    out = []

    def rec(f, in_redef):
        if f.redefines:
            in_redef = True
        if f.children:
            for c in f.children:
                rec(c, in_redef)
        elif not in_redef:
            out.append(f)
    rec(record, False)
    return out
