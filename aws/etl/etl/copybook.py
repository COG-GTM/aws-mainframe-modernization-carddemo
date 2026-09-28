"""Minimal COBOL copybook parser: levels, PIC, USAGE (DISPLAY/COMP/COMP-3), OCCURS, REDEFINES.

The parser computes byte offsets exactly as the COBOL compiler would for these copybooks (no SYNC,
no SIGN SEPARATE, no OCCURS DEPENDING ON) and exposes every elementary item as a flat ``Leaf``
addressable by name (``NAME`` or ``NAME(i)`` inside OCCURS, 1-based).
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from pathlib import Path

from etl import codec

_USAGE_ALIASES = {
    "COMP": "COMP",
    "COMPUTATIONAL": "COMP",
    "COMP-4": "COMP",
    "COMPUTATIONAL-4": "COMP",
    "COMP-5": "COMP",
    "COMPUTATIONAL-5": "COMP",
    "BINARY": "COMP",
    "COMP-3": "COMP-3",
    "COMPUTATIONAL-3": "COMP-3",
    "PACKED-DECIMAL": "COMP-3",
    "DISPLAY": "DISPLAY",
}


@dataclass(frozen=True)
class Picture:
    kind: str  # "X" (alphanumeric) or "9" (numeric)
    digits: int  # character count for X, digit count for 9
    scale: int = 0
    signed: bool = False

    @staticmethod
    def parse(pic: str) -> "Picture":
        text = pic.upper()
        expanded = re.sub(r"(.)\((\d+)\)", lambda m: m.group(1) * int(m.group(2)), text)
        if set(expanded) <= {"X", "A"}:
            return Picture("X", len(expanded))
        signed = expanded.startswith("S")
        body = expanded[1:] if signed else expanded
        if not set(body) <= {"9", "V"} or body.count("V") > 1:
            raise ValueError(f"unsupported PIC {pic}")
        int_part, _, frac = body.partition("V")
        return Picture("9", len(int_part) + len(frac), len(frac), signed)


@dataclass
class Item:
    level: int
    name: str
    pic: Picture | None = None
    usage: str = "DISPLAY"
    occurs: int = 1
    redefines: str | None = None
    children: list["Item"] = field(default_factory=list)
    offset: int = 0
    size: int = 0  # size of ONE occurrence

    @property
    def is_group(self) -> bool:
        return self.pic is None


@dataclass(frozen=True)
class Leaf:
    name: str
    offset: int
    length: int
    pic: Picture
    usage: str
    groups: tuple[str, ...]  # enclosing group names, outermost first

    def decode(self, record: bytes, blank_as_none: bool = False):
        raw = record[self.offset : self.offset + self.length]
        if len(raw) != self.length:
            raise codec.DecodeError(f"{self.name}: record too short")
        if self.pic.kind == "X":
            return codec.decode_alnum(raw)
        if blank_as_none and codec.is_blank(raw) and self.usage == "DISPLAY":
            return None
        try:
            if self.usage == "COMP-3":
                return codec.decode_packed(raw, self.pic.scale, self.pic.signed)
            if self.usage == "COMP":
                return codec.decode_binary(raw, self.pic.scale, self.pic.signed)
            return codec.decode_zoned(raw, self.pic.scale, self.pic.signed)
        except codec.DecodeError as exc:
            raise codec.DecodeError(f"{self.name} @{self.offset}: {exc}") from exc


def _storage_size(pic: Picture, usage: str) -> int:
    if pic.kind == "X" or usage == "DISPLAY":
        return pic.digits
    if usage == "COMP-3":
        return pic.digits // 2 + 1
    if pic.digits <= 4:
        return 2
    if pic.digits <= 9:
        return 4
    if pic.digits <= 18:
        return 8
    raise ValueError(f"binary field too large: {pic}")


def _source_text(path: Path) -> str:
    """Fixed-format source: columns 7-72, '*' or '/' in column 7 = comment."""
    parts = []
    for line in path.read_text(encoding="utf-8", errors="replace").splitlines():
        if len(line) < 7 or line[6] in "*/":
            continue
        parts.append(line[7:72])
    return " ".join(parts)


def _statements(text: str) -> list[list[str]]:
    """Split into period-terminated statements, keeping quoted literals intact."""
    statements, tokens, buf, quote = [], [], [], None
    for i, ch in enumerate(text):
        if quote:
            buf.append(ch)
            if ch == quote:
                quote = None
            continue
        if ch in "'\"":
            quote = ch
            buf.append(ch)
        elif ch.isspace():
            if buf:
                tokens.append("".join(buf))
                buf = []
        elif ch == "." and (i + 1 == len(text) or text[i + 1].isspace()):
            if buf:
                tokens.append("".join(buf))
                buf = []
            if tokens:
                statements.append(tokens)
                tokens = []
        else:
            buf.append(ch)
    if buf:
        tokens.append("".join(buf))
    if tokens:
        statements.append(tokens)
    return statements


def _parse_item(tokens: list[str]) -> Item | None:
    level = int(tokens[0])
    if level in (66, 88):
        return None
    rest = tokens[1:]
    name = "FILLER"
    if rest and rest[0].upper() not in {"PIC", "PICTURE", "REDEFINES", "OCCURS", "USAGE", "VALUE"} | set(
        _USAGE_ALIASES
    ):
        name = rest.pop(0).upper()
    item = Item(level, name)
    i = 0
    while i < len(rest):
        tok = rest[i].upper()
        if tok in ("PIC", "PICTURE"):
            i += 1
            if rest[i].upper() == "IS":
                i += 1
            item.pic = Picture.parse(rest[i])
        elif tok == "REDEFINES":
            i += 1
            item.redefines = rest[i].upper()
        elif tok == "OCCURS":
            i += 1
            item.occurs = int(rest[i])
            if i + 1 < len(rest) and rest[i + 1].upper() == "TIMES":
                i += 1
        elif tok == "USAGE":
            i += 1
            if rest[i].upper() == "IS":
                i += 1
            item.usage = _USAGE_ALIASES[rest[i].upper()]
        elif tok in _USAGE_ALIASES:
            item.usage = _USAGE_ALIASES[tok]
        elif tok == "VALUE":
            break
        i += 1
    return item


def _layout(item: Item, offset: int, usage: str) -> None:
    item.offset = offset
    if item.usage == "DISPLAY" and usage != "DISPLAY":
        item.usage = usage  # group-level USAGE is inherited
    if not item.is_group:
        item.size = _storage_size(item.pic, item.usage)
        return
    pos = offset
    by_name: dict[str, Item] = {}
    end = offset
    for child in item.children:
        if child.redefines:
            target = by_name[child.redefines]
            _layout(child, target.offset, item.usage)
            if child.size * child.occurs > target.size * target.occurs:
                raise ValueError(f"{child.name} is larger than the item it redefines ({target.name})")
        else:
            _layout(child, pos, item.usage)
            pos += child.size * child.occurs
        by_name[child.name] = child
        end = max(end, child.offset + child.size * child.occurs)
    item.size = end - offset


class Copybook:
    def __init__(self, path: str | Path):
        self.path = Path(path)
        root = Item(0, "<ROOT>")
        stack = [root]
        for tokens in _statements(_source_text(self.path)):
            if not tokens[0].isdigit():
                continue
            item = _parse_item(tokens)
            if item is None:
                continue
            while stack[-1].level >= item.level:
                stack.pop()
            stack[-1].children.append(item)
            stack.append(item)
        _layout(root, 0, "DISPLAY")
        self.root = root
        self.record_length = root.size
        self.leaves: dict[str, Leaf] = {}
        self._flatten(root, 0, (), "")

    def _flatten(self, item: Item, base: int, groups: tuple[str, ...], suffix: str) -> None:
        for n in range(item.occurs):
            occ_base = base + n * item.size
            occ_suffix = suffix + (f"({n + 1})" if item.occurs > 1 else "")
            if item.is_group:
                inner = groups + ((item.name,) if item.level else ())
                for child in item.children:
                    self._flatten(child, occ_base + child.offset - item.offset, inner, occ_suffix)
            elif item.name != "FILLER":
                name = item.name + occ_suffix
                if name in self.leaves:
                    raise ValueError(f"duplicate field name {name} in {self.path.name}")
                self.leaves[name] = Leaf(name, occ_base, item.size, item.pic, item.usage, groups)

    def leaf(self, name: str) -> Leaf:
        return self.leaves[name]

    def leaves_under(self, group: str) -> list[Leaf]:
        return [leaf for leaf in self.leaves.values() if group in leaf.groups]
