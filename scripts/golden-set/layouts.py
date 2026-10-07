"""Record layouts parsed from the copybooks in app/cpy, and the zoned-decimal codec they use.

Only what the golden-set datasets need: flat DISPLAY records (PIC X(n), 9(n), S9(n)V9(m)); group items carry no
storage of their own. Signed fields use the trailing overpunch that GnuCOBOL writes with `-fsign=EBCDIC` and that the
ASCII sample files carry ('{' / 'A'..'I' positive, '}' / 'J'..'R' negative), the same convention as the Java codec.
"""
from __future__ import annotations

import re
from dataclasses import dataclass
from decimal import Decimal
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
CPY = REPO / "app" / "cpy"

# dataset -> (copybook, LRECL, key fields)
DATASETS = {
    "ACCTDATA": ("CVACT01Y", 300, ("ACCT-ID",)),
    "CUSTDATA": ("CVCUS01Y", 500, ("CUST-ID",)),
    "CARDDATA": ("CVACT02Y", 150, ("CARD-NUM",)),
    "CARDXREF": ("CVACT03Y", 50, ("XREF-CARD-NUM",)),
    "TRANSACT": ("CVTRA05Y", 350, ("TRAN-ID",)),
    "TCATBALF": ("CVTRA01Y", 50, ("TRANCAT-ACCT-ID", "TRANCAT-TYPE-CD", "TRANCAT-CD")),
    "USRSEC": ("CSUSR01Y", 80, ("SEC-USR-ID",)),
}

POS = "{ABCDEFGHI"
NEG = "}JKLMNOPQR"


@dataclass(frozen=True)
class Field:
    name: str
    offset: int
    length: int
    numeric: bool
    signed: bool
    scale: int

    def raw(self, rec: str) -> str:
        return rec[self.offset:self.offset + self.length]


def _pic_len(part: str, sym: str) -> int:
    n = 0
    for m in re.finditer(rf"{sym}(?:\((\d+)\))?", part):
        n += int(m.group(1)) if m.group(1) else 1
    return n


def parse_copybook(name: str) -> list[Field]:
    fields, offset = [], 0
    for line in (CPY / f"{name}.cpy").read_text(encoding="latin-1").splitlines():
        text = line[6:72] if len(line) > 6 else ""
        if not text or text[0] == "*":
            continue
        m = re.match(r"\s*(\d\d)\s+([A-Z0-9-]+)(?:\s+PIC\s+(\S+?))?\.?\s*$", text[1:])
        if not m or not m.group(3):
            continue
        pic = m.group(3).rstrip(".")
        signed = pic.startswith("S")
        body = pic[1:] if signed else pic
        if "X" in body:
            fields.append(Field(m.group(2), offset, _pic_len(body, "X"), False, False, 0))
        else:
            ints, _, dec = body.partition("V")
            length = _pic_len(ints, "9") + _pic_len(dec, "9")
            fields.append(Field(m.group(2), offset, length, True, signed, _pic_len(dec, "9")))
        offset += fields[-1].length
    return fields


class Layout:
    def __init__(self, dataset: str):
        self.dataset = dataset
        self.copybook, self.lrecl, self.key_fields = DATASETS[dataset]
        self.fields = parse_copybook(self.copybook)
        size = sum(f.length for f in self.fields)
        if size != self.lrecl:
            raise ValueError(f"{self.copybook}: fields add up to {size}, LRECL is {self.lrecl}")
        self.by_name = {f.name: f for f in self.fields}

    def key(self, rec: str) -> str:
        return "".join(self.by_name[k].raw(rec) for k in self.key_fields)

    def get(self, rec: str, name: str) -> str:
        return self.by_name[name].raw(rec)

    def put(self, rec: str, name: str, value) -> str:
        f = self.by_name[name]
        text = encode(f, value) if f.numeric else str(value)[:f.length].ljust(f.length)
        return rec[:f.offset] + text + rec[f.offset + f.length:]

    def blank(self) -> str:
        return " " * self.lrecl


def decode(f: Field, raw: str) -> Decimal | None:
    """Numeric DISPLAY value, or None when the bytes are not a valid zoned number."""
    if not f.numeric or len(raw) != f.length:
        return None
    digits, sign = raw[:-1], 1
    last = raw[-1]
    if last.isdigit():
        digits += last
    elif f.signed and last in POS:
        digits += str(POS.index(last))
    elif f.signed and last in NEG:
        digits += str(NEG.index(last))
        sign = -1
    else:
        return None
    if not digits.isdigit():
        return None
    return sign * Decimal(int(digits)).scaleb(-f.scale)


def encode(f: Field, value) -> str:
    """MOVE of a numeric value to a DISPLAY field: truncated to the PIC (high-order digits and extra decimals)."""
    v = Decimal(str(value))
    units = int((abs(v) * (10 ** f.scale)).to_integral_value(rounding="ROUND_DOWN")) % (10 ** f.length)
    digits = str(units).zfill(f.length)
    if not f.signed:
        return digits
    return digits[:-1] + (NEG if v < 0 else POS)[int(digits[-1])]


def render(f: Field, raw: str) -> str:
    """Field value as shown in the reports: numbers as decimals, text right-trimmed (quoted only when blank)."""
    if f.numeric:
        d = decode(f, raw)
        if d is not None:
            return f"{d:.{f.scale}f}" if f.scale else str(int(d))
    text = raw.rstrip(" ")
    if not text:
        return "''"
    return text.replace("\x00", "\\0")


def read_records(path: Path, lrecl: int) -> list[str]:
    """Line-sequential file (one record per line, the format of docs/validation/baseline and `unload`)."""
    if not path.exists():
        return []
    lines = path.read_text(encoding="latin-1").split("\n")
    if lines and lines[-1] == "":
        lines.pop()
    out = []
    for ln in lines:
        ln = ln.rstrip("\r")
        if len(ln) > lrecl and ln[lrecl:].strip():
            raise ValueError(f"{path}: line longer than LRECL {lrecl}")
        out.append(ln[:lrecl].ljust(lrecl))
    return out


def write_records(path: Path, recs: list[str]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("".join(r + "\n" for r in recs), encoding="latin-1")
