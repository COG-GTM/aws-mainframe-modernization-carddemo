"""Decode EBCDIC (or ASCII parity) records with a layout and write load-ready CSV."""

from __future__ import annotations

import csv
import functools
import io
from collections.abc import Iterator
from dataclasses import dataclass
from decimal import Decimal
from pathlib import Path

import bcrypt

from etl import codec, ims
from etl import transforms as t
from etl.copybook import Copybook
from etl.layouts import LAYOUTS, SEED_RUN_ID, Layout, Output


@functools.cache
def copybook(path: Path) -> Copybook:
    return Copybook(path)


@dataclass(frozen=True)
class Record:
    output: Output
    data: bytes
    context: dict


def split_fixed(raw: bytes, length: int, name: str) -> list[bytes]:
    if len(raw) % length:
        raise codec.DecodeError(f"{name}: size {len(raw)} is not a multiple of LRECL {length}")
    return [raw[i : i + length] for i in range(0, len(raw), length)]


def read_ascii(path: Path, length: int) -> list[bytes]:
    """Line-sequential ASCII sample -> CP037 fixed records (contract data-model.md §5)."""
    records = []
    for n, line in enumerate(path.read_text(encoding="ascii").split("\n"), start=1):
        line = line.removesuffix("\r")
        if not line:
            continue
        if len(line) > length:
            raise codec.DecodeError(f"{path.name}:{n}: {len(line)} chars exceeds LRECL {length}")
        records.append(codec.text_to_ebcdic(line.ljust(length)))
    return records


def _cb(layout: Layout, output: Output) -> Copybook:
    path = output.copybook or layout.copybook
    assert path is not None
    return copybook(path)


def _output_for(layout: Layout, key: str) -> Output:
    for out in layout.outputs:
        if out.discriminator is None or out.discriminator == key:
            return out
    raise codec.DecodeError(f"{layout.name}: no output for discriminator {key!r}")


def iter_records(layout: Layout, raw: bytes | None = None, records: list[bytes] | None = None) -> Iterator[Record]:
    if layout.reader == "ims":
        parent: dict = {}
        for seg in ims.read_segments(raw if raw is not None else layout.input.read_bytes()):
            out = _output_for(layout, seg.name)
            cb = _cb(layout, out)
            if seg.name == "PAUTSUM0":
                if out.skip_blank and codec.is_blank(_raw(cb, out.skip_blank, seg.data)):
                    parent = {}
                    continue
                parent = {
                    "parent_acct_id": cb.leaf("PA-ACCT-ID").decode(seg.data),
                    "account_status": t.pg_array(
                        [cb.leaf(f"PA-ACCOUNT-STATUS({i})").decode(seg.data) for i in range(1, 6)]
                    ),
                }
                yield Record(out, seg.data, dict(parent))
            else:
                if not parent:
                    raise codec.DecodeError(f"child segment {seg.name} before any root segment")
                yield Record(out, seg.data, dict(parent))
        return

    assert layout.record_length is not None
    if records is None:
        records = split_fixed(raw if raw is not None else layout.input.read_bytes(), layout.record_length, layout.name)
    for seq, data in enumerate(records, start=1):
        if layout.discriminator:
            first = layout.outputs[0]
            key = t.text_nn(_cb(layout, first).leaf(layout.discriminator).decode(data))
            out = _output_for(layout, key)
        else:
            out = layout.outputs[0]
        if out.skip_blank and codec.is_blank(_raw(_cb(layout, out), out.skip_blank, data)):
            continue
        yield Record(out, data, {"seq": seq, "run_id": SEED_RUN_ID})


def _raw(cb: Copybook, name: str, data: bytes) -> bytes:
    leaf = cb.leaf(name)
    return data[leaf.offset : leaf.offset + leaf.length]


def _check_redefine(cb: Copybook, out: Output) -> None:
    if out.redefine_group:
        for col in out.columns:
            if col.source.startswith("@"):
                continue
            groups = cb.leaf(col.source).groups
            if len(groups) > 1 and out.redefine_group not in groups:
                raise ValueError(f"{out.name}.{col.name}: {col.source} is outside {out.redefine_group}")


def row(record: Record, previous_hashes: dict[str, str] | None = None) -> list:
    out = record.output
    cb = _cb_for_output(out)
    values = []
    for col in out.columns:
        if col.source.startswith("@"):
            values.append(record.context[col.source[1:]])
            continue
        raw = cb.leaf(col.source).decode(record.data)
        if col.transform is t.bcrypt_upper and previous_hashes is not None:
            key = values[0]
            old = previous_hashes.get(key)
            plain = t.upper_text(raw).encode("utf-8")
            values.append(old if old and bcrypt.checkpw(plain, old.encode("ascii")) else col.transform(raw))
        else:
            values.append(col.transform(raw))
    return values


_OUTPUT_COPYBOOK: dict[str, Path] = {}


def _cb_for_output(out: Output) -> Copybook:
    return copybook(_OUTPUT_COPYBOOK[out.name])


def _register_copybooks() -> None:
    for layout in LAYOUTS.values():
        for out in layout.outputs:
            path = out.copybook or layout.copybook
            assert path is not None
            _OUTPUT_COPYBOOK[out.name] = path
            _check_redefine(copybook(path), out)


_register_copybooks()


def decode(layout: Layout, raw: bytes | None = None, records: list[bytes] | None = None,
           previous_hashes: dict[str, str] | None = None) -> dict[str, list[list]]:
    """Return {output name: rows} for every output of the layout (empty outputs included)."""
    result: dict[str, list[list]] = {out.name: [] for out in layout.outputs}
    for rec in iter_records(layout, raw, records):
        result[rec.output.name].append(row(rec, previous_hashes))
    return result


def format_value(v) -> str:
    """COPY ... CSV semantics: unquoted empty = NULL, quoted "" = empty string."""
    if v is None:
        return ""
    if isinstance(v, Decimal):
        return format(v, "f")
    s = str(v)
    if s == "" or any(c in s for c in ',"\r\n') or s != s.strip():
        return '"' + s.replace('"', '""') + '"'
    return s


def to_csv(out: Output, rows: list[list]) -> str:
    buf = io.StringIO()
    buf.write(",".join(c.name for c in out.columns) + "\n")
    for r in rows:
        buf.write(",".join(format_value(v) for v in r) + "\n")
    return buf.getvalue()


def read_csv(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as fh:
        return list(csv.DictReader(fh))


def output_path(out_dir: Path, out: Output) -> Path:
    return out_dir / out.subdir / f"{out.name}.csv"


def _previous_hashes(path: Path, out: Output) -> dict[str, str]:
    cols = [c for c in out.columns if c.transform is t.bcrypt_upper]
    if not cols or not path.exists():
        return {}
    key = out.columns[0].name
    return {r[key]: r[cols[0].name] for r in read_csv(path)}


def convert_layout(layout: Layout, input_path: Path, targets: dict[str, Path]) -> dict[str, int]:
    """Decode ``input_path`` and write one CSV per output to ``targets[output name]``."""
    previous: dict[str, str] = {}
    for out in layout.outputs:
        previous.update(_previous_hashes(targets[out.name], out))
    rows = decode(layout, raw=input_path.read_bytes(), previous_hashes=previous)
    counts = {}
    for out in layout.outputs:
        path = targets[out.name]
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(to_csv(out, rows[out.name]), encoding="utf-8", newline="")
        counts[out.name] = len(rows[out.name])
    return counts


def convert_all(out_dir: Path) -> dict[str, int]:
    counts: dict[str, int] = {}
    for layout in LAYOUTS.values():
        for dup in layout.duplicates:
            if dup.read_bytes() != layout.input.read_bytes():
                raise ValueError(f"{dup.name} is no longer identical to {layout.input.name}")
        targets = {out.name: output_path(out_dir, out) for out in layout.outputs}
        counts.update(convert_layout(layout, layout.input, targets))
    return counts


def crosscheck(layout: Layout) -> tuple[int, list[str]]:
    """Compare EBCDIC-decoded rows with rows decoded from the ASCII sample. Returns (rows, diffs)."""
    if layout.ascii is None or layout.record_length is None:
        raise ValueError(f"{layout.name} has no ASCII counterpart")
    ebcdic = decode(layout, raw=layout.input.read_bytes())
    asc = decode(layout, records=read_ascii(layout.ascii, layout.record_length))
    diffs = []
    for out in layout.outputs:
        a, b = ebcdic[out.name], asc[out.name]
        if len(a) != len(b):
            diffs.append(f"{out.name}: {len(a)} EBCDIC rows vs {len(b)} ASCII rows")
        for i, (ra, rb) in enumerate(zip(a, b), start=1):
            for col, va, vb in zip(out.columns, ra, rb):
                if va != vb:
                    diffs.append(f"{out.name} row {i} {col.name}: {va!r} != {vb!r}")
    return sum(len(v) for v in ebcdic.values()), diffs
