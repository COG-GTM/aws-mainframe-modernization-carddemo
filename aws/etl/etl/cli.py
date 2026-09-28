"""python -m etl {convert,convert-all,crosscheck,load,layouts}"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

from etl import convert, load
from etl.layouts import LAYOUTS

DEFAULT_OUT = Path(__file__).resolve().parents[1] / "output"
DEFAULT_SCHEMA = Path(__file__).resolve().parents[2] / "db" / "schema.sql"


def _cmd_convert(args: argparse.Namespace) -> int:
    layout = LAYOUTS[args.layout]
    out = Path(args.out)
    if len(layout.outputs) == 1:
        if out.suffix.lower() != ".csv":
            out = out / f"{layout.outputs[0].name}.csv"
        targets = {layout.outputs[0].name: out}
    else:
        # Multi-record layouts (REDEFINES / IMS segments) write one CSV per record type into a directory.
        base = out.parent if out.suffix.lower() == ".csv" else out
        targets = {o.name: base / f"{o.name}.csv" for o in layout.outputs}
    counts = convert.convert_layout(layout, Path(args.input or layout.input), targets)
    for name, n in counts.items():
        print(f"{targets[name]}: {n} rows")
    return 0


def _cmd_convert_all(args: argparse.Namespace) -> int:
    for name, n in convert.convert_all(Path(args.out_dir)).items():
        print(f"{name}: {n}")
    return 0


def _cmd_crosscheck(args: argparse.Namespace) -> int:
    failed = False
    for layout in LAYOUTS.values():
        if layout.ascii is None:
            continue
        n, diffs = convert.crosscheck(layout)
        print(f"{layout.name}: {n} rows, {len(diffs)} differences vs {layout.ascii.name}")
        for d in diffs[:20]:
            print(f"  {d}")
        failed |= bool(diffs)
    return 1 if failed else 0


def _cmd_load(args: argparse.Namespace) -> int:
    schema = Path(args.schema) if args.apply_schema else None
    counts = load.load(Path(args.csv_dir), args.dsn, schema_sql=schema, truncate=not args.no_truncate)
    for tbl, n in counts.items():
        print(f"{load.SCHEMA}.{tbl}: {n}")
    return 0


def _cmd_layouts(_: argparse.Namespace) -> int:
    for layout in LAYOUTS.values():
        outs = ", ".join(o.name + (f"->{o.table}" if o.table else " (csv only)") for o in layout.outputs)
        print(f"{layout.name:14} {layout.input.name:45} {outs}")
    return 0


def main(argv: list[str] | None = None) -> int:
    p = argparse.ArgumentParser(prog="python -m etl", description=__doc__)
    sub = p.add_subparsers(dest="cmd", required=True)

    c = sub.add_parser("convert", help="decode one EBCDIC file with a layout into CSV")
    c.add_argument("--input", help="EBCDIC file (default: the layout's sample file)")
    c.add_argument("--layout", required=True, choices=sorted(LAYOUTS))
    c.add_argument("--out", required=True, help="CSV path (directory for multi-record layouts)")
    c.set_defaults(fn=_cmd_convert)

    a = sub.add_parser("convert-all", help="convert every sample file into --out-dir")
    a.add_argument("--out-dir", default=str(DEFAULT_OUT))
    a.set_defaults(fn=_cmd_convert_all)

    x = sub.add_parser("crosscheck", help="compare EBCDIC-decoded rows with the ASCII samples")
    x.set_defaults(fn=_cmd_crosscheck)

    ld = sub.add_parser("load", help="COPY <table>.csv files into the carddemo schema")
    ld.add_argument("--csv-dir", default=str(DEFAULT_OUT))
    ld.add_argument("--dsn", required=True, help="libpq DSN / URL, e.g. postgresql://user:pw@host:5432/carddemo")
    ld.add_argument("--apply-schema", action="store_true", help="run schema.sql first (idempotent)")
    ld.add_argument("--schema", default=str(DEFAULT_SCHEMA))
    ld.add_argument("--no-truncate", action="store_true", help="append instead of truncate+reload")
    ld.set_defaults(fn=_cmd_load)

    ls = sub.add_parser("layouts", help="list layouts")
    ls.set_defaults(fn=_cmd_layouts)

    args = p.parse_args(argv)
    return args.fn(args)


if __name__ == "__main__":
    sys.exit(main())
