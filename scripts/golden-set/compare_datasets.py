#!/usr/bin/env python3
"""Field-by-field comparison of keyed datasets (COBOL side vs Java side) using the copybook layouts in layouts.py.

Records are matched by their key fields (not by position), and every field of every record is compared; FILLER
fields included. A difference is (dataset, key, field, COBOL value, Java value) with values rendered per the PIC
(numbers as decimals, text right-trimmed). Records present on one side only are reported with field `*record*`.

--expected-diffs lists the known, justified differences, one per line:
    DATASET|key|FIELD|cobol-value|java-value|justification (ADR / rules doc / data note)
Each entry must match exactly one difference and must carry a justification; an entry that matches nothing (or
more than once) fails the run like an unexplained difference does. Key `*` is the one exception, for a field that
differs the same way in every record (e.g. TCATBALF FILLER, not persisted, ADR-0011): it must match the field of
every record compared on both sides, once each — a record where it does not differ fails the entry too.
Lines starting with # are comments.

Exit 0 only when every difference is explained and every entry is used. Writes --report (markdown) and --json.
"""
from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

from layouts import DATASETS, Layout, read_records, render


def load_expected(path: Path | None) -> list[dict]:
    entries = []
    if not path or not path.exists():
        return entries
    for n, raw in enumerate(path.read_text().splitlines(), 1):
        line = raw.rstrip("\n")
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        parts = line.split("|")
        if len(parts) != 6 or not parts[5].strip():
            raise SystemExit(f"{path}:{n}: expected DATASET|key|FIELD|cobol|java|justification, got {line!r}")
        entries.append({"dataset": parts[0], "key": parts[1], "field": parts[2], "cobol": parts[3],
                        "java": parts[4], "why": parts[5].strip(), "source": f"{path.name}:{n}", "matched": 0})
    return entries


def compare(dataset: str, cobol: list[str], java: list[str]) -> tuple[list[dict], dict]:
    lay = Layout(dataset)
    c = {lay.key(r): r for r in cobol}
    j = {lay.key(r): r for r in java}
    diffs = []
    for key in sorted(set(c) | set(j), key=lambda k: k.encode("latin-1")):
        shown = key.strip()
        if key not in c or key not in j:
            diffs.append({"dataset": dataset, "key": shown, "field": "*record*",
                          "cobol": "present" if key in c else "absent", "java": "present" if key in j else "absent"})
            continue
        for f in lay.fields:
            cv, jv = f.raw(c[key]), f.raw(j[key])
            if cv != jv:
                diffs.append({"dataset": dataset, "key": shown, "field": f.name,
                              "cobol": render(f, cv), "java": render(f, jv)})
    stats = {"dataset": dataset, "copybook": lay.copybook, "cobol": len(c), "java": len(j),
             "compared": len(set(c) & set(j)), "fields": len(lay.fields)}
    return diffs, stats


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--cobol-dir", type=Path, required=True)
    ap.add_argument("--java-dir", type=Path, required=True)
    ap.add_argument("--datasets", required=True, help="comma-separated, files are <dir>/<DATASET>.txt")
    ap.add_argument("--expected-diffs", type=Path)
    ap.add_argument("--title", required=True)
    ap.add_argument("--report", type=Path, required=True)
    ap.add_argument("--json", type=Path, required=True)
    a = ap.parse_args()

    entries = load_expected(a.expected_diffs)
    all_diffs, stats = [], []
    for ds in a.datasets.split(","):
        lrecl = DATASETS[ds][1]
        diffs, st = compare(ds, read_records(a.cobol_dir / f"{ds}.txt", lrecl),
                            read_records(a.java_dir / f"{ds}.txt", lrecl))
        for d in diffs:
            hits = [e for e in entries if (e["dataset"], e["field"], e["cobol"], e["java"]) ==
                    (d["dataset"], d["field"], d["cobol"], d["java"]) and e["key"] in (d["key"], "*")]
            for e in hits:
                e["matched"] += 1
            d["explained"] = hits[0]["why"] if hits else None
        for e in entries:
            if e["dataset"] == ds and e["key"] == "*":
                e["records"] = st["compared"]
        st["diffs"] = len(diffs)
        st["explained"] = sum(1 for d in diffs if d["explained"])
        st["unexplained"] = st["diffs"] - st["explained"]
        all_diffs += diffs
        stats.append(st)
    bad_entries = [e for e in entries if e["matched"] != e.get("records", 1) or e["matched"] == 0]
    unexplained = [d for d in all_diffs if not d["explained"]]
    ok = not unexplained and not bad_entries

    L = [f"## {a.title}", "",
         f"COBOL side: `{a.cobol_dir}` · Java side: `{a.java_dir}` · allow-list: "
         f"`{a.expected_diffs or '-'}` ({len(entries)} entries)", "",
         "| dataset | copybook | fields/record | COBOL records | Java records | records compared | differences | explained | unexplained |",
         "|---|---|---|---|---|---|---|---|---|"]
    for s in stats:
        L.append(f"| {s['dataset']} | {s['copybook']} | {s['fields']} | {s['cobol']} | {s['java']} | {s['compared']} "
                 f"| {s['diffs']} | {s['explained']} | {s['unexplained']} |")
    L += ["", "| dataset | key | field | COBOL value | Java value | explained by |", "|---|---|---|---|---|---|"]
    every = {(e["dataset"], e["field"], e["cobol"], e["java"]): e for e in entries if e["key"] == "*"}
    shown = set()
    for d in all_diffs:
        e = every.get((d["dataset"], d["field"], d["cobol"], d["java"]))
        if e and d["explained"]:
            if id(e) not in shown:
                shown.add(id(e))
                L.append(f"| {d['dataset']} | every record ({e['matched']}) | {d['field']} | `{d['cobol']}` | "
                         f"`{d['java']}` | {d['explained']} |")
            continue
        L.append(f"| {d['dataset']} | {d['key']} | {d['field']} | `{d['cobol']}` | `{d['java']}` | "
                 f"{d['explained'] or '**UNEXPLAINED**'} |")
    if not all_diffs:
        L.append("| - | - | - | - | - | no differences |")
    for e in bad_entries:
        L.append(f"| {e['dataset']} | {e['key']} | {e['field']} | `{e['cobol']}` | `{e['java']}` | "
                 f"**allow-list entry {e['source']} matched {e['matched']} times (must be {e.get('records', 1)})** |")
    L += ["", f"Result: **{'PASS' if ok else 'FAIL'}** — {len(all_diffs)} differences, "
          f"{len(all_diffs) - len(unexplained)} explained, {len(unexplained)} unexplained, "
          f"{len(bad_entries)} allow-list entries not matched exactly once.", ""]
    a.report.parent.mkdir(parents=True, exist_ok=True)
    a.report.write_text("\n".join(L))
    a.json.write_text(json.dumps({"title": a.title, "ok": ok, "datasets": stats, "differences": all_diffs,
                                  "allowList": entries}, indent=1) + "\n")
    print(f"{a.title}: {'PASS' if ok else 'FAIL'} ({len(all_diffs)} differences, {len(unexplained)} unexplained, "
          f"{len(bad_entries)} allow-list entries not matched exactly once)")
    for d in unexplained[:20]:
        print(f"  UNEXPLAINED {d['dataset']} key {d['key']} {d['field']}: COBOL {d['cobol']!r} / Java {d['java']!r}")
    if len(unexplained) > 20:
        print(f"  ... {len(unexplained) - 20} more unexplained differences (see {a.report})")
    for e in bad_entries:
        print(f"  ALLOW-LIST {e['source']} matched {e['matched']} times")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
