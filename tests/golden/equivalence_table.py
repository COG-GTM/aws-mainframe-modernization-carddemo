#!/usr/bin/env python3
"""Print the golden-set equivalence table for the posting slice and write it as
markdown + JSON to tests/golden/.work/equivalence-summary.{md,json}.

Usage: python3 tests/golden/equivalence_table.py [SET ...]

Reads tests/golden/sets/<set>/expected/run.json (the GnuCOBOL reference run) and
tests/golden/sets/<set>/candidate-pr9/{reconciliation,run}.json (the Java candidate
run + compare.py verdict). Stdlib only. Exit 0 if every set's compare.py exit
was 0 and every gated row passes; exit 1 otherwise.
"""
import hashlib
import json
import sys
from decimal import Decimal
from pathlib import Path

HERE = Path(__file__).resolve().parent
SETS_DIR = HERE / "sets"
WORK = HERE / ".work"
CBTRN02C_SHA = None


def dec(value):
    return Decimal(str(value))


def fmt_amount(d):
    return f"{d:.2f}"


def load_set(name):
    sdir = SETS_DIR / name
    ref_run = json.loads((sdir / "expected" / "run.json").read_text())
    cand_dir = sdir / "candidate-pr9"
    recon = json.loads((cand_dir / "reconciliation.json").read_text())
    cand_run = json.loads((cand_dir / "run.json").read_text())
    return ref_run, cand_run, recon


def cbtrn02c_sha256():
    src = HERE.parent.parent / "app" / "cbl" / "CBTRN02C.cbl"
    return hashlib.sha256(src.read_bytes()).hexdigest()


def rows_for(name, ref_run, cand_run, recon):
    """Return list of row dicts for one set."""
    files = {f["file"]: f for f in recon["files"]}
    ct_e = recon["control_totals"]["expected"]
    ct_c = recon["control_totals"]["candidate"]
    rows = []

    def file_row(file, amt_e, amt_c):
        f = files[file]
        return {
            "set": name, "dataset": file,
            "rec_exp": str(f["records_expected"]), "rec_cand": str(f["records_candidate"]),
            "amt_exp": fmt_amount(amt_e), "amt_cand": fmt_amount(amt_c),
            "identical": "yes" if f["byte_identical"] else "no",
            "result": "PASS" if f["byte_identical"] else "FAIL",
        }

    rows.append(file_row("TRANSACT", dec(ct_e["sum_accepted_amount"]), dec(ct_c["sum_accepted_amount"])))
    rows.append(file_row("DALYREJS", dec(ct_e["sum_rejected_amount"]), dec(ct_c["sum_rejected_amount"])))

    acct_e = sum(dec(v["ACCT-CURR-BAL"]) for v in ct_e["closing_account_balances"].values())
    acct_c = sum(dec(v["ACCT-CURR-BAL"]) for v in ct_c["closing_account_balances"].values())
    rows.append(file_row("ACCTFILE", acct_e, acct_c))

    cat_e = sum(dec(v) for v in ct_e["closing_category_balances"].values())
    cat_c = sum(dec(v) for v in ct_c["closing_category_balances"].values())
    rows.append(file_row("TCATBALF", cat_e, cat_c))

    rc = files["RETURN-CODE"]
    try:
        rc_ok = int(rc["expected_text"].strip()) == int(rc["candidate_text"].strip())
    except ValueError:
        rc_ok = False
    rows.append({
        "set": name, "dataset": "RETURN-CODE",
        "rec_exp": rc["expected_text"].strip(), "rec_cand": rc["candidate_text"].strip(),
        "amt_exp": "", "amt_cand": "",
        "identical": "yes" if rc_ok else "no",
        "result": "PASS" if rc_ok else "FAIL",
    })

    totals_equal = all(ct_e[k] == ct_c[k] for k in
                       ("records_in", "accepted", "rejected", "rejected_by_reason",
                        "sum_accepted_amount", "sum_rejected_amount",
                        "closing_account_balances", "closing_category_balances"))
    identity = (ct_e["records_in"] == ct_e["accepted"] + ct_e["rejected"]
                and ct_c["records_in"] == ct_c["accepted"] + ct_c["rejected"])
    rows.append({
        "set": name, "dataset": "CONTROL",
        "rec_exp": f"in={ct_e['records_in']} acc={ct_e['accepted']} rej={ct_e['rejected']}",
        "rec_cand": f"in={ct_c['records_in']} acc={ct_c['accepted']} rej={ct_c['rejected']}",
        "amt_exp": "", "amt_cand": "",
        "identical": "yes" if totals_equal and identity else "no",
        "result": "PASS" if totals_equal and identity else "FAIL",
    })

    rows.append({
        "set": name, "dataset": "SYSOUT",
        "rec_exp": "", "rec_cand": "",
        "amt_exp": "", "amt_cand": "",
        "identical": "n/a",
        "result": "INFO",
    })
    return rows


def render(rows, set_meta):
    cols = ["Set", "Dataset", "Records exp", "Records cand", "Amount exp", "Amount cand",
            "Byte-identical", "Result"]
    keys = ["set", "dataset", "rec_exp", "rec_cand", "amt_exp", "amt_cand", "identical", "result"]
    widths = [len(c) for c in cols]
    for r in rows:
        for i, k in enumerate(keys):
            widths[i] = max(widths[i], len(r[k]))

    def line(vals):
        return " | ".join(str(v).ljust(widths[i]) for i, v in enumerate(vals))

    out = [line(cols), "-+-".join("-" * w for w in widths)]
    out += [line(r[k] for k in keys) for r in rows]
    out.append("")
    for name, meta in set_meta.items():
        cr, rr, exit_code = meta
        out.append(
            f"{name}: compare.py exit={exit_code} ({'PASS' if exit_code == 0 else 'FAIL'}); "
            f"candidate commit={cr['candidate_commit']}, jar_sha256={cr['jar_sha256']}, "
            f"faketime=\"{cr['faketime']}\", {cr['java']}")
        out.append(
            f"{name}: reference {rr['gnucobol_version']}, "
            f"{rr['cobc_command_CBTRN02C']}")
        out.append(
            f"{name}: CBTRN02C.cbl sha256 {CBTRN02C_SHA}")
    return "\n".join(out)


def main():
    names = sys.argv[1:] or sorted(p.name for p in SETS_DIR.iterdir()
                                   if (p / "candidate-pr9" / "reconciliation.json").exists()
                                   and p.name in ("named", "volume"))
    if not names:
        print("no candidate runs found; run run_candidate_pr9.sh first", file=sys.stderr)
        return 1

    global CBTRN02C_SHA
    CBTRN02C_SHA = cbtrn02c_sha256()

    rows = []
    set_meta = {}
    all_pass = True
    for name in names:
        ref_run, cand_run, recon = load_set(name)
        exit_code = recon["exit_code"]
        set_meta[name] = (cand_run, ref_run, exit_code)
        rows.extend(rows_for(name, ref_run, cand_run, recon))
        if exit_code != 0:
            all_pass = False
    if any(r["result"] == "FAIL" for r in rows):
        all_pass = False

    table = render(rows, set_meta)
    verdicts = []
    for name, (_, _, exit_code) in set_meta.items():
        set_pass = exit_code == 0 and all(r["result"] != "FAIL" for r in rows if r["set"] == name)
        verdicts.append(f"{name}: {'PASS' if set_pass else 'FAIL'}")
    overall = "PASS" if all_pass else "FAIL"
    table += "\n\n" + "\n".join(verdicts) + f"\n\nOVERALL: {overall}"
    print(table)

    WORK.mkdir(exist_ok=True)
    md_rows = ["| Set | Dataset | Records exp | Records cand | Amount exp | Amount cand | Byte-identical | Result |",
               "|---|---|---:|---:|---:|---:|---|---|"]
    md_rows += [f"| {r['set']} | {r['dataset']} | {r['rec_exp']} | {r['rec_cand']} | "
                f"{r['amt_exp']} | {r['amt_cand']} | {r['identical']} | {r['result']} |" for r in rows]
    md = ["# Golden-set equivalence summary", "", *md_rows, ""]
    for name, (cr, rr, exit_code) in set_meta.items():
        md.append(f"- `{name}`: compare.py exit `{exit_code}`; candidate `{cr['candidate_commit']}`, "
                  f"jar sha256 `{cr['jar_sha256']}`, faketime `{cr['faketime']}`, {cr['java']}")
        md.append(f"  - reference: {rr['gnucobol_version']}, `{rr['cobc_command_CBTRN02C']}`")
        md.append(f"  - `CBTRN02C.cbl` sha256 `{CBTRN02C_SHA}`")
    md += ["", *[f"**{v.split(':')[0]}: {v.split(': ')[1]}**" for v in verdicts], "",
           f"**OVERALL: {overall}**", ""]
    (WORK / "equivalence-summary.md").write_text("\n".join(md))
    (WORK / "equivalence-summary.json").write_text(json.dumps(
        {"overall": overall, "cbtrn02c_sha256": CBTRN02C_SHA, "rows": rows,
         "sets": {n: {"candidate_commit": m[0]["candidate_commit"], "jar_sha256": m[0]["jar_sha256"],
                      "faketime": m[0]["faketime"], "java": m[0]["java"], "compare_exit": m[2],
                      "gnucobol": m[1]["gnucobol_version"], "cobc_command": m[1]["cobc_command_CBTRN02C"]}
                  for n, m in set_meta.items()}}, indent=2) + "\n")
    return 0 if all_pass else 1


if __name__ == "__main__":
    sys.exit(main())
