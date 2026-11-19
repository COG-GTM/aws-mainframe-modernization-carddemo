#!/usr/bin/env python3
"""Compare one POSTTRAN end-to-end leg (Java on docker compose PostgreSQL) with the GnuCOBOL oracle.

Usage: posttran_e2e_compare.py ACTUAL_DIR ORACLE_DIR [--tables-from DIR] [--no-tables] [--sysout-tail N]
ACTUAL_DIR holds RC, DALYREJS (absent = not created), SYSOUT and unload/*.unl; ORACLE_DIR the unchanged oracle files.
Rules (same as ParityHarness / CBTRN02C_parity_results.md §1): RC 0/4 equal, oracle 231 == Java 16 (D-039); DALYREJS byte
for byte; SYSOUT lines equal after dropping 'CEE3ABD ABCODE=' / 'libcob: ' lines and trailing blanks; tables padded to
LRECL with FILLER masked (D-020/D-036) and TRANSACT TRAN-PROC-TS centiseconds cols 325-326 masked (D-041).
Prints one line per artefact and exits 1 on any mismatch.
"""
import argparse, os, sys

LRECL = {"ACCTDATA": 300, "TCATBALF": 50, "TRANSACT": 350}
MASKS = {"ACCTDATA": [(122, 300)], "TCATBALF": [(28, 50)], "TRANSACT": [(324, 326), (330, 350)]}


def read(p):
    with open(p, "rb") as f:
        return f.read()


def table(path, name):
    if not os.path.exists(path):
        return None
    out = []
    for line in read(path).decode("latin-1").split("\n"):
        if line == "":
            continue
        rec = list(line.replace("\r", "").ljust(LRECL[name]))
        for a, b in MASKS[name]:
            rec[a:b] = "#" * (b - a)
        out.append("".join(rec))
    return out


def sysout(path, tail):
    lines = [l.rstrip() for l in read(path).decode("latin-1").splitlines()
             if not l.startswith("CEE3ABD ABCODE=") and not l.startswith("libcob: ")]
    return lines[-tail:] if tail else lines


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("actual"); ap.add_argument("oracle")
    ap.add_argument("--tables-from"); ap.add_argument("--no-tables", action="store_true")
    ap.add_argument("--sysout-tail", type=int, default=0)
    a = ap.parse_args()
    ok = True

    def res(name, good, detail=""):
        nonlocal ok
        ok &= good
        print(f"  {'PASS' if good else 'FAIL'} {name} {detail}".rstrip())

    orc = int(read(os.path.join(a.oracle, "RETURN-CODE")).strip())
    jrc = int(read(os.path.join(a.actual, "RC")).strip())
    res("RC", (16 if orc == 231 else orc) == jrc, f"(oracle {orc}, java {jrc})")
    od, ad = os.path.join(a.oracle, "DALYREJS"), os.path.join(a.actual, "DALYREJS")
    if os.path.exists(od):
        same = os.path.exists(ad) and read(od) == read(ad)
        res("DALYREJS", same, f"({len(read(od))} bytes, {len(read(od)) // 430} records)")
    else:
        res("DALYREJS", not os.path.exists(ad), "(oracle did not create it)")
    so, sa = sysout(os.path.join(a.oracle, "SYSOUT"), a.sysout_tail), sysout(os.path.join(a.actual, "SYSOUT"), a.sysout_tail)
    res("SYSOUT", so == sa, f"({len(so)} lines{', tail' if a.sysout_tail else ''})")
    if not a.no_tables:
        tdir = a.tables_from or a.oracle
        for n in LRECL:
            e, g = table(os.path.join(tdir, n + ".unl"), n), table(os.path.join(a.actual, "unload", n + ".unl"), n)
            diff = [i + 1 for i, (x, y) in enumerate(zip(e or [], g or [])) if x != y]
            good = e is not None and g is not None and len(e) == len(g) and not diff
            res(n, good, f"({len(e or [])} rows expected, {len(g or [])} actual{', first diff row ' + str(diff[0]) if diff else ''})")
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
