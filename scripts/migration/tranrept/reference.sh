#!/usr/bin/env bash
# Runs the COBOL reference of TRANREPT under GnuCOBOL for one test case and, with --write, stores
# the expected TRANREPT and RETURN-CODE next to the case for the Java tests.
#
#   scripts/migration/tranrept/reference.sh [--write] <case-dir> [<out-dir>]
#
# <case-dir> holds:
#   TRANSACT.txt     TRAN-RECORD (CVTRA05Y) lines, 350 bytes each: the TRANSACT KSDS unload
#   DATEPARM.txt     the DATEPARM record: 'YYYY-MM-DD YYYY-MM-DD' (may be empty: no record)
#   CARDXREF.txt / TRANTYPE.txt / TRANCATG.txt   optional; default app/data/ASCII/<name>.txt
#   MODE             optional: 'job' (default) = STEP05R SORT then STEP10R CBTRN03C, as TRANREPT.jcl;
#                    'program' = TRANSACT.txt is TRANFILE as is (no sort), to reach CBTRN03C paths
#                    the SORT step hides (out-of-range or unsorted records)
#
# The SORT step is emulated in Python (stable byte-order sort on TRAN-CARD-NUM 263,16 with the
# INCLUDE on TRAN-PROC-DT 305,10 between the DATEPARM dates: CORPT00C writes the same dates into
# SYMNAMES and DATEPARM). CBTRN03C itself is compiled from app/cbl unchanged; CEE3ABD is a stub
# that ends the run with RC 16. Needs cobc (GnuCOBOL 3.x with an indexed file handler) and python3.
set -euo pipefail

write=0
if [ "${1:-}" = "--write" ]; then write=1; shift; fi
case_dir="$(realpath "${1:?usage: $0 [--write] <case-dir> [<out-dir>]}")"
ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
HERE="$ROOT/scripts/migration/tranrept"
out="$(realpath -m "${2:-$(mktemp -d)}")"
rm -rf "$out" && mkdir -p "$out/bin" "$out/ksds"

cobc -x -std=ibm -fsign=EBCDIC -I "$ROOT/app/cpy" -o "$out/bin/CBTRN03C" "$ROOT/app/cbl/CBTRN03C.cbl" "$HERE/CEE3ABD.cbl" 2>"$out/compile.log"
cobc -x -std=ibm -o "$out/bin/IDXLOAD" "$HERE/IDXLOAD.cbl" 2>>"$out/compile.log"

for name in CARDXREF TRANTYPE TRANCATG; do
  src="$case_dir/$name.txt"
  [ -f "$src" ] || src="$ROOT/app/data/ASCII/$(echo "$name" | tr '[:upper:]' '[:lower:]').txt"
  tr -d '\r' < "$src" > "$out/ksds/$name.txt"
  DD_INFILE="$out/ksds/$name.txt" DD_CARDXREF="$out/ksds/CARDXREF" DD_TRANTYPE="$out/ksds/TRANTYPE" \
    DD_TRANCATG="$out/ksds/TRANCATG" "$out/bin/IDXLOAD" "$name" >>"$out/load.log"
done

mode="job"
[ -f "$case_dir/MODE" ] && mode="$(tr -d '[:space:]' < "$case_dir/MODE")"

python3 - "$case_dir" "$out" "$mode" <<'PY'
import sys, pathlib
case, out, mode = pathlib.Path(sys.argv[1]), pathlib.Path(sys.argv[2]), sys.argv[3]
recs = [l.rstrip("\r\n").ljust(350).encode("ascii") for l in (case / "TRANSACT.txt").read_text("ascii").splitlines()]
parm = (case / "DATEPARM.txt").read_text("ascii").rstrip("\r\n")
(out / "DATEPARM").write_bytes(parm.ljust(80).encode("ascii") if parm else b"")
(out / "TRANSACT.BKUP").write_bytes(b"".join(recs))
if mode == "job":
    start, end = parm.ljust(21)[0:10].encode(), parm.ljust(21)[11:21].encode()
    daly = sorted((r for r in recs if start <= r[304:314] <= end), key=lambda r: r[262:278])
else:
    daly = recs
(out / "TRANSACT.DALY").write_bytes(b"".join(daly))
PY

set +e
( cd "$out" && DD_TRANFILE="$out/TRANSACT.DALY" DD_CARDXREF="$out/ksds/CARDXREF" DD_TRANTYPE="$out/ksds/TRANTYPE" \
    DD_TRANCATG="$out/ksds/TRANCATG" DD_DATEPARM="$out/DATEPARM" DD_TRANREPT="$out/TRANREPT" \
    "$out/bin/CBTRN03C" >"$out/SYSOUT" 2>&1 )
rc=$?
set -e
echo "$rc" > "$out/RETURN-CODE"
echo "$(basename "$case_dir"): mode=$mode RC=$rc TRANREPT=$(( $(stat -c %s "$out/TRANREPT" 2>/dev/null || echo 0) / 133 )) records -> $out"

if [ "$write" = 1 ]; then
  mkdir -p "$case_dir/expected"
  echo "$rc" > "$case_dir/expected/RETURN-CODE"
  if [ "$rc" = 0 ]; then
    cp "$out/TRANREPT" "$case_dir/expected/TRANREPT"
  else
    # On z/OS DISP=(NEW,CATLG,DELETE) deletes TRANREPT(+1) when the step abends: no expected report.
    rm -f "$case_dir/expected/TRANREPT"
  fi
fi
