#!/usr/bin/env bash
# Prove that compare.py (a) returns 0 on an exact copy of the golden set,
# (b) catches every injected defect from mutate.py with the right exit code and
# names the right field / total / record key in reconciliation.md, and (c) that
# the explicit --tolerance path absorbs exactly what it names (exit 3 with a
# banner) and nothing more (exit 1 when the bound is too small), refusing a
# tolerance that names an unknown field or a non-finite bound (exit 64, no
# report), (d) that absence is a mismatch: no reachable input DALYTRAN, or a
# missing RETURN-CODE, each give exit 1 on an otherwise exact copy, (e) that
# --strict-sysout makes a SYSOUT difference of only edge whitespace a mismatch
# while the default still treats SYSOUT as informational, and (f) that
# RETURN-CODE is compared as an integer (`04` equals `4`; two non-integer files
# are a mismatch, not a match) and that two same-key DALYREJS records changing
# places is reported as an order difference (exit 2), not as field differences.
#
#   bash tests/golden/selftest.sh [--set named|volume|all] [--work DIR]
#
# Exit 0 only if every check passes.  Standard tools + python3 only.
set -u
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SETS="all"
WORK=""
while [ $# -gt 0 ]; do
  case "$1" in
    --set) SETS="$2"; shift 2 ;;
    --work) WORK="$2"; shift 2 ;;
    *) echo "unknown option $1" >&2; exit 64 ;;
  esac
done
[ "$SETS" = "all" ] && SETS="named volume"
if [ -z "$WORK" ]; then WORK="$(mktemp -d "${TMPDIR:-/tmp}/golden-selftest.XXXXXX")"; fi
mkdir -p "$WORK"

fail=0
total=0
caught=0
lines=()
mut_all=()      # "<set>/<mutant>" for every mutant run
mut_caught=()   # subset that was caught
exact_pass=()   # sets whose exact copy compared clean
tol_all=()      # "<set>/<tolerance check>" for every tolerance-path check
tol_pass=()     # subset that passed

check() {  # check <label> <expected_exit> <actual_exit> <report> <markers...>
  local label="$1" want="$2" got="$3" report="$4"; shift 4
  local missing=()
  for m in "$@"; do grep -qF -- "$m" "$report" || missing+=("$m"); done
  total=$((total+1))
  mut_all+=("$SETNAME/$label")
  if [ "$want" = "$got" ] && [ ${#missing[@]} -eq 0 ]; then
    caught=$((caught+1))
    mut_caught+=("$SETNAME/$label")
    lines+=("$(printf '  %-8s %-28s exit %s (expected %s)  named: %s  CAUGHT' "$SETNAME" "$label" "$got" "$want" "$*")")
  else
    fail=1
    lines+=("$(printf '  %-8s %-28s exit %s (expected %s)  MISSING markers: %s  NOT CAUGHT' "$SETNAME" "$label" "$got" "$want" "${missing[*]:-none}")")
  fi
}

abs_all=()      # absence checks run (missing input DALYTRAN, missing RETURN-CODE)
abs_pass=()     # absence checks that produced the expected exit code and markers
sys_all=()      # SYSOUT policy checks run (edge whitespace: default informational, --strict-sysout fatal)
sys_pass=()     # SYSOUT policy checks that passed
pair_all=()     # pairing checks run (RETURN-CODE as integer; same-key records swapped = order only; same-key extra row = extra)
pair_pass=()    # pairing checks that passed

tcheck() {  # tcheck <label> <expected_exit> <actual_exit> <report> <markers...>
  local label="$1" want="$2" got="$3" report="$4"; shift 4
  local missing=()
  for m in "$@"; do grep -qF -- "$m" "$report" || missing+=("$m"); done
  total=$((total+1))
  case "$label" in
    tolerance*)   tol_all+=("$SETNAME/$label") ;;
    sysout*)      sys_all+=("$SETNAME/$label") ;;
    return-code*|dup-key*) pair_all+=("$SETNAME/$label") ;;
    *)            abs_all+=("$SETNAME/$label") ;;
  esac
  if [ "$want" = "$got" ] && [ ${#missing[@]} -eq 0 ]; then
    caught=$((caught+1))
    case "$label" in
      tolerance*)   tol_pass+=("$SETNAME/$label") ;;
      sysout*)      sys_pass+=("$SETNAME/$label") ;;
      return-code*|dup-key*) pair_pass+=("$SETNAME/$label") ;;
      *)            abs_pass+=("$SETNAME/$label") ;;
    esac
    lines+=("$(printf '  %-8s %-28s exit %s (expected %s)  named: %s  PASS' "$SETNAME" "$label" "$got" "$want" "$*")")
  else
    fail=1
    lines+=("$(printf '  %-8s %-28s exit %s (expected %s)  MISSING markers: %s  FAIL' "$SETNAME" "$label" "$got" "$want" "${missing[*]:-none}")")
  fi
}

for SETNAME in $SETS; do
  EXP="$HERE/sets/$SETNAME/expected"
  if [ ! -f "$EXP/TRANSACT" ]; then
    echo "selftest: $EXP missing; run 'bash tests/golden/run_reference.sh' first" >&2
    exit 65
  fi
  W="$WORK/$SETNAME"; rm -rf "$W"; mkdir -p "$W"

  # (a) exact copy -> exit 0
  cp -r "$EXP" "$W/exact-copy"
  rm -f "$W/exact-copy/reconciliation.json" "$W/exact-copy/reconciliation.md"
  python3 "$HERE/compare.py" "$EXP" "$W/exact-copy" --out-dir "$W/exact-copy" --quiet
  rc=$?
  total=$((total+1))
  if [ $rc -eq 0 ] && grep -qF "EXACT MATCH" "$W/exact-copy/reconciliation.md"; then
    caught=$((caught+1))
    exact_pass+=("$SETNAME")
    lines+=("$(printf '  %-8s %-28s exit %s (expected 0)  EXACT MATCH  PASS' "$SETNAME" "exact-copy" "$rc")")
  else
    fail=1
    lines+=("$(printf '  %-8s %-28s exit %s (expected 0)  FAIL' "$SETNAME" "exact-copy" "$rc")")
  fi

  # (b) every mutant -> expected non-zero exit and named markers
  python3 "$HERE/mutate.py" "$EXP" "$W/mutants" >/dev/null || { echo "mutate.py failed" >&2; exit 66; }
  for md in "$W"/mutants/*/; do
    name="$(basename "$md")"
    want="$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["expected_exit"])' "$md/mutant.json")"
    mapfile -t markers < <(python3 -c 'import json,sys; [print(m) for m in json.load(open(sys.argv[1]))["expect_in_report"]]' "$md/mutant.json")
    python3 "$HERE/compare.py" "$EXP" "$md" --out-dir "$md" --quiet
    rc=$?
    check "$name" "$want" "$rc" "$md/reconciliation.md" "${markers[@]}"
  done

  # (c) the explicit tolerance path, on the one-cent mutant: a bound that covers the
  #     defect must absorb the field AND the control total it feeds (exit 3, banner);
  #     a bound that does not cover it must still fail (exit 1) while naming the bound.
  onecent="$W/mutants/amount_off_by_one_cent"
  python3 "$HERE/compare.py" "$EXP" "$onecent" --tolerance TRAN-AMT=0.01 --out-dir "$W/tolerance-absorbs" --quiet
  tcheck "tolerance TRAN-AMT=0.01" 3 $? "$W/tolerance-absorbs/reconciliation.md" \
    "MATCH WITHIN TOLERANCE" "Tolerance policy in effect" "WITHIN TOLERANCE ±0.01" "TRAN-AMT" "sum_accepted_amount"
  python3 "$HERE/compare.py" "$EXP" "$onecent" --tolerance TRAN-AMT=0.001 --out-dir "$W/tolerance-too-small" --quiet
  tcheck "tolerance TRAN-AMT=0.001" 1 $? "$W/tolerance-too-small/reconciliation.md" \
    "MISMATCH" "±0.001" "TRAN-AMT" "sum_accepted_amount"
  #     a tolerance must name a numeric output field and a finite bound; anything else
  #     is a usage error (exit 64) and no report is written
  mkdir -p "$W/tolerance-unknown" "$W/tolerance-nan"
  python3 "$HERE/compare.py" "$EXP" "$onecent" --tolerance NO-SUCH-FIELD=0.01 --out-dir "$W/tolerance-unknown" --quiet 2>"$W/tolerance-unknown/stderr.txt"
  rc=$?; [ -e "$W/tolerance-unknown/reconciliation.md" ] && rc="$rc+report-written"
  tcheck "tolerance NO-SUCH-FIELD=0.01" 64 "$rc" "$W/tolerance-unknown/stderr.txt" \
    "not a numeric field" "NO-SUCH-FIELD"
  python3 "$HERE/compare.py" "$EXP" "$onecent" --tolerance TRAN-AMT=NaN --out-dir "$W/tolerance-nan" --quiet 2>"$W/tolerance-nan/stderr.txt"
  rc=$?; [ -e "$W/tolerance-nan/reconciliation.md" ] && rc="$rc+report-written"
  tcheck "tolerance TRAN-AMT=NaN" 64 "$rc" "$W/tolerance-nan/stderr.txt" \
    "finite" "TRAN-AMT"

  # (d) absence is a mismatch, never a silent pass: an exact copy compared with no
  #     input DALYTRAN reachable (so records_in / in = accepted + rejected cannot be
  #     formed), and an exact copy whose RETURN-CODE file is missing.
  mkdir -p "$W/no-input"
  python3 "$HERE/compare.py" "$EXP" "$W/exact-copy" --input-dir "$W/no-input" --out-dir "$W/absent-input" --quiet
  tcheck "missing input DALYTRAN" 1 $? "$W/absent-input/reconciliation.md" \
    "MISMATCH" "INPUT ERROR" "input DALYTRAN missing"
  cp -r "$W/exact-copy" "$W/no-rc"; rm -f "$W/no-rc/RETURN-CODE"
  python3 "$HERE/compare.py" "$EXP" "$W/no-rc" --out-dir "$W/absent-rc" --quiet
  tcheck "missing RETURN-CODE" 1 $? "$W/absent-rc/reconciliation.md" \
    "MISMATCH" "RETURN-CODE | **MISMATCH**"

  # (e) SYSOUT policy: an exact copy whose operator log differs only by edge
  #     whitespace is informational by default (exit 0) but a byte-for-byte
  #     mismatch under --strict-sysout (exit 1).
  cp -r "$W/exact-copy" "$W/sysout-ws"; printf ' ' >> "$W/sysout-ws/SYSOUT"
  python3 "$HERE/compare.py" "$EXP" "$W/sysout-ws" --out-dir "$W/sysout-default" --quiet
  tcheck "sysout whitespace, default" 0 $? "$W/sysout-default/reconciliation.md" \
    "EXACT MATCH" "match ignoring edge whitespace (informational; operator log)"
  python3 "$HERE/compare.py" "$EXP" "$W/sysout-ws" --strict-sysout --out-dir "$W/sysout-strict" --quiet
  tcheck "sysout whitespace, --strict" 1 $? "$W/sysout-strict/reconciliation.md" \
    "MISMATCH" "**MISMATCH** (--strict-sysout)"

  # (f) pairing: RETURN-CODE is an integer, so `04` is the same status as `4`
  #     (exit 0) while two files that are not integers never agree (exit 1);
  #     and two DALYREJS records sharing a key but differing in content, swapped
  #     in the candidate, are the same records in a different order (exit 2).
  cp -r "$W/exact-copy" "$W/rc-04"; printf '0%s\n' "$(tr -d '[:space:]' < "$EXP/RETURN-CODE")" > "$W/rc-04/RETURN-CODE"
  python3 "$HERE/compare.py" "$EXP" "$W/rc-04" --out-dir "$W/rc-04-out" --quiet
  tcheck "return-code 04 equals 4" 0 $? "$W/rc-04-out/reconciliation.md" \
    "EXACT MATCH" "| RETURN-CODE | same |"
  cp -r "$EXP" "$W/rc-bad-exp"; cp -r "$W/exact-copy" "$W/rc-bad"
  rm -f "$W/rc-bad-exp/reconciliation.json" "$W/rc-bad-exp/reconciliation.md"
  printf 'abc\n' > "$W/rc-bad-exp/RETURN-CODE"; printf 'abc\n' > "$W/rc-bad/RETURN-CODE"
  python3 "$HERE/compare.py" "$W/rc-bad-exp" "$W/rc-bad" --input-dir "$HERE/sets/$SETNAME/input" \
    --out-dir "$W/rc-bad-out" --quiet
  tcheck "return-code non-integer" 1 $? "$W/rc-bad-out/reconciliation.md" \
    "MISMATCH" "**MISMATCH** (not an integer)"
  cp -r "$EXP" "$W/dup-exp"; rm -f "$W/dup-exp/reconciliation.json" "$W/dup-exp/reconciliation.md"
  cp -r "$W/dup-exp" "$W/dup-cand"
  python3 - "$HERE" "$W/dup-exp/DALYREJS" "$W/dup-cand/DALYREJS" <<'PY'
# give reject 2 the key of reject 1 (its other fields still differ), then swap the two in the candidate
import sys
sys.path.insert(0, sys.argv[1])
from layouts import DALYREJS
recs = DALYREJS.records(open(sys.argv[2], "rb").read())
kf = DALYREJS.field(DALYREJS.key_field)
recs[1] = recs[1][:kf.offset] + DALYREJS.key(recs[0]) + recs[1][kf.end:]
assert DALYREJS.key(recs[1]) == DALYREJS.key(recs[0]) and recs[1] != recs[0]
open(sys.argv[2], "wb").write(b"".join(recs))
open(sys.argv[3], "wb").write(b"".join([recs[1], recs[0]] + recs[2:]))
PY
  python3 "$HERE/compare.py" "$W/dup-exp" "$W/dup-cand" --input-dir "$HERE/sets/$SETNAME/input" \
    --out-dir "$W/dup-out" --quiet
  tcheck "dup-key rejects swapped" 2 $? "$W/dup-out/reconciliation.md" \
    "SAME RECORDS, DIFFERENT ORDER" "| field differences | 0 |" "DALYREJS"
  #     a second copy of an existing ACCTFILE row is an extra record, and stays a
  #     mismatch even when the only field difference is absorbed by a tolerance
  cp -r "$onecent" "$W/dup-acct"; rm -f "$W/dup-acct/reconciliation.json" "$W/dup-acct/reconciliation.md"
  python3 - "$HERE" "$W/dup-acct/ACCTFILE" <<'PY'
import sys
sys.path.insert(0, sys.argv[1])
from layouts import OUTPUT_LAYOUTS
recs = OUTPUT_LAYOUTS["ACCTFILE"].records(open(sys.argv[2], "rb").read())
open(sys.argv[2], "wb").write(b"".join(recs + [recs[0]]))
PY
  python3 "$HERE/compare.py" "$EXP" "$W/dup-acct" --tolerance TRAN-AMT=0.01 --out-dir "$W/dup-acct-out" --quiet
  tcheck "dup-key extra row + tolerance" 1 $? "$W/dup-acct-out/reconciliation.md" \
    "MISMATCH" "| missing / extra records | 0 / 1 |" "ACCTFILE"
done

# (c) record the evidence the documentation numbers are derived from (only for a
#     full run over both sets; a partial --set run must not overwrite it)
RESULT_JSON="$HERE/sets/selftest-result.json"
docs_line=""
if [ "$SETS" = "named volume" ]; then
  python3 - "$RESULT_JSON" "$(cd "$HERE/../.." && pwd)" "${#exact_pass[@]}" \
      "${mut_all[@]}" -- "${mut_caught[@]}" -- "${tol_all[@]}" -- "${tol_pass[@]}" -- "${abs_all[@]}" -- "${abs_pass[@]}" \
      -- "${sys_all[@]}" -- "${sys_pass[@]}" -- "${pair_all[@]}" -- "${pair_pass[@]}" <<'PY'
import json, os, sys
out, repo, n_exact, rest = sys.argv[1], sys.argv[2], int(sys.argv[3]), sys.argv[4:]
groups, cur = [], []
for a in rest:
    if a == "--":
        groups.append(cur); cur = []
    else:
        cur.append(a)
groups.append(cur)
mutants, caught, tol_all, tol_pass, abs_all, abs_pass, sys_all, sys_pass, pair_all, pair_pass = groups
doc = {
    "produced_by": "tests/golden/selftest.sh",
    "sets": ["named", "volume"],
    "exact_copy_sets_passed": n_exact,
    "mutants": mutants,
    "caught": caught,
    "tolerance_checks": tol_all,
    "tolerance_checks_passed": tol_pass,
    "absence_checks": abs_all,
    "absence_checks_passed": abs_pass,
    "sysout_policy_checks": sys_all,
    "sysout_policy_checks_passed": sys_pass,
    "pairing_checks": pair_all,
    "pairing_checks_passed": pair_pass,
}
with open(out, "w") as fh:
    json.dump(doc, fh, indent=2, sort_keys=True); fh.write("\n")
print("  wrote %s" % os.path.relpath(out, repo))
PY
  # (d) documentation numbers must be the ones the artefacts produce right now
  docs_rc=0
  python3 "$HERE/docs_numbers.py" --check || docs_rc=$?
  total=$((total+1))
  docs_line=" + docs sync"
  if [ $docs_rc -eq 0 ]; then caught=$((caught+1)); lines+=("  docs     docs_numbers.py --check      exit 0 (expected 0)  README/layouts/findings blocks current  PASS"); else fail=1; lines+=("  docs     docs_numbers.py --check      exit $docs_rc (expected 0)  FAIL"); fi
fi

echo "golden-set comparator self-test (compare.py vs mutate.py)"
printf '%s\n' "${lines[@]}"
n_mut=$(python3 "$HERE/mutate.py" --list | wc -l | tr -d ' ')
n_sets=$(echo $SETS | wc -w | tr -d ' ')
echo "  mutants defined: $n_mut; sets: $SETS"
echo "  checks passed: $caught of $total  (exact-copy x$n_sets + $n_mut mutants x$n_sets + 4 tolerance-path x$n_sets + 2 absence x$n_sets + 2 sysout-policy x$n_sets + 4 pairing x$n_sets$docs_line)"
if [ $fail -eq 0 ]; then
  echo "  RESULT: PASS - ${#mut_caught[@]} of ${#mut_all[@]} injected defects caught; exact copy compares clean; tolerance path ${#tol_pass[@]} of ${#tol_all[@]}; absence ${#abs_pass[@]} of ${#abs_all[@]}; sysout policy ${#sys_pass[@]} of ${#sys_all[@]}; pairing ${#pair_pass[@]} of ${#pair_all[@]}"
  echo "  work dir: $WORK"
  exit 0
else
  echo "  RESULT: FAIL"
  echo "  work dir: $WORK"
  exit 1
fi
