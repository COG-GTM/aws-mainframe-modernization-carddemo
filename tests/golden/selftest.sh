#!/usr/bin/env bash
# Prove that compare.py (a) returns 0 on an exact copy of the golden set,
# (b) catches every injected defect from mutate.py with the right exit code and
# names the right field / total / record key in reconciliation.md, and (c) that
# the explicit --tolerance path absorbs exactly what it names (exit 3 with a
# banner) and nothing more (exit 1 when the bound is too small), and (d) that
# absence is a mismatch: no reachable input DALYTRAN, or a missing RETURN-CODE,
# each give exit 1 on an otherwise exact copy.
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

tcheck() {  # tcheck <label> <expected_exit> <actual_exit> <report> <markers...>
  local label="$1" want="$2" got="$3" report="$4"; shift 4
  local missing=()
  for m in "$@"; do grep -qF -- "$m" "$report" || missing+=("$m"); done
  total=$((total+1))
  case "$label" in
    tolerance*) tol_all+=("$SETNAME/$label") ;;
    *)          abs_all+=("$SETNAME/$label") ;;
  esac
  if [ "$want" = "$got" ] && [ ${#missing[@]} -eq 0 ]; then
    caught=$((caught+1))
    case "$label" in
      tolerance*) tol_pass+=("$SETNAME/$label") ;;
      *)          abs_pass+=("$SETNAME/$label") ;;
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
done

# (c) record the evidence the documentation numbers are derived from (only for a
#     full run over both sets; a partial --set run must not overwrite it)
RESULT_JSON="$HERE/sets/selftest-result.json"
docs_line=""
if [ "$SETS" = "named volume" ]; then
  python3 - "$RESULT_JSON" "$(cd "$HERE/../.." && pwd)" "${#exact_pass[@]}" \
      "${mut_all[@]}" -- "${mut_caught[@]}" -- "${tol_all[@]}" -- "${tol_pass[@]}" -- "${abs_all[@]}" -- "${abs_pass[@]}" <<'PY'
import json, os, sys
out, repo, n_exact, rest = sys.argv[1], sys.argv[2], int(sys.argv[3]), sys.argv[4:]
groups, cur = [], []
for a in rest:
    if a == "--":
        groups.append(cur); cur = []
    else:
        cur.append(a)
groups.append(cur)
mutants, caught, tol_all, tol_pass, abs_all, abs_pass = groups
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
echo "  checks passed: $caught of $total  (exact-copy x$n_sets + $n_mut mutants x$n_sets + 2 tolerance-path x$n_sets + 2 absence x$n_sets$docs_line)"
if [ $fail -eq 0 ]; then
  echo "  RESULT: PASS - ${#mut_caught[@]} of ${#mut_all[@]} injected defects caught; exact copy compares clean; tolerance path ${#tol_pass[@]} of ${#tol_all[@]}; absence ${#abs_pass[@]} of ${#abs_all[@]}"
  echo "  work dir: $WORK"
  exit 0
else
  echo "  RESULT: FAIL"
  echo "  work dir: $WORK"
  exit 1
fi
