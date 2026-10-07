#!/usr/bin/env bash
# CI gate for the golden set (make golden-set-check, .github/workflows/modernization-ci.yml `golden-set` job, gate
# g-golden): runs scripts/golden-set/run_golden_set.sh into a scratch reconciliation directory and fails when
#   1. the run itself fails (any unexplained difference, allow-list entry not matched exactly once, missing dataset,
#      duplicate key, scenario/load failure), or
#   2. the reconciliation it wrote differs from the committed one (the newest docs/validation/golden-set/<date>/, or
#      GOLDEN_COMMITTED_DIR) — the golden-set equivalent of make baseline-check.
# Normalised before comparing: the `Toolchain:` line of reconciliation.md (JDK vendor/build and cobc patch level vary
# by host). Everything else — every table, count, digest and the redacted transcript — must be byte-identical.
# Writes $GOLDEN_OUT/summary.md (markdown for $GITHUB_STEP_SUMMARY) next to the run's summary.txt.
set -uo pipefail
cd "$(dirname "$0")/../.."
OUT="$(realpath -m "${GOLDEN_OUT:-build/golden-set}")"
export GOLDEN_OUT="$OUT"
export GOLDEN_DOC_DIR="${GOLDEN_DOC_DIR:-build/golden-set-doc}"
# Newest *tracked* dated directory: an untracked one left by a local `make golden-set` is not the reference.
COMMITTED="${GOLDEN_COMMITTED_DIR:-$(git ls-files 'docs/validation/golden-set/*/reconciliation.md' \
    | sed -nE 's|^(docs/validation/golden-set/[0-9]{4}-[0-9]{2}-[0-9]{2})/reconciliation.md$|\1|p' | sort | tail -1)}"
[ -n "$COMMITTED" ] && [ -d "$COMMITTED" ] || { echo "::error::no committed docs/validation/golden-set/<date>/" >&2; exit 2; }
rm -rf "$GOLDEN_DOC_DIR"

mkdir -p "$OUT"
run_log="$(mktemp)"
scripts/golden-set/run_golden_set.sh 2>&1 | tee "$run_log"
run_rc=${PIPESTATUS[0]}
mv "$run_log" "$OUT/run.log"

# jq 1.6 prints the API's BigDecimal 1020.00 as 1020 in the pretty-printed transcript, jq >= 1.7 keeps the literal:
# drop trailing fraction zeros on transcript lines that are a bare JSON number (`"key": n,` or `n,`).
normalise() {
    sed -E -e 's/^Toolchain: .*$/Toolchain: <normalised>/' \
        -e 's/^( *("[^"]*": )?-?[0-9]+)\.0+(,?)$/\1\3/' \
        -e 's/^( *("[^"]*": )?-?[0-9]+\.[0-9]*[1-9])0+(,?)$/\1\3/' "$1"
}
drift="$OUT/reproducibility.diff"
: >"$drift"
if [ -d "$GOLDEN_DOC_DIR" ]; then
    files=$( { (cd "$COMMITTED" && find . -type f); (cd "$GOLDEN_DOC_DIR" && find . -type f); } | sort -u)
    for f in $files; do
        a="$COMMITTED/$f"; b="$GOLDEN_DOC_DIR/$f"
        if [ ! -f "$a" ] || [ ! -f "$b" ]; then
            echo "only in $([ -f "$a" ] && echo "$COMMITTED" || echo "$GOLDEN_DOC_DIR"): ${f#./}" >>"$drift"
        else
            diff -u --label "committed/${f#./}" --label "run/${f#./}" <(normalise "$a") <(normalise "$b") >>"$drift"
        fi
    done
else
    echo "no reconciliation written to $GOLDEN_DOC_DIR" >>"$drift"
fi
if [ -s "$drift" ]; then repro_rc=1; else repro_rc=0; fi

{
    echo "## Golden set (gate g-golden)"
    echo
    echo "| check | result |"
    echo "|---|---|"
    echo "| \`run_golden_set.sh\` (zero unexplained differences, allow-list matched exactly once) | $([ $run_rc = 0 ] && echo PASS || echo "FAIL (exit $run_rc)") |"
    echo "| reconciliation reproduces \`$COMMITTED/\` (Toolchain line and jq number formatting normalised) | $([ $repro_rc = 0 ] && echo "PASS (identical)" || echo FAIL) |"
    echo
    echo '```'
    cat "$OUT/summary.txt" 2>/dev/null || echo "(no summary.txt: the run stopped before the reconciliation)"
    echo '```'
    if [ $run_rc != 0 ]; then
        echo
        echo "### Unexplained differences"
        echo
        echo '```'
        grep -E 'UNEXPLAINED|OVERLAP|ALLOW-LIST|FAIL|DIFFERENT|differ|mismatch|^run_golden_set:' "$OUT/run.log" \
            | grep -v -E ': PASS|golden-set: FAIL' | head -80
        echo '```'
    fi
    if [ $repro_rc != 0 ]; then
        echo
        echo "### Drift vs the committed reconciliation (first 80 lines)"
        echo
        echo '```diff'
        head -80 "$drift"
        echo '```'
    fi
} >"$OUT/summary.md"

if [ $repro_rc != 0 ]; then
    head -80 "$drift"
    echo "::error::golden-set reconciliation differs from $COMMITTED (full diff: $drift)" >&2
fi
[ $run_rc = 0 ] || echo "::error::golden-set run failed (exit $run_rc): unexplained differences, see $OUT/summary.md" >&2
[ $run_rc = 0 ] && [ $repro_rc = 0 ] || exit 1
echo "golden-set OK: zero unexplained differences, reconciliation identical to $COMMITTED/"
