#!/usr/bin/env bash
# Reference runner: compiles the UNMODIFIED app/cbl/CBTRN02C.cbl with GnuCOBOL,
# builds the indexed files from the generated sequential fixtures, runs the
# program once per golden set and captures every output as the golden set.
#
#   bash tests/golden/run_reference.sh                 # named + volume sets
#   bash tests/golden/run_reference.sh --set named     # one set
#   bash tests/golden/run_reference.sh --variant       # also produce expected-variant/ with env/posttran-variant.env
#   bash tests/golden/run_reference.sh --keep-inputs   # do not regenerate input/ if present
#
# Outputs per set (tests/golden/sets/<set>/expected/):
#   TRANSACT   posted transactions, dumped from the indexed file in TRAN-ID order   (CVTRA05Y, 350 bytes)
#   DALYREJS   rejects with reason trailer, as written                              (CVTRA06Y + 80, 430 bytes)
#   ACCTFILE   post-run account master, dumped in ACCT-ID order                     (CVACT01Y, 300 bytes)
#   TCATBALF   post-run category balances, dumped in TRAN-CAT-KEY order            (CVTRA01Y, 50 bytes)
#   RETURN-CODE, SYSOUT, run.json (toolchain + exact cobc command lines)
#   prediction-check.json  generator prediction vs. program outcome per record
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$HERE/../.." && pwd)"
cd "$REPO"   # so the recorded cobc command lines are repo-relative and reproducible
SETS_ROOT="$HERE/sets"
WORK_ROOT="$HERE/.work"
SRC="app/cbl/CBTRN02C.cbl"
CPY="app/cpy"
UTIL_SRC="tests/golden/cobol/GSIDXUTL.cbl"

SETS="named volume"
VARIANT=0
KEEP_INPUTS=0
while [ $# -gt 0 ]; do
  case "$1" in
    --set) SETS="$2"; shift 2 ;;
    --variant) VARIANT=1; shift ;;
    --keep-inputs) KEEP_INPUTS=1; shift ;;
    -h|--help) sed -n '2,20p' "$0"; exit 0 ;;
    *) echo "unknown option $1" >&2; exit 64 ;;
  esac
done

command -v cobc >/dev/null || { echo "cobc (GnuCOBOL) not found on PATH" >&2; exit 69; }
command -v python3 >/dev/null || { echo "python3 not found on PATH" >&2; exit 69; }

COBC_VERSION="$(cobc --version | head -1)"
echo "== $COBC_VERSION"
echo "== source: app/cbl/CBTRN02C.cbl sha256 $(sha256sum "$SRC" | cut -c1-64) (not modified)"

# ---------------------------------------------------------------------------
# 1. Compile.  -std=ibm      : IBM dialect (ddname-style ASSIGN resolved via env)
#              -fsign=EBCDIC : zoned-decimal overpunch as on the mainframe, which is
#                              also the encoding of the repository's ASCII sample data
#              -x            : build an executable
# ---------------------------------------------------------------------------
BUILD="tests/golden/.work/build"
rm -rf "$BUILD"; mkdir -p "$BUILD"
COBC_MAIN=(cobc -x -std=ibm -fsign=EBCDIC -I "$CPY" -o "$BUILD/CBTRN02C" "$SRC")
COBC_UTIL=(cobc -x -std=ibm -fsign=EBCDIC -o "$BUILD/GSIDXUTL" "$UTIL_SRC")
echo "== ${COBC_MAIN[*]}"
"${COBC_MAIN[@]}"
echo "== ${COBC_UTIL[*]}"
"${COBC_UTIL[@]}"

# ---------------------------------------------------------------------------
# helpers
# ---------------------------------------------------------------------------
# GSIDXUTL reads/writes the sequential side through ddname GSSEQ; the indexed
# side uses the same ddnames as CBTRN02C, so it sees exactly the files the
# program will see.  (DD_GSSEQ for the DD_* mapping, GSSEQ for the COB_FILE_PATH
# mapping used by env/posttran-variant.env.)
load_idx() {  # load_idx <SEQ fixture> <XREF|ACCT|TCAT|TRAN>
  DD_GSSEQ="$1" GSSEQ="$1" "$REPO/$BUILD/GSIDXUTL" LOAD "$2"
}
dump_idx() {  # dump_idx <XREF|ACCT|TCAT|TRAN> <SEQ output>
  DD_GSSEQ="$2" GSSEQ="$2" "$REPO/$BUILD/GSIDXUTL" DUMP "$1"
}

run_one() {   # run_one <set> <env-file> <expected-subdir>
  local set="$1" envfile="$2" outname="$3"
  local in="$SETS_ROOT/$set/input" out="$SETS_ROOT/$set/$outname"
  local work="$WORK_ROOT/$set-$outname"
  rm -rf "$work" "$out"; mkdir -p "$work" "$out"

  export GS_WORK="$work"
  export GS_FROZEN_CLOCK
  GS_FROZEN_CLOCK="$(python3 -c 'import json,sys;print(json.load(open(sys.argv[1]))["frozen_clock"]["COB_CURRENT_DATE"])' "$in/manifest.json")"
  # shellcheck disable=SC1090
  ( set -a; . "$envfile"; set +a
    cp "$in/DALYTRAN" "$work/DALYTRAN"
    load_idx "$in/XREFFILE" XREF >"$work/load.log"
    load_idx "$in/ACCTFILE" ACCT >>"$work/load.log"
    load_idx "$in/TCATBALF" TCAT >>"$work/load.log"
    : > "$work/TRANSACT.seq"; load_idx "$work/TRANSACT.seq" TRAN >>"$work/load.log"   # empty TRANFILE (POSTTRAN.jcl:28)

    rc=0
    "$REPO/$BUILD/CBTRN02C" >"$out/SYSOUT" 2>&1 || rc=$?
    echo "$rc" > "$out/RETURN-CODE"
    # CBTRN02C.cbl:227-231 defines exactly two normal completions: 0 (no rejects)
    # and 4 (at least one reject).  Anything else is an abnormal end (runtime
    # error, CEE3ABD path :707-711) and must never be published as golden output.
    if [ "$rc" -ne 0 ] && [ "$rc" -ne 4 ]; then
      echo "CBTRN02C ended abnormally with status $rc (expected 0 or 4); see $out/SYSOUT; not publishing $out" >&2
      tail -20 "$out/SYSOUT" >&2
      rm -rf "$out"
      exit 70
    fi

    dump_idx TRAN "$out/TRANSACT"  >"$work/dump.log"
    dump_idx ACCT "$out/ACCTFILE"  >>"$work/dump.log"
    dump_idx TCAT "$out/TCATBALF"  >>"$work/dump.log"
    cp "$work/DALYREJS" "$out/DALYREJS"
    cat "$work/load.log" "$work/dump.log" > "$out/GSIDXUTL.log"
    env | grep -E '^(DD_|COB_|COB_FILE_PATH|DALYTRAN=|TRANFILE=|XREFFILE=|DALYREJS=|ACCTFILE=|TCATBALF=)' \
      | sed "s#$REPO/#<repo>/#g" | sort > "$out/env.txt"    # repo-relative: evidence must not depend on one machine
  )
  python3 - "$out" "$in/manifest.json" "$envfile" "$COBC_VERSION" "${COBC_MAIN[*]}" "${COBC_UTIL[*]}" <<'PY'
import json, sys, os, hashlib
out, manifest_path, envfile, ver, cmd_main, cmd_util = sys.argv[1:7]
def rel(p):
    return os.path.relpath(p, os.getcwd())
run = {
    "gnucobol_version": ver,
    "cobc_command_CBTRN02C": cmd_main,
    "cobc_command_GSIDXUTL": cmd_util,
    "env_file": rel(envfile),
    "return_code": int(open(os.path.join(out, "RETURN-CODE")).read().strip()),
    "outputs": {},
}
for name in ("TRANSACT", "DALYREJS", "ACCTFILE", "TCATBALF", "SYSOUT"):
    p = os.path.join(out, name)
    data = open(p, "rb").read()
    run["outputs"][name] = {"bytes": len(data), "sha256": hashlib.sha256(data).hexdigest()}
json.dump(run, open(os.path.join(out, "run.json"), "w"), indent=2)
m = json.load(open(manifest_path))
m.setdefault("reference_runs", {})[os.path.basename(out)] = {
    "gnucobol_version": ver, "cobc_command_CBTRN02C": cmd_main, "cobc_command_GSIDXUTL": cmd_util,
    "env_file": rel(envfile), "return_code": run["return_code"]}
json.dump(m, open(manifest_path, "w"), indent=2); open(manifest_path, "a").write("\n")
PY
  echo "== $set/$outname: RETURN-CODE=$(cat "$out/RETURN-CODE")  $(grep -E 'TRANSACTIONS PROCESSED|TRANSACTIONS REJECTED' "$out/SYSOUT" | tr '\n' ' ')"
  echo "   TRANSACT=$(( $(stat -c %s "$out/TRANSACT") / 350 )) recs  DALYREJS=$(( $(stat -c %s "$out/DALYREJS") / 430 )) recs  ACCTFILE=$(( $(stat -c %s "$out/ACCTFILE") / 300 )) recs  TCATBALF=$(( $(stat -c %s "$out/TCATBALF") / 50 )) recs"
}

# ---------------------------------------------------------------------------
# 2. Generate inputs, 3. run, 4. check generator prediction against the program
# ---------------------------------------------------------------------------
for set in $SETS; do
  in="$SETS_ROOT/$set/input"
  if [ "$KEEP_INPUTS" = 1 ] && [ -f "$in/manifest.json" ]; then
    echo "== $set: keeping existing $in"
  else
    rm -rf "$in"
    python3 "$HERE/generate.py" --set "$set" --out "$in"
  fi
  run_one "$set" "$HERE/env/posttran.env" expected
  if [ "$VARIANT" = 1 ]; then
    run_one "$set" "$HERE/env/posttran-variant.env" expected-variant
  fi
  # The program wins; disagreements are printed and written to prediction-check.json
  # so they can be logged in docs/validation/golden-set/findings.md.  They do not
  # fail the run (the golden set IS the program's output).
  python3 "$HERE/check_prediction.py" "$in/manifest.json" "$SETS_ROOT/$set/expected" \
        --out "$SETS_ROOT/$set/expected/prediction-check.json"
done

rm -rf "$WORK_ROOT"
echo "== done"
