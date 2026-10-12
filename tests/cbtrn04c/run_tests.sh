#!/usr/bin/env bash
# CBTRN04C test harness. Linux + GnuCOBOL (cobc) + python3, nothing else.
#
#   bash tests/cbtrn04c/run_tests.sh            run every case
#   UPDATE_EXPECTED=1 bash tests/cbtrn04c/run_tests.sh
#                                               rewrite the expected files
#                                               from the current program
#
# Steps: quality-gate compile of CBTRN04C (zero diagnostics), syntax check
# of the untouched CBTRN02C, test build (adds -fsign=EBCDIC and the CEE3PRM
# stub), fixture drift check, reference file load, one run per case with
# byte-for-byte comparison of return code, stdout, DALYVALD, DALYRJ04 and
# the control-total report, then a docs/source synchronisation check.
set -u

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
T="$ROOT/tests/cbtrn04c"
BUILD="${CBTRN04C_BUILD_DIR:-$T/build}"
TOOLS="$T/tools"
PY=python3

PASS=0
FAIL=0
FAILED_CASES=()

say()  { printf '%s\n' "$*"; }
step() { printf '\n== %s\n' "$*"; }
die()  { say "FATAL: $*"; exit 1; }

rm -rf "$BUILD"
mkdir -p "$BUILD/bin" "$BUILD/ref" "$BUILD/run"
cd "$ROOT" || exit 1

# ---------------------------------------------------------------- compile
step "Quality gate: cobc -x -std=ibm -I app/cpy app/cbl/CBTRN04C.cbl"
GATE_OUT=$(cobc -x -std=ibm -I app/cpy app/cbl/CBTRN04C.cbl \
           -o "$BUILD/bin/cbtrn04c_gate" 2>&1)
GATE_RC=$?
if [ $GATE_RC -ne 0 ] || [ -n "$GATE_OUT" ]; then
    say "$GATE_OUT"
    die "CBTRN04C gate compile failed (rc=$GATE_RC) or produced diagnostics"
fi
say "OK: compiled with no errors and no warnings"

step "Quality gate: cobc -fsyntax-only -std=ibm -I app/cpy app/cbl/CBTRN02C.cbl"
SYN_OUT=$(cobc -fsyntax-only -std=ibm -I app/cpy app/cbl/CBTRN02C.cbl 2>&1)
SYN_RC=$?
if [ $SYN_RC -ne 0 ]; then
    say "$SYN_OUT"
    die "CBTRN02C syntax check failed (rc=$SYN_RC)"
fi
[ -n "$SYN_OUT" ] && say "$SYN_OUT"
say "OK: CBTRN02C still passes the syntax check"

step "Test build: CBTRN04C + CEE3PRM stub, -fsign=EBCDIC for overpunched signs"
cobc -x -std=ibm -fsign=EBCDIC -I app/cpy \
     app/cbl/CBTRN04C.cbl "$T/stubs/CEE3PRM.cbl" \
     -o "$BUILD/bin/cbtrn04c" || die "test build of CBTRN04C failed"
cobc -x -std=ibm -fsign=EBCDIC -I app/cpy "$TOOLS/LOADIDX.cbl" \
     -o "$BUILD/bin/loadidx" || die "build of LOADIDX failed"
say "OK"

# ---------------------------------------------------------------- fixtures
step "Fixture drift check: regenerate and compare with committed fixtures"
$PY "$TOOLS/gen_fixtures.py" "$BUILD/gen" >/dev/null || die "gen_fixtures failed"
if ! diff -r -x expected -x sample_data "$BUILD/gen/cases" "$T/cases" \
   || ! diff -r "$BUILD/gen/refdata" "$T/refdata"; then
    die "committed fixtures differ from tools/gen_fixtures.py output"
fi
say "OK: committed fixtures match the generator"

# ---------------------------------------------------------------- refdata
load_refset() {   # load_refset <name> <dir with trantype/trancatg/cardxref/acctdata/tcatbal[/transact] .txt>
    local name=$1 src=$2 dst="$BUILD/ref/$1"
    mkdir -p "$dst"
    # transact.txt holds already-posted TRANSACT records (CVTRA05Y); a set
    # without one, such as app/data/ASCII, gets an empty transaction file.
    if [ -f "$src/transact.txt" ]; then
        $PY "$TOOLS/seqfile.py" normalize "$src/transact.txt" "$dst/transact.txt" 350
    else
        : > "$dst/transact.txt"
    fi
    $PY "$TOOLS/seqfile.py" normalize "$src/trantype.txt" "$dst/trantype.txt" 60
    $PY "$TOOLS/seqfile.py" normalize "$src/trancatg.txt" "$dst/trancatg.txt" 60
    $PY "$TOOLS/seqfile.py" normalize "$src/cardxref.txt" "$dst/cardxref.txt" 50
    $PY "$TOOLS/seqfile.py" normalize "$src/acctdata.txt" "$dst/acctdata.txt" 300
    $PY "$TOOLS/seqfile.py" normalize "$src/tcatbal.txt"  "$dst/tcatbal.txt"  50
    DD_LOADIN="$dst/trantype.txt" DD_TRANTYPE="$dst/TRANTYPE.idx" "$BUILD/bin/loadidx" TRANTYPE || die "load $name TRANTYPE"
    DD_LOADIN="$dst/trancatg.txt" DD_TRANCATG="$dst/TRANCATG.idx" "$BUILD/bin/loadidx" TRANCATG || die "load $name TRANCATG"
    DD_LOADIN="$dst/cardxref.txt" DD_XREFFILE="$dst/XREFFILE.idx" "$BUILD/bin/loadidx" XREFFILE || die "load $name XREFFILE"
    DD_LOADIN="$dst/acctdata.txt" DD_ACCTFILE="$dst/ACCTFILE.idx" "$BUILD/bin/loadidx" ACCTFILE || die "load $name ACCTFILE"
    DD_LOADIN="$dst/tcatbal.txt"  DD_TCATBALF="$dst/TCATBALF.idx" "$BUILD/bin/loadidx" TCATBALF || die "load $name TCATBALF"
    DD_LOADIN="$dst/transact.txt" DD_TRANFILE="$dst/TRANFILE.idx" "$BUILD/bin/loadidx" TRANFILE || die "load $name TRANFILE"
}

step "Reference files: standard set (tests/cbtrn04c/refdata/standard)"
load_refset standard "$T/refdata/standard"

SAMPLE_DIR="$ROOT/app/data/ASCII"
HAVE_SAMPLE=0
if [ -f "$SAMPLE_DIR/dailytran.txt" ]; then
    step "Reference files: sample set (app/data/ASCII)"
    load_refset sample "$SAMPLE_DIR"
    mkdir -p "$BUILD/sample"
    $PY "$TOOLS/seqfile.py" txt2seq "$SAMPLE_DIR/dailytran.txt" \
        "$BUILD/sample/dalytran.dat" 350 || die "sample txt2seq failed"
    HAVE_SAMPLE=1
else
    say "app/data/ASCII/dailytran.txt not present; sample_data case skipped"
fi

# ---------------------------------------------------------------- one case
compare_file() {   # compare_file <case> <label> <actual> <expected>
    local case=$1 label=$2 actual=$3 expected=$4
    if [ "${UPDATE_EXPECTED:-0}" = 1 ]; then
        mkdir -p "$(dirname "$expected")"
        if [ -f "$actual" ]; then cp "$actual" "$expected"; else rm -f "$expected"; fi
        return 0
    fi
    if [ -f "$expected" ]; then
        if [ ! -f "$actual" ]; then
            say "   FAIL $label: expected output file missing"; return 1
        fi
        if ! cmp -s "$actual" "$expected"; then
            say "   FAIL $label: differs from expected"
            cmp "$actual" "$expected" | sed 's/^/        /'
            return 1
        fi
    else
        if [ -s "$actual" ]; then
            say "   FAIL $label: output produced but no expected file"; return 1
        fi
    fi
    return 0
}

run_case() {   # run_case <name> <case-dir> <input dalytran.dat> <refset>
    local name=$1 cdir=$2 input=$3 refset=$4
    local work="$BUILD/run/$name" exp="$cdir/expected" ref="$BUILD/ref/$refset"
    local ok=1 rc
    mkdir -p "$work" "$exp"

    if [ -f "$cdir/parm" ]; then
        export CBTRN04C_PARM
        CBTRN04C_PARM=$(tr -d '\r\n' < "$cdir/parm")
    else
        unset CBTRN04C_PARM
    fi
    local tranfile="$ref/TRANFILE.idx"
    [ -f "$cdir/no_tranfile" ] && tranfile="$work/absent/TRANFILE.idx"
    DD_DALYTRAN="$input" \
    DD_TRANTYPE="$ref/TRANTYPE.idx" DD_TRANCATG="$ref/TRANCATG.idx" \
    DD_XREFFILE="$ref/XREFFILE.idx" DD_ACCTFILE="$ref/ACCTFILE.idx" \
    DD_TCATBALF="$ref/TCATBALF.idx" DD_TRANFILE="$tranfile" \
    DD_DALYVALD="$work/dalyvald.dat" DD_DALYRJ04="$work/dalyrj04.dat" \
    DD_VALDRPT="$work/valdrpt.dat" \
        "$BUILD/bin/cbtrn04c" > "$work/stdout.txt" 2>&1
    rc=$?
    printf '%s\n' "$rc" > "$work/rc"
    if [ -f "$work/valdrpt.dat" ]; then
        $PY "$TOOLS/seqfile.py" seq2txt "$work/valdrpt.dat" "$work/valdrpt.txt" 133 \
            || { say "   FAIL report is not a whole number of 133-byte records"; ok=0; }
    fi
    if [ -f "$work/dalyrj04.dat" ]; then
        local sz; sz=$(stat -c %s "$work/dalyrj04.dat")
        [ $((sz % 430)) -eq 0 ] || { say "   FAIL DALYRJ04 size $sz not a multiple of 430"; ok=0; }
    fi
    if [ -f "$work/dalyvald.dat" ]; then
        local sz; sz=$(stat -c %s "$work/dalyvald.dat")
        [ $((sz % 350)) -eq 0 ] || { say "   FAIL DALYVALD size $sz not a multiple of 350"; ok=0; }
    fi

    compare_file "$name" "rc"       "$work/rc"         "$exp/rc"          || ok=0
    compare_file "$name" "stdout"   "$work/stdout.txt" "$exp/stdout.txt"  || ok=0
    compare_file "$name" "VALDRPT"  "$work/valdrpt.txt" "$exp/valdrpt.txt" || ok=0
    compare_file "$name" "DALYRJ04" "$work/dalyrj04.dat" "$exp/dalyrj04.dat" || ok=0
    compare_file "$name" "DALYVALD" "$work/dalyvald.dat" "$exp/dalyvald.dat" || ok=0

    if [ $ok -eq 1 ]; then
        say "PASS  $name (rc=$rc)"; PASS=$((PASS + 1))
    else
        say "FAIL  $name (rc=$rc)"; FAIL=$((FAIL + 1)); FAILED_CASES+=("$name")
    fi
}

step "Cases"
for cdir in "$T"/cases/*/; do
    cdir=${cdir%/}
    [ "$(basename "$cdir")" = sample_data ] && continue
    run_case "$(basename "$cdir")" "$cdir" "$cdir/dalytran.dat" standard
done
if [ $HAVE_SAMPLE -eq 1 ]; then
    run_case sample_data "$T/cases/sample_data" "$BUILD/sample/dalytran.dat" sample
fi

# ---------------------------------------------------------------- docs sync
step "Docs/source synchronisation check"
if $PY "$TOOLS/check_docs_sync.py"; then
    say "OK"
else
    FAIL=$((FAIL + 1)); FAILED_CASES+=("check_docs_sync")
fi

# ---------------------------------------------------------------- summary
step "Summary"
say "cases passed: $PASS"
say "cases failed: $FAIL"
if [ $FAIL -ne 0 ]; then
    say "failed: ${FAILED_CASES[*]}"
    exit 1
fi
say "ALL TESTS PASSED"
exit 0
