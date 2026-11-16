#!/usr/bin/env bash
# GnuCOBOL golden-master harness for the CardDemo batch jobs.
#
#   golden-master/run.sh            regenerate golden-master/baselines/
#   golden-master/run.sh --check    rerun and fail if the output differs from the committed baselines
#
# Runs POSTTRAN -> INTCALC -> COMBTRAN -> TRANREPT -> CREASTMT on app/data/ASCII
# with a frozen clock. See golden-master/README.md.
set -euo pipefail

GM_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$GM_DIR/.." && pwd)"
CBL="$REPO_DIR/app/cbl"
DATA="$REPO_DIR/app/data/ASCII"

MODE=generate
case "${1:-}" in
  "") ;;
  --check) MODE=check ;;
  -h|--help) sed -n '2,9p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
  *) echo "unknown option: $1" >&2; exit 2 ;;
esac

# Batch run date. The TRANREPT JCL reports 2022-01-01..2022-07-06 on TRAN-PROC-TS,
# which CBTRN02C and CBACT04C stamp from FUNCTION CURRENT-DATE.
RUN_CLOCK="2022/07/06 12:00:00.00"
INTCALC_PARM="2022071800"
TRANREPT_START="2022-01-01"
TRANREPT_END="2022-07-06"

command -v cobc >/dev/null || { echo "cobc (GnuCOBOL 3.x) not found" >&2; exit 1; }

WORK="$GM_DIR/.work"
BIN="$WORK/bin"
SRC="$WORK/src"
RUN="$WORK/run"
if [[ $MODE == check ]]; then OUT="$WORK/baselines"; else OUT="$GM_DIR/baselines"; fi
rm -rf "$WORK"
mkdir -p "$BIN" "$SRC/cpy" "$RUN"
rm -rf "$OUT"
mkdir -p "$OUT"

export LC_ALL=C
export COB_CURRENT_DATE="$RUN_CLOCK"
export COB_FILE_PATH="$RUN"
export COB_LIBRARY_PATH="$BIN"
unset COB_PRE_LOAD

COBC_FLAGS=(-std=ibm -fsign=EBCDIC -I "$SRC/cpy")

log() { printf '%s\n' "$*" >&2; }

# ---------------------------------------------------------------- build
# Copybooks: CUSTREC.cpy indents its fields with tabs that only line up in
# area B at a tab width of 3 (two tabs + 5 spaces = column 12, like CUST-ID).
# GnuCOBOL expands tabs to 8 and pushes the PICTUREs past column 72, so the
# build copy expands tabs at width 3.
for f in "$REPO_DIR"/app/cpy/*; do expand -t 3 "$f" > "$SRC/cpy/$(basename "$f")"; done

# CBSTM03A walks the z/OS PSA -> TCB -> TIOT control blocks to DISPLAY the job
# name and DD names. Those blocks do not exist off the mainframe, so that
# diagnostic block (from SET ADDRESS OF PSA-BLOCK up to the OPEN of the
# statement files) is removed from the build copy. Nothing else is changed.
awk '/SET ADDRESS OF PSA-BLOCK/ {skip=1}
     /OPEN OUTPUT STMT-FILE HTML-FILE/ {skip=0}
     !skip' "$CBL/CBSTM03A.CBL" > "$SRC/CBSTM03A.CBL"
grep -q 'PSA-BLOCK   TO' "$SRC/CBSTM03A.CBL" && { log "CBSTM03A patch failed"; exit 1; }

compile() { # out-name sources...
  local out=$1; shift
  cobc -x "${COBC_FLAGS[@]}" -o "$BIN/$out" "$@" 2>>"$WORK/compile.log" \
    || { cat "$WORK/compile.log" >&2; exit 1; }
}

ksdsutil() { # name reclen keylen [altpos altlen]
  local name=$1 reclen=$2 keylen=$3 altpos=${4:-} altlen=${5:-}
  local src="$SRC/$name.cbl"
  sed -e "s/@PGM@/$name/g" -e "s/@RECLEN@/$reclen/g" -e "s/@KEYLEN@/$keylen/g" \
      -e "s/@RESTLEN@/$((reclen - keylen))/g" \
      -e "s/@ALTPOS@/${altpos:-1}/g" -e "s/@ALTLEN@/${altlen:-1}/g" \
      "$GM_DIR/src/KSDSUTIL.cbl.in" > "$src"
  [[ -z $altpos ]] && sed -i '/*>ALTKEY$/d' "$src"
  compile "$name" "$src"
}

log "compiling with $(cobc --version | head -1)"
: > "$WORK/compile.log"
# KSDS layouts (record length, primary key length, alternate key) from the FDs.
ksdsutil KSACCT  300 11
ksdsutil KSXREF   50 16 25 11   # alternate key XREF-ACCT-ID (CBACT04C, AIX path)
ksdsutil KSCUST  500  9
ksdsutil KSDISC   50 16
ksdsutil KSTCAT   50 17
ksdsutil KSTRCAT  60  6
ksdsutil KSTRTYP  60  2
ksdsutil KSTRAN  350 16
ksdsutil KSTRXFL 350 32
compile CBTRN02C "$CBL/CBTRN02C.cbl" "$GM_DIR/src/CEE3ABD.cbl"
compile CBACT04C "$GM_DIR/src/PARMDRV.cbl" "$CBL/CBACT04C.cbl" "$GM_DIR/src/CEE3ABD.cbl"
compile CBTRN03C "$CBL/CBTRN03C.cbl" "$GM_DIR/src/CEE3ABD.cbl"
compile CBSTM03A "$SRC/CBSTM03A.CBL" "$CBL/CBSTM03B.CBL" "$GM_DIR/src/CEE3ABD.cbl"

# ---------------------------------------------------------------- helpers
# Baselines and inputs are text, one record per line, padded to the record
# length. The programs read and write fixed-length records without newlines.
to_fixed() { # reclen < lines > records
  awk -v n="$1" '{ sub(/\r$/, ""); if (length($0) > n) { print "record longer than " n ": " NR > "/dev/stderr"; exit 1 }
                   printf "%-" n "s", $0 }'
}
to_lines() { # reclen < records > lines
  local n=$1 f size
  f=$(mktemp "$WORK/rec.XXXXXX"); cat > "$f"; size=$(wc -c < "$f")
  (( size % n == 0 )) || { log "record file size $size is not a multiple of $n"; exit 1; }
  if (( size > 0 )); then fold -b -w "$n" "$f"; echo; fi
  rm -f "$f"
}

load()   { local util=$1 reclen=$2 src=$3 ksds=$4  # lines file -> indexed file
  to_fixed "$reclen" < "$src" > "$RUN/load.seq"
  DD_SEQFILE="$RUN/load.seq" DD_KSDSFILE="$RUN/$ksds" "$BIN/$util" LOAD
  rm -f "$RUN/load.seq"
}
unload() { local util=$1 reclen=$2 ksds=$3 dst=$4  # indexed file -> lines file
  DD_SEQFILE="$RUN/unload.seq" DD_KSDSFILE="$RUN/$ksds" "$BIN/$util" UNLOAD
  to_lines "$reclen" < "$RUN/unload.seq" > "$dst"
  rm -f "$RUN/unload.seq"
}

declare -A RC
run_step() { # job program args... ; env DD_* set by caller; SYSOUT -> baselines/<job>/sysout.txt
  local job=$1 pgm=$2; shift 2
  local rc=0
  ( cd "$RUN" && "$BIN/$pgm" "$@" ) > "$OUT/$job/sysout.txt" 2>&1 || rc=$?
  RC[$job]=$rc
  log "$job ($pgm) RC=$rc"
  (( rc <= 4 )) || { cat "$OUT/$job/sysout.txt" >&2; log "$job failed"; exit 1; }
}

STATE="$WORK/state"
mkdir -p "$STATE"

# ---------------------------------------------------------------- POSTTRAN
# CBTRN02C: post DALYTRAN to TRANSACT, ACCTDATA, TCATBALF; rejects to DALYREJS.
J=POSTTRAN; mkdir -p "$OUT/$J"
load KSXREF 50  "$DATA/cardxref.txt" xref.ksds
load KSACCT 300 "$DATA/acctdata.txt" acct.ksds
load KSTCAT 50  "$DATA/tcatbal.txt"  tcat.ksds
to_fixed 350 < "$DATA/dailytran.txt" > "$RUN/dalytran.seq"
rm -f "$RUN/transact.ksds"
DD_DALYTRAN="$RUN/dalytran.seq" DD_TRANFILE="$RUN/transact.ksds" DD_XREFFILE="$RUN/xref.ksds" \
DD_DALYREJS="$RUN/dalyrejs.seq" DD_ACCTFILE="$RUN/acct.ksds" DD_TCATBALF="$RUN/tcat.ksds" \
  run_step $J CBTRN02C
unload KSTRAN 350 transact.ksds "$OUT/$J/transact.txt"
unload KSACCT 300 acct.ksds     "$OUT/$J/acctdata.txt"
unload KSTCAT 50  tcat.ksds     "$OUT/$J/tcatbal.txt"
to_lines 430 < "$RUN/dalyrejs.seq" > "$OUT/$J/dalyrejs.txt"
cp "$OUT/$J/transact.txt" "$STATE/transact.txt"
cp "$OUT/$J/acctdata.txt" "$STATE/acctdata.txt"
cp "$OUT/$J/tcatbal.txt"  "$STATE/tcatbal.txt"

# ---------------------------------------------------------------- INTCALC
# CBACT04C PARM='2022071800': interest per TCATBALF entry -> SYSTRAN, ACCTDATA.
J=INTCALC; mkdir -p "$OUT/$J"
load KSTCAT 50  "$STATE/tcatbal.txt"  tcat.ksds
load KSXREF 50  "$DATA/cardxref.txt"  xref.ksds
load KSACCT 300 "$STATE/acctdata.txt" acct.ksds
load KSDISC 50  "$DATA/discgrp.txt"   disc.ksds
DD_TCATBALF="$RUN/tcat.ksds" DD_XREFFILE="$RUN/xref.ksds" DD_ACCTFILE="$RUN/acct.ksds" \
DD_DISCGRP="$RUN/disc.ksds" DD_TRANSACT="$RUN/systran.seq" \
  run_step $J CBACT04C CBACT04C "$INTCALC_PARM"
unload KSACCT 300 acct.ksds "$OUT/$J/acctdata.txt"
to_lines 350 < "$RUN/systran.seq" > "$OUT/$J/systran.txt"
cp "$OUT/$J/acctdata.txt" "$STATE/acctdata.txt"

# ---------------------------------------------------------------- COMBTRAN
# SORT TRANSACT backup + SYSTRAN on TRAN-ID, REPRO into TRANSACT KSDS.
J=COMBTRAN; mkdir -p "$OUT/$J"
cat "$STATE/transact.txt" "$OUT/INTCALC/systran.txt" | sort -s -t $'\x01' -k1.1,1.16 > "$WORK/combined.txt"
load KSTRAN 350 "$WORK/combined.txt" transact.ksds
unload KSTRAN 350 transact.ksds "$OUT/$J/transact.txt"
RC[$J]=0; log "$J (SORT/REPRO) RC=0"
cp "$OUT/$J/transact.txt" "$STATE/transact.txt"

# ---------------------------------------------------------------- TRANREPT
# REPRO TRANSACT -> BKUP; SORT INCLUDE TRAN-PROC-DT in [start,end] on TRAN-CARD-NUM;
# CBTRN03C prints the report.
J=TRANREPT; mkdir -p "$OUT/$J"
awk -v s="$TRANREPT_START" -v e="$TRANREPT_END" '{ d = substr($0, 305, 10); if (d >= s && d <= e) print }' \
  "$STATE/transact.txt" | sort -s -t $'\x01' -k1.263,1.278 > "$OUT/$J/transact-daly.txt"
to_fixed 350 < "$OUT/$J/transact-daly.txt" > "$RUN/tranfile.seq"
printf '%s %s\n' "$TRANREPT_START" "$TRANREPT_END" | to_fixed 80 > "$RUN/dateparm.seq"
load KSXREF  50 "$DATA/cardxref.txt" xref.ksds
load KSTRTYP 60 "$DATA/trantype.txt" trantype.ksds
load KSTRCAT 60 "$DATA/trancatg.txt" trancatg.ksds
DD_TRANFILE="$RUN/tranfile.seq" DD_CARDXREF="$RUN/xref.ksds" DD_TRANTYPE="$RUN/trantype.ksds" \
DD_TRANCATG="$RUN/trancatg.ksds" DD_DATEPARM="$RUN/dateparm.seq" DD_TRANREPT="$RUN/tranrept.seq" \
  run_step $J CBTRN03C
to_lines 133 < "$RUN/tranrept.seq" > "$OUT/$J/tranrept.txt"

# ---------------------------------------------------------------- CREASTMT
# SORT TRANSACT on card+tran id, OUTREC (1:263,16,17:1,262,279:279,50) -> TRXFL KSDS;
# CBSTM03A/CBSTM03B write the text and HTML statements.
J=CREASTMT; mkdir -p "$OUT/$J"
awk '{ printf "%-350s\n", substr($0, 263, 16) substr($0, 1, 262) substr($0, 279, 50) }' "$STATE/transact.txt" \
  | sort -s -t $'\x01' -k1.1,1.32 > "$WORK/trxfl.txt"
load KSTRXFL 350 "$WORK/trxfl.txt"         trxfl.ksds
load KSXREF   50 "$DATA/cardxref.txt"      xref.ksds
load KSACCT  300 "$STATE/acctdata.txt"     acct.ksds
# The statements print customer names and addresses. The sample customers are
# synthetic but look like real people, so the harness replaces those fields with
# placeholders keyed on CUST-ID before the load. Each placeholder keeps an
# inner space, so the statement STRING ... DELIMITED BY ' ' truncation still applies.
awk '{ sub(/\r$/, ""); id = substr($0, 1, 9)
       printf "%-500s\n", id sprintf("%-25s%-25s%-25s%-50s%-50s%-50s",
         "F" id " FIRST", "M" id " MIDDLE", "L" id " LAST",
         id " MAIN STREET", "UNIT " id, "C" id " CITY") substr($0, 235) }' \
  "$DATA/custdata.txt" > "$WORK/custdata-masked.txt"
load KSCUST  500 "$WORK/custdata-masked.txt" cust.ksds
DD_TRNXFILE="$RUN/trxfl.ksds" DD_XREFFILE="$RUN/xref.ksds" DD_ACCTFILE="$RUN/acct.ksds" \
DD_CUSTFILE="$RUN/cust.ksds" DD_STMTFILE="$RUN/statemnt.seq" DD_HTMLFILE="$RUN/statemnt.html.seq" \
  run_step $J CBSTM03A
unload KSTRXFL 350 trxfl.ksds "$OUT/$J/trxfl.txt"
to_lines 80  < "$RUN/statemnt.seq"      > "$OUT/$J/statemnt.txt"
to_lines 100 < "$RUN/statemnt.html.seq" > "$OUT/$J/statemnt.html"

# ---------------------------------------------------------------- manifest
{
  echo "# CardDemo batch golden master (GnuCOBOL). Generated by golden-master/run.sh; do not edit."
  echo "# clock=$RUN_CLOCK intcalc_parm=$INTCALC_PARM tranrept=$TRANREPT_START..$TRANREPT_END"
  echo "# job       return-code"
  for j in POSTTRAN INTCALC COMBTRAN TRANREPT CREASTMT; do printf '%-10s  %s\n' "$j" "${RC[$j]}"; done
  echo "# file                                   records  sha256"
  ( cd "$OUT" && find . -type f ! -name MANIFEST.txt | sed 's|^\./||' | sort | while read -r f; do
      printf '%-40s %7s  %s\n' "$f" "$(wc -l < "$f")" "$(sha256sum "$f" | cut -d' ' -f1)"
    done )
} > "$OUT/MANIFEST.txt"

if [[ $MODE == check ]]; then
  if diff -r "$GM_DIR/baselines" "$OUT" > "$WORK/check.diff"; then
    log "golden master matches committed baselines"
  else
    head -200 "$WORK/check.diff" >&2
    log "golden master differs from committed baselines (full diff: $WORK/check.diff)"
    exit 1
  fi
else
  log "baselines written to ${OUT#$REPO_DIR/}"
fi
