#!/usr/bin/env bash
# GnuCOBOL golden-master run of POSTTRAN (app/jcl/POSTTRAN.jcl STEP15 -> CBTRN02C).
# Reads app/** only (never modifies it). Writes everything under $OUT (default ${TMPDIR:-/tmp}/carddemo-posttran-oracle),
# which must not exist or be empty: the script never deletes anything.
# Usage: .migration/tools/posttran_gnucobol_oracle.sh [OUT_DIR] [DALYTRAN_ASCII_FILE]
# Env:   ORACLE_NOW  clock pinned via GnuCOBOL COB_CURRENT_DATE for CBTRN02C's TRAN-PROC-TS
#        (FUNCTION CURRENT-DATE, CBTRN02C.cbl:693); default '2022/07/18 10:30:45'; empty = real clock.
set -euo pipefail
REPO=$(cd "$(dirname "$0")/../.." && pwd)
OUT=${1:-${TMPDIR:-/tmp}/carddemo-posttran-oracle}
DALY=${2:-$REPO/app/data/ASCII/dailytran.txt}
ORACLE_NOW=${ORACLE_NOW-2022/07/18 10:30:45}
# resolve before the cd below so relative arguments work from any directory
OUT=$(realpath -m -- "$OUT")
DALY=$(realpath -e -- "$DALY")
if [ -e "$OUT" ] && [ -n "$(ls -A "$OUT" 2>/dev/null)" ]; then
  echo "refusing to write into non-empty OUT_DIR '$OUT' (pass a new or empty directory; nothing was deleted)" >&2; exit 2
fi
mkdir -p "$OUT/bin" "$OUT/data" "$OUT/expected"
cd "$OUT/bin"

# CEE3ABD stub: LE abend -> process exit with ABCODE (999 -> RC 231 on Linux, 999 mod 256)
cat > CEE3ABD.cbl <<'COB'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CEE3ABD.
       DATA DIVISION.
       LINKAGE SECTION.
       01 ABCODE PIC S9(9) BINARY.
       01 TIMING PIC S9(9) BINARY.
       PROCEDURE DIVISION USING ABCODE TIMING.
           DISPLAY 'CEE3ABD ABCODE=' ABCODE
           STOP RUN RETURNING ABCODE.
COB

# Generic line-sequential <-> indexed converters (key at offset 0)
gen() { # name mode reclen keylen
  local in_org out_org
  local nl=$'\n                  '
  if [ "$2" = load ]; then in_org="LINE SEQUENTIAL"; out_org="INDEXED${nl}ACCESS SEQUENTIAL${nl}RECORD KEY IS O-KEY"
  else in_org="INDEXED${nl}ACCESS SEQUENTIAL${nl}RECORD KEY IS I-KEY"; out_org="LINE SEQUENTIAL"; fi
  cat > "$1.cbl" <<COB
       IDENTIFICATION DIVISION.
       PROGRAM-ID. $1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IFILE ASSIGN TO INFILE
                  ORGANIZATION $in_org
                  FILE STATUS IS IST.
           SELECT OFILE ASSIGN TO OUTFILE
                  ORGANIZATION $out_org
                  FILE STATUS IS OST.
       DATA DIVISION.
       FILE SECTION.
       FD IFILE.
       01 IREC.
          05 I-KEY PIC X($4).
          05 FILLER PIC X($(( $3 - $4 ))).
       FD OFILE.
       01 OREC.
          05 O-KEY PIC X($4).
          05 FILLER PIC X($(( $3 - $4 ))).
       WORKING-STORAGE SECTION.
       01 IST PIC XX.
       01 OST PIC XX.
       PROCEDURE DIVISION.
           OPEN INPUT IFILE OUTPUT OFILE
           PERFORM UNTIL IST NOT = '00'
              READ IFILE
                 AT END CONTINUE
                 NOT AT END
                    MOVE IREC TO OREC
                    WRITE OREC
                    IF OST NOT = '00'
                       DISPLAY '$1 WRITE ' OST
                       STOP RUN RETURNING 16
                    END-IF
              END-READ
           END-PERFORM
           CLOSE IFILE OFILE
           STOP RUN.
COB
  cobc -x -std=ibm "$1.cbl" -o "$1"
}
gen LDACCT load 300 11; gen LDXREF load 50 16; gen LDTCAT load 50 17
gen ULACCT unload 300 11; gen ULTRAN unload 350 16; gen ULTCAT unload 50 17

cobc -m -std=ibm CEE3ABD.cbl
# -fsign=EBCDIC: sample ASCII files carry EBCDIC-style zoned overpunch ('{', 'A'-'I', '}', 'J'-'R')
cobc -x -std=ibm -fsign=EBCDIC -I "$REPO/app/cpy" "$REPO/app/cbl/CBTRN02C.cbl" -o CBTRN02C

# Normalize sample data: strip CR, right-pad to LRECL
pad() { tr -d '\r' < "$1" | awk -v n="$2" '{printf "%-" n "s\n", $0}'; }
pad "$REPO/app/data/ASCII/acctdata.txt" 300 > "$OUT/data/acctdata.lseq"
pad "$REPO/app/data/ASCII/cardxref.txt" 50  > "$OUT/data/cardxref.lseq"
pad "$REPO/app/data/ASCII/tcatbal.txt"  50  > "$OUT/data/tcatbal.lseq"
# DALYTRAN is RECFM=FB LRECL=350 sequential: fixed records, no newline
pad "$DALY" 350 | tr -d '\n' > "$OUT/data/DALYTRAN"

load() { INFILE=$2 OUTFILE=$3 ./"$1"; }
load LDACCT "$OUT/data/acctdata.lseq" "$OUT/data/ACCTDATA"
load LDXREF "$OUT/data/cardxref.lseq" "$OUT/data/CARDXREF"
load LDTCAT "$OUT/data/tcatbal.lseq"  "$OUT/data/TCATBALF"

export COB_LIBRARY_PATH="$OUT/bin"
export DD_DALYTRAN="$OUT/data/DALYTRAN" DD_TRANFILE="$OUT/data/TRANSACT" DD_XREFFILE="$OUT/data/CARDXREF"
export DD_DALYREJS="$OUT/expected/DALYREJS" DD_ACCTFILE="$OUT/data/ACCTDATA" DD_TCATBALF="$OUT/data/TCATBALF"
set +e
if [ -n "$ORACLE_NOW" ]; then COB_CURRENT_DATE="$ORACLE_NOW" ./CBTRN02C > "$OUT/expected/SYSOUT" 2>&1
else ./CBTRN02C > "$OUT/expected/SYSOUT" 2>&1; fi
RC=$?
set -e
echo "$RC" > "$OUT/expected/RETURN-CODE"
INFILE="$OUT/data/TRANSACT" OUTFILE="$OUT/expected/TRANSACT.unl" ./ULTRAN || true
INFILE="$OUT/data/ACCTDATA" OUTFILE="$OUT/expected/ACCTDATA.unl" ./ULACCT
INFILE="$OUT/data/TCATBALF" OUTFILE="$OUT/expected/TCATBALF.unl" ./ULTCAT
echo "RC=$RC"; tail -5 "$OUT/expected/SYSOUT"
(cd "$OUT/expected" && wc -c DALYREJS && wc -l TRANSACT.unl ACCTDATA.unl TCATBALF.unl && sha256sum *)
