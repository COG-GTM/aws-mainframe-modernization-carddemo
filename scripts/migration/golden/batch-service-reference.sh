#!/usr/bin/env bash
# Reference outputs for the batch-service golden sets (UNT149-15): runs the unmodified COBOL of
# CBSTM03A/B, CBEXPORT, CBIMPORT, CBACT01C, CBACT02C, CBACT03C and CBCUS01C under GnuCOBOL on the
# app/data sample files (plus the edge-case inputs committed next to the expected outputs) and writes
# what they produce to modernization/batch-service/src/test/resources/golden/<SET>/expected/.
# The Java tests (GoldenSetTest) run the Java jobs on the same inputs and compare byte for byte.
#
# Compile shims (temporary copies, app/ is never modified; docs/migration/scope.md §5):
#   CBSTM03A  CUSTREC.cpy tabs expanded; the TIOT walk (DISPLAY-only, dereferences z/OS control
#             blocks from a NULL pointer) removed.
#   CBEXPORT/CBIMPORT  FD key field at offset 27 for RECORD KEY EXPORT-SEQUENCE-NUM.
#   COBDATFT  the Assembler routine CBACT01C calls, re-expressed in COBOL byte for byte.
# Needs: cobc (GnuCOBOL 3.x with an indexed handler), faketime, python3.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
GOLDEN="$ROOT/modernization/batch-service/src/test/resources/golden"
DATA="$ROOT/app/data/ASCII"
NOW="${GOLDEN_NOW:-2022-07-18 10:30:45}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
mkdir -p "$WORK/cpy" "$WORK/cbl" "$WORK/bin"
export TZ=UTC COB_LIBRARY_PATH="$WORK/bin"
COBC=(cobc -std=ibm -fsign=EBCDIC -I "$WORK/cpy" -I "$ROOT/app/cpy")

# ---- shims ------------------------------------------------------------------------------------
expand -t 1 "$ROOT/app/cpy/CUSTREC.cpy" > "$WORK/cpy/CUSTREC.cpy"
sed -e '/SET ADDRESS OF PSA-BLOCK/,/OPEN OUTPUT STMT-FILE/{/OPEN OUTPUT STMT-FILE/!d}' \
    "$ROOT/app/cbl/CBSTM03A.CBL" > "$WORK/cbl/CBSTM03A.cbl"
for spec in CBEXPORT:EXPORT-OUTPUT-RECORD CBIMPORT:EXPORT-INPUT-RECORD; do
  pgm="${spec%%:*}" rec="${spec#*:}"
  sed -e "s/RECORD KEY IS EXPORT-SEQUENCE-NUM/RECORD KEY IS SHIM-EXP-SEQ-KEY/" \
      -e "s/^       01  $rec  *PIC X(500)\./       01  $rec.\n           05  FILLER PIC X(27).\n           05  SHIM-EXP-SEQ-KEY PIC 9(9) COMP.\n           05  FILLER PIC X(469)./" \
      "$ROOT/app/cbl/$pgm.cbl" > "$WORK/cbl/$pgm.cbl"
done
cat > "$WORK/cbl/COBDATFT.cbl" <<'COB'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBDATFT.
      * app/asm/COBDATFT.asm in COBOL: the same byte moves, same checks.
       DATA DIVISION.
       LINKAGE SECTION.
       COPY CODATECN.
       PROCEDURE DIVISION USING CODATECN-REC.
           EVALUATE CODATECN-TYPE
             WHEN '1'
               IF CODATECN-INP-DATE(5:1) = '-' OR CODATECN-OUTTYPE = '2'
                 MOVE 'INVALID INPUT' TO CODATECN-ERROR-MSG(1:13)
               ELSE
                 MOVE CODATECN-INP-DATE(1:4) TO CODATECN-0UT-DATE(1:4)
                 MOVE '-' TO CODATECN-0UT-DATE(5:1)
                 MOVE CODATECN-INP-DATE(5:2) TO CODATECN-0UT-DATE(6:2)
                 MOVE '-' TO CODATECN-0UT-DATE(8:1)
                 MOVE CODATECN-INP-DATE(7:2) TO CODATECN-0UT-DATE(9:2)
               END-IF
             WHEN '2'
               IF CODATECN-OUTTYPE = '1'
                 MOVE 'INVALID INPUT' TO CODATECN-ERROR-MSG(1:13)
               ELSE
                 MOVE CODATECN-INP-DATE(1:4) TO CODATECN-0UT-DATE(1:4)
                 MOVE CODATECN-INP-DATE(6:2) TO CODATECN-0UT-DATE(5:2)
                 MOVE CODATECN-INP-DATE(9:2) TO CODATECN-0UT-DATE(7:2)
               END-IF
             WHEN OTHER
               MOVE 'INVALID INPUT' TO CODATECN-ERROR-MSG(1:13)
           END-EVALUATE
           GOBACK.
COB

# ---- KSDS loader: one record file (line sequential or fixed) into a GnuCOBOL indexed file ------
# loader NAME LRECL KEYOFF KEYLEN LINE|FIXED
loader() {
  local name="$1" len="$2" off="$3" klen="$4" org="$5" pre="" rest
  rest=$((len - off - klen))
  [ "$off" -gt 0 ] && pre="           05  FILLER PIC X($off)."
  local post=""; [ "$rest" -gt 0 ] && post="           05  FILLER PIC X($rest)."
  local inorg="LINE SEQUENTIAL"; [ "$org" = FIXED ] && inorg="SEQUENTIAL"
  cat > "$WORK/cbl/$name.cbl" <<COB
       IDENTIFICATION DIVISION.
       PROGRAM-ID. $name.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO INFILE
               ORGANIZATION IS $inorg.
           SELECT OUT-FILE ASSIGN TO OUTFILE ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS OUT-KEY
               FILE STATUS IS OUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC PIC X($len).
       FD  OUT-FILE.
       01  OUT-REC.
$pre
           05  OUT-KEY PIC X($klen).
$post
       WORKING-STORAGE SECTION.
       01  OUT-STATUS PIC XX.
       01  EOF PIC X VALUE 'N'.
       PROCEDURE DIVISION.
           OPEN INPUT IN-FILE OUTPUT OUT-FILE
           PERFORM UNTIL EOF = 'Y'
             READ IN-FILE
               AT END MOVE 'Y' TO EOF
               NOT AT END
                 MOVE IN-REC TO OUT-REC
                 WRITE OUT-REC
                 IF OUT-STATUS NOT = '00'
                   DISPLAY 'LOAD FAILED ' OUT-STATUS UPON SYSERR
                   STOP RUN RETURNING 8
                 END-IF
             END-READ
           END-PERFORM
           CLOSE IN-FILE OUT-FILE
           STOP RUN.
COB
  "${COBC[@]}" -x -o "$WORK/bin/$name" "$WORK/cbl/$name.cbl"
}
loader LDCUST 500 0 9 LINE
loader LDACCT 300 0 11 LINE
loader LDXREF 50 0 16 LINE
loader LDCARD 150 0 16 LINE
loader LDTRAN 350 0 16 LINE
loader LDTRNX 350 0 32 FIXED
loader LDEXPT 500 27 4 FIXED

ksds() { DD_INFILE="$2" DD_OUTFILE="$3" "$WORK/bin/$1"; }

# ---- programs ---------------------------------------------------------------------------------
"${COBC[@]}" -m -o "$WORK/bin/COBDATFT.so" "$WORK/cbl/COBDATFT.cbl"
"${COBC[@]}" -m -o "$WORK/bin/CBSTM03B.so" "$ROOT/app/cbl/CBSTM03B.CBL"
for pgm in CBACT01C CBACT02C CBACT03C CBCUS01C; do
  "${COBC[@]}" -x -o "$WORK/bin/$pgm" "$ROOT/app/cbl/$pgm.cbl"
done
for pgm in CBEXPORT CBIMPORT CBSTM03A; do
  "${COBC[@]}" -x -o "$WORK/bin/$pgm" "$WORK/cbl/$pgm.cbl"
done

run() { (cd "$1" && faketime -f "$NOW" "${@:2}"); }
fresh() { rm -rf "$GOLDEN/$1/expected"; mkdir -p "$GOLDEN/$1/expected"; echo "$GOLDEN/$1/expected"; }

# Sample-data KSDS files, as the IDCAMS REPRO of ACCTFILE/CARDFILE/... jobs load them.
K="$WORK/ksds"; mkdir -p "$K"
ksds LDCUST "$DATA/custdata.txt" "$K/CUSTFILE"
ksds LDACCT "$DATA/acctdata.txt" "$K/ACCTFILE"
ksds LDXREF "$DATA/cardxref.txt" "$K/XREFFILE"
ksds LDCARD "$DATA/carddata.txt" "$K/CARDFILE"
# TRANSACT: the DALYTRAN sample records (CVTRA06Y has the CVTRA05Y layout) as the posted file.
ksds LDTRAN "$DATA/dailytran.txt" "$K/TRANSACT"

rc() { local r=0; "$@" || r=$?; echo "$r" > "$OUT/RETURN-CODE"; }

# READACCT / READCARD / READXREF / READCUST
OUT=$(fresh READACCT); rc run "$OUT" env DD_ACCTFILE="$K/ACCTFILE" DD_OUTFILE=OUTFILE DD_ARRYFILE=ARRYFILE \
    DD_VBRCFILE=VBRCFILE "$WORK/bin/CBACT01C" > "$OUT/SYSOUT"
OUT=$(fresh READCARD); rc run "$OUT" env DD_CARDFILE="$K/CARDFILE" "$WORK/bin/CBACT02C" > "$OUT/SYSOUT"
OUT=$(fresh READXREF); rc run "$OUT" env DD_XREFFILE="$K/XREFFILE" "$WORK/bin/CBACT03C" > "$OUT/SYSOUT"
OUT=$(fresh READCUST); rc run "$OUT" env DD_CUSTFILE="$K/CUSTFILE" "$WORK/bin/CBCUS01C" > "$OUT/SYSOUT"

# READACCT edge set: non-zero ACCT-CURR-CYC-DEBIT records (OUT-ACCT-CURR-CYC-DEBIT is not moved).
E="$GOLDEN/READACCT-EDGE"
ksds LDACCT "$E/input/acctdata.txt" "$K/ACCTEDGE"
OUT=$(fresh READACCT-EDGE); rc run "$OUT" env DD_ACCTFILE="$K/ACCTEDGE" DD_OUTFILE=OUTFILE DD_ARRYFILE=ARRYFILE \
    DD_VBRCFILE=VBRCFILE "$WORK/bin/CBACT01C" > "$OUT/SYSOUT"

# CBEXPORT, then CBIMPORT of its EXPFILE (KSDS keyed by EXPORT-SEQUENCE-NUM).
OUT=$(fresh CBEXPORT); rc run "$OUT" env DD_CUSTFILE="$K/CUSTFILE" DD_ACCTFILE="$K/ACCTFILE" DD_XREFFILE="$K/XREFFILE" \
    DD_TRANSACT="$K/TRANSACT" DD_CARDFILE="$K/CARDFILE" DD_EXPFILE="$K/EXPFILE" "$WORK/bin/CBEXPORT" > "$OUT/SYSOUT"
OUT_EXP="$OUT"
cat > "$WORK/cbl/UNLOAD.cbl" <<'COB'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNLOAD.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO INFILE ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL RECORD KEY IS IN-KEY.
           SELECT OUT-FILE ASSIGN TO OUTFILE ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC.
           05  FILLER PIC X(27).
           05  IN-KEY PIC X(4).
           05  FILLER PIC X(469).
       FD  OUT-FILE.
       01  OUT-REC PIC X(500).
       WORKING-STORAGE SECTION.
       01  EOF PIC X VALUE 'N'.
       PROCEDURE DIVISION.
           OPEN INPUT IN-FILE OUTPUT OUT-FILE
           PERFORM UNTIL EOF = 'Y'
             READ IN-FILE NEXT
               AT END MOVE 'Y' TO EOF
               NOT AT END WRITE OUT-REC FROM IN-REC
             END-READ
           END-PERFORM
           CLOSE IN-FILE OUT-FILE
           STOP RUN.
COB
"${COBC[@]}" -x -o "$WORK/bin/UNLOAD" "$WORK/cbl/UNLOAD.cbl"
DD_INFILE="$K/EXPFILE" DD_OUTFILE="$OUT_EXP/EXPFILE" "$WORK/bin/UNLOAD"

OUT=$(fresh CBIMPORT); rc run "$OUT" env DD_EXPFILE="$K/EXPFILE" DD_CUSTOUT=CUSTOUT DD_ACCTOUT=ACCTOUT \
    DD_XREFOUT=XREFOUT DD_TRNXOUT=TRNXOUT DD_CARDOUT=CARDOUT DD_ERROUT=ERROUT "$WORK/bin/CBIMPORT" > "$OUT/SYSOUT"

# CBIMPORT edge set: the CBEXPORT file plus records of an unknown type (ERROUT path).
E="$GOLDEN/CBIMPORT-EDGE"
python3 - "$OUT_EXP/EXPFILE" "$E/input/EXPFILE" <<'PY2'
# First record of each type from the CBEXPORT output, then two copies with unknown types 'Z' and 'c'
# and new sequence numbers (EXPORT-SEQUENCE-NUM, PIC 9(9) COMP big-endian at offset 27).
import sys
data = open(sys.argv[1], "rb").read()
recs = [data[i:i + 500] for i in range(0, len(data), 500)]
picked, seen = [], set()
for r in recs:
    if r[0:1] not in seen:
        seen.add(r[0:1]); picked.append(r)
seq = max(int.from_bytes(r[27:31], "big") for r in picked)
for t in (b"Z", b"c"):
    seq += 1
    picked.append(t + picked[0][1:27] + seq.to_bytes(4, "big") + picked[0][31:])
open(sys.argv[2], "wb").write(b"".join(picked))
PY2
ksds LDEXPT "$E/input/EXPFILE" "$K/EXPEDGE"
OUT=$(fresh CBIMPORT-EDGE); rc run "$OUT" env DD_EXPFILE="$K/EXPEDGE" DD_CUSTOUT=CUSTOUT DD_ACCTOUT=ACCTOUT \
    DD_XREFOUT=XREFOUT DD_TRNXOUT=TRNXOUT DD_CARDOUT=CARDOUT DD_ERROUT=ERROUT "$WORK/bin/CBIMPORT" > "$OUT/SYSOUT"

# CREASTMT: STEP010 SORT FIELDS=(263,16,CH,A,1,16,CH,A) OUTREC FIELDS=(1:263,16,17:1,262,279:279,50),
# STEP020 REPRO into the TRXFL KSDS (key 32), STEP040 CBSTM03A (+ CBSTM03B).
python3 - "$DATA/dailytran.txt" "$WORK/TRXFL.SEQ" <<'PY'
import sys
recs = [l.rstrip(b"\r\n").ljust(350, b" ")[:350] for l in open(sys.argv[1], "rb")]
recs.sort(key=lambda r: (r[262:278], r[0:16]))
with open(sys.argv[2], "wb") as out:
    for r in recs:
        out.write((r[262:278] + r[0:262] + r[278:328]).ljust(350, b" "))
PY
ksds LDTRNX "$WORK/TRXFL.SEQ" "$K/TRXFL"
OUT=$(fresh CREASTMT); cp "$WORK/TRXFL.SEQ" "$OUT/TRXFL.SEQ"
rc run "$OUT" env DD_TRNXFILE="$K/TRXFL" DD_XREFFILE="$K/XREFFILE" DD_ACCTFILE="$K/ACCTFILE" \
    DD_CUSTFILE="$K/CUSTFILE" DD_STMTFILE=STMTFILE DD_HTMLFILE=HTMLFILE "$WORK/bin/CBSTM03A" > "$OUT/SYSOUT"

for d in "$GOLDEN"/*/expected; do printf '%-28s RC=%s  %s\n' "${d#$GOLDEN/}" "$(cat "$d/RETURN-CODE")" "$(ls "$d" | grep -v RETURN-CODE | tr '\n' ' ')"; done
