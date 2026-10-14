#!/usr/bin/env bash
# Golden values for the pure-COBOL statements of COMEN01C (option parsing, validity test, the three STRINGs).
# COMEN01C itself cannot run under GnuCOBOL (EXEC CICS), so this builds a driver around the statements
# copied verbatim, by line number, from app/cbl/COMEN01C.cbl, with the real COMEN02Y copybook.
# Usage: tools/gnucobol/comen01c/run.sh   (from java/carddemo-online; needs cobc, e.g. apt-get install gnucobol)
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
APP="$(cd "$HERE/../../../../../app" && pwd)"
SRC="$APP/cbl/COMEN01C.cbl"
OUT="${OUT:-$HERE/../../../src/test/resources/golden/comen01c}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

lines() { sed -n "$1p" "$SRC"; }

{
cat <<'COB'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMEN01G.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTFILE ASSIGN TO 'options.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTFILE ASSIGN TO 'out.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  OPTFILE.
       01  OPT-REC                 PIC X(4).
       FD  OUTFILE.
       01  OUT-REC                 PIC X(100).
       WORKING-STORAGE SECTION.
       01 WS-EOF                   PIC X VALUE 'N'.
       01 WS-VALID                 PIC X.
       01 WS-I                     PIC 9(02).
COB
# WORKING-STORAGE of COMEN01C (lines 35-48) and its COMEN02Y table
lines 35,48
echo "       COPY COMEN02Y."
cat <<'COB'
       01 COMEN1AI.
          05 OPTIONI               PIC X(2).
       01 COMEN1AO.
          05 OPTIONO               PIC X(2).
       PROCEDURE DIVISION.
           OPEN INPUT OPTFILE OUTPUT OUTFILE
           PERFORM UNTIL WS-EOF = 'Y'
             READ OPTFILE AT END MOVE 'Y' TO WS-EOF
             NOT AT END
               MOVE OPT-REC(2:2) TO OPTIONI OF COMEN1AI
COB
lines 117,125
lines 127,129
cat <<'COB'
                 MOVE 'I' TO WS-VALID
               ELSE
                 MOVE 'V' TO WS-VALID
               END-IF
               MOVE SPACES TO OUT-REC
               STRING 'P|[' OPTIONI OF COMEN1AI '][' WS-OPTION-X ']['
                      WS-OPTION '][' OPTIONO OF COMEN1AO ']|' WS-VALID
                      DELIMITED BY SIZE INTO OUT-REC
               WRITE OUT-REC
             END-READ
           END-PERFORM
           MOVE 'Card Rewards' TO CDEMO-MENU-OPT-NAME(12)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 12
             MOVE WS-I TO WS-OPTION WS-IDX
             IF WS-I = 12
               MOVE '12' TO CDEMO-MENU-OPT-NUM(12)
             END-IF
             MOVE SPACES TO WS-MESSAGE
COB
lines 163,167
cat <<'COB'
             MOVE SPACES TO OUT-REC
             STRING 'N|' WS-I '|[' WS-MESSAGE ']' DELIMITED BY SIZE
                    INTO OUT-REC
             WRITE OUT-REC
             MOVE SPACES TO WS-MESSAGE
COB
lines 172,176
cat <<'COB'
             MOVE SPACES TO OUT-REC
             STRING 'D|' WS-I '|[' WS-MESSAGE ']' DELIMITED BY SIZE
                    INTO OUT-REC
             WRITE OUT-REC
             MOVE SPACES TO WS-MENU-OPT-TXT
COB
lines 269,272
cat <<'COB'
             MOVE SPACES TO OUT-REC
             STRING 'B|' WS-I '|[' WS-MENU-OPT-TXT ']' DELIMITED BY SIZE
                    INTO OUT-REC
             WRITE OUT-REC
           END-PERFORM
           CLOSE OPTFILE OUTFILE
           STOP RUN.
COB
} > "$WORK/COMEN01G.cbl"

cd "$WORK"
# -frelax-syntax-checks: COMEN02Y redefines the 11-entry VALUE data with an OCCURS 12 table (larger),
# which GnuCOBOL otherwise rejects
cobc -x -std=ibm -frelax-syntax-checks -I "$APP/cpy" -o comen01g COMEN01G.cbl
# each option is bracketed so line-sequential I/O keeps its spaces
printf '%s\n' '[01]' '[1 ]' '[ 1]' '[  ]' '[00]' '[0 ]' '[11]' '[12]' '[99]' '[10]' '[A1]' '[1A]' '[-1]' '[ 9]' > options.txt
./comen01g
mkdir -p "$OUT"
cp out.txt "$OUT/comen01c-golden.txt"
echo "wrote $OUT/comen01c-golden.txt"
