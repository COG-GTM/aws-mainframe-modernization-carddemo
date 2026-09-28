#!/usr/bin/env bash
# Emits a GnuCOBOL program that loads (LOAD) or unloads (UNLD) an indexed file.
# usage: gen-idxutil.sh <progname> <mode LOAD|UNLD> <reclen> <keyoff> <keylen> [<altoff> <altlen>]
set -euo pipefail
prog=$1 mode=$2 len=$3 koff=$4 klen=$5 aoff=${6:-} alen=${7:-}
rec() {
  local pos=0
  if [[ -n "$aoff" ]]; then
    # key and alternate key never overlap in the CardDemo layouts; key precedes alt key.
    (( koff > 0 )) && echo "           05 FILLER PIC X($koff)."
    echo "           05 IDX-KEY PIC X($klen)."
    pos=$((koff + klen))
    (( aoff > pos )) && echo "           05 FILLER PIC X($((aoff - pos)))."
    echo "           05 IDX-ALT PIC X($alen)."
    pos=$((aoff + alen))
  else
    (( koff > 0 )) && echo "           05 FILLER PIC X($koff)."
    echo "           05 IDX-KEY PIC X($klen)."
    pos=$((koff + klen))
  fi
  (( len > pos )) && echo "           05 FILLER PIC X($((len - pos)))."
  return 0
}
altclause=""
[[ -n "$aoff" ]] && altclause="
               ALTERNATE RECORD KEY IS IDX-ALT WITH DUPLICATES"
if [[ $mode == LOAD ]]; then
  idxmode="OUTPUT"; seqmode="INPUT"; access="SEQUENTIAL"
else
  idxmode="INPUT"; seqmode="OUTPUT"; access="SEQUENTIAL"
fi
cat <<COB
       IDENTIFICATION DIVISION.
       PROGRAM-ID. $prog.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO SEQFILE
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT IDX-FILE ASSIGN TO IDXFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS $access
               RECORD KEY IS IDX-KEY$altclause
               FILE STATUS IS WS-STAT.
       DATA DIVISION.
       FILE SECTION.
       FD  SEQ-FILE.
       01  SEQ-REC PIC X($len).
       FD  IDX-FILE.
       01  IDX-REC.
$(rec)
       WORKING-STORAGE SECTION.
       01  WS-STAT PIC XX.
       01  WS-EOF  PIC X VALUE 'N'.
       PROCEDURE DIVISION.
           OPEN $seqmode SEQ-FILE.
           OPEN $idxmode IDX-FILE.
COB
if [[ $mode == LOAD ]]; then cat <<'COB'
           PERFORM UNTIL WS-EOF = 'Y'
               READ SEQ-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE SEQ-REC TO IDX-REC
                       WRITE IDX-REC
                       IF WS-STAT NOT = '00'
                           DISPLAY 'WRITE STATUS ' WS-STAT
                           MOVE 12 TO RETURN-CODE
                       END-IF
               END-READ
           END-PERFORM.
COB
else cat <<'COB'
           PERFORM UNTIL WS-EOF = 'Y'
               READ IDX-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE IDX-REC TO SEQ-REC
                       WRITE SEQ-REC
               END-READ
           END-PERFORM.
COB
fi
cat <<'COB'
           CLOSE SEQ-FILE IDX-FILE.
           GOBACK.
COB
