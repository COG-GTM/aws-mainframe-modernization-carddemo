       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXPCOPY.
      * Copies the export file between a flat 500-byte record file
      * (EXPFLAT) and the indexed export KSDS stand-in (EXPFILE, key
      * KEYS(4 28) as in CBEXPORT.jcl). Argument: DUMP or LOAD.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FLAT-FILE ASSIGN TO EXPFLAT
                  ORGANIZATION IS RECORD SEQUENTIAL.
           SELECT IDX-FILE ASSIGN TO EXPFILE
                  ORGANIZATION IS INDEXED ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS IDX-KEY
                  FILE STATUS IS WS-FS.
       DATA DIVISION.
       FILE SECTION.
       FD FLAT-FILE.
       01 FLAT-REC PIC X(500).
       FD IDX-FILE.
       01 IDX-REC.
          05 FILLER  PIC X(28).
          05 IDX-KEY PIC X(4).
          05 FILLER  PIC X(468).
       WORKING-STORAGE SECTION.
       01 WS-FS   PIC XX.
       01 WS-MODE PIC X(4).
       01 WS-EOF  PIC X VALUE 'N'.
       PROCEDURE DIVISION.
           ACCEPT WS-MODE FROM COMMAND-LINE
           IF WS-MODE = 'DUMP'
             OPEN INPUT IDX-FILE OUTPUT FLAT-FILE
             PERFORM UNTIL WS-EOF = 'Y'
               READ IDX-FILE AT END MOVE 'Y' TO WS-EOF
               NOT AT END WRITE FLAT-REC FROM IDX-REC END-READ
             END-PERFORM
             CLOSE IDX-FILE FLAT-FILE
           ELSE
             OPEN INPUT FLAT-FILE OUTPUT IDX-FILE
             PERFORM UNTIL WS-EOF = 'Y'
               READ FLAT-FILE AT END MOVE 'Y' TO WS-EOF
               NOT AT END WRITE IDX-REC FROM FLAT-REC END-READ
             END-PERFORM
             CLOSE FLAT-FILE IDX-FILE
           END-IF
           GOBACK.
