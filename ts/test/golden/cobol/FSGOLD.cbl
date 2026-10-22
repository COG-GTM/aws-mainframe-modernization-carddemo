       IDENTIFICATION DIVISION.
       PROGRAM-ID. FSGOLD.
      * Golden FILE STATUS sequence for an indexed file with a
      * non-unique alternate key (compare with test/golden.test.ts).
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TFILE ASSIGN TO 'fsgold.dat'
                  ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
                  RECORD KEY IS T-KEY
                  ALTERNATE RECORD KEY IS T-ALT WITH DUPLICATES
                  FILE STATUS IS WS-FS.
           SELECT MFILE ASSIGN TO 'fsmissing.dat'
                  ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
                  RECORD KEY IS M-KEY
                  FILE STATUS IS WS-FS.
       DATA DIVISION.
       FILE SECTION.
       FD TFILE.
       01 T-REC.
          05 T-KEY  PIC X(4).
          05 T-ALT  PIC X(3).
          05 T-DATA PIC X(3).
       FD MFILE.
       01 M-REC.
          05 M-KEY  PIC X(4).
       WORKING-STORAGE SECTION.
       01 WS-FS PIC XX.
       01 WS-STEP PIC 99 VALUE 0.
       PROCEDURE DIVISION.
           OPEN INPUT MFILE                     PERFORM SHOW
           OPEN OUTPUT TFILE                    PERFORM SHOW
           MOVE '0001AAAd01' TO T-REC WRITE T-REC PERFORM SHOW
           MOVE '0002AAAd02' TO T-REC WRITE T-REC PERFORM SHOW
           MOVE '0003BBBd03' TO T-REC WRITE T-REC PERFORM SHOW
           MOVE '0001ZZZx01' TO T-REC WRITE T-REC PERFORM SHOW
           CLOSE TFILE                          PERFORM SHOW
           OPEN I-O TFILE                       PERFORM SHOW
           MOVE '0004' TO T-KEY READ TFILE      PERFORM SHOW
           MOVE '0002' TO T-KEY READ TFILE      PERFORM SHOW
           MOVE 'AAA' TO T-ALT
           READ TFILE KEY IS T-ALT              PERFORM SHOW
           READ TFILE NEXT                      PERFORM SHOW
           READ TFILE NEXT                      PERFORM SHOW
           READ TFILE NEXT                      PERFORM SHOW
           READ TFILE NEXT                      PERFORM SHOW
           MOVE '0004' TO T-KEY
           START TFILE KEY >= T-KEY             PERFORM SHOW
           MOVE '0002' TO T-KEY
           START TFILE KEY >= T-KEY             PERFORM SHOW
           READ TFILE NEXT                      PERFORM SHOW
           MOVE '0002QQQd02' TO T-REC
           REWRITE T-REC                        PERFORM SHOW
           MOVE '0009' TO T-KEY DELETE TFILE    PERFORM SHOW
           MOVE '0003' TO T-KEY DELETE TFILE    PERFORM SHOW
           MOVE '0001' TO T-KEY
           START TFILE KEY >= T-KEY             PERFORM SHOW
           READ TFILE NEXT                      PERFORM SHOW
           READ TFILE NEXT                      PERFORM SHOW
           READ TFILE NEXT                      PERFORM SHOW
           OPEN I-O TFILE                       PERFORM SHOW
           CLOSE TFILE                          PERFORM SHOW
           CLOSE TFILE                          PERFORM SHOW
           OPEN INPUT TFILE                     PERFORM SHOW
           MOVE '0007NEWd07' TO T-REC WRITE T-REC PERFORM SHOW
           MOVE '0001AAAd01' TO T-REC
           REWRITE T-REC                        PERFORM SHOW
           CLOSE TFILE                          PERFORM SHOW
           STOP RUN.
       SHOW.
           ADD 1 TO WS-STEP
           DISPLAY WS-STEP ' ' WS-FS.
