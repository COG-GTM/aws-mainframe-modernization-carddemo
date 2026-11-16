      ******************************************************************
      * Program     : LOADVSAM.CBL
      * Purpose     : ORACLE HARNESS UTILITY - NOT PART OF THE LEGACY
      *               APPLICATION.
      *               Loads the fixed-width ASCII sample datasets that
      *               ship in app/data/ASCII into GnuCOBOL INDEXED
      *               (VSAM KSDS equivalent) files so that the
      *               UNMODIFIED legacy program CBACT04C can be executed
      *               against them to produce a true behavioural oracle.
      *
      *               Record layouts are copied verbatim from the FD
      *               entries of app/cbl/CBACT04C.cbl.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LOADVSAM.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TCATSEQ-FILE ASSIGN TO TCATSEQ
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS TCATSEQ-STATUS.

           SELECT TCATBAL-FILE ASSIGN TO TCATBALF
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS FD-TRAN-CAT-KEY
                  FILE STATUS  IS TCATBALF-STATUS.

           SELECT XREFSEQ-FILE ASSIGN TO XREFSEQ
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS XREFSEQ-STATUS.

           SELECT XREF-FILE ASSIGN TO   XREFFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS FD-XREF-CARD-NUM
                  ALTERNATE RECORD KEY IS FD-XREF-ACCT-ID
                  FILE STATUS  IS XREFFILE-STATUS.

           SELECT ACCTSEQ-FILE ASSIGN TO ACCTSEQ
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS ACCTSEQ-STATUS.

           SELECT ACCOUNT-FILE ASSIGN TO ACCTFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS FD-ACCT-ID
                  FILE STATUS  IS ACCTFILE-STATUS.

           SELECT DISCSEQ-FILE ASSIGN TO DISCSEQ
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS DISCSEQ-STATUS.

           SELECT DISCGRP-FILE ASSIGN TO DISCGRP
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS FD-DISCGRP-KEY
                  FILE STATUS  IS DISCGRP-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TCATSEQ-FILE.
       01  TCATSEQ-REC                           PIC X(50).
       FD  TCATBAL-FILE.
       01  FD-TRAN-CAT-BAL-RECORD.
           05 FD-TRAN-CAT-KEY.
              10 FD-TRANCAT-ACCT-ID             PIC 9(11).
              10 FD-TRANCAT-TYPE-CD             PIC X(02).
              10 FD-TRANCAT-CD                  PIC 9(04).
           05 FD-FD-TRAN-CAT-DATA               PIC X(33).

       FD  XREFSEQ-FILE.
       01  XREFSEQ-REC                           PIC X(50).
       FD  XREF-FILE.
       01  FD-XREFFILE-REC.
           05 FD-XREF-CARD-NUM                  PIC X(16).
           05 FD-XREF-CUST-NUM                  PIC 9(09).
           05 FD-XREF-ACCT-ID                   PIC 9(11).
           05 FD-XREF-FILLER                    PIC X(14).

       FD  ACCTSEQ-FILE.
       01  ACCTSEQ-REC                           PIC X(300).
       FD  ACCOUNT-FILE.
       01  FD-ACCTFILE-REC.
           05 FD-ACCT-ID                        PIC 9(11).
           05 FD-ACCT-DATA                      PIC X(289).

       FD  DISCSEQ-FILE.
       01  DISCSEQ-REC                           PIC X(50).
       FD  DISCGRP-FILE.
       01  FD-DISCGRP-REC.
           05 FD-DISCGRP-KEY.
              10 FD-DIS-ACCT-GROUP-ID           PIC X(10).
              10 FD-DIS-TRAN-TYPE-CD            PIC X(02).
              10 FD-DIS-TRAN-CAT-CD             PIC 9(04).
           05 FD-DISCGRP-DATA                   PIC X(34).

       WORKING-STORAGE SECTION.
       01  TCATSEQ-STATUS          PIC XX.
       01  TCATBALF-STATUS         PIC XX.
       01  XREFSEQ-STATUS          PIC XX.
       01  XREFFILE-STATUS         PIC XX.
       01  ACCTSEQ-STATUS          PIC XX.
       01  ACCTFILE-STATUS         PIC XX.
       01  DISCSEQ-STATUS          PIC XX.
       01  DISCGRP-STATUS          PIC XX.
       01  WS-EOF                  PIC X VALUE 'N'.
       01  WS-COUNT                PIC 9(05) VALUE 0.

       PROCEDURE DIVISION.
           PERFORM 100-LOAD-TCATBAL
           PERFORM 200-LOAD-XREF
           PERFORM 300-LOAD-ACCT
           PERFORM 400-LOAD-DISCGRP
           GOBACK.

       100-LOAD-TCATBAL.
           MOVE 'N' TO WS-EOF
           MOVE 0   TO WS-COUNT
           OPEN INPUT TCATSEQ-FILE
           PERFORM 900-CHECK-OPEN
           OPEN OUTPUT TCATBAL-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ TCATSEQ-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE TCATSEQ-REC TO FD-TRAN-CAT-BAL-RECORD
                       WRITE FD-TRAN-CAT-BAL-RECORD
                       ADD 1 TO WS-COUNT
               END-READ
           END-PERFORM
           CLOSE TCATSEQ-FILE
           CLOSE TCATBAL-FILE
           DISPLAY 'LOADVSAM TCATBALF RECORDS LOADED: ' WS-COUNT
           EXIT.

       200-LOAD-XREF.
           MOVE 'N' TO WS-EOF
           MOVE 0   TO WS-COUNT
           OPEN INPUT XREFSEQ-FILE
           PERFORM 900-CHECK-OPEN
           OPEN OUTPUT XREF-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ XREFSEQ-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE XREFSEQ-REC TO FD-XREFFILE-REC
                       WRITE FD-XREFFILE-REC
                       ADD 1 TO WS-COUNT
               END-READ
           END-PERFORM
           CLOSE XREFSEQ-FILE
           CLOSE XREF-FILE
           DISPLAY 'LOADVSAM XREFFILE RECORDS LOADED: ' WS-COUNT
           EXIT.

       300-LOAD-ACCT.
           MOVE 'N' TO WS-EOF
           MOVE 0   TO WS-COUNT
           OPEN INPUT ACCTSEQ-FILE
           PERFORM 900-CHECK-OPEN
           OPEN OUTPUT ACCOUNT-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ ACCTSEQ-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE ACCTSEQ-REC TO FD-ACCTFILE-REC
                       WRITE FD-ACCTFILE-REC
                       ADD 1 TO WS-COUNT
               END-READ
           END-PERFORM
           CLOSE ACCTSEQ-FILE
           CLOSE ACCOUNT-FILE
           DISPLAY 'LOADVSAM ACCTFILE RECORDS LOADED: ' WS-COUNT
           EXIT.

       400-LOAD-DISCGRP.
           MOVE 'N' TO WS-EOF
           MOVE 0   TO WS-COUNT
           OPEN INPUT DISCSEQ-FILE
           PERFORM 900-CHECK-OPEN
           OPEN OUTPUT DISCGRP-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ DISCSEQ-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE DISCSEQ-REC TO FD-DISCGRP-REC
                       WRITE FD-DISCGRP-REC
                       ADD 1 TO WS-COUNT
               END-READ
           END-PERFORM
           CLOSE DISCSEQ-FILE
           CLOSE DISCGRP-FILE
           DISPLAY 'LOADVSAM DISCGRP  RECORDS LOADED: ' WS-COUNT
           EXIT.

       900-CHECK-OPEN.
           EXIT.
