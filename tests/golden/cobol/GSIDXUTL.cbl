      ******************************************************************
      * Program     : GSIDXUTL.CBL
      * Purpose     : Golden-set indexed-file utility for GnuCOBOL.
      *               LOAD : record-sequential fixture -> indexed file
      *               DUMP : indexed file -> record-sequential (key
      *                      order, copybook layout, no delimiters)
      * Usage       : GSIDXUTL <LOAD|DUMP> <XREF|ACCT|TCAT|TRAN>
      *               Sequential side : DD_GSSEQ
      *               Indexed side    : DD_XREFFILE / DD_ACCTFILE /
      *                                 DD_TCATBALF / DD_TRANFILE
      *               (the same ASSIGN names CBTRN02C uses)
      * This program is test scaffolding only. It contains no business
      * logic; it exists because GnuCOBOL indexed files are a runtime
      * specific on-disk format that must be created through the COBOL
      * runtime. Record layouts are taken from the app/cpy copybooks.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GSIDXUTL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-XREF   ASSIGN TO GSSEQ
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS SEQ-STATUS.
           SELECT SEQ-ACCT   ASSIGN TO GSSEQ
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS SEQ-STATUS.
           SELECT SEQ-TCAT   ASSIGN TO GSSEQ
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS SEQ-STATUS.
           SELECT SEQ-TRAN   ASSIGN TO GSSEQ
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS SEQ-STATUS.

           SELECT XREF-FILE ASSIGN TO   XREFFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS FD-XREF-CARD-NUM
                  FILE STATUS  IS IDX-STATUS.
           SELECT ACCOUNT-FILE ASSIGN TO ACCTFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS FD-ACCT-ID
                  FILE STATUS  IS IDX-STATUS.
           SELECT TCATBAL-FILE ASSIGN TO TCATBALF
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS FD-TRAN-CAT-KEY
                  FILE STATUS  IS IDX-STATUS.
           SELECT TRANSACT-FILE ASSIGN TO TRANFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS FD-TRANS-ID
                  FILE STATUS  IS IDX-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEQ-XREF.
       01  SEQ-XREF-REC                     PIC X(50).
       FD  SEQ-ACCT.
       01  SEQ-ACCT-REC                     PIC X(300).
       FD  SEQ-TCAT.
       01  SEQ-TCAT-REC                     PIC X(50).
       FD  SEQ-TRAN.
       01  SEQ-TRAN-REC                     PIC X(350).

      * Indexed FDs mirror app/cbl/CBTRN02C.cbl lines 104-125 so the
      * key offsets and record lengths are identical.
       FD  XREF-FILE.
       01  FD-XREFFILE-REC.
           05 FD-XREF-CARD-NUM              PIC X(16).
           05 FD-XREF-DATA                  PIC X(34).
       FD  ACCOUNT-FILE.
       01  FD-ACCTFILE-REC.
           05 FD-ACCT-ID                    PIC 9(11).
           05 FD-ACCT-DATA                  PIC X(289).
       FD  TCATBAL-FILE.
       01  FD-TRAN-CAT-BAL-RECORD.
           05 FD-TRAN-CAT-KEY.
              10 FD-TRANCAT-ACCT-ID         PIC 9(11).
              10 FD-TRANCAT-TYPE-CD         PIC X(02).
              10 FD-TRANCAT-CD              PIC 9(04).
           05 FD-FD-TRAN-CAT-DATA           PIC X(33).
       FD  TRANSACT-FILE.
       01  FD-TRANFILE-REC.
           05 FD-TRANS-ID                   PIC X(16).
           05 FD-ACCT-DATA2                 PIC X(334).

       WORKING-STORAGE SECTION.
       01  SEQ-STATUS                       PIC XX.
       01  IDX-STATUS                       PIC XX.
       01  WS-ARGS                          PIC X(40).
       01  WS-MODE                          PIC X(04).
       01  WS-FILE                          PIC X(04).
       01  WS-COUNT                         PIC 9(09) VALUE 0.
       01  WS-EOF                           PIC X VALUE 'N'.

       PROCEDURE DIVISION.
           ACCEPT WS-ARGS FROM COMMAND-LINE
           UNSTRING WS-ARGS DELIMITED BY ALL SPACES
               INTO WS-MODE WS-FILE
           END-UNSTRING
           EVALUATE WS-MODE
             WHEN 'LOAD'  PERFORM 1000-LOAD
             WHEN 'DUMP'  PERFORM 2000-DUMP
             WHEN OTHER
               DISPLAY 'GSIDXUTL: usage <LOAD|DUMP> '
                       '<XREF|ACCT|TCAT|TRAN>'
               MOVE 16 TO RETURN-CODE
           END-EVALUATE
           GOBACK.

       1000-LOAD.
           EVALUATE WS-FILE
             WHEN 'XREF'
               OPEN INPUT SEQ-XREF
               PERFORM 9000-CHECK-SEQ
               OPEN OUTPUT XREF-FILE
               PERFORM 9100-CHECK-IDX
               PERFORM UNTIL WS-EOF = 'Y'
                 READ SEQ-XREF
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                     MOVE SEQ-XREF-REC TO FD-XREFFILE-REC
                     WRITE FD-XREFFILE-REC
                     PERFORM 9100-CHECK-IDX
                     ADD 1 TO WS-COUNT
                 END-READ
               END-PERFORM
               CLOSE SEQ-XREF XREF-FILE
             WHEN 'ACCT'
               OPEN INPUT SEQ-ACCT
               PERFORM 9000-CHECK-SEQ
               OPEN OUTPUT ACCOUNT-FILE
               PERFORM 9100-CHECK-IDX
               PERFORM UNTIL WS-EOF = 'Y'
                 READ SEQ-ACCT
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                     MOVE SEQ-ACCT-REC TO FD-ACCTFILE-REC
                     WRITE FD-ACCTFILE-REC
                     PERFORM 9100-CHECK-IDX
                     ADD 1 TO WS-COUNT
                 END-READ
               END-PERFORM
               CLOSE SEQ-ACCT ACCOUNT-FILE
             WHEN 'TCAT'
               OPEN INPUT SEQ-TCAT
               PERFORM 9000-CHECK-SEQ
               OPEN OUTPUT TCATBAL-FILE
               PERFORM 9100-CHECK-IDX
               PERFORM UNTIL WS-EOF = 'Y'
                 READ SEQ-TCAT
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                     MOVE SEQ-TCAT-REC TO FD-TRAN-CAT-BAL-RECORD
                     WRITE FD-TRAN-CAT-BAL-RECORD
                     PERFORM 9100-CHECK-IDX
                     ADD 1 TO WS-COUNT
                 END-READ
               END-PERFORM
               CLOSE SEQ-TCAT TCATBAL-FILE
             WHEN 'TRAN'
               OPEN INPUT SEQ-TRAN
               PERFORM 9000-CHECK-SEQ
               OPEN OUTPUT TRANSACT-FILE
               PERFORM 9100-CHECK-IDX
               PERFORM UNTIL WS-EOF = 'Y'
                 READ SEQ-TRAN
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                     MOVE SEQ-TRAN-REC TO FD-TRANFILE-REC
                     WRITE FD-TRANFILE-REC
                     PERFORM 9100-CHECK-IDX
                     ADD 1 TO WS-COUNT
                 END-READ
               END-PERFORM
               CLOSE SEQ-TRAN TRANSACT-FILE
             WHEN OTHER
               DISPLAY 'GSIDXUTL: unknown file ' WS-FILE
               MOVE 16 TO RETURN-CODE
           END-EVALUATE
           DISPLAY 'GSIDXUTL LOAD ' WS-FILE ' RECORDS=' WS-COUNT.

       2000-DUMP.
           EVALUATE WS-FILE
             WHEN 'XREF'
               OPEN INPUT XREF-FILE
               PERFORM 9100-CHECK-IDX
               OPEN OUTPUT SEQ-XREF
               PERFORM 9000-CHECK-SEQ
               PERFORM UNTIL WS-EOF = 'Y'
                 READ XREF-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                     WRITE SEQ-XREF-REC FROM FD-XREFFILE-REC
                     PERFORM 9000-CHECK-SEQ
                     ADD 1 TO WS-COUNT
                 END-READ
               END-PERFORM
               CLOSE SEQ-XREF XREF-FILE
             WHEN 'ACCT'
               OPEN INPUT ACCOUNT-FILE
               PERFORM 9100-CHECK-IDX
               OPEN OUTPUT SEQ-ACCT
               PERFORM 9000-CHECK-SEQ
               PERFORM UNTIL WS-EOF = 'Y'
                 READ ACCOUNT-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                     WRITE SEQ-ACCT-REC FROM FD-ACCTFILE-REC
                     PERFORM 9000-CHECK-SEQ
                     ADD 1 TO WS-COUNT
                 END-READ
               END-PERFORM
               CLOSE SEQ-ACCT ACCOUNT-FILE
             WHEN 'TCAT'
               OPEN INPUT TCATBAL-FILE
               PERFORM 9100-CHECK-IDX
               OPEN OUTPUT SEQ-TCAT
               PERFORM 9000-CHECK-SEQ
               PERFORM UNTIL WS-EOF = 'Y'
                 READ TCATBAL-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                     WRITE SEQ-TCAT-REC FROM FD-TRAN-CAT-BAL-RECORD
                     PERFORM 9000-CHECK-SEQ
                     ADD 1 TO WS-COUNT
                 END-READ
               END-PERFORM
               CLOSE SEQ-TCAT TCATBAL-FILE
             WHEN 'TRAN'
               OPEN INPUT TRANSACT-FILE
               PERFORM 9100-CHECK-IDX
               OPEN OUTPUT SEQ-TRAN
               PERFORM 9000-CHECK-SEQ
               PERFORM UNTIL WS-EOF = 'Y'
                 READ TRANSACT-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                     WRITE SEQ-TRAN-REC FROM FD-TRANFILE-REC
                     PERFORM 9000-CHECK-SEQ
                     ADD 1 TO WS-COUNT
                 END-READ
               END-PERFORM
               CLOSE SEQ-TRAN TRANSACT-FILE
             WHEN OTHER
               DISPLAY 'GSIDXUTL: unknown file ' WS-FILE
               MOVE 16 TO RETURN-CODE
           END-EVALUATE
           DISPLAY 'GSIDXUTL DUMP ' WS-FILE ' RECORDS=' WS-COUNT.

       9000-CHECK-SEQ.
           IF SEQ-STATUS NOT = '00'
              DISPLAY 'GSIDXUTL: sequential file status ' SEQ-STATUS
                      ' for ' WS-MODE ' ' WS-FILE
              MOVE 12 TO RETURN-CODE
              STOP RUN
           END-IF.

       9100-CHECK-IDX.
           IF IDX-STATUS NOT = '00'
              DISPLAY 'GSIDXUTL: indexed file status ' IDX-STATUS
                      ' for ' WS-MODE ' ' WS-FILE
              MOVE 12 TO RETURN-CODE
              STOP RUN
           END-IF.
