      ******************************************************************
      * Program     : CBGLX01C
      * Type        : Batch COBOL
      * Function    : Build and reconcile a ledger-summary interface.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBGLX01C.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GLPARM-FILE ASSIGN TO GLPARM
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS GLPARM-STATUS.
           SELECT TRANFILE-FILE ASSIGN TO TRANFILE
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS TRANFILE-STATUS.
           SELECT XREFFILE-FILE ASSIGN TO XREFFILE
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS XREFFILE-STATUS.
           SELECT TCATBALF-FILE ASSIGN TO TCATBALF
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS TCATBALF-STATUS.
           SELECT GLSUMOUT-FILE ASSIGN TO GLSUMOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS GLSUMOUT-STATUS.
           SELECT GLRECRPT-FILE ASSIGN TO GLRECRPT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS GLRECRPT-STATUS.
           SELECT SORT-WORK-FILE ASSIGN TO SORTWK01.

       DATA DIVISION.
       FILE SECTION.
       FD  GLPARM-FILE
           RECORD CONTAINS 80 CHARACTERS.
       COPY CVGLCTL.

       FD  TRANFILE-FILE
           RECORD CONTAINS 350 CHARACTERS.
       COPY CVTRA05Y.

       FD  XREFFILE-FILE
           RECORD CONTAINS 50 CHARACTERS.
       COPY CVACT03Y.

       FD  TCATBALF-FILE
           RECORD CONTAINS 50 CHARACTERS.
       COPY CVTRA01Y.

       FD  GLSUMOUT-FILE
           RECORD CONTAINS 150 CHARACTERS.
       01  GLSUMOUT-RECORD                         PIC X(150).

       FD  GLRECRPT-FILE
           RECORD CONTAINS 132 CHARACTERS.
       01  GLRECRPT-RECORD                         PIC X(132).

       SD  SORT-WORK-FILE.
       01  SORT-RECORD.
           05  SORT-ACCT                           PIC 9(11).
           05  SORT-TYPE                           PIC X(2).
           05  SORT-CAT                            PIC 9(4).
           05  SORT-TRAN-ID                        PIC X(16).
           05  SORT-AMT                            PIC S9(9)V99.

       WORKING-STORAGE SECTION.
       01  GLPARM-STATUS                           PIC XX.
       01  TRANFILE-STATUS                         PIC XX.
       01  XREFFILE-STATUS                         PIC XX.
       01  TCATBALF-STATUS                         PIC XX.
       01  GLSUMOUT-STATUS                         PIC XX.
       01  GLRECRPT-STATUS                         PIC XX.

       COPY CVGLHDR.
       COPY CVGLDTL.
       COPY CVGLTRL.

       01  WS-FILE-FLAGS.
           05  WS-REPORT-OPEN                      PIC X VALUE 'N'.
           05  WS-PARM-OPEN                        PIC X VALUE 'N'.
           05  WS-TRAN-OPEN                        PIC X VALUE 'N'.
           05  WS-XREF-OPEN                        PIC X VALUE 'N'.
           05  WS-TCAT-OPEN                        PIC X VALUE 'N'.
           05  WS-OUT-OPEN                         PIC X VALUE 'N'.
           05  WS-OUT-INPUT-OPEN                   PIC X VALUE 'N'.

       01  WS-RUN-STATE.
           05  WS-ABORT-FLAG                       PIC X VALUE 'N'.
           05  WS-ABORT-REASON                     PIC X(50)
                                                   VALUE SPACES.
           05  WS-RETURN-CODE                      PIC 9(4)
                                                   COMP-3 VALUE 0.
           05  WS-EOF                               PIC X VALUE 'N'.
           05  WS-TCAT-EOF                         PIC X VALUE 'N'.
           05  WS-SORT-EOF                         PIC X VALUE 'N'.
           05  WS-HAVE-PREV-TRAN                   PIC X VALUE 'N'.
           05  WS-HAVE-KEY                         PIC X VALUE 'N'.
           05  WS-HAVE-TCAT                        PIC X VALUE 'N'.
           05  WS-XREF-FOUND                       PIC X VALUE 'N'.
           05  WS-VALID-DATE                       PIC X VALUE 'N'.
           05  WS-HAS-EXPECTED                     PIC X VALUE 'N'.
           05  WS-HEADER-WRITTEN                   PIC X VALUE 'N'.
           05  WS-TRAILER-SEEN                     PIC X VALUE 'N'.

       01  WS-CONTROL-DATA.
           05  WS-RUN-DATE                         PIC X(10)
                                                   VALUE SPACES.
           05  WS-RUN-DATE-8                       PIC X(8)
                                                   VALUE SPACES.
           05  WS-SOURCE-ID                        PIC X(8)
                                                   VALUE SPACES.
           05  WS-EXPECTED-COUNT                   PIC 9(9)
                                                   COMP-3 VALUE 0.
           05  WS-EXPECTED-NET                     PIC S9(13)V99
                                                   COMP-3 VALUE 0.

       01  WS-DATE-CHECK.
           05  WS-DATE-TEXT                        PIC X(10).
           05  WS-DATE-YEAR                        PIC 9(4) VALUE 0.
           05  WS-DATE-MONTH                       PIC 9(2) VALUE 0.
           05  WS-DATE-DAY                         PIC 9(2) VALUE 0.
           05  WS-DATE-MAX-DAY                     PIC 9(2) VALUE 0.
           05  WS-DATE-REMAINDER                   PIC 9(4) VALUE 0.
           05  WS-DATE-QUOTIENT                    PIC 9(4) VALUE 0.

       01  WS-XREF-COUNT                            PIC 9(5)
                                                   COMP-3 VALUE 0.
       01  WS-XREF-TABLE.
           05  WS-XREF-ENTRY OCCURS 0 TO 10000 TIMES
               DEPENDING ON WS-XREF-COUNT
               ASCENDING KEY IS WS-XREF-CARD
               INDEXED BY WS-XREF-IDX.
               10  WS-XREF-CARD                    PIC X(16).
               10  WS-XREF-ACCOUNT                 PIC 9(11).
       01  WS-XREF-PREV-CARD                        PIC X(16)
                                                   VALUE SPACES.

       01  WS-INPUT-COUNTS.
           05  WS-TRAN-READ-COUNT                   PIC 9(11)
                                                   COMP-3 VALUE 0.
           05  WS-TRAN-ACCEPT-COUNT                PIC 9(11)
                                                   COMP-3 VALUE 0.
           05  WS-TRAN-REJECT-COUNT                PIC 9(11)
                                                   COMP-3 VALUE 0.
           05  WS-TRAN-WARN-COUNT                  PIC 9(11)
                                                   COMP-3 VALUE 0.
           05  WS-TCAT-READ-COUNT                  PIC 9(11)
                                                   COMP-3 VALUE 0.
           05  WS-PREV-TRAN-ID                     PIC X(16)
                                                   VALUE SPACES.

       01  WS-SOURCE-TOTALS.
           05  WS-SOURCE-DEBITS                    PIC S9(15)V99
                                                   COMP-3 VALUE 0.
           05  WS-SOURCE-CREDITS                   PIC S9(15)V99
                                                   COMP-3 VALUE 0.
           05  WS-SOURCE-NET                       PIC S9(15)V99
                                                   COMP-3 VALUE 0.

       01  WS-ACTIVITY-KEY.
           05  WS-CURRENT-ACCOUNT                  PIC 9(11)
                                                   VALUE 0.
           05  WS-CURRENT-TYPE                     PIC X(2)
                                                   VALUE SPACES.
           05  WS-CURRENT-CATEGORY                 PIC 9(4)
                                                   VALUE 0.
       01  WS-KEY-TOTALS.
           05  WS-KEY-COUNT                        PIC 9(9)
                                                   COMP-3 VALUE 0.
           05  WS-KEY-DEBITS                       PIC S9(13)V99
                                                   COMP-3 VALUE 0.
           05  WS-KEY-CREDITS                      PIC S9(13)V99
                                                   COMP-3 VALUE 0.
           05  WS-KEY-NET                          PIC S9(13)V99
                                                   COMP-3 VALUE 0.
           05  WS-ABS-AMOUNT                       PIC S9(9)V99
                                                   COMP-3 VALUE 0.
           05  WS-CATEGORY-BALANCE                 PIC S9(13)V99
                                                   COMP-3 VALUE 0.
           05  WS-TIE-STATUS                       PIC X VALUE SPACE.
           05  WS-TIE-FOUND                        PIC X VALUE 'N'.

       01  WS-TCAT-CHECK.
           05  WS-TCAT-PREV-KEY                    PIC X(17)
                                                   VALUE SPACES.
           05  WS-TCAT-CURRENT-KEY                 PIC X(17)
                                                   VALUE SPACES.

       01  WS-TCAT-ACTIVITY-KEY.
           05  WS-TCAT-ACTIVITY-ACCOUNT             PIC 9(11).
           05  WS-TCAT-ACTIVITY-TYPE                PIC X(2).
           05  WS-TCAT-ACTIVITY-CATEGORY            PIC 9(4).

       01  WS-BUILD-TOTALS.
           05  WS-BUILD-DETAIL-COUNT               PIC 9(9)
                                                   COMP-3 VALUE 0.
           05  WS-BUILD-TRAN-COUNT                 PIC 9(11)
                                                   COMP-3 VALUE 0.
           05  WS-BUILD-DEBITS                     PIC S9(15)V99
                                                   COMP-3 VALUE 0.
           05  WS-BUILD-CREDITS                    PIC S9(15)V99
                                                   COMP-3 VALUE 0.
           05  WS-BUILD-NET                        PIC S9(15)V99
                                                   COMP-3 VALUE 0.
           05  WS-BUILD-HASH                       PIC 9(18)
                                                   COMP-3 VALUE 0.
       01  WS-CANDIDATE-TOTALS.
           05  WS-CANDIDATE-DETAIL-COUNT           PIC 9(9)
                                                   COMP-3 VALUE 0.
           05  WS-CANDIDATE-TRAN-COUNT             PIC 9(11)
                                                   COMP-3 VALUE 0.
           05  WS-CANDIDATE-DEBITS                 PIC S9(15)V99
                                                   COMP-3 VALUE 0.
           05  WS-CANDIDATE-CREDITS                PIC S9(15)V99
                                                   COMP-3 VALUE 0.
           05  WS-CANDIDATE-HASH                   PIC 9(18)
                                                   COMP-3 VALUE 0.
       01  WS-TIE-COUNTS.
           05  WS-KEYS-TIED                        PIC 9(9)
                                                   COMP-3 VALUE 0.
           05  WS-KEYS-DIFFERING                   PIC 9(9)
                                                   COMP-3 VALUE 0.
           05  WS-KEYS-MISSING                     PIC 9(9)
                                                   COMP-3 VALUE 0.
           05  WS-BALANCE-ONLY                     PIC 9(9)
                                                   COMP-3 VALUE 0.

       01  WS-RE-READ-TOTALS.
           05  WS-READ-DETAIL-COUNT                PIC 9(9)
                                                   COMP-3 VALUE 0.
           05  WS-READ-TRAN-COUNT                  PIC 9(11)
                                                   COMP-3 VALUE 0.
           05  WS-READ-DEBITS                      PIC S9(15)V99
                                                   COMP-3 VALUE 0.
           05  WS-READ-CREDITS                     PIC S9(15)V99
                                                   COMP-3 VALUE 0.
           05  WS-READ-NET                         PIC S9(15)V99
                                                   COMP-3 VALUE 0.
           05  WS-READ-HASH                        PIC 9(18)
                                                   COMP-3 VALUE 0.
           05  WS-READ-RECORD-COUNT                PIC 9(9)
                                                   COMP-3 VALUE 0.
           05  WS-READ-TRAILER-DETAIL              PIC 9(9)
                                                   COMP-3 VALUE 0.
           05  WS-READ-TRAILER-TRAN                PIC 9(11)
                                                   COMP-3 VALUE 0.
           05  WS-READ-TRAILER-DEBITS              PIC S9(15)V99
                                                   COMP-3 VALUE 0.
           05  WS-READ-TRAILER-CREDITS             PIC S9(15)V99
                                                   COMP-3 VALUE 0.
           05  WS-READ-TRAILER-NET                 PIC S9(15)V99
                                                   COMP-3 VALUE 0.
           05  WS-READ-TRAILER-HASH                PIC 9(18)
                                                   COMP-3 VALUE 0.
           05  WS-READ-TRAILER-RECORDS             PIC 9(9)
                                                   COMP-3 VALUE 0.
           05  WS-VERIFY-LAST-KEY                  PIC X(17)
                                                   VALUE SPACES.
           05  WS-VERIFY-EXPECTED-SEQ              PIC 9(9)
                                                   COMP-3 VALUE 1.
           05  WS-VERIFY-NET                       PIC S9(13)V99
                                                   COMP-3 VALUE 0.
           05  WS-VERIFY-DETAIL-OK                 PIC X VALUE 'Y'.
           05  WS-VERIFY-HEADER-COUNT               PIC 9(4)
                                                   COMP-3 VALUE 0.
           05  WS-VERIFY-TRAILER-COUNT              PIC 9(4)
                                                   COMP-3 VALUE 0.

       01  WS-READ-RECORD                           PIC X(150).
       01  WS-READ-HEADER REDEFINES WS-READ-RECORD.
           05  VRH-TYPE                            PIC X.
           05  VRH-INTERFACE                       PIC X(8).
           05  VRH-SOURCE                           PIC X(8).
           05  VRH-DATE                             PIC 9(8).
           05  VRH-VERSION                          PIC X(4).
           05  FILLER                               PIC X(121).
       01  WS-READ-DETAIL REDEFINES WS-READ-RECORD.
           05  VRD-TYPE                            PIC X.
           05  VRD-SEQ                             PIC 9(9).
           05  VRD-ACCOUNT                         PIC 9(11).
           05  VRD-TRANS-TYPE                      PIC X(2).
           05  VRD-CATEGORY                        PIC 9(4).
           05  VRD-TRANS-COUNT                     PIC 9(9).
           05  VRD-DEBIT                           PIC S9(13)V99
                                                   SIGN IS LEADING
                                                   SEPARATE CHARACTER.
           05  VRD-CREDIT                          PIC S9(13)V99
                                                   SIGN IS LEADING
                                                   SEPARATE CHARACTER.
           05  VRD-NET                             PIC S9(13)V99
                                                   SIGN IS LEADING
                                                   SEPARATE CHARACTER.
           05  VRD-CATBAL                          PIC S9(13)V99
                                                   SIGN IS LEADING
                                                   SEPARATE CHARACTER.
           05  VRD-TIE                             PIC X.
           05  FILLER                               PIC X(49).
       01  WS-READ-TRAILER REDEFINES WS-READ-RECORD.
           05  VRT-TYPE                            PIC X.
           05  VRT-DETAIL                          PIC 9(9).
           05  VRT-TRAN                            PIC 9(11).
           05  VRT-DEBITS                          PIC S9(15)V99
                                                   SIGN IS LEADING
                                                   SEPARATE CHARACTER.
           05  VRT-CREDITS                         PIC S9(15)V99
                                                   SIGN IS LEADING
                                                   SEPARATE CHARACTER.
           05  VRT-NET                             PIC S9(15)V99
                                                   SIGN IS LEADING
                                                   SEPARATE CHARACTER.
           05  VRT-HASH                            PIC 9(18).
           05  VRT-RECORDS                         PIC 9(9).
           05  FILLER                               PIC X(48).

       01  WS-EXCEPTION-DATA.
           05  WS-EXCEPTION-COUNT                  PIC 9(4)
                                                   COMP-3 VALUE 0.
           05  WS-MORE-EXCEPTIONS                  PIC 9(9)
                                                   COMP-3 VALUE 0.
           05  WS-EXCEPTION-LINE OCCURS 50 TIMES
               PIC X(132).
           05  WS-EX-CODE                           PIC X(3).
           05  WS-EX-KEY                            PIC X(30).
           05  WS-EX-DESC                           PIC X(70).
           05  WS-EX-RC                             PIC 9(2) COMP-3.
           05  WS-EX-LINE                           PIC X(132).

       01  WS-IO-ERROR.
           05  WS-IO-DD                             PIC X(8).
           05  WS-IO-STATUS                         PIC XX.

       01  WS-REPORT-LINE                           PIC X(132).
       01  WS-REPORT-NUMBERS.
           05  RP-CONTROL-COUNT PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.
           05  RP-CONTROL-AMOUNT PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  RP-MORE-EXCEPTIONS PIC ZZZ,ZZZ,ZZ9.
           05  RP-RC                                 PIC 9(4).

       01  WS-REPORT-TEXT.
           05  WS-REPORT-RESULT                     PIC X(50)
                                                   VALUE SPACES.
           05  WS-REPORT-LABEL                      PIC X(40)
                                                   VALUE SPACES.
           05  WS-REPORT-COUNT                      PIC 9(18)
                                                   COMP-3 VALUE 0.
           05  WS-TOTAL-STATUS                      PIC X(4)
                                                   VALUE SPACES.
           05  WS-DETAILS-STATUS                    PIC X(4)
                                                   VALUE SPACES.
           05  WS-FEEDER-COUNT-STATUS               PIC X(4)
                                                   VALUE SPACES.
           05  WS-FEEDER-NET-STATUS                 PIC X(4)
                                                   VALUE SPACES.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 0100-OPEN-REPORT
           IF WS-ABORT-FLAG = 'N'
               PERFORM 0200-READ-CONTROL
           END-IF
           IF WS-ABORT-FLAG = 'N'
               PERFORM 0300-OPEN-SOURCE-FILES
           END-IF
           IF WS-ABORT-FLAG = 'N'
               PERFORM 0400-OPEN-OUTPUT
           END-IF
           IF WS-ABORT-FLAG = 'N'
               PERFORM 0500-LOAD-XREF
           END-IF
           IF WS-ABORT-FLAG = 'N'
               SORT SORT-WORK-FILE
                   ON ASCENDING KEY SORT-ACCT SORT-TYPE SORT-CAT
                                    SORT-TRAN-ID
                   INPUT PROCEDURE IS 3000-SORT-INPUT
                   OUTPUT PROCEDURE IS 5000-SORT-OUTPUT
           END-IF
           IF WS-ABORT-FLAG = 'N'
               COMPUTE WS-SOURCE-NET =
                   WS-SOURCE-DEBITS - WS-SOURCE-CREDITS
                   ON SIZE ERROR PERFORM 8930-RUN-TOTAL-OVERFLOW
               END-COMPUTE
           END-IF
           PERFORM 0600-CLOSE-SOURCE-FILES
           IF WS-OUT-OPEN = 'Y'
               CLOSE GLSUMOUT-FILE
               IF GLSUMOUT-STATUS NOT = '00'
                   MOVE 'GLSUMOUT' TO WS-IO-DD
                   MOVE GLSUMOUT-STATUS TO WS-IO-STATUS
                   PERFORM 9000-FILE-ERROR
               ELSE
                   MOVE 'N' TO WS-OUT-OPEN
               END-IF
           END-IF
           IF WS-ABORT-FLAG = 'N'
               PERFORM 7100-CHECK-FEEDER
           END-IF
           IF WS-ABORT-FLAG = 'N'
               PERFORM 7000-VERIFY-OUTPUT
           END-IF
           PERFORM 8000-WRITE-REPORT
           PERFORM 8100-CLOSE-REPORT
           PERFORM 8200-DISPLAY-RESULT
           MOVE WS-RETURN-CODE TO RETURN-CODE
           GOBACK.

       0100-OPEN-REPORT.
           OPEN OUTPUT GLRECRPT-FILE
           IF GLRECRPT-STATUS = '00'
               MOVE 'Y' TO WS-REPORT-OPEN
           ELSE
               MOVE 'GLRECRPT' TO WS-IO-DD
               MOVE GLRECRPT-STATUS TO WS-IO-STATUS
               PERFORM 9000-FILE-ERROR
           END-IF.

       0200-READ-CONTROL.
           OPEN INPUT GLPARM-FILE
           IF GLPARM-STATUS = '00'
               MOVE 'Y' TO WS-PARM-OPEN
           ELSE
               MOVE 'GLPARM' TO WS-IO-DD
               MOVE GLPARM-STATUS TO WS-IO-STATUS
               PERFORM 9000-FILE-ERROR
           END-IF
           IF WS-ABORT-FLAG = 'N'
               READ GLPARM-FILE
                   AT END CONTINUE
               END-READ
               IF GLPARM-STATUS = '10'
                   MOVE 'E02' TO WS-EX-CODE
                   MOVE 'GLPARM' TO WS-EX-KEY
                   MOVE 'CONTROL CARD MISSING' TO WS-EX-DESC
                   MOVE 12 TO WS-EX-RC
                   PERFORM 8800-ADD-EXCEPTION
               ELSE
                   IF GLPARM-STATUS NOT = '00'
                       MOVE 'GLPARM' TO WS-IO-DD
                       MOVE GLPARM-STATUS TO WS-IO-STATUS
                       PERFORM 9000-FILE-ERROR
                   ELSE
                       PERFORM 0210-VALIDATE-CONTROL
                   END-IF
               END-IF
           END-IF
           IF WS-PARM-OPEN = 'Y'
               CLOSE GLPARM-FILE
               IF GLPARM-STATUS NOT = '00'
                   MOVE 'GLPARM' TO WS-IO-DD
                   MOVE GLPARM-STATUS TO WS-IO-STATUS
                   PERFORM 9000-FILE-ERROR
               ELSE
                   MOVE 'N' TO WS-PARM-OPEN
               END-IF
           END-IF.

       0210-VALIDATE-CONTROL.
           MOVE CTL-RUN-DATE TO WS-DATE-TEXT
           PERFORM 8600-CHECK-DATE
           IF WS-VALID-DATE NOT = 'Y'
               OR WS-DATE-YEAR < 1900
               OR WS-DATE-YEAR > 2099
               PERFORM 0220-CONTROL-ERROR
           ELSE
               MOVE CTL-RUN-DATE TO WS-RUN-DATE
               MOVE SPACES TO WS-RUN-DATE-8
               STRING CTL-RUN-DATE (1:4)
                      CTL-RUN-DATE (6:2)
                      CTL-RUN-DATE (9:2)
                   DELIMITED BY SIZE INTO WS-RUN-DATE-8
               MOVE CTL-SOURCE-SYS-ID TO WS-SOURCE-ID
               IF CTL-SOURCE-SYS-ID = SPACES
                   PERFORM 0220-CONTROL-ERROR
               END-IF
           END-IF
           IF GLPARM-RECORD (21:9) = SPACES
               AND GLPARM-RECORD (31:16) = SPACES
               MOVE 'N' TO WS-HAS-EXPECTED
           ELSE
               IF CTL-EXPECTED-COUNT IS NUMERIC
                   AND CTL-EXPECTED-NET IS NUMERIC
                   IF CTL-EXPECTED-NET (1:1) = '+'
                       OR CTL-EXPECTED-NET (1:1) = '-'
                       MOVE CTL-EXPECTED-COUNT
                         TO WS-EXPECTED-COUNT
                       MOVE CTL-EXPECTED-NET TO WS-EXPECTED-NET
                       MOVE 'Y' TO WS-HAS-EXPECTED
                   ELSE
                       PERFORM 0220-CONTROL-ERROR
                   END-IF
               ELSE
                   PERFORM 0220-CONTROL-ERROR
               END-IF
           END-IF.

       0220-CONTROL-ERROR.
           IF WS-ABORT-FLAG = 'N'
               MOVE 'E02' TO WS-EX-CODE
               MOVE 'GLPARM' TO WS-EX-KEY
               MOVE 'CONTROL CARD INVALID' TO WS-EX-DESC
               MOVE 12 TO WS-EX-RC
               PERFORM 8800-ADD-EXCEPTION
           END-IF.

       0300-OPEN-SOURCE-FILES.
           OPEN INPUT XREFFILE-FILE
           IF XREFFILE-STATUS = '00'
               MOVE 'Y' TO WS-XREF-OPEN
           ELSE
               MOVE 'XREFFILE' TO WS-IO-DD
               MOVE XREFFILE-STATUS TO WS-IO-STATUS
               PERFORM 9000-FILE-ERROR
           END-IF
           IF WS-ABORT-FLAG = 'N'
               OPEN INPUT TRANFILE-FILE
               IF TRANFILE-STATUS = '00'
                   MOVE 'Y' TO WS-TRAN-OPEN
               ELSE
                   MOVE 'TRANFILE' TO WS-IO-DD
                   MOVE TRANFILE-STATUS TO WS-IO-STATUS
                   PERFORM 9000-FILE-ERROR
               END-IF
           END-IF
           IF WS-ABORT-FLAG = 'N'
               OPEN INPUT TCATBALF-FILE
               IF TCATBALF-STATUS = '00'
                   MOVE 'Y' TO WS-TCAT-OPEN
               ELSE
                   MOVE 'TCATBALF' TO WS-IO-DD
                   MOVE TCATBALF-STATUS TO WS-IO-STATUS
                   PERFORM 9000-FILE-ERROR
               END-IF
           END-IF.

       0400-OPEN-OUTPUT.
           OPEN OUTPUT GLSUMOUT-FILE
           IF GLSUMOUT-STATUS = '00'
               MOVE 'Y' TO WS-OUT-OPEN
               PERFORM 0410-WRITE-HEADER
           ELSE
               MOVE 'GLSUMOUT' TO WS-IO-DD
               MOVE GLSUMOUT-STATUS TO WS-IO-STATUS
               PERFORM 9000-FILE-ERROR
           END-IF.

       0410-WRITE-HEADER.
           MOVE SPACES TO GLSUM-HEADER
           MOVE 'H' TO GLH-REC-TYPE
           MOVE 'GLSUMM01' TO GLH-INTERFACE-ID
           MOVE WS-SOURCE-ID TO GLH-SOURCE-SYS-ID
           MOVE WS-RUN-DATE-8 TO GLH-RUN-DATE
           MOVE '0100' TO GLH-LAYOUT-VERSION
           MOVE GLSUM-HEADER TO GLSUMOUT-RECORD
           WRITE GLSUMOUT-RECORD
           IF GLSUMOUT-STATUS = '00'
               MOVE 'Y' TO WS-HEADER-WRITTEN
           ELSE
               MOVE 'GLSUMOUT' TO WS-IO-DD
               MOVE GLSUMOUT-STATUS TO WS-IO-STATUS
               PERFORM 9000-FILE-ERROR
           END-IF.

       0500-LOAD-XREF.
           MOVE SPACES TO WS-XREF-PREV-CARD
           PERFORM UNTIL WS-EOF = 'Y' OR WS-ABORT-FLAG = 'Y'
               READ XREFFILE-FILE
                   AT END CONTINUE
               END-READ
               EVALUATE XREFFILE-STATUS
                   WHEN '00'
                       ADD 1 TO WS-XREF-COUNT
                       PERFORM 0510-CHECK-XREF
                   WHEN '10'
                       MOVE 'Y' TO WS-EOF
                   WHEN OTHER
                       MOVE 'XREFFILE' TO WS-IO-DD
                       MOVE XREFFILE-STATUS TO WS-IO-STATUS
                       PERFORM 9000-FILE-ERROR
               END-EVALUATE
           END-PERFORM
           MOVE 'N' TO WS-EOF
           IF WS-XREF-OPEN = 'Y'
               CLOSE XREFFILE-FILE
               IF XREFFILE-STATUS NOT = '00'
                   MOVE 'XREFFILE' TO WS-IO-DD
                   MOVE XREFFILE-STATUS TO WS-IO-STATUS
                   PERFORM 9000-FILE-ERROR
               ELSE
                   MOVE 'N' TO WS-XREF-OPEN
               END-IF
           END-IF.

       0510-CHECK-XREF.
           IF WS-XREF-COUNT > 10000
               MOVE 'E05' TO WS-EX-CODE
               MOVE 'XREFFILE' TO WS-EX-KEY
               MOVE 'XREF ENTRY LIMIT EXCEEDED' TO WS-EX-DESC
               MOVE 12 TO WS-EX-RC
               PERFORM 8800-ADD-EXCEPTION
           ELSE
               IF XREF-ACCT-ID IS NOT NUMERIC
                   OR (WS-XREF-COUNT > 1
                       AND XREF-CARD-NUM <= WS-XREF-PREV-CARD)
                   MOVE 'E05' TO WS-EX-CODE
                   MOVE XREF-CARD-NUM TO WS-EX-KEY
                   MOVE 'XREF ORDER OR ACCOUNT INVALID' TO WS-EX-DESC
                   MOVE 12 TO WS-EX-RC
                   PERFORM 8800-ADD-EXCEPTION
               ELSE
                   MOVE XREF-CARD-NUM TO WS-XREF-CARD (WS-XREF-COUNT)
                   MOVE XREF-ACCT-ID
                     TO WS-XREF-ACCOUNT (WS-XREF-COUNT)
                   MOVE XREF-CARD-NUM TO WS-XREF-PREV-CARD
               END-IF
           END-IF.

       0600-CLOSE-SOURCE-FILES.
           IF WS-XREF-OPEN = 'Y'
               CLOSE XREFFILE-FILE
               IF XREFFILE-STATUS NOT = '00'
                   MOVE 'XREFFILE' TO WS-IO-DD
                   MOVE XREFFILE-STATUS TO WS-IO-STATUS
                   PERFORM 9000-FILE-ERROR
               ELSE
                   MOVE 'N' TO WS-XREF-OPEN
               END-IF
           END-IF
           IF WS-TRAN-OPEN = 'Y'
               CLOSE TRANFILE-FILE
               IF TRANFILE-STATUS NOT = '00'
                   MOVE 'TRANFILE' TO WS-IO-DD
                   MOVE TRANFILE-STATUS TO WS-IO-STATUS
                   PERFORM 9000-FILE-ERROR
               ELSE
                   MOVE 'N' TO WS-TRAN-OPEN
               END-IF
           END-IF
           IF WS-TCAT-OPEN = 'Y'
               CLOSE TCATBALF-FILE
               IF TCATBALF-STATUS NOT = '00'
                   MOVE 'TCATBALF' TO WS-IO-DD
                   MOVE TCATBALF-STATUS TO WS-IO-STATUS
                   PERFORM 9000-FILE-ERROR
               ELSE
                   MOVE 'N' TO WS-TCAT-OPEN
               END-IF
           END-IF.

       8600-CHECK-DATE.
           MOVE 'N' TO WS-VALID-DATE
           IF WS-DATE-TEXT (1:4) IS NUMERIC
               AND WS-DATE-TEXT (5:1) = '-'
               AND WS-DATE-TEXT (6:2) IS NUMERIC
               AND WS-DATE-TEXT (8:1) = '-'
               AND WS-DATE-TEXT (9:2) IS NUMERIC
               MOVE WS-DATE-TEXT (1:4) TO WS-DATE-YEAR
               MOVE WS-DATE-TEXT (6:2) TO WS-DATE-MONTH
               MOVE WS-DATE-TEXT (9:2) TO WS-DATE-DAY
               IF WS-DATE-YEAR > 0 AND WS-DATE-MONTH >= 1
                   AND WS-DATE-MONTH <= 12
                   MOVE 31 TO WS-DATE-MAX-DAY
                   IF WS-DATE-MONTH = 4 OR 6 OR 9 OR 11
                       MOVE 30 TO WS-DATE-MAX-DAY
                   END-IF
                   IF WS-DATE-MONTH = 2
                       MOVE 28 TO WS-DATE-MAX-DAY
                       DIVIDE WS-DATE-YEAR BY 400
                           GIVING WS-DATE-QUOTIENT
                           REMAINDER WS-DATE-REMAINDER
                       IF WS-DATE-REMAINDER = 0
                           MOVE 29 TO WS-DATE-MAX-DAY
                       ELSE
                           DIVIDE WS-DATE-YEAR BY 100
                               GIVING WS-DATE-QUOTIENT
                               REMAINDER WS-DATE-REMAINDER
                           IF WS-DATE-REMAINDER NOT = 0
                               DIVIDE WS-DATE-YEAR BY 4
                                   GIVING WS-DATE-QUOTIENT
                                   REMAINDER WS-DATE-REMAINDER
                               IF WS-DATE-REMAINDER = 0
                                   MOVE 29 TO WS-DATE-MAX-DAY
                               END-IF
                           END-IF
                       END-IF
                   END-IF
                   IF WS-DATE-DAY >= 1
                       AND WS-DATE-DAY <= WS-DATE-MAX-DAY
                       MOVE 'Y' TO WS-VALID-DATE
                   END-IF
               END-IF
           END-IF.

       8800-ADD-EXCEPTION.
           IF WS-EX-RC > WS-RETURN-CODE
               MOVE WS-EX-RC TO WS-RETURN-CODE
           END-IF
           IF WS-EXCEPTION-COUNT < 50
               ADD 1 TO WS-EXCEPTION-COUNT
               MOVE SPACES TO WS-EX-LINE
               MOVE WS-EX-CODE TO WS-EX-LINE (1:3)
               MOVE WS-EX-KEY TO WS-EX-LINE (7:30)
               MOVE WS-EX-DESC TO WS-EX-LINE (39:70)
               MOVE WS-EX-LINE
                 TO WS-EXCEPTION-LINE (WS-EXCEPTION-COUNT)
           ELSE
               ADD 1 TO WS-MORE-EXCEPTIONS
           END-IF
           IF WS-EX-RC = 12 AND WS-ABORT-FLAG = 'N'
               MOVE 'Y' TO WS-ABORT-FLAG
               IF WS-EX-CODE = 'E06'
                   MOVE 'AMOUNT OR HASH CAPACITY EXCEEDED'
                     TO WS-ABORT-REASON
               ELSE
                   IF WS-EX-CODE = 'E04'
                   MOVE 'CATEGORY BALANCE RECORD INVALID'
                     TO WS-ABORT-REASON
                   ELSE
                       MOVE WS-EX-DESC TO WS-ABORT-REASON
                   END-IF
               END-IF
           END-IF.

       9000-FILE-ERROR.
           DISPLAY 'ABEND DD=' WS-IO-DD
                   ' STATUS=' WS-IO-STATUS
           MOVE 'E01' TO WS-EX-CODE
           MOVE WS-IO-DD TO WS-EX-KEY
           MOVE 'FILE OPEN READ WRITE OR CLOSE ERROR'
             TO WS-EX-DESC
           MOVE 12 TO WS-EX-RC
           PERFORM 8800-ADD-EXCEPTION.

       3000-SORT-INPUT.
           MOVE 'N' TO WS-EOF
           MOVE 'N' TO WS-HAVE-PREV-TRAN
           MOVE SPACES TO WS-PREV-TRAN-ID
           PERFORM UNTIL WS-EOF = 'Y'
               READ TRANFILE-FILE
                   AT END CONTINUE
               END-READ
               EVALUATE TRANFILE-STATUS
                   WHEN '00'
                       ADD 1 TO WS-TRAN-READ-COUNT
                       PERFORM 3010-PROCESS-TRAN
                   WHEN '10'
                       MOVE 'Y' TO WS-EOF
                   WHEN OTHER
                       MOVE 'TRANFILE' TO WS-IO-DD
                       MOVE TRANFILE-STATUS TO WS-IO-STATUS
                       PERFORM 9000-FILE-ERROR
                       MOVE 'Y' TO WS-EOF
               END-EVALUATE
           END-PERFORM
           IF WS-TRAN-READ-COUNT = 0
               MOVE 'W03' TO WS-EX-CODE
               MOVE 'TRANFILE' TO WS-EX-KEY
               MOVE 'NO TRANSACTIONS READ' TO WS-EX-DESC
               MOVE 4 TO WS-EX-RC
               PERFORM 8800-ADD-EXCEPTION
           END-IF
           IF WS-TRAN-OPEN = 'Y'
               CLOSE TRANFILE-FILE
               IF TRANFILE-STATUS NOT = '00'
                   MOVE 'TRANFILE' TO WS-IO-DD
                   MOVE TRANFILE-STATUS TO WS-IO-STATUS
                   PERFORM 9000-FILE-ERROR
               ELSE
                   MOVE 'N' TO WS-TRAN-OPEN
               END-IF
           END-IF.

       3010-PROCESS-TRAN.
           IF WS-ABORT-FLAG = 'N'
               IF WS-HAVE-PREV-TRAN = 'Y'
                   IF TRAN-ID < WS-PREV-TRAN-ID
                       MOVE 'E03' TO WS-EX-CODE
                       MOVE TRAN-ID TO WS-EX-KEY
                       MOVE 'TRANSACTION KEY OUT OF ORDER'
                         TO WS-EX-DESC
                       MOVE 12 TO WS-EX-RC
                       PERFORM 8800-ADD-EXCEPTION
                   ELSE
                       IF TRAN-ID = WS-PREV-TRAN-ID
                           MOVE 'R01' TO WS-EX-CODE
                           MOVE TRAN-ID TO WS-EX-KEY
                           MOVE 'DUPLICATE TRANSACTION KEY'
                             TO WS-EX-DESC
                           MOVE 4 TO WS-EX-RC
                           PERFORM 8800-ADD-EXCEPTION
                           ADD 1 TO WS-TRAN-REJECT-COUNT
                       ELSE
                           PERFORM 3020-CHECK-TRANSACTION
                       END-IF
                   END-IF
               ELSE
                   PERFORM 3020-CHECK-TRANSACTION
               END-IF
               MOVE TRAN-ID TO WS-PREV-TRAN-ID
               MOVE 'Y' TO WS-HAVE-PREV-TRAN
           END-IF.

       3020-CHECK-TRANSACTION.
           IF TRAN-AMT IS NOT NUMERIC
               MOVE 'R02' TO WS-EX-CODE
               MOVE TRAN-ID TO WS-EX-KEY
               MOVE 'TRANSACTION AMOUNT NOT NUMERIC' TO WS-EX-DESC
               MOVE 4 TO WS-EX-RC
               PERFORM 8800-ADD-EXCEPTION
               ADD 1 TO WS-TRAN-REJECT-COUNT
           ELSE
               IF TRAN-CAT-CD IS NOT NUMERIC
                   MOVE 'R03' TO WS-EX-CODE
                   MOVE TRAN-ID TO WS-EX-KEY
                   MOVE 'TRANSACTION CATEGORY NOT NUMERIC'
                     TO WS-EX-DESC
                   MOVE 4 TO WS-EX-RC
                   PERFORM 8800-ADD-EXCEPTION
                   ADD 1 TO WS-TRAN-REJECT-COUNT
               ELSE
                   PERFORM 3030-LOOKUP-XREF
               END-IF
           END-IF.

       3030-LOOKUP-XREF.
           MOVE 'N' TO WS-XREF-FOUND
           IF WS-XREF-COUNT > 0
               SEARCH ALL WS-XREF-ENTRY
                   AT END CONTINUE
                   WHEN WS-XREF-CARD (WS-XREF-IDX)
                       = TRAN-CARD-NUM
                       MOVE 'Y' TO WS-XREF-FOUND
               END-SEARCH
           END-IF
           IF WS-XREF-FOUND = 'N'
               MOVE 'R04' TO WS-EX-CODE
               MOVE TRAN-CARD-NUM TO WS-EX-KEY
               MOVE 'CARD NOT FOUND IN CROSS REFERENCE'
                 TO WS-EX-DESC
               MOVE 4 TO WS-EX-RC
               PERFORM 8800-ADD-EXCEPTION
               ADD 1 TO WS-TRAN-REJECT-COUNT
           ELSE
               PERFORM 3040-ACCEPT-TRANSACTION
           END-IF.

       3040-ACCEPT-TRANSACTION.
           ADD 1 TO WS-TRAN-ACCEPT-COUNT
           IF TRAN-AMT >= 0
               ADD TRAN-AMT TO WS-SOURCE-DEBITS
                   ON SIZE ERROR PERFORM 8930-RUN-TOTAL-OVERFLOW
               END-ADD
           ELSE
               COMPUTE WS-ABS-AMOUNT = 0 - TRAN-AMT
                   ON SIZE ERROR PERFORM 8930-RUN-TOTAL-OVERFLOW
               END-COMPUTE
               ADD WS-ABS-AMOUNT TO WS-SOURCE-CREDITS
                   ON SIZE ERROR PERFORM 8930-RUN-TOTAL-OVERFLOW
               END-ADD
           END-IF
           IF WS-ABORT-FLAG = 'N'
               MOVE TRAN-PROC-TS (1:10) TO WS-DATE-TEXT
               PERFORM 8600-CHECK-DATE
               IF WS-VALID-DATE NOT = 'Y'
                   ADD 1 TO WS-TRAN-WARN-COUNT
                   MOVE 'W01' TO WS-EX-CODE
                   MOVE TRAN-ID TO WS-EX-KEY
                   MOVE 'PROCESSING DATE IS INVALID'
                     TO WS-EX-DESC
                   MOVE 4 TO WS-EX-RC
                   PERFORM 8800-ADD-EXCEPTION
               ELSE
                   IF TRAN-PROC-TS (1:10) > WS-RUN-DATE
                       ADD 1 TO WS-TRAN-WARN-COUNT
                       MOVE 'W02' TO WS-EX-CODE
                       MOVE TRAN-ID TO WS-EX-KEY
                       MOVE 'PROCESSING DATE AFTER RUN DATE'
                         TO WS-EX-DESC
                       MOVE 4 TO WS-EX-RC
                       PERFORM 8800-ADD-EXCEPTION
                   END-IF
               END-IF
           END-IF
           IF WS-ABORT-FLAG = 'N'
               MOVE WS-XREF-ACCOUNT (WS-XREF-IDX) TO SORT-ACCT
               MOVE TRAN-TYPE-CD TO SORT-TYPE
               MOVE TRAN-CAT-CD TO SORT-CAT
               MOVE TRAN-ID TO SORT-TRAN-ID
               MOVE TRAN-AMT TO SORT-AMT
               RELEASE SORT-RECORD
           END-IF.

       5000-SORT-OUTPUT.
           MOVE 'N' TO WS-SORT-EOF
           IF WS-ABORT-FLAG = 'N'
               MOVE 'N' TO WS-TCAT-EOF
               MOVE 'N' TO WS-HAVE-TCAT
               MOVE SPACES TO WS-TCAT-PREV-KEY
               PERFORM 5100-READ-TCAT
           END-IF
           PERFORM UNTIL WS-SORT-EOF = 'Y'
               RETURN SORT-WORK-FILE
                   AT END MOVE 'Y' TO WS-SORT-EOF
               END-RETURN
               IF WS-SORT-EOF = 'N'
                   IF WS-ABORT-FLAG = 'N'
                       PERFORM 5010-PROCESS-SORT-RECORD
                   END-IF
               END-IF
           END-PERFORM
           IF WS-ABORT-FLAG = 'N' AND WS-HAVE-KEY = 'Y'
               PERFORM 5200-FINALIZE-KEY
           END-IF
           IF WS-ABORT-FLAG = 'N'
               PERFORM 5410-DRAIN-TCAT
           END-IF
           IF WS-ABORT-FLAG = 'N'
               PERFORM 5300-WRITE-TRAILER
           END-IF
           IF WS-TCAT-OPEN = 'Y'
               CLOSE TCATBALF-FILE
               IF TCATBALF-STATUS NOT = '00'
                   MOVE 'TCATBALF' TO WS-IO-DD
                   MOVE TCATBALF-STATUS TO WS-IO-STATUS
                   PERFORM 9000-FILE-ERROR
               ELSE
                   MOVE 'N' TO WS-TCAT-OPEN
               END-IF
           END-IF.

       5010-PROCESS-SORT-RECORD.
           IF WS-HAVE-KEY = 'N'
               PERFORM 5020-START-KEY
           ELSE
               IF SORT-ACCT NOT = WS-CURRENT-ACCOUNT
                   OR SORT-TYPE NOT = WS-CURRENT-TYPE
                   OR SORT-CAT NOT = WS-CURRENT-CATEGORY
                   PERFORM 5200-FINALIZE-KEY
                   IF WS-ABORT-FLAG = 'N'
                       PERFORM 5020-START-KEY
                   END-IF
               END-IF
           END-IF
           IF WS-ABORT-FLAG = 'N'
               PERFORM 5030-ADD-SORT-AMOUNT
           END-IF.

       5020-START-KEY.
           MOVE SORT-ACCT TO WS-CURRENT-ACCOUNT
           MOVE SORT-TYPE TO WS-CURRENT-TYPE
           MOVE SORT-CAT TO WS-CURRENT-CATEGORY
           MOVE 0 TO WS-KEY-COUNT
           MOVE 0 TO WS-KEY-DEBITS
           MOVE 0 TO WS-KEY-CREDITS
           MOVE 0 TO WS-KEY-NET
           MOVE 'Y' TO WS-HAVE-KEY.

       5030-ADD-SORT-AMOUNT.
           ADD 1 TO WS-KEY-COUNT
               ON SIZE ERROR PERFORM 8920-KEY-COUNT-OVERFLOW
           END-ADD
           IF SORT-AMT >= 0
               ADD SORT-AMT TO WS-KEY-DEBITS
                   ON SIZE ERROR PERFORM 8910-KEY-AMOUNT-OVERFLOW
               END-ADD
           ELSE
               COMPUTE WS-ABS-AMOUNT = 0 - SORT-AMT
                   ON SIZE ERROR PERFORM 8910-KEY-AMOUNT-OVERFLOW
               END-COMPUTE
               ADD WS-ABS-AMOUNT TO WS-KEY-CREDITS
                   ON SIZE ERROR PERFORM 8910-KEY-AMOUNT-OVERFLOW
               END-ADD
           END-IF.

       5100-READ-TCAT.
           READ TCATBALF-FILE
               AT END CONTINUE
           END-READ
           EVALUATE TCATBALF-STATUS
               WHEN '00'
                   ADD 1 TO WS-TCAT-READ-COUNT
                   MOVE 'Y' TO WS-HAVE-TCAT
                   MOVE TRAN-CAT-KEY TO WS-TCAT-CURRENT-KEY
                   IF TRAN-CAT-BAL IS NOT NUMERIC
                       OR TRANCAT-ACCT-ID IS NOT NUMERIC
                       OR TRANCAT-CD IS NOT NUMERIC
                       OR (WS-TCAT-READ-COUNT > 1
                           AND TRAN-CAT-KEY <= WS-TCAT-PREV-KEY)
                       MOVE 'E04' TO WS-EX-CODE
                       MOVE WS-TCAT-CURRENT-KEY TO WS-EX-KEY
                       MOVE 'CATEGORY BALANCE ORDER OR VALUE INVALID'
                         TO WS-EX-DESC
                       MOVE 12 TO WS-EX-RC
                       PERFORM 8800-ADD-EXCEPTION
                   ELSE
                       MOVE TRAN-CAT-KEY TO WS-TCAT-PREV-KEY
                   END-IF
               WHEN '10'
                   MOVE 'N' TO WS-HAVE-TCAT
                   MOVE 'Y' TO WS-TCAT-EOF
               WHEN OTHER
                   MOVE 'TCATBALF' TO WS-IO-DD
                   MOVE TCATBALF-STATUS TO WS-IO-STATUS
                   PERFORM 9000-FILE-ERROR
                   MOVE 'N' TO WS-HAVE-TCAT
                   MOVE 'Y' TO WS-TCAT-EOF
           END-EVALUATE.

       5200-FINALIZE-KEY.
           MOVE WS-ACTIVITY-KEY TO WS-TCAT-ACTIVITY-KEY
           PERFORM UNTIL WS-HAVE-TCAT NOT = 'Y'
               OR WS-ABORT-FLAG = 'Y'
               IF WS-TCAT-CURRENT-KEY < WS-TCAT-ACTIVITY-KEY
                   PERFORM 5400-REPORT-BALANCE-ONLY
                   IF WS-ABORT-FLAG = 'N'
                       PERFORM 5100-READ-TCAT
                   END-IF
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM
           IF WS-ABORT-FLAG = 'N'
               MOVE 0 TO WS-CATEGORY-BALANCE
               MOVE 'N' TO WS-TIE-FOUND
               IF WS-HAVE-TCAT = 'Y'
                   IF WS-TCAT-CURRENT-KEY = WS-TCAT-ACTIVITY-KEY
                       MOVE TRAN-CAT-BAL TO WS-CATEGORY-BALANCE
                       MOVE 'Y' TO WS-TIE-FOUND
                   END-IF
               END-IF
               COMPUTE WS-KEY-NET = WS-KEY-DEBITS - WS-KEY-CREDITS
                   ON SIZE ERROR PERFORM 8910-KEY-AMOUNT-OVERFLOW
               END-COMPUTE
               IF WS-ABORT-FLAG = 'N'
                   IF WS-TIE-FOUND = 'N'
                       MOVE 'M' TO WS-TIE-STATUS
                       MOVE 'B02' TO WS-EX-CODE
                       MOVE WS-TCAT-ACTIVITY-KEY TO WS-EX-KEY
                       MOVE 'ACTIVITY KEY MISSING FROM BALANCE FILE'
                         TO WS-EX-DESC
                       MOVE 8 TO WS-EX-RC
                       PERFORM 8800-ADD-EXCEPTION
                   ELSE
                       IF WS-KEY-NET = WS-CATEGORY-BALANCE
                           MOVE 'T' TO WS-TIE-STATUS
                       ELSE
                           MOVE 'D' TO WS-TIE-STATUS
                           MOVE 'B01' TO WS-EX-CODE
                           MOVE WS-TCAT-ACTIVITY-KEY TO WS-EX-KEY
                           MOVE 'ACTIVITY DIFFERS FROM CATEGORY BALANCE'
                             TO WS-EX-DESC
                           MOVE 8 TO WS-EX-RC
                           PERFORM 8800-ADD-EXCEPTION
                       END-IF
                   END-IF
                   PERFORM 5210-WRITE-DETAIL
               END-IF
               IF WS-ABORT-FLAG = 'N'
                   AND WS-TIE-FOUND = 'Y'
                   PERFORM 5100-READ-TCAT
               END-IF
           END-IF
           MOVE 'N' TO WS-HAVE-KEY.

       5210-WRITE-DETAIL.
           MOVE WS-BUILD-DETAIL-COUNT TO WS-CANDIDATE-DETAIL-COUNT
           ADD 1 TO WS-CANDIDATE-DETAIL-COUNT
               ON SIZE ERROR PERFORM 8950-RUN-DETAIL-COUNT-OVERFLOW
           END-ADD
           MOVE WS-BUILD-TRAN-COUNT TO WS-CANDIDATE-TRAN-COUNT
           ADD WS-KEY-COUNT TO WS-CANDIDATE-TRAN-COUNT
               ON SIZE ERROR PERFORM 8940-RUN-TRAN-COUNT-OVERFLOW
           END-ADD
           MOVE WS-BUILD-DEBITS TO WS-CANDIDATE-DEBITS
           ADD WS-KEY-DEBITS TO WS-CANDIDATE-DEBITS
               ON SIZE ERROR PERFORM 8930-RUN-TOTAL-OVERFLOW
           END-ADD
           MOVE WS-BUILD-CREDITS TO WS-CANDIDATE-CREDITS
           ADD WS-KEY-CREDITS TO WS-CANDIDATE-CREDITS
               ON SIZE ERROR PERFORM 8930-RUN-TOTAL-OVERFLOW
           END-ADD
           MOVE WS-BUILD-HASH TO WS-CANDIDATE-HASH
           ADD WS-CURRENT-ACCOUNT TO WS-CANDIDATE-HASH
               ON SIZE ERROR PERFORM 8960-HASH-OVERFLOW
           END-ADD
           IF WS-ABORT-FLAG = 'N'
               MOVE SPACES TO GLSUM-DETAIL
               MOVE 'D' TO GLD-REC-TYPE
               MOVE WS-CANDIDATE-DETAIL-COUNT TO GLD-SEQ-NO
               MOVE WS-CURRENT-ACCOUNT TO GLD-ACCT-ID
               MOVE WS-CURRENT-TYPE TO GLD-TYPE-CD
               MOVE WS-CURRENT-CATEGORY TO GLD-CAT-CD
               MOVE WS-KEY-COUNT TO GLD-TRAN-COUNT
               MOVE WS-KEY-DEBITS TO GLD-DEBIT-AMT
               MOVE WS-KEY-CREDITS TO GLD-CREDIT-AMT
               MOVE WS-KEY-NET TO GLD-NET-AMT
               MOVE WS-CATEGORY-BALANCE TO GLD-CATBAL-AMT
               MOVE WS-TIE-STATUS TO GLD-TIE-STATUS
               MOVE GLSUM-DETAIL TO GLSUMOUT-RECORD
               WRITE GLSUMOUT-RECORD
               IF GLSUMOUT-STATUS = '00'
                   MOVE WS-CANDIDATE-DETAIL-COUNT
                     TO WS-BUILD-DETAIL-COUNT
                   MOVE WS-CANDIDATE-TRAN-COUNT
                     TO WS-BUILD-TRAN-COUNT
                   MOVE WS-CANDIDATE-DEBITS TO WS-BUILD-DEBITS
                   MOVE WS-CANDIDATE-CREDITS TO WS-BUILD-CREDITS
                   MOVE WS-CANDIDATE-HASH TO WS-BUILD-HASH
                   COMPUTE WS-BUILD-NET =
                       WS-BUILD-DEBITS - WS-BUILD-CREDITS
                       ON SIZE ERROR PERFORM 8930-RUN-TOTAL-OVERFLOW
                   END-COMPUTE
                   EVALUATE WS-TIE-STATUS
                       WHEN 'T'
                           ADD 1 TO WS-KEYS-TIED
                               ON SIZE ERROR
                                 PERFORM 8970-COUNT-OVERFLOW
                           END-ADD
                       WHEN 'D'
                           ADD 1 TO WS-KEYS-DIFFERING
                               ON SIZE ERROR
                                 PERFORM 8970-COUNT-OVERFLOW
                           END-ADD
                       WHEN 'M'
                           ADD 1 TO WS-KEYS-MISSING
                               ON SIZE ERROR
                                 PERFORM 8970-COUNT-OVERFLOW
                           END-ADD
                   END-EVALUATE
               ELSE
                   MOVE 'GLSUMOUT' TO WS-IO-DD
                   MOVE GLSUMOUT-STATUS TO WS-IO-STATUS
                   PERFORM 9000-FILE-ERROR
               END-IF
           END-IF.

       5300-WRITE-TRAILER.
           MOVE SPACES TO GLSUM-TRAILER
           MOVE 'T' TO GLT-REC-TYPE
           MOVE WS-BUILD-DETAIL-COUNT TO GLT-DETAIL-COUNT
           MOVE WS-BUILD-TRAN-COUNT TO GLT-TRAN-COUNT
           MOVE WS-BUILD-DEBITS TO GLT-TOTAL-DEBITS
           MOVE WS-BUILD-CREDITS TO GLT-TOTAL-CREDITS
           MOVE WS-BUILD-NET TO GLT-TOTAL-NET
           MOVE WS-BUILD-HASH TO GLT-HASH-TOTAL
           COMPUTE GLT-RECORD-COUNT =
               WS-BUILD-DETAIL-COUNT + 2
               ON SIZE ERROR PERFORM 8950-RUN-DETAIL-COUNT-OVERFLOW
           END-COMPUTE
           IF WS-ABORT-FLAG = 'N'
               MOVE GLSUM-TRAILER TO GLSUMOUT-RECORD
               WRITE GLSUMOUT-RECORD
               IF GLSUMOUT-STATUS NOT = '00'
                   MOVE 'GLSUMOUT' TO WS-IO-DD
                   MOVE GLSUMOUT-STATUS TO WS-IO-STATUS
                   PERFORM 9000-FILE-ERROR
               END-IF
           END-IF.

       5400-REPORT-BALANCE-ONLY.
           IF TRAN-CAT-BAL NOT = 0
               ADD 1 TO WS-BALANCE-ONLY
                   ON SIZE ERROR
                     PERFORM 8970-COUNT-OVERFLOW
               END-ADD
               MOVE 'B03' TO WS-EX-CODE
               MOVE WS-TCAT-CURRENT-KEY TO WS-EX-KEY
               MOVE 'NONZERO BALANCE HAS NO ACTIVITY'
                 TO WS-EX-DESC
               MOVE 8 TO WS-EX-RC
               PERFORM 8800-ADD-EXCEPTION
           END-IF.

       5410-DRAIN-TCAT.
           PERFORM UNTIL WS-HAVE-TCAT NOT = 'Y'
               OR WS-ABORT-FLAG = 'Y'
               PERFORM 5400-REPORT-BALANCE-ONLY
               IF WS-ABORT-FLAG = 'N'
                   PERFORM 5100-READ-TCAT
               END-IF
           END-PERFORM.

       8900-AMOUNT-OVERFLOW.
           MOVE 'E06' TO WS-EX-CODE
           MOVE 12 TO WS-EX-RC
           PERFORM 8800-ADD-EXCEPTION.

       8910-KEY-AMOUNT-OVERFLOW.
           MOVE WS-ACTIVITY-KEY TO WS-EX-KEY
           MOVE 'KEY TOTAL EXCEEDS S9(13)V99' TO WS-EX-DESC
           PERFORM 8900-AMOUNT-OVERFLOW.

       8920-KEY-COUNT-OVERFLOW.
           MOVE WS-ACTIVITY-KEY TO WS-EX-KEY
           MOVE 'KEY COUNT EXCEEDS 9(9)' TO WS-EX-DESC
           PERFORM 8900-AMOUNT-OVERFLOW.

       8930-RUN-TOTAL-OVERFLOW.
           MOVE 'RUN TOTALS' TO WS-EX-KEY
           MOVE 'RUN TOTAL EXCEEDS S9(15)V99' TO WS-EX-DESC
           PERFORM 8900-AMOUNT-OVERFLOW.

       8940-RUN-TRAN-COUNT-OVERFLOW.
           MOVE 'RUN TOTALS' TO WS-EX-KEY
           MOVE 'TRANSACTION COUNT EXCEEDS 9(11)' TO WS-EX-DESC
           PERFORM 8900-AMOUNT-OVERFLOW.

       8950-RUN-DETAIL-COUNT-OVERFLOW.
           MOVE 'RUN TOTALS' TO WS-EX-KEY
           MOVE 'DETAIL COUNT EXCEEDS 9(9)' TO WS-EX-DESC
           PERFORM 8900-AMOUNT-OVERFLOW.

       8960-HASH-OVERFLOW.
           MOVE 'RUN TOTALS' TO WS-EX-KEY
           MOVE 'HASH TOTAL EXCEEDS 9(18)' TO WS-EX-DESC
           PERFORM 8900-AMOUNT-OVERFLOW.

       8970-COUNT-OVERFLOW.
           MOVE 'RUN TOTALS' TO WS-EX-KEY
           MOVE 'RECONCILIATION COUNT EXCEEDS 9(9)'
             TO WS-EX-DESC
           PERFORM 8900-AMOUNT-OVERFLOW.

       7000-VERIFY-OUTPUT.
           MOVE 'Y' TO WS-VERIFY-DETAIL-OK
           MOVE 0 TO WS-VERIFY-HEADER-COUNT
           MOVE 0 TO WS-VERIFY-TRAILER-COUNT
           MOVE 0 TO WS-READ-RECORD-COUNT
           MOVE 0 TO WS-READ-DETAIL-COUNT
           MOVE 0 TO WS-READ-TRAN-COUNT
           MOVE 0 TO WS-READ-DEBITS
           MOVE 0 TO WS-READ-CREDITS
           MOVE 0 TO WS-READ-NET
           MOVE 0 TO WS-READ-HASH
           MOVE 0 TO WS-READ-TRAILER-DETAIL
           MOVE 0 TO WS-READ-TRAILER-TRAN
           MOVE 0 TO WS-READ-TRAILER-DEBITS
           MOVE 0 TO WS-READ-TRAILER-CREDITS
           MOVE 0 TO WS-READ-TRAILER-NET
           MOVE 0 TO WS-READ-TRAILER-HASH
           MOVE 0 TO WS-READ-TRAILER-RECORDS
           MOVE SPACES TO WS-VERIFY-LAST-KEY
           MOVE 1 TO WS-VERIFY-EXPECTED-SEQ
           MOVE 'N' TO WS-TRAILER-SEEN
           MOVE 'N' TO WS-EOF
           OPEN INPUT GLSUMOUT-FILE
           IF GLSUMOUT-STATUS = '00'
               MOVE 'Y' TO WS-OUT-INPUT-OPEN
           ELSE
               MOVE 'GLSUMOUT' TO WS-IO-DD
               MOVE GLSUMOUT-STATUS TO WS-IO-STATUS
               PERFORM 9000-FILE-ERROR
           END-IF
           IF WS-OUT-INPUT-OPEN = 'Y'
               PERFORM UNTIL WS-EOF = 'Y'
                   READ GLSUMOUT-FILE
                       AT END CONTINUE
                   END-READ
                   EVALUATE GLSUMOUT-STATUS
                       WHEN '00'
                           ADD 1 TO WS-READ-RECORD-COUNT
                           MOVE GLSUMOUT-RECORD TO WS-READ-RECORD
                           PERFORM 7010-VERIFY-RECORD
                       WHEN '10'
                           MOVE 'Y' TO WS-EOF
                       WHEN OTHER
                           MOVE 'GLSUMOUT' TO WS-IO-DD
                           MOVE GLSUMOUT-STATUS TO WS-IO-STATUS
                           PERFORM 9000-FILE-ERROR
                           MOVE 'Y' TO WS-EOF
                   END-EVALUATE
               END-PERFORM
               IF WS-OUT-INPUT-OPEN = 'Y'
                   CLOSE GLSUMOUT-FILE
                   IF GLSUMOUT-STATUS NOT = '00'
                       MOVE 'GLSUMOUT' TO WS-IO-DD
                       MOVE GLSUMOUT-STATUS TO WS-IO-STATUS
                       PERFORM 9000-FILE-ERROR
                   ELSE
                       MOVE 'N' TO WS-OUT-INPUT-OPEN
                   END-IF
               END-IF
               PERFORM 7050-CHECK-RE-READ-TOTALS
           END-IF.

       7010-VERIFY-RECORD.
           IF WS-TRAILER-SEEN = 'Y'
               PERFORM 7040-VERIFY-FAIL
           ELSE
               EVALUATE WS-READ-RECORD (1:1)
                   WHEN 'H'
                       IF WS-READ-RECORD-COUNT = 1
                           AND WS-VERIFY-HEADER-COUNT = 0
                           ADD 1 TO WS-VERIFY-HEADER-COUNT
                       ELSE
                           PERFORM 7040-VERIFY-FAIL
                       END-IF
                   WHEN 'D'
                       IF WS-VERIFY-HEADER-COUNT = 1
                           AND WS-VERIFY-TRAILER-COUNT = 0
                           PERFORM 7020-VERIFY-DETAIL
                       ELSE
                           PERFORM 7040-VERIFY-FAIL
                       END-IF
                   WHEN 'T'
                       IF WS-VERIFY-HEADER-COUNT = 1
                           AND WS-VERIFY-TRAILER-COUNT = 0
                           PERFORM 7030-VERIFY-TRAILER
                           MOVE 'Y' TO WS-TRAILER-SEEN
                       ELSE
                           PERFORM 7040-VERIFY-FAIL
                       END-IF
                   WHEN OTHER
                       PERFORM 7040-VERIFY-FAIL
               END-EVALUATE
           END-IF.

       7020-VERIFY-DETAIL.
           IF VRD-SEQ IS NOT NUMERIC
               OR VRD-ACCOUNT IS NOT NUMERIC
               OR VRD-CATEGORY IS NOT NUMERIC
               OR VRD-TRANS-COUNT IS NOT NUMERIC
               OR VRD-DEBIT IS NOT NUMERIC
               OR VRD-CREDIT IS NOT NUMERIC
               OR VRD-NET IS NOT NUMERIC
               OR VRD-CATBAL IS NOT NUMERIC
               PERFORM 7040-VERIFY-FAIL
           ELSE
               IF VRD-SEQ NOT = WS-VERIFY-EXPECTED-SEQ
                   OR VRD-DEBIT < 0
                   OR VRD-CREDIT < 0
                   PERFORM 7040-VERIFY-FAIL
               END-IF
               IF WS-READ-DETAIL-COUNT > 0
                   IF WS-READ-RECORD (11:17)
                       <= WS-VERIFY-LAST-KEY
                       PERFORM 7040-VERIFY-FAIL
                   END-IF
               END-IF
               MOVE WS-READ-RECORD (11:17)
                 TO WS-VERIFY-LAST-KEY
               COMPUTE WS-VERIFY-NET = VRD-DEBIT - VRD-CREDIT
                   ON SIZE ERROR PERFORM 7040-VERIFY-FAIL
               END-COMPUTE
               IF WS-VERIFY-NET NOT = VRD-NET
                   PERFORM 7040-VERIFY-FAIL
               END-IF
               ADD 1 TO WS-READ-DETAIL-COUNT
                   ON SIZE ERROR PERFORM 7040-VERIFY-FAIL
               END-ADD
               ADD VRD-TRANS-COUNT TO WS-READ-TRAN-COUNT
                   ON SIZE ERROR PERFORM 7040-VERIFY-FAIL
               END-ADD
               ADD VRD-DEBIT TO WS-READ-DEBITS
                   ON SIZE ERROR PERFORM 7040-VERIFY-FAIL
               END-ADD
               ADD VRD-CREDIT TO WS-READ-CREDITS
                   ON SIZE ERROR PERFORM 7040-VERIFY-FAIL
               END-ADD
               ADD VRD-ACCOUNT TO WS-READ-HASH
                   ON SIZE ERROR PERFORM 7040-VERIFY-FAIL
               END-ADD
               ADD 1 TO WS-VERIFY-EXPECTED-SEQ
                   ON SIZE ERROR PERFORM 7040-VERIFY-FAIL
               END-ADD
           END-IF.

       7030-VERIFY-TRAILER.
           IF VRT-DETAIL IS NOT NUMERIC
               OR VRT-TRAN IS NOT NUMERIC
               OR VRT-DEBITS IS NOT NUMERIC
               OR VRT-CREDITS IS NOT NUMERIC
               OR VRT-NET IS NOT NUMERIC
               OR VRT-HASH IS NOT NUMERIC
               OR VRT-RECORDS IS NOT NUMERIC
               PERFORM 7040-VERIFY-FAIL
           ELSE
               ADD 1 TO WS-VERIFY-TRAILER-COUNT
               MOVE VRT-DETAIL TO WS-READ-TRAILER-DETAIL
               MOVE VRT-TRAN TO WS-READ-TRAILER-TRAN
               MOVE VRT-DEBITS TO WS-READ-TRAILER-DEBITS
               MOVE VRT-CREDITS TO WS-READ-TRAILER-CREDITS
               MOVE VRT-NET TO WS-READ-TRAILER-NET
               MOVE VRT-HASH TO WS-READ-TRAILER-HASH
               MOVE VRT-RECORDS TO WS-READ-TRAILER-RECORDS
           END-IF.

       7040-VERIFY-FAIL.
           MOVE 'N' TO WS-VERIFY-DETAIL-OK.

       7050-CHECK-RE-READ-TOTALS.
           COMPUTE WS-READ-NET = WS-READ-DEBITS - WS-READ-CREDITS
           IF WS-VERIFY-HEADER-COUNT NOT = 1
               OR WS-VERIFY-TRAILER-COUNT NOT = 1
               OR WS-READ-RECORD-COUNT NOT =
                   WS-READ-DETAIL-COUNT + 2
               OR WS-READ-TRAILER-DETAIL NOT =
                   WS-READ-DETAIL-COUNT
               OR WS-READ-TRAILER-TRAN NOT =
                   WS-READ-TRAN-COUNT
               OR WS-READ-TRAILER-DEBITS NOT = WS-READ-DEBITS
               OR WS-READ-TRAILER-CREDITS NOT = WS-READ-CREDITS
               OR WS-READ-TRAILER-NET NOT = WS-READ-NET
               OR WS-READ-TRAILER-HASH NOT = WS-READ-HASH
               OR WS-READ-TRAILER-RECORDS NOT =
                   WS-READ-RECORD-COUNT
               OR WS-READ-DETAIL-COUNT NOT =
                   WS-BUILD-DETAIL-COUNT
               OR WS-READ-TRAN-COUNT NOT =
                   WS-TRAN-ACCEPT-COUNT
               OR WS-READ-DEBITS NOT = WS-SOURCE-DEBITS
               OR WS-READ-CREDITS NOT = WS-SOURCE-CREDITS
               OR WS-READ-NET NOT = WS-SOURCE-NET
               OR WS-READ-HASH NOT = WS-BUILD-HASH
               OR WS-VERIFY-DETAIL-OK NOT = 'Y'
               MOVE 'N' TO WS-VERIFY-DETAIL-OK
           END-IF
           IF WS-VERIFY-DETAIL-OK NOT = 'Y'
               MOVE 'B06' TO WS-EX-CODE
               MOVE 'GLSUMOUT' TO WS-EX-KEY
               MOVE 'RE-READ TOTALS OR STRUCTURE DO NOT AGREE'
                 TO WS-EX-DESC
               MOVE 8 TO WS-EX-RC
               PERFORM 8800-ADD-EXCEPTION
           END-IF.

       7100-CHECK-FEEDER.
           COMPUTE WS-SOURCE-NET =
               WS-SOURCE-DEBITS - WS-SOURCE-CREDITS
           IF WS-HAS-EXPECTED = 'Y'
               IF WS-EXPECTED-COUNT NOT = WS-TRAN-ACCEPT-COUNT
                   MOVE 'B04' TO WS-EX-CODE
                   MOVE 'GLPARM' TO WS-EX-KEY
                   MOVE 'EXPECTED TRANSACTION COUNT DIFFERS'
                     TO WS-EX-DESC
                   MOVE 8 TO WS-EX-RC
                   PERFORM 8800-ADD-EXCEPTION
               END-IF
               IF WS-EXPECTED-NET NOT = WS-SOURCE-NET
                   MOVE 'B05' TO WS-EX-CODE
                   MOVE 'GLPARM' TO WS-EX-KEY
                   MOVE 'EXPECTED NET AMOUNT DIFFERS'
                     TO WS-EX-DESC
                   MOVE 8 TO WS-EX-RC
                   PERFORM 8800-ADD-EXCEPTION
               END-IF
           END-IF.

       8000-WRITE-REPORT.
           IF WS-REPORT-OPEN = 'Y'
               PERFORM 8010-WRITE-REPORT-TITLE
               PERFORM 8020-WRITE-INPUT-COUNTS
               PERFORM 8030-WRITE-EXCEPTIONS
               PERFORM 8040-WRITE-CONTROL-TOTALS
               PERFORM 8050-WRITE-TIE-OUT
               IF WS-HAS-EXPECTED = 'Y'
                   PERFORM 8060-WRITE-FEEDER
               END-IF
               PERFORM 8070-WRITE-RESULT
           END-IF.

       8010-WRITE-REPORT-TITLE.
           MOVE SPACES TO WS-REPORT-LINE
           MOVE 'CBGLX01C  LEDGER-SUMMARY INTERFACE GLSUMM01 -'
             TO WS-REPORT-LINE
           MOVE ' RECONCILIATION REPORT'
             TO WS-REPORT-LINE (46:22)
           PERFORM 8080-WRITE-REPORT-LINE
           MOVE SPACES TO WS-REPORT-LINE
           STRING 'RUN DATE ' DELIMITED BY SIZE
                  WS-RUN-DATE DELIMITED BY SIZE
                  '    SOURCE ' DELIMITED BY SIZE
                  WS-SOURCE-ID DELIMITED BY SIZE
               INTO WS-REPORT-LINE
           PERFORM 8080-WRITE-REPORT-LINE
           MOVE ALL '=' TO WS-REPORT-LINE
           PERFORM 8080-WRITE-REPORT-LINE.

       8020-WRITE-INPUT-COUNTS.
           MOVE SPACES TO WS-REPORT-LINE
           MOVE 'INPUT COUNTS' TO WS-REPORT-LINE
           PERFORM 8080-WRITE-REPORT-LINE
           MOVE ALL '-' TO WS-REPORT-LINE
           PERFORM 8080-WRITE-REPORT-LINE
           MOVE 'TRANSACT RECORDS READ' TO WS-REPORT-LABEL
           MOVE WS-TRAN-READ-COUNT TO WS-REPORT-COUNT
           PERFORM 8021-WRITE-COUNT-LINE
           MOVE 'ACCEPTED' TO WS-REPORT-LABEL
           MOVE WS-TRAN-ACCEPT-COUNT TO WS-REPORT-COUNT
           PERFORM 8021-WRITE-COUNT-LINE
           MOVE 'REJECTED' TO WS-REPORT-LABEL
           MOVE WS-TRAN-REJECT-COUNT TO WS-REPORT-COUNT
           PERFORM 8021-WRITE-COUNT-LINE
           MOVE 'ACCEPTED WITH WARNINGS' TO WS-REPORT-LABEL
           MOVE WS-TRAN-WARN-COUNT TO WS-REPORT-COUNT
           PERFORM 8021-WRITE-COUNT-LINE
           MOVE 'XREF ENTRIES LOADED' TO WS-REPORT-LABEL
           MOVE WS-XREF-COUNT TO WS-REPORT-COUNT
           PERFORM 8021-WRITE-COUNT-LINE
           MOVE 'TCATBALF RECORDS READ' TO WS-REPORT-LABEL
           MOVE WS-TCAT-READ-COUNT TO WS-REPORT-COUNT
           PERFORM 8021-WRITE-COUNT-LINE
           MOVE ALL '-' TO WS-REPORT-LINE
           PERFORM 8080-WRITE-REPORT-LINE.

       8021-WRITE-COUNT-LINE.
           MOVE WS-REPORT-COUNT TO RP-CONTROL-COUNT
           MOVE SPACES TO WS-REPORT-LINE
           MOVE WS-REPORT-LABEL TO WS-REPORT-LINE (1:40)
           MOVE RP-CONTROL-COUNT TO WS-REPORT-LINE (42:23)
           PERFORM 8080-WRITE-REPORT-LINE.

       8030-WRITE-EXCEPTIONS.
           MOVE SPACES TO WS-REPORT-LINE
           MOVE 'EXCEPTIONS' TO WS-REPORT-LINE
           PERFORM 8080-WRITE-REPORT-LINE
           MOVE ALL '-' TO WS-REPORT-LINE
           PERFORM 8080-WRITE-REPORT-LINE
           MOVE SPACES TO WS-REPORT-LINE
           MOVE 'CODE' TO WS-REPORT-LINE (1:4)
           MOVE 'KEY' TO WS-REPORT-LINE (7:3)
           MOVE 'DESCRIPTION' TO WS-REPORT-LINE (39:11)
           PERFORM 8080-WRITE-REPORT-LINE
           IF WS-EXCEPTION-COUNT = 0
               MOVE SPACES TO WS-REPORT-LINE
               MOVE 'NONE' TO WS-REPORT-LINE
               PERFORM 8080-WRITE-REPORT-LINE
           ELSE
               PERFORM VARYING WS-VERIFY-EXPECTED-SEQ FROM 1 BY 1
                   UNTIL WS-VERIFY-EXPECTED-SEQ >
                       WS-EXCEPTION-COUNT
                   MOVE WS-EXCEPTION-LINE
                       (WS-VERIFY-EXPECTED-SEQ)
                     TO WS-REPORT-LINE
                   PERFORM 8080-WRITE-REPORT-LINE
               END-PERFORM
           END-IF
           IF WS-MORE-EXCEPTIONS > 0
               MOVE WS-MORE-EXCEPTIONS TO RP-MORE-EXCEPTIONS
               MOVE SPACES TO WS-REPORT-LINE
               STRING RP-MORE-EXCEPTIONS DELIMITED BY SIZE
                      ' MORE EXCEPTIONS NOT LISTED'
                      DELIMITED BY SIZE
                   INTO WS-REPORT-LINE
               PERFORM 8080-WRITE-REPORT-LINE
           END-IF
           MOVE ALL '-' TO WS-REPORT-LINE
           PERFORM 8080-WRITE-REPORT-LINE.

       8040-WRITE-CONTROL-TOTALS.
           MOVE SPACES TO WS-REPORT-LINE
           MOVE 'CONTROL TOTALS' TO WS-REPORT-LINE
           PERFORM 8080-WRITE-REPORT-LINE
           MOVE ALL '-' TO WS-REPORT-LINE
           PERFORM 8080-WRITE-REPORT-LINE
           MOVE SPACES TO WS-REPORT-LINE
           MOVE 'ITEM' TO WS-REPORT-LINE (1:4)
           MOVE 'SOURCE' TO WS-REPORT-LINE (27:6)
           MOVE 'FILE DETAIL' TO WS-REPORT-LINE (53:11)
           MOVE 'FILE TRAILER' TO WS-REPORT-LINE (79:12)
           MOVE 'STATUS' TO WS-REPORT-LINE (109:6)
           PERFORM 8080-WRITE-REPORT-LINE
           IF WS-TRAN-ACCEPT-COUNT = WS-READ-TRAN-COUNT
               AND WS-TRAN-ACCEPT-COUNT = WS-READ-TRAILER-TRAN
               MOVE 'OK' TO WS-TOTAL-STATUS
           ELSE
               MOVE 'DIFF' TO WS-TOTAL-STATUS
           END-IF
           MOVE SPACES TO WS-REPORT-LINE
           MOVE 'TRANSACTIONS' TO WS-REPORT-LINE (1:26)
           MOVE WS-TRAN-ACCEPT-COUNT TO RP-CONTROL-COUNT
           MOVE RP-CONTROL-COUNT TO WS-REPORT-LINE (30:23)
           MOVE WS-READ-TRAN-COUNT TO RP-CONTROL-COUNT
           MOVE RP-CONTROL-COUNT TO WS-REPORT-LINE (56:23)
           MOVE WS-READ-TRAILER-TRAN TO RP-CONTROL-COUNT
           MOVE RP-CONTROL-COUNT TO WS-REPORT-LINE (82:23)
           MOVE WS-TOTAL-STATUS TO WS-REPORT-LINE (109:4)
           PERFORM 8080-WRITE-REPORT-LINE
           PERFORM 8041-WRITE-AMOUNT-ROW
           IF WS-BUILD-DETAIL-COUNT = WS-READ-DETAIL-COUNT
               AND WS-BUILD-DETAIL-COUNT =
                   WS-READ-TRAILER-DETAIL
               MOVE 'OK' TO WS-DETAILS-STATUS
           ELSE
               MOVE 'DIFF' TO WS-DETAILS-STATUS
           END-IF
           MOVE SPACES TO WS-REPORT-LINE
           MOVE 'DETAIL RECORDS' TO WS-REPORT-LINE (1:26)
           MOVE WS-BUILD-DETAIL-COUNT TO RP-CONTROL-COUNT
           MOVE RP-CONTROL-COUNT TO WS-REPORT-LINE (30:23)
           MOVE WS-READ-DETAIL-COUNT TO RP-CONTROL-COUNT
           MOVE RP-CONTROL-COUNT TO WS-REPORT-LINE (56:23)
           MOVE WS-READ-TRAILER-DETAIL TO RP-CONTROL-COUNT
           MOVE RP-CONTROL-COUNT TO WS-REPORT-LINE (82:23)
           MOVE WS-DETAILS-STATUS TO WS-REPORT-LINE (109:4)
           PERFORM 8080-WRITE-REPORT-LINE
           IF WS-BUILD-HASH = WS-READ-HASH
               AND WS-BUILD-HASH = WS-READ-TRAILER-HASH
               MOVE 'OK' TO WS-TOTAL-STATUS
           ELSE
               MOVE 'DIFF' TO WS-TOTAL-STATUS
           END-IF
           MOVE SPACES TO WS-REPORT-LINE
           MOVE 'HASH TOTAL' TO WS-REPORT-LINE (1:26)
           MOVE WS-BUILD-HASH TO RP-CONTROL-COUNT
           MOVE RP-CONTROL-COUNT TO WS-REPORT-LINE (30:23)
           MOVE WS-READ-HASH TO RP-CONTROL-COUNT
           MOVE RP-CONTROL-COUNT TO WS-REPORT-LINE (56:23)
           MOVE WS-READ-TRAILER-HASH TO RP-CONTROL-COUNT
           MOVE RP-CONTROL-COUNT TO WS-REPORT-LINE (82:23)
           MOVE WS-TOTAL-STATUS TO WS-REPORT-LINE (109:4)
           PERFORM 8080-WRITE-REPORT-LINE
           MOVE ALL '-' TO WS-REPORT-LINE
           PERFORM 8080-WRITE-REPORT-LINE.

       8041-WRITE-AMOUNT-ROW.
           IF WS-SOURCE-DEBITS = WS-READ-DEBITS
               AND WS-SOURCE-DEBITS = WS-READ-TRAILER-DEBITS
               MOVE 'OK' TO WS-TOTAL-STATUS
           ELSE
               MOVE 'DIFF' TO WS-TOTAL-STATUS
           END-IF
           MOVE SPACES TO WS-REPORT-LINE
           MOVE 'DEBITS' TO WS-REPORT-LINE (1:26)
           MOVE WS-SOURCE-DEBITS TO RP-CONTROL-AMOUNT
           MOVE RP-CONTROL-AMOUNT TO WS-REPORT-LINE (30:23)
           MOVE WS-READ-DEBITS TO RP-CONTROL-AMOUNT
           MOVE RP-CONTROL-AMOUNT TO WS-REPORT-LINE (56:23)
           MOVE WS-READ-TRAILER-DEBITS TO RP-CONTROL-AMOUNT
           MOVE RP-CONTROL-AMOUNT TO WS-REPORT-LINE (82:23)
           MOVE WS-TOTAL-STATUS TO WS-REPORT-LINE (109:4)
           PERFORM 8080-WRITE-REPORT-LINE
           IF WS-SOURCE-CREDITS = WS-READ-CREDITS
               AND WS-SOURCE-CREDITS = WS-READ-TRAILER-CREDITS
               MOVE 'OK' TO WS-TOTAL-STATUS
           ELSE
               MOVE 'DIFF' TO WS-TOTAL-STATUS
           END-IF
           MOVE SPACES TO WS-REPORT-LINE
           MOVE 'CREDITS' TO WS-REPORT-LINE (1:26)
           MOVE WS-SOURCE-CREDITS TO RP-CONTROL-AMOUNT
           MOVE RP-CONTROL-AMOUNT TO WS-REPORT-LINE (30:23)
           MOVE WS-READ-CREDITS TO RP-CONTROL-AMOUNT
           MOVE RP-CONTROL-AMOUNT TO WS-REPORT-LINE (56:23)
           MOVE WS-READ-TRAILER-CREDITS TO RP-CONTROL-AMOUNT
           MOVE RP-CONTROL-AMOUNT TO WS-REPORT-LINE (82:23)
           MOVE WS-TOTAL-STATUS TO WS-REPORT-LINE (109:4)
           PERFORM 8080-WRITE-REPORT-LINE
           COMPUTE WS-SOURCE-NET =
               WS-SOURCE-DEBITS - WS-SOURCE-CREDITS
           COMPUTE WS-READ-NET =
               WS-READ-DEBITS - WS-READ-CREDITS
           IF WS-SOURCE-NET = WS-READ-NET
               AND WS-SOURCE-NET = WS-READ-TRAILER-NET
               MOVE 'OK' TO WS-TOTAL-STATUS
           ELSE
               MOVE 'DIFF' TO WS-TOTAL-STATUS
           END-IF
           MOVE SPACES TO WS-REPORT-LINE
           MOVE 'NET' TO WS-REPORT-LINE (1:26)
           MOVE WS-SOURCE-NET TO RP-CONTROL-AMOUNT
           MOVE RP-CONTROL-AMOUNT TO WS-REPORT-LINE (30:23)
           MOVE WS-READ-NET TO RP-CONTROL-AMOUNT
           MOVE RP-CONTROL-AMOUNT TO WS-REPORT-LINE (56:23)
           MOVE WS-READ-TRAILER-NET TO RP-CONTROL-AMOUNT
           MOVE RP-CONTROL-AMOUNT TO WS-REPORT-LINE (82:23)
           MOVE WS-TOTAL-STATUS TO WS-REPORT-LINE (109:4)
           PERFORM 8080-WRITE-REPORT-LINE.

       8050-WRITE-TIE-OUT.
           MOVE SPACES TO WS-REPORT-LINE
           MOVE 'LEDGER TIE-OUT' TO WS-REPORT-LINE
           PERFORM 8080-WRITE-REPORT-LINE
           MOVE ALL '-' TO WS-REPORT-LINE
           PERFORM 8080-WRITE-REPORT-LINE
           MOVE 'KEYS TIED' TO WS-REPORT-LABEL
           MOVE WS-KEYS-TIED TO WS-REPORT-COUNT
           PERFORM 8021-WRITE-COUNT-LINE
           MOVE 'KEYS DIFFERING' TO WS-REPORT-LABEL
           MOVE WS-KEYS-DIFFERING TO WS-REPORT-COUNT
           PERFORM 8021-WRITE-COUNT-LINE
           MOVE 'KEYS MISSING FROM TCATBALF' TO WS-REPORT-LABEL
           MOVE WS-KEYS-MISSING TO WS-REPORT-COUNT
           PERFORM 8021-WRITE-COUNT-LINE
           MOVE 'BALANCE-ONLY KEYS (NON-ZERO)'
             TO WS-REPORT-LABEL
           MOVE WS-BALANCE-ONLY TO WS-REPORT-COUNT
           PERFORM 8021-WRITE-COUNT-LINE
           MOVE ALL '-' TO WS-REPORT-LINE
           PERFORM 8080-WRITE-REPORT-LINE.

       8060-WRITE-FEEDER.
           MOVE SPACES TO WS-REPORT-LINE
           MOVE 'FEEDER CONTROL TOTALS' TO WS-REPORT-LINE
           PERFORM 8080-WRITE-REPORT-LINE
           MOVE ALL '-' TO WS-REPORT-LINE
           PERFORM 8080-WRITE-REPORT-LINE
           MOVE SPACES TO WS-REPORT-LINE
           MOVE 'ITEM' TO WS-REPORT-LINE (1:4)
           MOVE 'EXPECTED' TO WS-REPORT-LINE (27:8)
           MOVE 'ACTUAL' TO WS-REPORT-LINE (53:6)
           MOVE 'STATUS' TO WS-REPORT-LINE (109:6)
           PERFORM 8080-WRITE-REPORT-LINE
           IF WS-EXPECTED-COUNT = WS-TRAN-ACCEPT-COUNT
               MOVE 'OK' TO WS-FEEDER-COUNT-STATUS
           ELSE
               MOVE 'DIFF' TO WS-FEEDER-COUNT-STATUS
           END-IF
           MOVE SPACES TO WS-REPORT-LINE
           MOVE 'TRANSACTIONS' TO WS-REPORT-LINE (1:26)
           MOVE WS-EXPECTED-COUNT TO RP-CONTROL-COUNT
           MOVE RP-CONTROL-COUNT TO WS-REPORT-LINE (30:23)
           MOVE WS-TRAN-ACCEPT-COUNT TO RP-CONTROL-COUNT
           MOVE RP-CONTROL-COUNT TO WS-REPORT-LINE (56:23)
           MOVE WS-FEEDER-COUNT-STATUS TO WS-REPORT-LINE (109:4)
           PERFORM 8080-WRITE-REPORT-LINE
           IF WS-EXPECTED-NET = WS-SOURCE-NET
               MOVE 'OK' TO WS-FEEDER-NET-STATUS
           ELSE
               MOVE 'DIFF' TO WS-FEEDER-NET-STATUS
           END-IF
           MOVE SPACES TO WS-REPORT-LINE
           MOVE 'NET' TO WS-REPORT-LINE (1:26)
           MOVE WS-EXPECTED-NET TO RP-CONTROL-AMOUNT
           MOVE RP-CONTROL-AMOUNT TO WS-REPORT-LINE (30:23)
           MOVE WS-SOURCE-NET TO RP-CONTROL-AMOUNT
           MOVE RP-CONTROL-AMOUNT TO WS-REPORT-LINE (56:23)
           MOVE WS-FEEDER-NET-STATUS TO WS-REPORT-LINE (109:4)
           PERFORM 8080-WRITE-REPORT-LINE
           MOVE ALL '-' TO WS-REPORT-LINE
           PERFORM 8080-WRITE-REPORT-LINE.

       8070-WRITE-RESULT.
           IF WS-RETURN-CODE >= 12
               MOVE SPACES TO WS-REPORT-RESULT
               STRING 'RUN ABORTED - ' DELIMITED BY SIZE
                      WS-ABORT-REASON DELIMITED BY SIZE
                   INTO WS-REPORT-RESULT
           ELSE
               EVALUATE WS-RETURN-CODE
                   WHEN 8
                       MOVE 'OUT OF BALANCE' TO WS-REPORT-RESULT
                   WHEN 4
                       MOVE 'BALANCED WITH WARNINGS'
                         TO WS-REPORT-RESULT
                   WHEN OTHER
                       MOVE 'BALANCED' TO WS-REPORT-RESULT
               END-EVALUATE
           END-IF
           MOVE WS-RETURN-CODE TO RP-RC
           MOVE SPACES TO WS-REPORT-LINE
           STRING 'RESULT: ' DELIMITED BY SIZE
                  WS-REPORT-RESULT DELIMITED BY SIZE
                  '  RETURN CODE ' DELIMITED BY SIZE
                  RP-RC DELIMITED BY SIZE
               INTO WS-REPORT-LINE
           PERFORM 8080-WRITE-REPORT-LINE.

       8080-WRITE-REPORT-LINE.
           IF WS-REPORT-OPEN = 'Y'
               MOVE WS-REPORT-LINE TO GLRECRPT-RECORD
               WRITE GLRECRPT-RECORD
               IF GLRECRPT-STATUS NOT = '00'
                   MOVE 'GLRECRPT' TO WS-IO-DD
                   MOVE GLRECRPT-STATUS TO WS-IO-STATUS
                   PERFORM 9000-FILE-ERROR
               END-IF
           END-IF.

       8100-CLOSE-REPORT.
           IF WS-REPORT-OPEN = 'Y'
               CLOSE GLRECRPT-FILE
               IF GLRECRPT-STATUS NOT = '00'
                   MOVE 'GLRECRPT' TO WS-IO-DD
                   MOVE GLRECRPT-STATUS TO WS-IO-STATUS
                   PERFORM 9000-FILE-ERROR
               ELSE
                   MOVE 'N' TO WS-REPORT-OPEN
               END-IF
           END-IF.

       8200-DISPLAY-RESULT.
           IF WS-RETURN-CODE >= 12
               MOVE SPACES TO WS-REPORT-RESULT
               STRING 'RUN ABORTED - ' DELIMITED BY SIZE
                      WS-ABORT-REASON DELIMITED BY SIZE
                   INTO WS-REPORT-RESULT
           ELSE
               EVALUATE WS-RETURN-CODE
                   WHEN 8
                       MOVE 'OUT OF BALANCE' TO WS-REPORT-RESULT
                   WHEN 4
                       MOVE 'BALANCED WITH WARNINGS'
                         TO WS-REPORT-RESULT
                   WHEN OTHER
                       MOVE 'BALANCED' TO WS-REPORT-RESULT
               END-EVALUATE
           END-IF
           MOVE WS-RETURN-CODE TO RP-RC
           DISPLAY 'RESULT: ' WS-REPORT-RESULT
                   ' RETURN CODE ' RP-RC.
