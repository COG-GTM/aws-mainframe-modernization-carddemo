      ******************************************************************
      * Program     : CBTRN04C.CBL
      * Application : Card transaction batch cycle
      * Type        : BATCH COBOL Program
      * Function    : Pre-posting validation of the daily transaction
      *               file. Reads DALYTRAN, applies the validation
      *               rules in a fixed order, writes accepted records
      *               unchanged to DALYVALD, writes rejected records
      *               with a reason trailer to DALYRJ04 and prints a
      *               control-total report to VALDRPT. Runs ahead of
      *               CBTRN02C so a bad feed is stopped before the
      *               master files are touched.
      * Parm        : Run date YYYYMMDD (EXEC PGM=...,PARM='YYYYMMDD'),
      *               retrieved with the Language Environment service
      *               CEE3PRM so the program stays a plain main program.
      * Return code : 0 nothing rejected, 4 records rejected,
      *               8 parm invalid, 12 file error.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBTRN04C.
       AUTHOR.        BATCH SUSTAINMENT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DALYTRAN-FILE ASSIGN TO DALYTRAN
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  FILE STATUS  IS DALYTRAN-STATUS.

           SELECT TRANTYPE-FILE ASSIGN TO TRANTYPE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS FD-TRAN-TYPE
                  FILE STATUS  IS TRANTYPE-STATUS.

           SELECT TRANCATG-FILE ASSIGN TO TRANCATG
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS FD-TRAN-CAT-KEY
                  FILE STATUS  IS TRANCATG-STATUS.

           SELECT XREF-FILE ASSIGN TO   XREFFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS FD-XREF-CARD-NUM
                  FILE STATUS  IS XREFFILE-STATUS.

           SELECT TCATBAL-FILE ASSIGN TO TCATBALF
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS FD-TCATBAL-KEY
                  FILE STATUS  IS TCATBALF-STATUS.

           SELECT DALYVALD-FILE ASSIGN TO DALYVALD
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  FILE STATUS  IS DALYVALD-STATUS.

           SELECT DALYRJ04-FILE ASSIGN TO DALYRJ04
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  FILE STATUS  IS DALYRJ04-STATUS.

           SELECT REPORT-FILE ASSIGN TO VALDRPT
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  FILE STATUS  IS VALDRPT-STATUS.

      *
       DATA DIVISION.
       FILE SECTION.
       FD  DALYTRAN-FILE.
       01  FD-TRAN-RECORD.
           05 FD-TRAN-ID                        PIC X(16).
           05 FD-CUST-DATA                      PIC X(334).

       FD  TRANTYPE-FILE.
       01  FD-TRANTYPE-REC.
           05 FD-TRAN-TYPE                      PIC X(02).
           05 FD-TRAN-TYPE-DATA                 PIC X(58).

       FD  TRANCATG-FILE.
       01  FD-TRANCATG-REC.
           05 FD-TRAN-CAT-KEY.
              10 FD-TRAN-TYPE-CD                PIC X(02).
              10 FD-TRAN-CAT-CD                 PIC 9(04).
           05 FD-TRAN-CAT-DATA                  PIC X(54).

       FD  XREF-FILE.
       01  FD-XREFFILE-REC.
           05 FD-XREF-CARD-NUM                  PIC X(16).
           05 FD-XREF-DATA                      PIC X(34).

       FD  TCATBAL-FILE.
       01  FD-TRAN-CAT-BAL-RECORD.
           05 FD-TCATBAL-KEY.
              10 FD-TCATBAL-ACCT-ID             PIC 9(11).
              10 FD-TCATBAL-TYPE-CD             PIC X(02).
              10 FD-TCATBAL-CD                  PIC 9(04).
           05 FD-TCATBAL-DATA                   PIC X(33).

       FD  DALYVALD-FILE.
       01  FD-VALD-RECORD                       PIC X(350).

       FD  DALYRJ04-FILE.
       01  FD-REJS-RECORD.
           05 FD-REJECT-RECORD                  PIC X(350).
           05 FD-VALIDATION-TRAILER             PIC X(80).

       FD  REPORT-FILE.
       01  FD-REPTFILE-REC                      PIC X(133).

       WORKING-STORAGE SECTION.

      *****************************************************************
       COPY CVTRA06Y.
       01  DALYTRAN-STATUS.
           05 DALYTRAN-STAT1                    PIC X.
           05 DALYTRAN-STAT2                    PIC X.

       COPY CVTRA03Y.
       01  TRANTYPE-STATUS.
           05 TRANTYPE-STAT1                    PIC X.
           05 TRANTYPE-STAT2                    PIC X.

       COPY CVTRA04Y.
       01  TRANCATG-STATUS.
           05 TRANCATG-STAT1                    PIC X.
           05 TRANCATG-STAT2                    PIC X.

       COPY CVACT03Y.
       01  XREFFILE-STATUS.
           05 XREFFILE-STAT1                    PIC X.
           05 XREFFILE-STAT2                    PIC X.

       COPY CVTRA01Y.
       01  TCATBALF-STATUS.
           05 TCATBALF-STAT1                    PIC X.
           05 TCATBALF-STAT2                    PIC X.

       01  DALYVALD-STATUS.
           05 DALYVALD-STAT1                    PIC X.
           05 DALYVALD-STAT2                    PIC X.

       01  DALYRJ04-STATUS.
           05 DALYRJ04-STAT1                    PIC X.
           05 DALYRJ04-STAT2                    PIC X.

       01  VALDRPT-STATUS.
           05 VALDRPT-STAT1                     PIC X.
           05 VALDRPT-STAT2                     PIC X.

       01  IO-STATUS.
           05 IO-STAT1                          PIC X.
           05 IO-STAT2                          PIC X.
       01  TWO-BYTES-BINARY                     PIC 9(4) BINARY.
       01  TWO-BYTES-ALPHA REDEFINES TWO-BYTES-BINARY.
           05 TWO-BYTES-LEFT                    PIC X.
           05 TWO-BYTES-RIGHT                   PIC X.
       01  IO-STATUS-04.
           05 IO-STATUS-0401                    PIC 9    VALUE 0.
           05 IO-STATUS-0403                    PIC 999  VALUE 0.

       01  APPL-RESULT                          PIC S9(9) COMP.
           88 APPL-AOK                          VALUE 0.
           88 APPL-EOF                          VALUE 16.

       01  END-OF-FILE                          PIC X(01) VALUE 'N'.

      *----------------------------------------------------------------
      * Run date received through the PARM (YYYYMMDD).
      *----------------------------------------------------------------
       01  WS-RUN-DATE-PARM.
           05 WS-RUN-YYYY                       PIC X(04).
           05 WS-RUN-MM                         PIC X(02).
           05 WS-RUN-DD                         PIC X(02).
       01  WS-RUN-DATE-ISO                      PIC X(10).
       01  WS-RUN-DATE-INT                      PIC 9(08).
       01  WS-RUN-DATE-JULIAN                   PIC 9(07).

      *----------------------------------------------------------------
      * Reject trailer written behind the 350-byte record (80 bytes).
      *----------------------------------------------------------------
       01  REJECT-RECORD.
           05 REJECT-TRAN-DATA                  PIC X(350).
           05 VALIDATION-TRAILER                PIC X(80).

       01  WS-VALIDATION-TRAILER.
           05 WS-VALIDATION-FAIL-REASON         PIC 9(04).
           05 WS-VALIDATION-FAIL-REASON-DESC    PIC X(76).

      *----------------------------------------------------------------
      * Reason codes in evaluation order. 0100 is the code CBTRN02C
      * assigns to an unknown card number; it is reused unchanged.
      * New codes use 0201-0209 so they stay clear of the 0100-0109
      * range CBTRN02C uses for posting-time rejects.
      *----------------------------------------------------------------
       01  WS-REASON-TABLE-DATA.
           05 FILLER PIC X(54) VALUE
              '0201TRANSACTION ID MISSING (SPACES OR LOW-VALUES)     '.
           05 FILLER PIC X(54) VALUE
              '0202TRANSACTION TYPE CODE NOT IN TRANTYPE FILE        '.
           05 FILLER PIC X(54) VALUE
              '0203TRANSACTION TYPE/CATEGORY NOT IN TRANCATG FILE    '.
           05 FILLER PIC X(54) VALUE
              '0204TRANSACTION AMOUNT NOT NUMERIC OR INVALID SIGN    '.
           05 FILLER PIC X(54) VALUE
              '0100INVALID CARD NUMBER FOUND                         '.
           05 FILLER PIC X(54) VALUE
              '0205AMOUNT WOULD OVERFLOW CATEGORY BALANCE S9(09)V99  '.
           05 FILLER PIC X(54) VALUE
              '0206ORIGINATION TIMESTAMP DATE INVALID                '.
           05 FILLER PIC X(54) VALUE
              '0207PROCESSING TIMESTAMP DATE INVALID                 '.
           05 FILLER PIC X(54) VALUE
              '0208ORIGINATION DATE AFTER PROCESSING DATE            '.
           05 FILLER PIC X(54) VALUE
              '0209TRANSACTION DATE AFTER RUN DATE                   '.
       01  WS-REASON-TABLE REDEFINES WS-REASON-TABLE-DATA.
           05 WS-REASON-ENTRY OCCURS 10 TIMES.
              10 WS-RSN-CODE                    PIC 9(04).
              10 WS-RSN-DESC                    PIC X(50).
       01  WS-REASON-MAX                        PIC 9(02) VALUE 10.
       01  WS-RSN-IX                            PIC 9(02).
       01  WS-RSN-FOUND                         PIC X(01).

      *----------------------------------------------------------------
      * Control totals. All money is carried packed decimal (COMP-3).
      * Amount ceiling is the narrowest downstream PIC, S9(09)V99
      * (TRAN-AMT in CVTRA05Y and TRAN-CAT-BAL in CVTRA01Y).
      *----------------------------------------------------------------
       01  WS-COUNTERS.
           05 WS-READ-COUNT                     PIC 9(09) COMP-3
                                                VALUE 0.
           05 WS-ACCEPT-COUNT                   PIC 9(09) COMP-3
                                                VALUE 0.
           05 WS-REJECT-COUNT                   PIC 9(09) COMP-3
                                                VALUE 0.
           05 WS-NONNUM-AMT-COUNT               PIC 9(09) COMP-3
                                                VALUE 0.
           05 WS-CHECK-COUNT                    PIC 9(09) COMP-3
                                                VALUE 0.
       01  WS-REASON-COUNTS.
           05 WS-RSN-COUNT OCCURS 10 TIMES      PIC 9(09) COMP-3.

       01  WS-AMOUNTS.
           05 WS-TRAN-AMT-P                     PIC S9(09)V99 COMP-3
                                                VALUE 0.
           05 WS-AMT-CEILING                    PIC S9(09)V99 COMP-3
                                                VALUE 999999999.99.
           05 WS-AMT-FLOOR                      PIC S9(09)V99 COMP-3
                                                VALUE -999999999.99.
           05 WS-PROJ-CAT-BAL                   PIC S9(11)V99 COMP-3
                                                VALUE 0.
           05 WS-TOTAL-AMT                      PIC S9(13)V99 COMP-3
                                                VALUE 0.
           05 WS-REJECT-AMT                     PIC S9(13)V99 COMP-3
                                                VALUE 0.
           05 WS-CHECK-AMT                      PIC S9(13)V99 COMP-3
                                                VALUE 0.
       01  WS-ACCEPT-AMT                        PIC S9(13)V99 COMP-3
                                                VALUE 0.
       01  WS-ACCEPT-AMT-BYTES REDEFINES WS-ACCEPT-AMT
                                                PIC X(08).

       01  WS-AMT-NUMERIC-FLAG                  PIC X(01).

      *----------------------------------------------------------------
      * Projected category balances for keys already accepted in this
      * run. CBTRN02C rewrites TRAN-CAT-BAL after every posting, so a
      * second record for the same account/type/category must be
      * checked against the balance the first one will leave behind,
      * not against the balance on file. Rejected records do not
      * advance the projection because CBTRN02C never posts them.
      *----------------------------------------------------------------
       01  WS-BAL-TABLE-MAX                     PIC 9(05) VALUE 20000.
       01  WS-BAL-TABLE-COUNT                   PIC 9(05) VALUE 0.
       01  WS-BAL-IX                            PIC 9(05) VALUE 0.
       01  WS-BAL-FOUND-IX                      PIC 9(05) VALUE 0.
       01  WS-BAL-SEARCH-KEY.
           05 WS-BAL-SEARCH-ACCT-ID             PIC 9(11).
           05 WS-BAL-SEARCH-TYPE-CD             PIC X(02).
           05 WS-BAL-SEARCH-CAT-CD              PIC 9(04).
       01  WS-BAL-TABLE.
           05 WS-BAL-ENTRY OCCURS 20000 TIMES.
              10 WS-BAL-KEY                     PIC X(17).
              10 WS-BAL-PROJ                    PIC S9(11)V99 COMP-3.

      *----------------------------------------------------------------
      * Date work areas. Timestamps are DB2 style, date in bytes 1-10
      * as YYYY-MM-DD (see CBTRN02C Z-GET-DB2-FORMAT-TIMESTAMP).
      *----------------------------------------------------------------
       01  WS-DATE-IN.
           05 WS-DATE-IN-YYYY                   PIC X(04).
           05 WS-DATE-IN-SEP1                   PIC X(01).
           05 WS-DATE-IN-MM                     PIC X(02).
           05 WS-DATE-IN-SEP2                   PIC X(01).
           05 WS-DATE-IN-DD                     PIC X(02).
       01  WS-DATE-IN-YYYY-N REDEFINES WS-DATE-IN.
           05 WS-DATE-YYYY-N                    PIC 9(04).
           05 FILLER                            PIC X(01).
           05 WS-DATE-MM-N                      PIC 9(02).
           05 FILLER                            PIC X(01).
           05 WS-DATE-DD-N                      PIC 9(02).
       01  WS-DATE-VALID                        PIC X(01).
       01  WS-DATE-INT.
           05 WS-DATE-INT-YYYY                  PIC 9(04).
           05 WS-DATE-INT-MM                    PIC 9(02).
           05 WS-DATE-INT-DD                    PIC 9(02).
       01  WS-DATE-INT-N REDEFINES WS-DATE-INT  PIC 9(08).
       01  WS-DATE-JULIAN.
           05 WS-DATE-JULIAN-YYYY               PIC 9(04).
           05 WS-DATE-JULIAN-DDD                PIC 9(03).
       01  WS-DATE-JULIAN-N REDEFINES WS-DATE-JULIAN PIC 9(07).
       01  WS-DAYS-IN-MONTH                     PIC 9(02).
       01  WS-LEAP-YEAR                         PIC X(01).
       01  WS-LEAP-WORK                         PIC 9(04).
       01  WS-LEAP-REM                          PIC 9(04).
       01  WS-MONTH-IX                          PIC 9(02).
       01  WS-DAYS-BEFORE-MONTH-DATA.
           05 FILLER PIC X(36) VALUE
              '000031059090120151181212243273304334'.
       01  WS-DAYS-BEFORE-MONTH
           REDEFINES WS-DAYS-BEFORE-MONTH-DATA.
           05 WS-DAYS-BEFORE OCCURS 12 TIMES    PIC 9(03).

       01  WS-ORIG-DATE-INT                     PIC 9(08).
       01  WS-ORIG-DATE-JULIAN                  PIC 9(07).
       01  WS-PROC-DATE-INT                     PIC 9(08).
       01  WS-PROC-DATE-JULIAN                  PIC 9(07).
       01  WS-PROC-TS-PRESENT                   PIC X(01).
       01  WS-ACCEPT-JULIAN-MIN                 PIC 9(07).
       01  WS-ACCEPT-JULIAN-MAX                 PIC 9(07).

      *----------------------------------------------------------------
      * Packed to hexadecimal rendering for the control report.
      *----------------------------------------------------------------
       01  WS-HEX-DIGITS                        PIC X(16)
                                    VALUE '0123456789ABCDEF'.
       01  WS-HEX-IN                            PIC X(08).
       01  WS-HEX-IN-BYTE REDEFINES WS-HEX-IN.
           05 WS-HEX-BYTE OCCURS 8 TIMES        PIC X(01).
       01  WS-HEX-OUT                           PIC X(16).
       01  WS-HEX-OUT-CHAR REDEFINES WS-HEX-OUT.
           05 WS-HEX-CHAR OCCURS 16 TIMES       PIC X(01).
       01  WS-HEX-IX                            PIC 9(02).
       01  WS-HEX-OX                            PIC 9(02).
       01  WS-HEX-VAL                           PIC 9(03).
       01  WS-HEX-HI                            PIC 9(02).
       01  WS-HEX-LO                            PIC 9(02).

      *----------------------------------------------------------------
      * Control-total report lines (133 bytes, RECFM=FB like TRANREPT).
      *----------------------------------------------------------------
       01  WS-REPORT-LINE                       PIC X(133).
       01  WS-RPT-HEADER.
           05 FILLER                            PIC X(01) VALUE SPACE.
           05 FILLER                            PIC X(56) VALUE
             'CBTRN04C  DAILY TRANSACTION PRE-POSTING VALIDATION - CON'.
           05 FILLER                            PIC X(76) VALUE
              'TROL TOTALS'.
       01  WS-RPT-RUN-DATE.
           05 FILLER                            PIC X(01) VALUE SPACE.
           05 FILLER                            PIC X(24) VALUE
              'RUN DATE (GREGORIAN)  : '.
           05 WS-RPT-RUN-DATE-ISO               PIC X(10).
           05 FILLER                            PIC X(24) VALUE
              '   RUN DATE (JULIAN)  : '.
           05 WS-RPT-RUN-DATE-JUL               PIC X(07).
           05 FILLER                            PIC X(67) VALUE SPACES.
       01  WS-RPT-COUNT-LINE.
           05 FILLER                            PIC X(01) VALUE SPACE.
           05 WS-RPT-COUNT-LABEL                PIC X(50).
           05 FILLER                            PIC X(02) VALUE ': '.
           05 WS-RPT-COUNT-VALUE                PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                            PIC X(69) VALUE SPACES.
       01  WS-RPT-REASON-LINE.
           05 FILLER                            PIC X(03) VALUE SPACES.
           05 WS-RPT-RSN-CODE                   PIC 9(04).
           05 FILLER                            PIC X(01) VALUE SPACE.
           05 WS-RPT-RSN-DESC                   PIC X(50).
           05 FILLER                            PIC X(02) VALUE ': '.
           05 WS-RPT-RSN-COUNT                  PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                            PIC X(62) VALUE SPACES.
       01  WS-RPT-AMOUNT-LINE.
           05 FILLER                            PIC X(01) VALUE SPACE.
           05 WS-RPT-AMT-LABEL                  PIC X(50).
           05 FILLER                            PIC X(02) VALUE ': '.
           05 WS-RPT-AMT-VALUE
                             PIC --,---,---,---,--9.99.
           05 FILLER                            PIC X(59) VALUE SPACES.
       01  WS-RPT-HEX-LINE.
           05 FILLER                            PIC X(01) VALUE SPACE.
           05 FILLER                            PIC X(50) VALUE
              'ACCEPTED AMOUNT COMP-3 IMAGE (S9(13)V99, 8 BYTES)'.
           05 FILLER                            PIC X(02) VALUE ': '.
           05 WS-RPT-HEX-VALUE                  PIC X(16).
           05 FILLER                            PIC X(64) VALUE SPACES.
       01  WS-RPT-JULIAN-LINE.
           05 FILLER                            PIC X(01) VALUE SPACE.
           05 FILLER                            PIC X(50) VALUE
              'ACCEPTED ORIGINATION DATE RANGE (JULIAN YYYYDDD)'.
           05 FILLER                            PIC X(02) VALUE ': '.
           05 WS-RPT-JUL-MIN                    PIC X(07).
           05 FILLER                            PIC X(03) VALUE ' - '.
           05 WS-RPT-JUL-MAX                    PIC X(07).
           05 FILLER                            PIC X(63) VALUE SPACES.
       01  WS-RPT-RECON-LINE.
           05 FILLER                            PIC X(01) VALUE SPACE.
           05 WS-RPT-RECON-TEXT                 PIC X(120).
           05 WS-RPT-RECON-RESULT               PIC X(12).
       01  WS-RPT-RC-LINE.
           05 FILLER                            PIC X(01) VALUE SPACE.
           05 FILLER                            PIC X(50) VALUE
              'RETURN CODE'.
           05 FILLER                            PIC X(02) VALUE ': '.
           05 WS-RPT-RC-VALUE                   PIC Z9.
           05 FILLER                            PIC X(78) VALUE SPACES.
       01  WS-RECON-COUNT-TEXT.
           05 FILLER                            PIC X(50) VALUE
              'RECONCILIATION: RECORDS READ = ACCEPTED + REJECTED'.
           05 FILLER                            PIC X(70) VALUE SPACES.
       01  WS-RECON-AMT-TEXT.
           05 FILLER                            PIC X(49) VALUE
              'RECONCILIATION: TOTAL AMOUNT = ACCEPTED AMOUNT + '.
           05 FILLER                            PIC X(71) VALUE
              'REJECTED AMOUNT'.
       01  WS-RECON-STATUS                      PIC X(01) VALUE 'Y'.

      *----------------------------------------------------------------
      * CEE3PRM (Language Environment) returns the EXEC PARM string as
      * a fixed 80-byte field plus a 12-byte feedback code. CEE000 is
      * all binary zeros, so severity 0 means the call succeeded.
      *----------------------------------------------------------------
       01  WS-CEE3PRM-PARM-STRING.
           05 WS-PARM-RUN-DATE                  PIC X(08).
           05 WS-PARM-REMAINDER                 PIC X(72).
       01  WS-CEE3PRM-FEEDBACK.
           05 WS-FC-SEVERITY                    PIC S9(04) COMP.
           05 WS-FC-MSG-NO                      PIC S9(04) COMP.
           05 WS-FC-CASE-SEV-CTL                PIC X(01).
           05 WS-FC-FACILITY-ID                 PIC X(03).
           05 WS-FC-ISI                         PIC S9(09) COMP.
       01  WS-CEE3PRM-AVAILABLE                 PIC X(01) VALUE 'Y'.

      *****************************************************************
       PROCEDURE DIVISION.
           DISPLAY 'START OF EXECUTION OF PROGRAM CBTRN04C'.
           INITIALIZE WS-REASON-COUNTS.
           PERFORM 0050-VALIDATE-PARM.
           PERFORM 0000-DALYTRAN-OPEN.
           PERFORM 0100-TRANTYPE-OPEN.
           PERFORM 0200-TRANCATG-OPEN.
           PERFORM 0300-XREFFILE-OPEN.
           PERFORM 0400-TCATBALF-OPEN.
           PERFORM 0500-DALYVALD-OPEN.
           PERFORM 0600-DALYRJ04-OPEN.
           PERFORM 0700-VALDRPT-OPEN.

           PERFORM UNTIL END-OF-FILE = 'Y'
               IF  END-OF-FILE = 'N'
                   PERFORM 1000-DALYTRAN-GET-NEXT
                   IF  END-OF-FILE = 'N'
                       ADD 1 TO WS-READ-COUNT
                       MOVE 0 TO WS-VALIDATION-FAIL-REASON
                       MOVE SPACES TO WS-VALIDATION-FAIL-REASON-DESC
                       PERFORM 1400-ACCUMULATE-READ-AMT
                       PERFORM 1500-VALIDATE-TRAN
                       IF  WS-VALIDATION-FAIL-REASON = 0
                           PERFORM 2000-WRITE-ACCEPTED-REC
                       ELSE
                           PERFORM 2500-WRITE-REJECT-REC
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

           IF  WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           END-IF.
           PERFORM 9000-DALYTRAN-CLOSE.
           PERFORM 9100-TRANTYPE-CLOSE.
           PERFORM 9200-TRANCATG-CLOSE.
           PERFORM 9300-XREFFILE-CLOSE.
           PERFORM 9400-TCATBALF-CLOSE.
           PERFORM 9500-DALYVALD-CLOSE.
           PERFORM 9600-DALYRJ04-CLOSE.

      *    Every other file is closed before the report is written so
      *    the return code printed on it is the one the step ends with.
           PERFORM 3000-WRITE-CONTROL-REPORT.
           PERFORM 9700-VALDRPT-CLOSE.
           DISPLAY 'TRANSACTIONS READ      :' WS-READ-COUNT.
           DISPLAY 'TRANSACTIONS ACCEPTED  :' WS-ACCEPT-COUNT.
           DISPLAY 'TRANSACTIONS REJECTED  :' WS-REJECT-COUNT.
           DISPLAY 'END OF EXECUTION OF PROGRAM CBTRN04C'.

           GOBACK.

      *---------------------------------------------------------------*
      * The run date arrives as PARM='YYYYMMDD'. Anything else stops
      * the run with return code 8 before any file is opened.
      *---------------------------------------------------------------*
       0050-VALIDATE-PARM.
           MOVE 'N'    TO WS-DATE-VALID
           MOVE SPACES TO WS-CEE3PRM-PARM-STRING
           MOVE LOW-VALUES TO WS-CEE3PRM-FEEDBACK
           CALL 'CEE3PRM' USING WS-CEE3PRM-PARM-STRING
                                WS-CEE3PRM-FEEDBACK
               ON EXCEPTION
                  MOVE 'N' TO WS-CEE3PRM-AVAILABLE
           END-CALL
           IF  WS-CEE3PRM-AVAILABLE = 'N'
               DISPLAY 'CEE3PRM NOT AVAILABLE, RUN DATE PARM '
                       'CANNOT BE RETRIEVED'
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF
           IF  WS-FC-SEVERITY = 0
           AND WS-PARM-REMAINDER = SPACES
               MOVE WS-PARM-RUN-DATE TO WS-RUN-DATE-PARM
               IF  WS-RUN-DATE-PARM IS NUMERIC
                   MOVE WS-RUN-YYYY TO WS-DATE-IN-YYYY
                   MOVE '-'         TO WS-DATE-IN-SEP1
                   MOVE WS-RUN-MM   TO WS-DATE-IN-MM
                   MOVE '-'         TO WS-DATE-IN-SEP2
                   MOVE WS-RUN-DD   TO WS-DATE-IN-DD
                   PERFORM 5000-VALIDATE-GREGORIAN-DATE
                   THRU 5000-VALIDATE-GREGORIAN-EXIT
               END-IF
           END-IF
           IF  WS-DATE-VALID = 'Y'
               MOVE WS-DATE-IN       TO WS-RUN-DATE-ISO
               MOVE WS-DATE-INT-N    TO WS-RUN-DATE-INT
               MOVE WS-DATE-JULIAN-N TO WS-RUN-DATE-JULIAN
           ELSE
               DISPLAY 'INVALID OR MISSING RUN DATE PARM, '
                       'EXPECTED PARM=YYYYMMDD'
               DISPLAY 'PARM RECEIVED        : ' WS-PARM-RUN-DATE
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF
           EXIT.

      *---------------------------------------------------------------*
       0000-DALYTRAN-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN INPUT DALYTRAN-FILE
           IF  DALYTRAN-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING DALYTRAN'
               MOVE DALYTRAN-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       0100-TRANTYPE-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN INPUT TRANTYPE-FILE
           IF  TRANTYPE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING TRANSACTION TYPE FILE'
               MOVE TRANTYPE-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       0200-TRANCATG-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN INPUT TRANCATG-FILE
           IF  TRANCATG-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING TRANSACTION CATEGORY FILE'
               MOVE TRANCATG-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       0300-XREFFILE-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN INPUT XREF-FILE
           IF  XREFFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING CROSS REF FILE'
               MOVE XREFFILE-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       0400-TCATBALF-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN INPUT TCATBAL-FILE
           IF  TCATBALF-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING TRANSACTION BALANCE FILE'
               MOVE TCATBALF-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       0500-DALYVALD-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN OUTPUT DALYVALD-FILE
           IF  DALYVALD-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING VALIDATED TRANSACTION FILE'
               MOVE DALYVALD-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       0600-DALYRJ04-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN OUTPUT DALYRJ04-FILE
           IF  DALYRJ04-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING VALIDATION REJECTS FILE'
               MOVE DALYRJ04-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       0700-VALDRPT-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN OUTPUT REPORT-FILE
           IF  VALDRPT-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING VALIDATION REPORT FILE'
               MOVE VALDRPT-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.

      *---------------------------------------------------------------*
       1000-DALYTRAN-GET-NEXT.
           READ DALYTRAN-FILE INTO DALYTRAN-RECORD.
           IF  DALYTRAN-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               IF  DALYTRAN-STATUS = '10'
                   MOVE 16 TO APPL-RESULT
               ELSE
                   MOVE 12 TO APPL-RESULT
               END-IF
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               IF  APPL-EOF
                   MOVE 'Y' TO END-OF-FILE
               ELSE
                   DISPLAY 'ERROR READING DALYTRAN FILE'
                   MOVE DALYTRAN-STATUS TO IO-STATUS
                   PERFORM 9910-DISPLAY-IO-STATUS
                   PERFORM 9999-ABEND-PROGRAM
               END-IF
           END-IF
           EXIT.

      *---------------------------------------------------------------*
      * Total of every summable amount read, kept independently of
      * the accepted and rejected totals so the report can prove
      * read = accepted + rejected.
      *---------------------------------------------------------------*
       1400-ACCUMULATE-READ-AMT.
           IF  DALYTRAN-AMT IS NUMERIC
               MOVE 'Y' TO WS-AMT-NUMERIC-FLAG
               MOVE DALYTRAN-AMT TO WS-TRAN-AMT-P
               ADD WS-TRAN-AMT-P TO WS-TOTAL-AMT
           ELSE
               MOVE 'N' TO WS-AMT-NUMERIC-FLAG
               MOVE 0 TO WS-TRAN-AMT-P
               ADD 1 TO WS-NONNUM-AMT-COUNT
           END-IF
           EXIT.

      *---------------------------------------------------------------*
      * Rules run in a fixed order. The first failure sets the reason
      * and the remaining rules are skipped, so a record is rejected
      * once with one reason code.
      *---------------------------------------------------------------*
       1500-VALIDATE-TRAN.
           PERFORM 1510-CHECK-TRAN-ID.
           IF  WS-VALIDATION-FAIL-REASON = 0
               PERFORM 1520-LOOKUP-TRANTYPE
           END-IF
           IF  WS-VALIDATION-FAIL-REASON = 0
               PERFORM 1530-LOOKUP-TRANCATG
           END-IF
           IF  WS-VALIDATION-FAIL-REASON = 0
               PERFORM 1540-CHECK-AMOUNT
           END-IF
           IF  WS-VALIDATION-FAIL-REASON = 0
               PERFORM 1550-LOOKUP-XREF
           END-IF
           IF  WS-VALIDATION-FAIL-REASON = 0
               PERFORM 1560-CHECK-CAT-BAL-RANGE
           END-IF
           IF  WS-VALIDATION-FAIL-REASON = 0
               PERFORM 1570-CHECK-ORIG-DATE
           END-IF
           IF  WS-VALIDATION-FAIL-REASON = 0
               PERFORM 1580-CHECK-PROC-DATE
           END-IF
           IF  WS-VALIDATION-FAIL-REASON = 0
               PERFORM 1590-CHECK-FUTURE-DATES
           END-IF
           EXIT.

      *---------------------------------------------------------------*
       1510-CHECK-TRAN-ID.
           IF  DALYTRAN-ID = SPACES
           OR  DALYTRAN-ID = LOW-VALUES
               MOVE 1 TO WS-RSN-IX
               PERFORM 1900-SET-REASON
           END-IF
           EXIT.

      *---------------------------------------------------------------*
       1520-LOOKUP-TRANTYPE.
           MOVE DALYTRAN-TYPE-CD TO FD-TRAN-TYPE
           READ TRANTYPE-FILE INTO TRAN-TYPE-RECORD
                INVALID KEY
                MOVE 2 TO WS-RSN-IX
                PERFORM 1900-SET-REASON
                NOT INVALID KEY
                CONTINUE
           END-READ
           IF  TRANTYPE-STATUS = '00' OR '23'
               CONTINUE
           ELSE
               DISPLAY 'ERROR READING TRANSACTION TYPE FILE'
               MOVE TRANTYPE-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.

      *---------------------------------------------------------------*
      * TRANCATG is keyed on type + category (CVTRA04Y TRAN-CAT-KEY),
      * so the category is validated as a combination with its type.
      *---------------------------------------------------------------*
       1530-LOOKUP-TRANCATG.
           MOVE DALYTRAN-TYPE-CD TO FD-TRAN-TYPE-CD
           MOVE DALYTRAN-CAT-CD  TO FD-TRAN-CAT-CD
           READ TRANCATG-FILE INTO TRAN-CAT-RECORD
                INVALID KEY
                MOVE 3 TO WS-RSN-IX
                PERFORM 1900-SET-REASON
                NOT INVALID KEY
                CONTINUE
           END-READ
           IF  TRANCATG-STATUS = '00' OR '23'
               CONTINUE
           ELSE
               DISPLAY 'ERROR READING TRANSACTION CATEGORY FILE'
               MOVE TRANCATG-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.

      *---------------------------------------------------------------*
      * NUMERIC class test on the signed zoned field accepts digits
      * with a valid sign zone only. The value is then carried packed.
      *---------------------------------------------------------------*
       1540-CHECK-AMOUNT.
           IF  WS-AMT-NUMERIC-FLAG = 'Y'
               CONTINUE
           ELSE
               MOVE 4 TO WS-RSN-IX
               PERFORM 1900-SET-REASON
           END-IF
           EXIT.

      *---------------------------------------------------------------*
      * Same lookup and same reason code (0100) as CBTRN02C
      * 1500-A-LOOKUP-XREF.
      *---------------------------------------------------------------*
       1550-LOOKUP-XREF.
           MOVE DALYTRAN-CARD-NUM TO FD-XREF-CARD-NUM
           READ XREF-FILE INTO CARD-XREF-RECORD
                INVALID KEY
                MOVE 5 TO WS-RSN-IX
                PERFORM 1900-SET-REASON
                NOT INVALID KEY
                CONTINUE
           END-READ
           IF  XREFFILE-STATUS = '00' OR '23'
               CONTINUE
           ELSE
               DISPLAY 'ERROR READING CROSS REF FILE'
               MOVE XREFFILE-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.

      *---------------------------------------------------------------*
      * CBTRN02C adds the amount to TRAN-CAT-BAL (S9(09)V99) with no
      * SIZE ERROR clause. Project the balance the posting would
      * produce and reject when it cannot be held. A missing balance
      * record is not an error: CBTRN02C creates it at posting time.
      * The starting balance is the projection left by earlier
      * accepted records for the same key when there is one, else the
      * balance on file.
      *---------------------------------------------------------------*
       1560-CHECK-CAT-BAL-RANGE.
           MOVE XREF-ACCT-ID     TO WS-BAL-SEARCH-ACCT-ID
           MOVE DALYTRAN-TYPE-CD TO WS-BAL-SEARCH-TYPE-CD
           MOVE DALYTRAN-CAT-CD  TO WS-BAL-SEARCH-CAT-CD
           MOVE 0 TO WS-BAL-FOUND-IX
           PERFORM VARYING WS-BAL-IX FROM 1 BY 1
               UNTIL WS-BAL-IX > WS-BAL-TABLE-COUNT
               OR    WS-BAL-FOUND-IX > 0
               IF  WS-BAL-KEY (WS-BAL-IX) = WS-BAL-SEARCH-KEY
                   MOVE WS-BAL-IX TO WS-BAL-FOUND-IX
               END-IF
           END-PERFORM
           IF  WS-BAL-FOUND-IX > 0
               MOVE WS-BAL-PROJ (WS-BAL-FOUND-IX) TO WS-PROJ-CAT-BAL
           ELSE
               PERFORM 1565-READ-CAT-BAL
           END-IF
           ADD WS-TRAN-AMT-P TO WS-PROJ-CAT-BAL
           IF  WS-PROJ-CAT-BAL > WS-AMT-CEILING
           OR  WS-PROJ-CAT-BAL < WS-AMT-FLOOR
               MOVE 6 TO WS-RSN-IX
               PERFORM 1900-SET-REASON
           END-IF
           EXIT.

      *---------------------------------------------------------------*
       1565-READ-CAT-BAL.
           MOVE WS-BAL-SEARCH-ACCT-ID TO FD-TCATBAL-ACCT-ID
           MOVE WS-BAL-SEARCH-TYPE-CD TO FD-TCATBAL-TYPE-CD
           MOVE WS-BAL-SEARCH-CAT-CD  TO FD-TCATBAL-CD
           MOVE 0 TO WS-PROJ-CAT-BAL
           READ TCATBAL-FILE INTO TRAN-CAT-BAL-RECORD
                INVALID KEY
                MOVE 0 TO WS-PROJ-CAT-BAL
                NOT INVALID KEY
                MOVE TRAN-CAT-BAL TO WS-PROJ-CAT-BAL
           END-READ
           IF  TCATBALF-STATUS = '00' OR '23'
               CONTINUE
           ELSE
               DISPLAY 'ERROR READING TRANSACTION BALANCE FILE'
               MOVE TCATBALF-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.

      *---------------------------------------------------------------*
       1570-CHECK-ORIG-DATE.
           MOVE DALYTRAN-ORIG-TS (1:10) TO WS-DATE-IN
           PERFORM 5000-VALIDATE-GREGORIAN-DATE
                   THRU 5000-VALIDATE-GREGORIAN-EXIT
           IF  WS-DATE-VALID = 'Y'
               MOVE WS-DATE-INT-N    TO WS-ORIG-DATE-INT
               MOVE WS-DATE-JULIAN-N TO WS-ORIG-DATE-JULIAN
           ELSE
               MOVE 7 TO WS-RSN-IX
               PERFORM 1900-SET-REASON
           END-IF
           EXIT.

      *---------------------------------------------------------------*
      * The feed may carry a blank processing timestamp; CBTRN02C
      * overwrites TRAN-PROC-TS with the posting time, so a blank
      * value is accepted and the date rules apply only when present.
      *---------------------------------------------------------------*
       1580-CHECK-PROC-DATE.
           MOVE 0 TO WS-PROC-DATE-INT
                     WS-PROC-DATE-JULIAN
           IF  DALYTRAN-PROC-TS = SPACES
           OR  DALYTRAN-PROC-TS = LOW-VALUES
               MOVE 'N' TO WS-PROC-TS-PRESENT
           ELSE
               MOVE 'Y' TO WS-PROC-TS-PRESENT
               MOVE DALYTRAN-PROC-TS (1:10) TO WS-DATE-IN
               PERFORM 5000-VALIDATE-GREGORIAN-DATE
                   THRU 5000-VALIDATE-GREGORIAN-EXIT
               IF  WS-DATE-VALID = 'Y'
                   MOVE WS-DATE-INT-N    TO WS-PROC-DATE-INT
                   MOVE WS-DATE-JULIAN-N TO WS-PROC-DATE-JULIAN
                   IF  WS-ORIG-DATE-INT > WS-PROC-DATE-INT
                       MOVE 9 TO WS-RSN-IX
                       PERFORM 1900-SET-REASON
                   END-IF
               ELSE
                   MOVE 8 TO WS-RSN-IX
                   PERFORM 1900-SET-REASON
               END-IF
           END-IF
           EXIT.

      *---------------------------------------------------------------*
       1590-CHECK-FUTURE-DATES.
           IF  WS-ORIG-DATE-INT > WS-RUN-DATE-INT
               MOVE 10 TO WS-RSN-IX
               PERFORM 1900-SET-REASON
           ELSE
               IF  WS-PROC-TS-PRESENT = 'Y'
               AND WS-PROC-DATE-INT > WS-RUN-DATE-INT
                   MOVE 10 TO WS-RSN-IX
                   PERFORM 1900-SET-REASON
               END-IF
           END-IF
           EXIT.

      *---------------------------------------------------------------*
       1900-SET-REASON.
           MOVE WS-RSN-CODE (WS-RSN-IX)
                TO WS-VALIDATION-FAIL-REASON
           MOVE WS-RSN-DESC (WS-RSN-IX)
                TO WS-VALIDATION-FAIL-REASON-DESC
           ADD 1 TO WS-RSN-COUNT (WS-RSN-IX)
           EXIT.

      *---------------------------------------------------------------*
      * Accepted records are written from the input buffer so the
      * 350 bytes leave exactly as they arrived.
      *---------------------------------------------------------------*
      * Called once a record is accepted: carry its amount forward in
      * the projection for its account/type/category key.
      *---------------------------------------------------------------*
       1950-UPDATE-BAL-PROJECTION.
           IF  WS-BAL-FOUND-IX > 0
               MOVE WS-PROJ-CAT-BAL TO WS-BAL-PROJ (WS-BAL-FOUND-IX)
           ELSE
               IF  WS-BAL-TABLE-COUNT < WS-BAL-TABLE-MAX
                   ADD 1 TO WS-BAL-TABLE-COUNT
                   MOVE WS-BAL-SEARCH-KEY
                     TO WS-BAL-KEY (WS-BAL-TABLE-COUNT)
                   MOVE WS-PROJ-CAT-BAL
                     TO WS-BAL-PROJ (WS-BAL-TABLE-COUNT)
               ELSE
                   DISPLAY 'CATEGORY BALANCE PROJECTION TABLE FULL, '
                           'MORE THAN ' WS-BAL-TABLE-MAX
                           ' ACCOUNT/TYPE/CATEGORY KEYS IN FEED'
                   PERFORM 9999-ABEND-PROGRAM
               END-IF
           END-IF
           EXIT.

      *---------------------------------------------------------------*
       2000-WRITE-ACCEPTED-REC.
           ADD 1 TO WS-ACCEPT-COUNT
           ADD WS-TRAN-AMT-P TO WS-ACCEPT-AMT
           PERFORM 1950-UPDATE-BAL-PROJECTION
           IF  WS-ACCEPT-COUNT = 1
               MOVE WS-ORIG-DATE-JULIAN TO WS-ACCEPT-JULIAN-MIN
                                           WS-ACCEPT-JULIAN-MAX
           ELSE
               IF  WS-ORIG-DATE-JULIAN < WS-ACCEPT-JULIAN-MIN
                   MOVE WS-ORIG-DATE-JULIAN TO WS-ACCEPT-JULIAN-MIN
               END-IF
               IF  WS-ORIG-DATE-JULIAN > WS-ACCEPT-JULIAN-MAX
                   MOVE WS-ORIG-DATE-JULIAN TO WS-ACCEPT-JULIAN-MAX
               END-IF
           END-IF
           MOVE 8 TO APPL-RESULT
           WRITE FD-VALD-RECORD FROM FD-TRAN-RECORD
           IF  DALYVALD-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR WRITING TO VALIDATED TRANSACTION FILE'
               MOVE DALYVALD-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.

      *---------------------------------------------------------------*
       2500-WRITE-REJECT-REC.
           ADD 1 TO WS-REJECT-COUNT
           ADD WS-TRAN-AMT-P TO WS-REJECT-AMT
           MOVE FD-TRAN-RECORD TO REJECT-TRAN-DATA
           MOVE WS-VALIDATION-TRAILER TO VALIDATION-TRAILER
           MOVE 8 TO APPL-RESULT
           WRITE FD-REJS-RECORD FROM REJECT-RECORD
           IF  DALYRJ04-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR WRITING TO VALIDATION REJECTS FILE'
               MOVE DALYRJ04-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.

      *---------------------------------------------------------------*
       3000-WRITE-CONTROL-REPORT.
           MOVE WS-RPT-HEADER TO WS-REPORT-LINE
           PERFORM 3900-WRITE-REPORT-REC
           MOVE WS-RUN-DATE-ISO    TO WS-RPT-RUN-DATE-ISO
           MOVE WS-RUN-DATE-JULIAN TO WS-RPT-RUN-DATE-JUL
           MOVE WS-RPT-RUN-DATE TO WS-REPORT-LINE
           PERFORM 3900-WRITE-REPORT-REC
           MOVE SPACES TO WS-REPORT-LINE
           PERFORM 3900-WRITE-REPORT-REC

           MOVE 'RECORDS READ'      TO WS-RPT-COUNT-LABEL
           MOVE WS-READ-COUNT       TO WS-RPT-COUNT-VALUE
           MOVE WS-RPT-COUNT-LINE   TO WS-REPORT-LINE
           PERFORM 3900-WRITE-REPORT-REC
           MOVE 'RECORDS ACCEPTED'  TO WS-RPT-COUNT-LABEL
           MOVE WS-ACCEPT-COUNT     TO WS-RPT-COUNT-VALUE
           MOVE WS-RPT-COUNT-LINE   TO WS-REPORT-LINE
           PERFORM 3900-WRITE-REPORT-REC
           MOVE 'RECORDS REJECTED'  TO WS-RPT-COUNT-LABEL
           MOVE WS-REJECT-COUNT     TO WS-RPT-COUNT-VALUE
           MOVE WS-RPT-COUNT-LINE   TO WS-REPORT-LINE
           PERFORM 3900-WRITE-REPORT-REC
           MOVE SPACES TO WS-REPORT-LINE
           PERFORM 3900-WRITE-REPORT-REC

           MOVE ' REJECTED BY REASON CODE' TO WS-REPORT-LINE
           PERFORM 3900-WRITE-REPORT-REC
           PERFORM VARYING WS-RSN-IX FROM 1 BY 1
                   UNTIL WS-RSN-IX > WS-REASON-MAX
               MOVE WS-RSN-CODE (WS-RSN-IX)  TO WS-RPT-RSN-CODE
               MOVE WS-RSN-DESC (WS-RSN-IX)  TO WS-RPT-RSN-DESC
               MOVE WS-RSN-COUNT (WS-RSN-IX) TO WS-RPT-RSN-COUNT
               MOVE WS-RPT-REASON-LINE       TO WS-REPORT-LINE
               PERFORM 3900-WRITE-REPORT-REC
           END-PERFORM
           MOVE SPACES TO WS-REPORT-LINE
           PERFORM 3900-WRITE-REPORT-REC

           MOVE 'ACCEPTED AMOUNT TOTAL' TO WS-RPT-AMT-LABEL
           MOVE WS-ACCEPT-AMT           TO WS-RPT-AMT-VALUE
           MOVE WS-RPT-AMOUNT-LINE      TO WS-REPORT-LINE
           PERFORM 3900-WRITE-REPORT-REC
           MOVE 'REJECTED AMOUNT TOTAL (NUMERIC AMOUNTS ONLY)'
                                        TO WS-RPT-AMT-LABEL
           MOVE WS-REJECT-AMT           TO WS-RPT-AMT-VALUE
           MOVE WS-RPT-AMOUNT-LINE      TO WS-REPORT-LINE
           PERFORM 3900-WRITE-REPORT-REC
           MOVE 'TOTAL AMOUNT READ (NUMERIC AMOUNTS ONLY)'
                                        TO WS-RPT-AMT-LABEL
           MOVE WS-TOTAL-AMT            TO WS-RPT-AMT-VALUE
           MOVE WS-RPT-AMOUNT-LINE      TO WS-REPORT-LINE
           PERFORM 3900-WRITE-REPORT-REC
           MOVE 'REJECTED WITH NON-NUMERIC AMOUNT (NOT SUMMED)'
                                        TO WS-RPT-COUNT-LABEL
           MOVE WS-NONNUM-AMT-COUNT     TO WS-RPT-COUNT-VALUE
           MOVE WS-RPT-COUNT-LINE       TO WS-REPORT-LINE
           PERFORM 3900-WRITE-REPORT-REC
           MOVE WS-ACCEPT-AMT-BYTES     TO WS-HEX-IN
           PERFORM 8000-PACKED-TO-HEX
           MOVE WS-HEX-OUT              TO WS-RPT-HEX-VALUE
           MOVE WS-RPT-HEX-LINE         TO WS-REPORT-LINE
           PERFORM 3900-WRITE-REPORT-REC
           MOVE SPACES TO WS-REPORT-LINE
           PERFORM 3900-WRITE-REPORT-REC

           IF  WS-ACCEPT-COUNT > 0
               MOVE WS-ACCEPT-JULIAN-MIN TO WS-RPT-JUL-MIN
               MOVE WS-ACCEPT-JULIAN-MAX TO WS-RPT-JUL-MAX
           ELSE
               MOVE 'NONE   ' TO WS-RPT-JUL-MIN
                                 WS-RPT-JUL-MAX
           END-IF
           MOVE WS-RPT-JULIAN-LINE TO WS-REPORT-LINE
           PERFORM 3900-WRITE-REPORT-REC
           MOVE SPACES TO WS-REPORT-LINE
           PERFORM 3900-WRITE-REPORT-REC

           COMPUTE WS-CHECK-COUNT = WS-ACCEPT-COUNT + WS-REJECT-COUNT
           MOVE WS-RECON-COUNT-TEXT TO WS-RPT-RECON-TEXT
           IF  WS-CHECK-COUNT = WS-READ-COUNT
               MOVE 'OK' TO WS-RPT-RECON-RESULT
           ELSE
               MOVE 'OUT OF BAL' TO WS-RPT-RECON-RESULT
               MOVE 'N' TO WS-RECON-STATUS
           END-IF
           MOVE WS-RPT-RECON-LINE TO WS-REPORT-LINE
           PERFORM 3900-WRITE-REPORT-REC

           COMPUTE WS-CHECK-AMT = WS-ACCEPT-AMT + WS-REJECT-AMT
           MOVE WS-RECON-AMT-TEXT TO WS-RPT-RECON-TEXT
           IF  WS-CHECK-AMT = WS-TOTAL-AMT
               MOVE 'OK' TO WS-RPT-RECON-RESULT
           ELSE
               MOVE 'OUT OF BAL' TO WS-RPT-RECON-RESULT
               MOVE 'N' TO WS-RECON-STATUS
           END-IF
           MOVE WS-RPT-RECON-LINE TO WS-REPORT-LINE
           PERFORM 3900-WRITE-REPORT-REC

           IF  WS-RECON-STATUS = 'N'
               DISPLAY 'CONTROL TOTALS DO NOT RECONCILE'
               MOVE 12 TO RETURN-CODE
           END-IF
           MOVE RETURN-CODE TO WS-RPT-RC-VALUE
           MOVE WS-RPT-RC-LINE TO WS-REPORT-LINE
           PERFORM 3900-WRITE-REPORT-REC
           EXIT.

      *---------------------------------------------------------------*
       3900-WRITE-REPORT-REC.
           MOVE 8 TO APPL-RESULT
           WRITE FD-REPTFILE-REC FROM WS-REPORT-LINE
           IF  VALDRPT-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR WRITING TO VALIDATION REPORT FILE'
               MOVE VALDRPT-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.

      *---------------------------------------------------------------*
      * Gregorian date check and Gregorian to Julian conversion.
      * Input  : WS-DATE-IN  as YYYY-MM-DD
      * Output : WS-DATE-VALID Y/N, WS-DATE-INT YYYYMMDD,
      *          WS-DATE-JULIAN YYYYDDD
      * Leap year: divisible by 4, except centuries unless divisible
      * by 400. Implemented in COBOL arithmetic; no Language
      * Environment service (CEEDAYS) is called.
      *---------------------------------------------------------------*
       5000-VALIDATE-GREGORIAN-DATE.
           MOVE 'N' TO WS-DATE-VALID
           MOVE 0   TO WS-DATE-INT-N
                       WS-DATE-JULIAN-N
           IF  WS-DATE-IN-SEP1 NOT = '-'
           OR  WS-DATE-IN-SEP2 NOT = '-'
           OR  WS-DATE-IN-YYYY IS NOT NUMERIC
           OR  WS-DATE-IN-MM   IS NOT NUMERIC
           OR  WS-DATE-IN-DD   IS NOT NUMERIC
               GO TO 5000-VALIDATE-GREGORIAN-EXIT
           END-IF
           IF  WS-DATE-YYYY-N < 1
           OR  WS-DATE-MM-N < 1 OR WS-DATE-MM-N > 12
           OR  WS-DATE-DD-N < 1
               GO TO 5000-VALIDATE-GREGORIAN-EXIT
           END-IF
           PERFORM 5100-SET-LEAP-YEAR
           MOVE WS-DATE-MM-N TO WS-MONTH-IX
           EVALUATE WS-MONTH-IX
               WHEN 4
               WHEN 6
               WHEN 9
               WHEN 11
                   MOVE 30 TO WS-DAYS-IN-MONTH
               WHEN 2
                   IF  WS-LEAP-YEAR = 'Y'
                       MOVE 29 TO WS-DAYS-IN-MONTH
                   ELSE
                       MOVE 28 TO WS-DAYS-IN-MONTH
                   END-IF
               WHEN OTHER
                   MOVE 31 TO WS-DAYS-IN-MONTH
           END-EVALUATE
           IF  WS-DATE-DD-N > WS-DAYS-IN-MONTH
               GO TO 5000-VALIDATE-GREGORIAN-EXIT
           END-IF
           MOVE 'Y' TO WS-DATE-VALID
           MOVE WS-DATE-YYYY-N TO WS-DATE-INT-YYYY
                                  WS-DATE-JULIAN-YYYY
           MOVE WS-DATE-MM-N   TO WS-DATE-INT-MM
           MOVE WS-DATE-DD-N   TO WS-DATE-INT-DD
           COMPUTE WS-DATE-JULIAN-DDD =
                   WS-DAYS-BEFORE (WS-MONTH-IX) + WS-DATE-DD-N
           IF  WS-LEAP-YEAR = 'Y' AND WS-MONTH-IX > 2
               ADD 1 TO WS-DATE-JULIAN-DDD
           END-IF.
       5000-VALIDATE-GREGORIAN-EXIT.
           EXIT.

      *---------------------------------------------------------------*
       5100-SET-LEAP-YEAR.
           MOVE 'N' TO WS-LEAP-YEAR
           DIVIDE WS-DATE-YYYY-N BY 4 GIVING WS-LEAP-WORK
                  REMAINDER WS-LEAP-REM
           IF  WS-LEAP-REM = 0
               MOVE 'Y' TO WS-LEAP-YEAR
               DIVIDE WS-DATE-YYYY-N BY 100 GIVING WS-LEAP-WORK
                      REMAINDER WS-LEAP-REM
               IF  WS-LEAP-REM = 0
                   MOVE 'N' TO WS-LEAP-YEAR
                   DIVIDE WS-DATE-YYYY-N BY 400 GIVING WS-LEAP-WORK
                          REMAINDER WS-LEAP-REM
                   IF  WS-LEAP-REM = 0
                       MOVE 'Y' TO WS-LEAP-YEAR
                   END-IF
               END-IF
           END-IF
           EXIT.

      *---------------------------------------------------------------*
      * Render the 8 bytes of a COMP-3 field as 16 hex characters so
      * the packed image (digits + sign nibble) is visible on the
      * report.
      *---------------------------------------------------------------*
       8000-PACKED-TO-HEX.
           MOVE 0 TO WS-HEX-OX
           PERFORM VARYING WS-HEX-IX FROM 1 BY 1
                   UNTIL WS-HEX-IX > 8
               COMPUTE WS-HEX-VAL =
                       FUNCTION ORD (WS-HEX-BYTE (WS-HEX-IX)) - 1
               DIVIDE WS-HEX-VAL BY 16 GIVING WS-HEX-HI
                      REMAINDER WS-HEX-LO
               ADD 1 TO WS-HEX-OX
               MOVE WS-HEX-DIGITS (WS-HEX-HI + 1:1)
                    TO WS-HEX-CHAR (WS-HEX-OX)
               ADD 1 TO WS-HEX-OX
               MOVE WS-HEX-DIGITS (WS-HEX-LO + 1:1)
                    TO WS-HEX-CHAR (WS-HEX-OX)
           END-PERFORM
           EXIT.

      *---------------------------------------------------------------*
       9000-DALYTRAN-CLOSE.
           MOVE 8 TO APPL-RESULT.
           CLOSE DALYTRAN-FILE
           IF  DALYTRAN-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR CLOSING DALYTRAN FILE'
               MOVE DALYTRAN-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       9100-TRANTYPE-CLOSE.
           MOVE 8 TO APPL-RESULT.
           CLOSE TRANTYPE-FILE
           IF  TRANTYPE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR CLOSING TRANSACTION TYPE FILE'
               MOVE TRANTYPE-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       9200-TRANCATG-CLOSE.
           MOVE 8 TO APPL-RESULT.
           CLOSE TRANCATG-FILE
           IF  TRANCATG-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR CLOSING TRANSACTION CATEGORY FILE'
               MOVE TRANCATG-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       9300-XREFFILE-CLOSE.
           MOVE 8 TO APPL-RESULT.
           CLOSE XREF-FILE
           IF  XREFFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR CLOSING CROSS REF FILE'
               MOVE XREFFILE-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       9400-TCATBALF-CLOSE.
           MOVE 8 TO APPL-RESULT.
           CLOSE TCATBAL-FILE
           IF  TCATBALF-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR CLOSING TRANSACTION BALANCE FILE'
               MOVE TCATBALF-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       9500-DALYVALD-CLOSE.
           MOVE 8 TO APPL-RESULT.
           CLOSE DALYVALD-FILE
           IF  DALYVALD-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR CLOSING VALIDATED TRANSACTION FILE'
               MOVE DALYVALD-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       9600-DALYRJ04-CLOSE.
           MOVE 8 TO APPL-RESULT.
           CLOSE DALYRJ04-FILE
           IF  DALYRJ04-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR CLOSING VALIDATION REJECTS FILE'
               MOVE DALYRJ04-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       9700-VALDRPT-CLOSE.
           MOVE 8 TO APPL-RESULT.
           CLOSE REPORT-FILE
           IF  VALDRPT-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR CLOSING VALIDATION REPORT FILE'
               MOVE VALDRPT-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.

      *---------------------------------------------------------------*
      * File errors end the run with return code 12, the APPL-RESULT
      * value CBTRN02C assigns to a failed I/O. CBTRN02C then abends
      * through CEE3ABD (user abend 999); this program ends with the
      * return code instead so a following job step can test it with
      * COND and so the same code path runs under GnuCOBOL.
      *---------------------------------------------------------------*
       9999-ABEND-PROGRAM.
           DISPLAY 'ABENDING PROGRAM'
           MOVE 12 TO RETURN-CODE
           GOBACK.

      *****************************************************************
       9910-DISPLAY-IO-STATUS.
           IF  IO-STATUS NOT NUMERIC
           OR  IO-STAT1 = '9'
               MOVE IO-STAT1 TO IO-STATUS-04(1:1)
               MOVE 0        TO TWO-BYTES-BINARY
               MOVE IO-STAT2 TO TWO-BYTES-RIGHT
               MOVE TWO-BYTES-BINARY TO IO-STATUS-0403
               DISPLAY 'FILE STATUS IS: NNNN' IO-STATUS-04
           ELSE
               MOVE '0000' TO IO-STATUS-04
               MOVE IO-STATUS TO IO-STATUS-04(3:2)
               DISPLAY 'FILE STATUS IS: NNNN' IO-STATUS-04
           END-IF
           EXIT.
