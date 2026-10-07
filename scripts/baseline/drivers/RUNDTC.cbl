      ******************************************************************
      * RUNDTC - batch driver for CSUTLDTC, the date-validation
      * subprogram (no JCL runs it; online programs CORPT00C and
      * COTRN02C CALL 'CSUTLDTC' USING date, mask, result).
      * Feeds a fixed set of dates through CSUTLDTC + the CEEDAYS stub
      * and prints the 80-byte result area and RETURN-CODE for each, so
      * the baseline captures the message text the online programs see
      * (CSUTLDTC-RESULT-SEV-CD / -MSG-NUM / -MSG).
      * New for this baseline.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUNDTC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CASES.
           05  FILLER PIC X(20) VALUE '2022-07-06YYYY-MM-DD'.
           05  FILLER PIC X(20) VALUE '2024-02-29YYYY-MM-DD'.
           05  FILLER PIC X(20) VALUE '2023-02-29YYYY-MM-DD'.
           05  FILLER PIC X(20) VALUE '2022-13-01YYYY-MM-DD'.
           05  FILLER PIC X(20) VALUE '2022-00-10YYYY-MM-DD'.
           05  FILLER PIC X(20) VALUE '2022-04-31YYYY-MM-DD'.
           05  FILLER PIC X(20) VALUE '20AB-01-01YYYY-MM-DD'.
           05  FILLER PIC X(20) VALUE '          YYYY-MM-DD'.
           05  FILLER PIC X(20) VALUE '1600-01-01YYYY-MM-DD'.
           05  FILLER PIC X(20) VALUE '20220706  YYYYMMDD  '.
           05  FILLER PIC X(20) VALUE '07/06/2022MM/DD/YYYY'.
           05  FILLER PIC X(20) VALUE '2022-07-06          '.
       01  WS-CASE-TABLE REDEFINES WS-CASES.
           05  WS-CASE OCCURS 12 TIMES.
               10  WS-CASE-DATE    PIC X(10).
               10  WS-CASE-MASK    PIC X(10).
       01  WS-I                    PIC 9(2).
       01  WS-DATE                 PIC X(10).
       01  WS-MASK                 PIC X(10).
       01  WS-RESULT               PIC X(80).
       01  WS-RC                   PIC 9(4).
       PROCEDURE DIVISION.
           DISPLAY 'RUNDTC: CSUTLDTC baseline cases'.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 12
              MOVE WS-CASE-DATE(WS-I) TO WS-DATE
              MOVE WS-CASE-MASK(WS-I) TO WS-MASK
              MOVE SPACES TO WS-RESULT
              MOVE 0 TO RETURN-CODE
              CALL 'CSUTLDTC' USING WS-DATE, WS-MASK, WS-RESULT
              MOVE RETURN-CODE TO WS-RC
              DISPLAY 'CASE ' WS-I ' IN=[' WS-DATE '] MASK=[' WS-MASK
                      '] RC=' WS-RC
              DISPLAY '        RESULT=[' WS-RESULT ']'
           END-PERFORM.
           MOVE 0 TO RETURN-CODE.
           GOBACK.
