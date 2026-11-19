      ******************************************************************
      * Unit tests for CSUTLDTC (date validation utility).
      * Each case calls CSUTLDTC with a date + picture mask and asserts
      * RETURN-CODE (CEEDAYS severity), the message number and the
      * result text CSUTLDTC builds in LS-RESULT.
      * Exit code = number of failed assertions (0 = all passed).
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTDTC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-DATE           PIC X(10).
       01  WS-FMT            PIC X(10).
       01  WS-RESULT         PIC X(80).
       01  WS-RC             PIC S9(4) COMP.
       01  WS-FAILS          PIC 9(4) VALUE 0.
       01  WS-PASSES         PIC 9(4) VALUE 0.
       01  WS-EXP-RC         PIC 9(4).
       01  WS-EXP-MSG        PIC X(04).
       01  WS-EXP-TEXT       PIC X(15).
       01  WS-CASE           PIC X(40).
       01  WS-RC-DISP        PIC 9(4).

       01  WS-VSTR-DATE.
           05  WS-VD-LEN     PIC S9(4) BINARY VALUE 10.
           05  WS-VD-TXT     PIC X(10).
       01  WS-VSTR-FMT.
           05  WS-VF-LEN     PIC S9(4) BINARY VALUE 10.
           05  WS-VF-TXT     PIC X(10) VALUE 'YYYY-MM-DD'.
       01  WS-LILIAN         PIC S9(9) BINARY.
       01  WS-FC             PIC X(12).
       01  WS-EXP-LILIAN     PIC S9(9).
       01  WS-INT-DATE       PIC 9(8).

       PROCEDURE DIVISION.
      *    ---- valid dates ------------------------------------------
           MOVE 'valid ordinary date' TO WS-CASE
           MOVE '2024-06-15' TO WS-DATE  MOVE 'YYYY-MM-DD' TO WS-FMT
           MOVE 0 TO WS-EXP-RC MOVE '0000' TO WS-EXP-MSG
           MOVE 'Date is valid' TO WS-EXP-TEXT
           PERFORM RUN-CASE

           MOVE 'leap day in leap year 2024' TO WS-CASE
           MOVE '2024-02-29' TO WS-DATE
           PERFORM RUN-CASE

           MOVE 'leap day in 400-year century 2000' TO WS-CASE
           MOVE '2000-02-29' TO WS-DATE
           PERFORM RUN-CASE

           MOVE 'year-end boundary 1999-12-31' TO WS-CASE
           MOVE '1999-12-31' TO WS-DATE
           PERFORM RUN-CASE

           MOVE 'year-start boundary 2000-01-01' TO WS-CASE
           MOVE '2000-01-01' TO WS-DATE
           PERFORM RUN-CASE

           MOVE 'first Lilian day 1582-10-15' TO WS-CASE
           MOVE '1582-10-15' TO WS-DATE
           PERFORM RUN-CASE

           MOVE 'last supported day 9999-12-31' TO WS-CASE
           MOVE '9999-12-31' TO WS-DATE
           PERFORM RUN-CASE

           MOVE 'compact mask YYYYMMDD' TO WS-CASE
           MOVE '20240229' TO WS-DATE  MOVE 'YYYYMMDD' TO WS-FMT
           PERFORM RUN-CASE
           MOVE 'YYYY-MM-DD' TO WS-FMT

      *    ---- invalid day for month / leap rules -------------------
           MOVE 3 TO WS-EXP-RC MOVE '2508' TO WS-EXP-MSG
           MOVE 'Datevalue error' TO WS-EXP-TEXT

           MOVE 'Feb 29 in non-leap year 2023' TO WS-CASE
           MOVE '2023-02-29' TO WS-DATE
           PERFORM RUN-CASE

           MOVE 'Feb 29 in century non-leap 1900' TO WS-CASE
           MOVE '1900-02-29' TO WS-DATE
           PERFORM RUN-CASE

           MOVE 'April 31' TO WS-CASE
           MOVE '2024-04-31' TO WS-DATE
           PERFORM RUN-CASE

           MOVE 'day 00' TO WS-CASE
           MOVE '2024-01-00' TO WS-DATE
           PERFORM RUN-CASE

           MOVE 'day 32' TO WS-CASE
           MOVE '2024-01-32' TO WS-DATE
           PERFORM RUN-CASE

      *    ---- invalid month ----------------------------------------
           MOVE '2517' TO WS-EXP-MSG
           MOVE 'Invalid month' TO WS-EXP-TEXT

           MOVE 'month 13' TO WS-CASE
           MOVE '2024-13-01' TO WS-DATE
           PERFORM RUN-CASE

           MOVE 'month 00' TO WS-CASE
           MOVE '2024-00-10' TO WS-DATE
           PERFORM RUN-CASE

      *    ---- range boundaries -------------------------------------
           MOVE '2513' TO WS-EXP-MSG
           MOVE 'Unsupp. Range' TO WS-EXP-TEXT

           MOVE 'day before Lilian epoch 1582-10-14' TO WS-CASE
           MOVE '1582-10-14' TO WS-DATE
           PERFORM RUN-CASE

           MOVE 'year 0000' TO WS-CASE
           MOVE '0000-01-01' TO WS-DATE
           PERFORM RUN-CASE

      *    ---- bad formats ------------------------------------------
           MOVE '2520' TO WS-EXP-MSG
           MOVE 'Nonnumeric data' TO WS-EXP-TEXT

           MOVE 'alphabetic month' TO WS-CASE
           MOVE '2024-AB-01' TO WS-DATE
           PERFORM RUN-CASE

           MOVE 'alphabetic year' TO WS-CASE
           MOVE 'YYYY-01-01' TO WS-DATE
           PERFORM RUN-CASE

           MOVE '2507' TO WS-EXP-MSG
           MOVE 'Insufficient' TO WS-EXP-TEXT

           MOVE 'blank date' TO WS-CASE
           MOVE SPACES TO WS-DATE
           PERFORM RUN-CASE

           MOVE 'truncated date 2024-02-1' TO WS-CASE
           MOVE '2024-02-1' TO WS-DATE
           PERFORM RUN-CASE

           MOVE 'date longer than YYYYMMDD mask' TO WS-CASE
           MOVE '2024-02-29' TO WS-DATE  MOVE 'YYYYMMDD' TO WS-FMT
           PERFORM RUN-CASE

           MOVE '2518' TO WS-EXP-MSG
           MOVE 'Bad Pic String' TO WS-EXP-TEXT

           MOVE 'unsupported mask XXXX-XX-XX' TO WS-CASE
           MOVE '2024-02-29' TO WS-DATE  MOVE 'XXXX-XX-XX' TO WS-FMT
           PERFORM RUN-CASE

           MOVE 'mask without day token' TO WS-CASE
           MOVE '2024-02' TO WS-DATE  MOVE 'YYYY-MM' TO WS-FMT
           PERFORM RUN-CASE

      *    ---- CEEDAYS test double: Lilian numbers -------------------
           PERFORM CHECK-LILIAN

           DISPLAY 'CSUTLDTC: ' WS-PASSES ' passed, '
                   WS-FAILS ' failed'
           MOVE WS-FAILS TO RETURN-CODE
           STOP RUN.

       RUN-CASE.
      *    Note: CSUTLDTC echoes the whole VSTRING (2-byte binary length
      *    + text) into the 10-byte TstDate field, so only the first 8
      *    date characters appear at offset 48 of LS-RESULT.
           MOVE SPACES TO WS-RESULT
           CALL 'CSUTLDTC' USING WS-DATE WS-FMT WS-RESULT
           MOVE RETURN-CODE TO WS-RC
           MOVE 0 TO RETURN-CODE
           MOVE WS-RC TO WS-RC-DISP
           IF WS-RC-DISP = WS-EXP-RC
              AND WS-RESULT(1:4) = WS-EXP-RC
              AND WS-RESULT(16:4) = WS-EXP-MSG
              AND WS-RESULT(21:15) = WS-EXP-TEXT
              AND WS-RESULT(48:8) = WS-DATE(1:8)
              AND WS-RESULT(67:10) = WS-FMT
              ADD 1 TO WS-PASSES
              DISPLAY 'PASS  ' WS-CASE
           ELSE
              ADD 1 TO WS-FAILS
              DISPLAY 'FAIL  ' WS-CASE
              DISPLAY '      expected RC=' WS-EXP-RC ' msg=' WS-EXP-MSG
                      ' text=' WS-EXP-TEXT
              DISPLAY '      actual   RC=' WS-RC-DISP
                      ' result=' WS-RESULT
           END-IF.

       CHECK-LILIAN.
      *    1582-10-15 is Lilian day 1 by definition.
           MOVE '1582-10-15' TO WS-VD-TXT
           MOVE 1 TO WS-EXP-LILIAN
           PERFORM LILIAN-CASE
      *    Cross-check against GnuCOBOL's INTEGER-OF-DATE
      *    (1601-01-01 = 1 = Lilian 6654).
           MOVE '2024-02-29' TO WS-VD-TXT
           MOVE 20240229 TO WS-INT-DATE
           COMPUTE WS-EXP-LILIAN =
                   FUNCTION INTEGER-OF-DATE(WS-INT-DATE) + 6653
           PERFORM LILIAN-CASE
           MOVE '9999-12-31' TO WS-VD-TXT
           MOVE 99991231 TO WS-INT-DATE
           COMPUTE WS-EXP-LILIAN =
                   FUNCTION INTEGER-OF-DATE(WS-INT-DATE) + 6653
           PERFORM LILIAN-CASE.

       LILIAN-CASE.
           MOVE 0 TO WS-LILIAN
           CALL 'CEEDAYS' USING WS-VSTR-DATE WS-VSTR-FMT
                                WS-LILIAN WS-FC
           IF WS-LILIAN = WS-EXP-LILIAN AND WS-FC = LOW-VALUES
              ADD 1 TO WS-PASSES
              DISPLAY 'PASS  CEEDAYS Lilian day of ' WS-VD-TXT
           ELSE
              ADD 1 TO WS-FAILS
              DISPLAY 'FAIL  CEEDAYS Lilian day of ' WS-VD-TXT
                      ' expected ' WS-EXP-LILIAN ' got ' WS-LILIAN
           END-IF.
