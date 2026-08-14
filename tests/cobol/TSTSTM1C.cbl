       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TSTSTM1C.
       AUTHOR.        AWS.
      ******************************************************************
      * Program     : TSTSTM1C.cbl
      * Application : CardDemo test suite
      * Type        : BATCH COBOL unit test
      * Function    : Unit tests for CBSTM03B (statement file access
      *               subroutine). Uses the indexed test files
      *               created by the STMFIXTC fixture and asserts on
      *               the returned file status codes and record data
      *               for open, sequential read, keyed read and close
      *               operations.
      ******************************************************************
      * Copyright Amazon.com, Inc. or its affiliates.
      * All Rights Reserved.
      *
      * Licensed under the Apache License, Version 2.0 (the "License").
      * You may not use this file except in compliance with the License.
      * You may obtain a copy of the License at
      *
      *    http://www.apache.org/licenses/LICENSE-2.0
      *
      * Unless required by applicable law or agreed to in writing,
      * software distributed under the License is distributed on an
      * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
      * either express or implied. See the License for the specific
      * language governing permissions and limitations under the License
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TEST-HARNESS.
           COPY TSTASWY.

      ******************************************************************
      * Communication area, mirroring LK-M03B-AREA in CBSTM03B.
      ******************************************************************
       01  WS-M03B-AREA.
           05  WS-M03B-DD          PIC X(08).
           05  WS-M03B-OPER        PIC X(01).
           05  WS-M03B-RC          PIC X(02).
           05  WS-M03B-KEY         PIC X(25).
           05  WS-M03B-KEY-LN      PIC S9(4).
           05  WS-M03B-FLDT        PIC X(1000).

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'TSTSTM1C: unit tests for CBSTM03B'

           PERFORM 1000-TEST-TRNX-SEQUENTIAL
              THRU 1000-EXIT
           PERFORM 2000-TEST-XREF-SEQUENTIAL
              THRU 2000-EXIT
           PERFORM 3000-TEST-CUST-KEYED
              THRU 3000-EXIT
           PERFORM 4000-TEST-ACCT-KEYED
              THRU 4000-EXIT

           PERFORM 8900-TEST-SUMMARY
              THRU 8900-TEST-SUMMARY-EXIT
           GOBACK
           .

      ******************************************************************
      * TRNXFILE: open, read both records sequentially, hit end of
      * file, close.
      ******************************************************************
       1000-TEST-TRNX-SEQUENTIAL.
           MOVE 'CBSTM03B opens TRNXFILE'
                                         TO WS-TEST-NAME
           MOVE 'TRNXFILE'               TO WS-M03B-DD
           MOVE 'O'                      TO WS-M03B-OPER
           PERFORM 7000-CALL-CBSTM03B
              THRU 7000-EXIT
           PERFORM 7100-ASSERT-RC-00
              THRU 7100-EXIT

           MOVE 'CBSTM03B reads first TRNXFILE record'
                                         TO WS-TEST-NAME
           MOVE 'R'                      TO WS-M03B-OPER
           PERFORM 7000-CALL-CBSTM03B
              THRU 7000-EXIT
           IF WS-M03B-RC = '00'
              AND WS-M03B-FLDT (1:16) = '4111111111111111'
              AND WS-M03B-FLDT (17:16) = '0000000000000001'
              PERFORM 8000-TEST-PASSED
                 THRU 8000-TEST-PASSED-EXIT
           ELSE
              PERFORM 8100-TEST-FAILED
                 THRU 8100-TEST-FAILED-EXIT
           END-IF

           MOVE 'CBSTM03B reads second TRNXFILE record'
                                         TO WS-TEST-NAME
           MOVE 'R'                      TO WS-M03B-OPER
           PERFORM 7000-CALL-CBSTM03B
              THRU 7000-EXIT
           IF WS-M03B-RC = '00'
              AND WS-M03B-FLDT (17:16) = '0000000000000002'
              PERFORM 8000-TEST-PASSED
                 THRU 8000-TEST-PASSED-EXIT
           ELSE
              PERFORM 8100-TEST-FAILED
                 THRU 8100-TEST-FAILED-EXIT
           END-IF

           MOVE 'CBSTM03B returns status 10 at TRNXFILE end of file'
                                         TO WS-TEST-NAME
           MOVE 'R'                      TO WS-M03B-OPER
           PERFORM 7000-CALL-CBSTM03B
              THRU 7000-EXIT
           IF WS-M03B-RC = '10'
              PERFORM 8000-TEST-PASSED
                 THRU 8000-TEST-PASSED-EXIT
           ELSE
              PERFORM 8100-TEST-FAILED
                 THRU 8100-TEST-FAILED-EXIT
           END-IF

           MOVE 'CBSTM03B closes TRNXFILE'
                                         TO WS-TEST-NAME
           MOVE 'C'                      TO WS-M03B-OPER
           PERFORM 7000-CALL-CBSTM03B
              THRU 7000-EXIT
           PERFORM 7100-ASSERT-RC-00
              THRU 7100-EXIT
           .
       1000-EXIT.
           EXIT
           .

      ******************************************************************
      * XREFFILE: open, read the single record, close.
      ******************************************************************
       2000-TEST-XREF-SEQUENTIAL.
           MOVE 'CBSTM03B opens XREFFILE'
                                         TO WS-TEST-NAME
           MOVE 'XREFFILE'               TO WS-M03B-DD
           MOVE 'O'                      TO WS-M03B-OPER
           PERFORM 7000-CALL-CBSTM03B
              THRU 7000-EXIT
           PERFORM 7100-ASSERT-RC-00
              THRU 7100-EXIT

           MOVE 'CBSTM03B reads XREFFILE record by card number'
                                         TO WS-TEST-NAME
           MOVE 'R'                      TO WS-M03B-OPER
           PERFORM 7000-CALL-CBSTM03B
              THRU 7000-EXIT
           IF WS-M03B-RC = '00'
              AND WS-M03B-FLDT (1:16) = '4111111111111111'
              PERFORM 8000-TEST-PASSED
                 THRU 8000-TEST-PASSED-EXIT
           ELSE
              PERFORM 8100-TEST-FAILED
                 THRU 8100-TEST-FAILED-EXIT
           END-IF

           MOVE 'CBSTM03B closes XREFFILE'
                                         TO WS-TEST-NAME
           MOVE 'C'                      TO WS-M03B-OPER
           PERFORM 7000-CALL-CBSTM03B
              THRU 7000-EXIT
           PERFORM 7100-ASSERT-RC-00
              THRU 7100-EXIT
           .
       2000-EXIT.
           EXIT
           .

      ******************************************************************
      * CUSTFILE: open, keyed read found and not found, close.
      ******************************************************************
       3000-TEST-CUST-KEYED.
           MOVE 'CBSTM03B opens CUSTFILE'
                                         TO WS-TEST-NAME
           MOVE 'CUSTFILE'               TO WS-M03B-DD
           MOVE 'O'                      TO WS-M03B-OPER
           PERFORM 7000-CALL-CBSTM03B
              THRU 7000-EXIT
           PERFORM 7100-ASSERT-RC-00
              THRU 7100-EXIT

           MOVE 'CBSTM03B reads CUSTFILE record by key'
                                         TO WS-TEST-NAME
           MOVE 'K'                      TO WS-M03B-OPER
           MOVE '000000001'              TO WS-M03B-KEY
           MOVE 9                        TO WS-M03B-KEY-LN
           PERFORM 7000-CALL-CBSTM03B
              THRU 7000-EXIT
           IF WS-M03B-RC = '00'
              AND WS-M03B-FLDT (10:22) = 'JOHN DOE TEST CUSTOMER'
              PERFORM 8000-TEST-PASSED
                 THRU 8000-TEST-PASSED-EXIT
           ELSE
              PERFORM 8100-TEST-FAILED
                 THRU 8100-TEST-FAILED-EXIT
           END-IF

           MOVE 'CBSTM03B returns status 23 for missing CUSTFILE key'
                                         TO WS-TEST-NAME
           MOVE 'K'                      TO WS-M03B-OPER
           MOVE '999999999'              TO WS-M03B-KEY
           MOVE 9                        TO WS-M03B-KEY-LN
           PERFORM 7000-CALL-CBSTM03B
              THRU 7000-EXIT
           IF WS-M03B-RC = '23'
              PERFORM 8000-TEST-PASSED
                 THRU 8000-TEST-PASSED-EXIT
           ELSE
              PERFORM 8100-TEST-FAILED
                 THRU 8100-TEST-FAILED-EXIT
           END-IF

           MOVE 'CBSTM03B closes CUSTFILE'
                                         TO WS-TEST-NAME
           MOVE 'C'                      TO WS-M03B-OPER
           PERFORM 7000-CALL-CBSTM03B
              THRU 7000-EXIT
           PERFORM 7100-ASSERT-RC-00
              THRU 7100-EXIT
           .
       3000-EXIT.
           EXIT
           .

      ******************************************************************
      * ACCTFILE: open, keyed read, close.
      ******************************************************************
       4000-TEST-ACCT-KEYED.
           MOVE 'CBSTM03B opens ACCTFILE'
                                         TO WS-TEST-NAME
           MOVE 'ACCTFILE'               TO WS-M03B-DD
           MOVE 'O'                      TO WS-M03B-OPER
           PERFORM 7000-CALL-CBSTM03B
              THRU 7000-EXIT
           PERFORM 7100-ASSERT-RC-00
              THRU 7100-EXIT

           MOVE 'CBSTM03B reads ACCTFILE record by key'
                                         TO WS-TEST-NAME
           MOVE 'K'                      TO WS-M03B-OPER
           MOVE '00000000001'            TO WS-M03B-KEY
           MOVE 11                       TO WS-M03B-KEY-LN
           PERFORM 7000-CALL-CBSTM03B
              THRU 7000-EXIT
           IF WS-M03B-RC = '00'
              AND WS-M03B-FLDT (12:19) = 'TEST ACCOUNT RECORD'
              PERFORM 8000-TEST-PASSED
                 THRU 8000-TEST-PASSED-EXIT
           ELSE
              PERFORM 8100-TEST-FAILED
                 THRU 8100-TEST-FAILED-EXIT
           END-IF

           MOVE 'CBSTM03B closes ACCTFILE'
                                         TO WS-TEST-NAME
           MOVE 'C'                      TO WS-M03B-OPER
           PERFORM 7000-CALL-CBSTM03B
              THRU 7000-EXIT
           PERFORM 7100-ASSERT-RC-00
              THRU 7100-EXIT
           .
       4000-EXIT.
           EXIT
           .

       7000-CALL-CBSTM03B.
           MOVE SPACES                   TO WS-M03B-RC
                                            WS-M03B-FLDT
           CALL 'CBSTM03B' USING WS-M03B-AREA
           MOVE 0                        TO RETURN-CODE
           .
       7000-EXIT.
           EXIT
           .

       7100-ASSERT-RC-00.
           IF WS-M03B-RC = '00'
              PERFORM 8000-TEST-PASSED
                 THRU 8000-TEST-PASSED-EXIT
           ELSE
              PERFORM 8100-TEST-FAILED
                 THRU 8100-TEST-FAILED-EXIT
           END-IF
           .
       7100-EXIT.
           EXIT
           .

           COPY TSTASPY.
