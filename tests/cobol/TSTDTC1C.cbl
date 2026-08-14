       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TSTDTC1C.
       AUTHOR.        AWS.
      ******************************************************************
      * Program     : TSTDTC1C.cbl
      * Application : CardDemo test suite
      * Type        : BATCH COBOL unit test
      * Function    : Unit tests for CSUTLDTC (date validation
      *               subroutine). Calls CSUTLDTC with valid and
      *               invalid dates and asserts on the RETURN-CODE
      *               (severity) and on the result text placed in
      *               the output message area.
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

       01  WS-CALL-AREA.
           05 WS-DATE-TO-TEST      PIC X(10).
           05 WS-DATE-FORMAT       PIC X(10).
           05 WS-RESULT            PIC X(80).
      ******************************************************************
      * The result message layout produced by CSUTLDTC:
      * pos 01-04 severity, pos 05-15 'Mesg Code:', pos 16-19 message
      * number, pos 21-35 result text.
      ******************************************************************
           05 WS-RESULT-TEXT REDEFINES WS-RESULT.
              10 WS-RES-SEVERITY   PIC X(4).
              10 FILLER            PIC X(11).
              10 WS-RES-MSG-NO     PIC X(4).
              10 FILLER            PIC X(1).
              10 WS-RES-MESSAGE    PIC X(15).
              10 FILLER            PIC X(45).
           05 WS-CALL-RC           PIC S9(4) VALUE 0.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'TSTDTC1C: unit tests for CSUTLDTC'

           PERFORM 1000-TEST-VALID-CCYYMMDD
              THRU 1000-EXIT
           PERFORM 1100-TEST-VALID-WITH-SEP
              THRU 1100-EXIT
           PERFORM 1200-TEST-INVALID-MONTH
              THRU 1200-EXIT
           PERFORM 1300-TEST-INVALID-DAY
              THRU 1300-EXIT
           PERFORM 1400-TEST-NON-NUMERIC
              THRU 1400-EXIT
           PERFORM 1500-TEST-BLANK-DATE
              THRU 1500-EXIT

           PERFORM 8900-TEST-SUMMARY
              THRU 8900-TEST-SUMMARY-EXIT
           GOBACK
           .

       1000-TEST-VALID-CCYYMMDD.
           MOVE 'CSUTLDTC accepts valid date 20220719 (YYYYMMDD)'
                                         TO WS-TEST-NAME
           MOVE '20220719'               TO WS-DATE-TO-TEST
           MOVE 'YYYYMMDD'               TO WS-DATE-FORMAT
           PERFORM 7000-CALL-CSUTLDTC
              THRU 7000-EXIT
           IF WS-CALL-RC = ZERO
              AND WS-RES-MESSAGE = 'Date is valid'
              PERFORM 8000-TEST-PASSED
                 THRU 8000-TEST-PASSED-EXIT
           ELSE
              PERFORM 8100-TEST-FAILED
                 THRU 8100-TEST-FAILED-EXIT
           END-IF
           .
       1000-EXIT.
           EXIT
           .

       1100-TEST-VALID-WITH-SEP.
           MOVE 'CSUTLDTC accepts valid date 2022-07-19 (YYYY-MM-DD)'
                                         TO WS-TEST-NAME
           MOVE '2022-07-19'             TO WS-DATE-TO-TEST
           MOVE 'YYYY-MM-DD'             TO WS-DATE-FORMAT
           PERFORM 7000-CALL-CSUTLDTC
              THRU 7000-EXIT
           IF WS-CALL-RC = ZERO
              AND WS-RES-MESSAGE = 'Date is valid'
              PERFORM 8000-TEST-PASSED
                 THRU 8000-TEST-PASSED-EXIT
           ELSE
              PERFORM 8100-TEST-FAILED
                 THRU 8100-TEST-FAILED-EXIT
           END-IF
           .
       1100-EXIT.
           EXIT
           .

       1200-TEST-INVALID-MONTH.
           MOVE 'CSUTLDTC rejects month 13 with severity code'
                                         TO WS-TEST-NAME
           MOVE '20221319'               TO WS-DATE-TO-TEST
           MOVE 'YYYYMMDD'               TO WS-DATE-FORMAT
           PERFORM 7000-CALL-CSUTLDTC
              THRU 7000-EXIT
           IF WS-CALL-RC NOT = ZERO
              AND WS-RES-MESSAGE = 'Invalid month'
              PERFORM 8000-TEST-PASSED
                 THRU 8000-TEST-PASSED-EXIT
           ELSE
              PERFORM 8100-TEST-FAILED
                 THRU 8100-TEST-FAILED-EXIT
           END-IF
           .
       1200-EXIT.
           EXIT
           .

       1300-TEST-INVALID-DAY.
           MOVE 'CSUTLDTC rejects 30 February as date value error'
                                         TO WS-TEST-NAME
           MOVE '20220230'               TO WS-DATE-TO-TEST
           MOVE 'YYYYMMDD'               TO WS-DATE-FORMAT
           PERFORM 7000-CALL-CSUTLDTC
              THRU 7000-EXIT
           IF WS-CALL-RC NOT = ZERO
              AND WS-RES-MESSAGE = 'Datevalue error'
              PERFORM 8000-TEST-PASSED
                 THRU 8000-TEST-PASSED-EXIT
           ELSE
              PERFORM 8100-TEST-FAILED
                 THRU 8100-TEST-FAILED-EXIT
           END-IF
           .
       1300-EXIT.
           EXIT
           .

       1400-TEST-NON-NUMERIC.
           MOVE 'CSUTLDTC rejects non numeric date text'
                                         TO WS-TEST-NAME
           MOVE 'ABCDEFGH'               TO WS-DATE-TO-TEST
           MOVE 'YYYYMMDD'               TO WS-DATE-FORMAT
           PERFORM 7000-CALL-CSUTLDTC
              THRU 7000-EXIT
           IF WS-CALL-RC NOT = ZERO
              AND WS-RES-MESSAGE = 'Nonnumeric data'
              PERFORM 8000-TEST-PASSED
                 THRU 8000-TEST-PASSED-EXIT
           ELSE
              PERFORM 8100-TEST-FAILED
                 THRU 8100-TEST-FAILED-EXIT
           END-IF
           .
       1400-EXIT.
           EXIT
           .

       1500-TEST-BLANK-DATE.
           MOVE 'CSUTLDTC rejects blank date as insufficient'
                                         TO WS-TEST-NAME
           MOVE SPACES                   TO WS-DATE-TO-TEST
           MOVE 'YYYYMMDD'               TO WS-DATE-FORMAT
           PERFORM 7000-CALL-CSUTLDTC
              THRU 7000-EXIT
           IF WS-CALL-RC NOT = ZERO
              AND WS-RES-MESSAGE = 'Insufficient'
              PERFORM 8000-TEST-PASSED
                 THRU 8000-TEST-PASSED-EXIT
           ELSE
              PERFORM 8100-TEST-FAILED
                 THRU 8100-TEST-FAILED-EXIT
           END-IF
           .
       1500-EXIT.
           EXIT
           .

       7000-CALL-CSUTLDTC.
           MOVE SPACES                   TO WS-RESULT
           MOVE 0                        TO RETURN-CODE
           CALL 'CSUTLDTC' USING WS-DATE-TO-TEST
                                 WS-DATE-FORMAT
                                 WS-RESULT
           MOVE RETURN-CODE              TO WS-CALL-RC
           MOVE 0                        TO RETURN-CODE
           .
       7000-EXIT.
           EXIT
           .

           COPY TSTASPY.
