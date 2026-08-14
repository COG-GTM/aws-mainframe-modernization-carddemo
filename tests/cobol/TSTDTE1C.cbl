       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TSTDTE1C.
       AUTHOR.        AWS.
      ******************************************************************
      * Program     : TSTDTE1C.cbl
      * Application : CardDemo test suite
      * Type        : BATCH COBOL unit test
      * Function    : Unit tests for the reusable date edit logic in
      *               copybooks CSUTLDWY (working storage) and
      *               CSUTLDPY (procedure division), the same way the
      *               online programs (for example COACTUPC) use them.
      *               Exercises EDIT-DATE-CCYYMMDD and
      *               EDIT-DATE-OF-BIRTH and asserts on the edit
      *               flags and the returned error messages.
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
      * Working storage expected by CSUTLDPY, mirroring the
      * declarations used by COACTUPC.
      ******************************************************************
       01  WS-MISC-STORAGE.
         05 WS-EDIT-VARIABLE-NAME                  PIC X(25).
         05 WS-CALCULATION-VARS.
          10 WS-DIV-BY                             PIC S9(4) COMP-3
                                                   VALUE 4.
          10 WS-DIVIDEND                           PIC S9(4) COMP-3
                                                   VALUE 0.
          10 WS-REMAINDER                          PIC S9(4) COMP-3
                                                   VALUE 0.
         05 WS-DATE-EDIT-VARS.
           COPY CSUTLDWY.
         05  WS-INPUT-FLAG                         PIC X(1).
           88  INPUT-OK                            VALUE '0'.
           88  INPUT-ERROR                         VALUE '1'.
           88  INPUT-PENDING                       VALUE LOW-VALUES.
         05  WS-RETURN-MSG                         PIC X(75).
           88  WS-RETURN-MSG-OFF                   VALUE SPACES.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'TSTDTE1C: unit tests for CSUTLDPY date edits'

           PERFORM 1000-TEST-VALID-DATE
              THRU 1000-EXIT
           PERFORM 1100-TEST-LEAP-DAY-VALID
              THRU 1100-EXIT
           PERFORM 1200-TEST-LEAP-DAY-INVALID
              THRU 1200-EXIT
           PERFORM 1300-TEST-BAD-MONTH
              THRU 1300-EXIT
           PERFORM 1400-TEST-DAY-31-IN-JUNE
              THRU 1400-EXIT
           PERFORM 1500-TEST-BLANK-YEAR
              THRU 1500-EXIT
           PERFORM 1600-TEST-BAD-CENTURY
              THRU 1600-EXIT
           PERFORM 1700-TEST-DOB-IN-FUTURE
              THRU 1700-EXIT
           PERFORM 1800-TEST-DOB-IN-PAST
              THRU 1800-EXIT

           PERFORM 8900-TEST-SUMMARY
              THRU 8900-TEST-SUMMARY-EXIT
           GOBACK
           .

       1000-TEST-VALID-DATE.
           MOVE 'EDIT-DATE-CCYYMMDD accepts 20220719'
                                         TO WS-TEST-NAME
           MOVE '20220719'               TO WS-EDIT-DATE-CCYYMMDD
           PERFORM 7000-RESET-AND-EDIT
              THRU 7000-EXIT
           IF WS-EDIT-DATE-IS-VALID
              AND NOT INPUT-ERROR
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

       1100-TEST-LEAP-DAY-VALID.
           MOVE 'EDIT-DATE-CCYYMMDD accepts 29 Feb 2020 (leap year)'
                                         TO WS-TEST-NAME
           MOVE '20200229'               TO WS-EDIT-DATE-CCYYMMDD
           PERFORM 7000-RESET-AND-EDIT
              THRU 7000-EXIT
           IF WS-EDIT-DATE-IS-VALID
              AND NOT INPUT-ERROR
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

       1200-TEST-LEAP-DAY-INVALID.
           MOVE 'EDIT-DATE-CCYYMMDD rejects 29 Feb 2021 (not leap)'
                                         TO WS-TEST-NAME
           MOVE '20210229'               TO WS-EDIT-DATE-CCYYMMDD
           PERFORM 7000-RESET-AND-EDIT
              THRU 7000-EXIT
           IF INPUT-ERROR
              AND WS-RETURN-MSG NOT = SPACES
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

       1300-TEST-BAD-MONTH.
           MOVE 'EDIT-DATE-CCYYMMDD rejects month 13'
                                         TO WS-TEST-NAME
           MOVE '20221315'               TO WS-EDIT-DATE-CCYYMMDD
           PERFORM 7000-RESET-AND-EDIT
              THRU 7000-EXIT
           IF INPUT-ERROR
              AND FLG-MONTH-NOT-OK
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

       1400-TEST-DAY-31-IN-JUNE.
           MOVE 'EDIT-DATE-CCYYMMDD rejects 31 June'
                                         TO WS-TEST-NAME
           MOVE '20220631'               TO WS-EDIT-DATE-CCYYMMDD
           PERFORM 7000-RESET-AND-EDIT
              THRU 7000-EXIT
           IF INPUT-ERROR
              AND FLG-DAY-NOT-OK
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

       1500-TEST-BLANK-YEAR.
           MOVE 'EDIT-DATE-CCYYMMDD flags blank year'
                                         TO WS-TEST-NAME
           MOVE SPACES                   TO WS-EDIT-DATE-CCYYMMDD
           PERFORM 7000-RESET-AND-EDIT
              THRU 7000-EXIT
           IF INPUT-ERROR
              AND FLG-YEAR-BLANK
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

       1600-TEST-BAD-CENTURY.
           MOVE 'EDIT-DATE-CCYYMMDD rejects century 18'
                                         TO WS-TEST-NAME
           MOVE '18991231'               TO WS-EDIT-DATE-CCYYMMDD
           PERFORM 7000-RESET-AND-EDIT
              THRU 7000-EXIT
           IF INPUT-ERROR
              AND FLG-YEAR-NOT-OK
              PERFORM 8000-TEST-PASSED
                 THRU 8000-TEST-PASSED-EXIT
           ELSE
              PERFORM 8100-TEST-FAILED
                 THRU 8100-TEST-FAILED-EXIT
           END-IF
           .
       1600-EXIT.
           EXIT
           .

       1700-TEST-DOB-IN-FUTURE.
           MOVE 'EDIT-DATE-OF-BIRTH rejects a future date'
                                         TO WS-TEST-NAME
           MOVE '20991231'               TO WS-EDIT-DATE-CCYYMMDD
           MOVE 'Date of Birth'          TO WS-EDIT-VARIABLE-NAME
           SET INPUT-OK                  TO TRUE
           SET WS-RETURN-MSG-OFF         TO TRUE
           PERFORM EDIT-DATE-OF-BIRTH
              THRU EDIT-DATE-OF-BIRTH-EXIT
           IF INPUT-ERROR
              PERFORM 8000-TEST-PASSED
                 THRU 8000-TEST-PASSED-EXIT
           ELSE
              PERFORM 8100-TEST-FAILED
                 THRU 8100-TEST-FAILED-EXIT
           END-IF
           .
       1700-EXIT.
           EXIT
           .

       1800-TEST-DOB-IN-PAST.
           MOVE 'EDIT-DATE-OF-BIRTH accepts a past date'
                                         TO WS-TEST-NAME
           MOVE '19851126'               TO WS-EDIT-DATE-CCYYMMDD
           MOVE 'Date of Birth'          TO WS-EDIT-VARIABLE-NAME
           SET INPUT-OK                  TO TRUE
           SET WS-RETURN-MSG-OFF         TO TRUE
           PERFORM EDIT-DATE-OF-BIRTH
              THRU EDIT-DATE-OF-BIRTH-EXIT
           IF NOT INPUT-ERROR
              PERFORM 8000-TEST-PASSED
                 THRU 8000-TEST-PASSED-EXIT
           ELSE
              PERFORM 8100-TEST-FAILED
                 THRU 8100-TEST-FAILED-EXIT
           END-IF
           .
       1800-EXIT.
           EXIT
           .

       7000-RESET-AND-EDIT.
           MOVE 'Test Date'              TO WS-EDIT-VARIABLE-NAME
           SET INPUT-OK                  TO TRUE
           SET WS-RETURN-MSG-OFF         TO TRUE
           MOVE LOW-VALUES               TO WS-EDIT-DATE-FLGS
           PERFORM EDIT-DATE-CCYYMMDD
              THRU EDIT-DATE-CCYYMMDD-EXIT
           MOVE 0                        TO RETURN-CODE
           .
       7000-EXIT.
           EXIT
           .

           COPY TSTASPY.

      ******************************************************************
      * Reusable date edit paragraphs shared with the application
      * programs (system under test).
      ******************************************************************
           COPY CSUTLDPY.
