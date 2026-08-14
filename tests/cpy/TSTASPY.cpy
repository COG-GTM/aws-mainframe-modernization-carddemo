      ******************************************************************
      *Procedure Division Copybook for CardDemo unit test harness
      *Accompanying Working Storage copybook is TSTASWY
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
      * ***  PERFORM 8000-TEST-PASSED or 8100-TEST-FAILED
      *      after evaluating each assertion.
      *      PERFORM 8900-TEST-SUMMARY once at the end of the run.
      *      The number of failed assertions is placed in RETURN-CODE
      *      so batch runners (JCL or shell) can detect failures.
      ******************************************************************
       8000-TEST-PASSED.
           ADD 1                         TO WS-TESTS-RUN
           ADD 1                         TO WS-TESTS-PASSED
           DISPLAY 'PASS: ' WS-TEST-NAME
           .
       8000-TEST-PASSED-EXIT.
           EXIT
           .
       8100-TEST-FAILED.
           ADD 1                         TO WS-TESTS-RUN
           ADD 1                         TO WS-TESTS-FAILED
           DISPLAY 'FAIL: ' WS-TEST-NAME
           .
       8100-TEST-FAILED-EXIT.
           EXIT
           .
       8900-TEST-SUMMARY.
           DISPLAY '----------------------------------------'
           DISPLAY 'TESTS RUN....: ' WS-TESTS-RUN
           DISPLAY 'TESTS PASSED.: ' WS-TESTS-PASSED
           DISPLAY 'TESTS FAILED.: ' WS-TESTS-FAILED
           DISPLAY '----------------------------------------'
           MOVE WS-TESTS-FAILED          TO RETURN-CODE
           .
       8900-TEST-SUMMARY-EXIT.
           EXIT
           .
