       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CEEDAYS.
       AUTHOR.        AWS.
      ******************************************************************
      * Program     : CEEDAYS.cbl
      * Application : CardDemo test suite
      * Type        : Test stub (off-mainframe runs only)
      * Function    : Stand-in for the z/OS Language Environment
      *               CEEDAYS callable service so that CSUTLDTC and
      *               the date edit routines can be unit tested with
      *               GnuCOBOL. Validates a date value against a
      *               picture mask and returns a Lilian day number
      *               plus a feedback code whose token values match
      *               the conditions tested in CSUTLDTC.
      *               NEVER deploy this program to z/OS: there the
      *               real LE service is used instead.
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
       01  WS-INDEXES.
           05 WS-I                 PIC S9(4) COMP VALUE 0.
           05 WS-Y-CNT             PIC S9(4) COMP VALUE 0.
           05 WS-M-CNT             PIC S9(4) COMP VALUE 0.
           05 WS-D-CNT             PIC S9(4) COMP VALUE 0.

       01  WS-PARSED-DATE.
           05 WS-YEAR-X            PIC X(4)  VALUE SPACES.
           05 WS-YEAR-N REDEFINES WS-YEAR-X
                                   PIC 9(4).
           05 WS-MONTH-X           PIC X(2)  VALUE SPACES.
           05 WS-MONTH-N REDEFINES WS-MONTH-X
                                   PIC 9(2).
           05 WS-DAY-X             PIC X(2)  VALUE SPACES.
           05 WS-DAY-N REDEFINES WS-DAY-X
                                   PIC 9(2).

       01  WS-WORK.
           05 WS-MASK-CHAR         PIC X.
           05 WS-DATE-CHAR         PIC X.
           05 WS-MAX-DAY           PIC 9(2)  VALUE 0.
           05 WS-DATE-INT          PIC 9(8)  VALUE 0.
           05 WS-REMAINDER         PIC 9(4)  VALUE 0.
           05 WS-LEAP-DIV          PIC 9(4)  VALUE 4.
           05 WS-QUOTIENT          PIC 9(4)  VALUE 0.

      ******************************************************************
      * Feedback code tokens. These byte values match the level 88
      * conditions declared in CSUTLDTC (FEEDBACK-CODE).
      ******************************************************************
       01  WS-FEEDBACK-TOKENS.
           05 FB-VALID             PIC X(8)
              VALUE X'0000000000000000'.
           05 FB-INSUFFICIENT      PIC X(8)
              VALUE X'000309CB59C3C5C5'.
           05 FB-BAD-DATE-VALUE    PIC X(8)
              VALUE X'000309CC59C3C5C5'.
           05 FB-INVALID-MONTH     PIC X(8)
              VALUE X'000309D559C3C5C5'.
           05 FB-NON-NUMERIC       PIC X(8)
              VALUE X'000309D859C3C5C5'.

      ******************************************************************
      * Lilian day 1 = 15 October 1582.
      * FUNCTION INTEGER-OF-DATE day 1 = 1 January 1601.
      * 1 January 1601 is Lilian day 6654, hence the 6653 offset.
      ******************************************************************
       01  WS-LILIAN-OFFSET        PIC S9(9) COMP VALUE 6653.

       LINKAGE SECTION.
       01  LK-INPUT-CHAR-DATE.
           05 LK-DATE-LENGTH       PIC S9(4) BINARY.
           05 LK-DATE-TEXT         PIC X(256).
       01  LK-PICTURE-STRING.
           05 LK-MASK-LENGTH       PIC S9(4) BINARY.
           05 LK-MASK-TEXT         PIC X(256).
       01  LK-OUTPUT-LILIAN        PIC S9(9) BINARY.
       01  LK-FEEDBACK-CODE.
           05 LK-FC-TOKEN          PIC X(8).
           05 LK-FC-I-S-INFO       PIC S9(9) BINARY.

       PROCEDURE DIVISION USING LK-INPUT-CHAR-DATE
                                LK-PICTURE-STRING
                                LK-OUTPUT-LILIAN
                                LK-FEEDBACK-CODE.
       0000-MAIN.
           MOVE 0                  TO LK-OUTPUT-LILIAN
                                      LK-FC-I-S-INFO
           MOVE SPACES             TO WS-YEAR-X
                                      WS-MONTH-X
                                      WS-DAY-X
           MOVE 0                  TO WS-Y-CNT
                                      WS-M-CNT
                                      WS-D-CNT

           PERFORM 1000-PARSE-BY-MASK
              THRU 1000-PARSE-BY-MASK-EXIT

           PERFORM 2000-VALIDATE
              THRU 2000-VALIDATE-EXIT

           GOBACK
           .

      ******************************************************************
      * Walk the picture mask and pick year, month and day digits out
      * of the date value. Characters other than Y, M and D in the
      * mask (separators) are skipped.
      ******************************************************************
       1000-PARSE-BY-MASK.
           PERFORM VARYING WS-I FROM 1 BY 1
              UNTIL WS-I > LK-MASK-LENGTH
              MOVE LK-MASK-TEXT (WS-I:1)   TO WS-MASK-CHAR
              IF WS-I > LK-DATE-LENGTH
                 MOVE SPACE                TO WS-DATE-CHAR
              ELSE
                 MOVE LK-DATE-TEXT (WS-I:1)
                                           TO WS-DATE-CHAR
              END-IF
              EVALUATE WS-MASK-CHAR
                 WHEN 'Y'
                    IF WS-Y-CNT < 4
                       ADD 1               TO WS-Y-CNT
                       MOVE WS-DATE-CHAR
                         TO WS-YEAR-X (WS-Y-CNT:1)
                    END-IF
                 WHEN 'M'
                    IF WS-M-CNT < 2
                       ADD 1               TO WS-M-CNT
                       MOVE WS-DATE-CHAR
                         TO WS-MONTH-X (WS-M-CNT:1)
                    END-IF
                 WHEN 'D'
                    IF WS-D-CNT < 2
                       ADD 1               TO WS-D-CNT
                       MOVE WS-DATE-CHAR
                         TO WS-DAY-X (WS-D-CNT:1)
                    END-IF
                 WHEN OTHER
                    CONTINUE
              END-EVALUATE
           END-PERFORM
           .
       1000-PARSE-BY-MASK-EXIT.
           EXIT
           .

      ******************************************************************
      * Validate the parsed components and set the feedback token.
      ******************************************************************
       2000-VALIDATE.
           IF WS-YEAR-X  = SPACES
           OR WS-MONTH-X = SPACES
           OR WS-DAY-X   = SPACES
              MOVE FB-INSUFFICIENT         TO LK-FC-TOKEN
              GO TO 2000-VALIDATE-EXIT
           END-IF

           IF WS-YEAR-X  IS NOT NUMERIC
           OR WS-MONTH-X IS NOT NUMERIC
           OR WS-DAY-X   IS NOT NUMERIC
              MOVE FB-NON-NUMERIC          TO LK-FC-TOKEN
              GO TO 2000-VALIDATE-EXIT
           END-IF

           IF WS-MONTH-N < 1 OR WS-MONTH-N > 12
              MOVE FB-INVALID-MONTH        TO LK-FC-TOKEN
              GO TO 2000-VALIDATE-EXIT
           END-IF

           IF WS-YEAR-N < 1601 OR WS-YEAR-N > 9999
              MOVE FB-BAD-DATE-VALUE       TO LK-FC-TOKEN
              GO TO 2000-VALIDATE-EXIT
           END-IF

           PERFORM 2100-SET-MAX-DAY
              THRU 2100-SET-MAX-DAY-EXIT

           IF WS-DAY-N < 1 OR WS-DAY-N > WS-MAX-DAY
              MOVE FB-BAD-DATE-VALUE       TO LK-FC-TOKEN
              GO TO 2000-VALIDATE-EXIT
           END-IF

           MOVE FB-VALID                   TO LK-FC-TOKEN
           COMPUTE WS-DATE-INT =
               WS-YEAR-N * 10000 + WS-MONTH-N * 100 + WS-DAY-N
           COMPUTE LK-OUTPUT-LILIAN =
               FUNCTION INTEGER-OF-DATE (WS-DATE-INT)
               + WS-LILIAN-OFFSET
           .
       2000-VALIDATE-EXIT.
           EXIT
           .

       2100-SET-MAX-DAY.
           EVALUATE WS-MONTH-N
              WHEN 1
              WHEN 3
              WHEN 5
              WHEN 7
              WHEN 8
              WHEN 10
              WHEN 12
                 MOVE 31                   TO WS-MAX-DAY
              WHEN 4
              WHEN 6
              WHEN 9
              WHEN 11
                 MOVE 30                   TO WS-MAX-DAY
              WHEN 2
                 IF FUNCTION MOD (WS-YEAR-N, 400) = 0
                    MOVE 29                TO WS-MAX-DAY
                 ELSE
                    IF FUNCTION MOD (WS-YEAR-N, 100) = 0
                       MOVE 28             TO WS-MAX-DAY
                    ELSE
                       IF FUNCTION MOD (WS-YEAR-N, 4) = 0
                          MOVE 29          TO WS-MAX-DAY
                       ELSE
                          MOVE 28          TO WS-MAX-DAY
                       END-IF
                    END-IF
                 END-IF
           END-EVALUATE
           .
       2100-SET-MAX-DAY-EXIT.
           EXIT
           .
