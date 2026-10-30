      ******************************************************************
      * Program     : CSUTLPWC.CBL
      * Application : CardDemo
      * Type        : Called subprogram (batch and CICS)
      * Function    : Check a user password against the CardDemo
      *               password policy. Used by the USRSEC bootstrap
      *               load (CBUSR01C) and the sign-on password change
      *               (COSGN00C). Passwords are compared upper case,
      *               the same way COSGN00C compares them at sign-on.
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
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CSUTLPWC.
       AUTHOR.     AWS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-PWC-VARIABLES.
         05 WS-PWD                     PIC X(08).
         05 WS-USER-ID                 PIC X(08).
         05 WS-IDX                     PIC S9(04) COMP.
         05 WS-ALPHA-CNT               PIC S9(04) COMP.
         05 WS-DIGIT-CNT               PIC S9(04) COMP.
         05 WS-BLANK-CNT               PIC S9(04) COMP.

      * Published or trivially guessable passwords that are never
      * accepted. PASSWORD is the value the original DUSRSECJ seed
      * data and README shipped for every account.
       01 WS-WEAK-PASSWORD-LIST.
         05 FILLER                     PIC X(08) VALUE 'PASSWORD'.
         05 FILLER                     PIC X(08) VALUE 'PASSW0RD'.
         05 FILLER                     PIC X(08) VALUE 'PASSWD01'.
         05 FILLER                     PIC X(08) VALUE 'CARDDEMO'.
         05 FILLER                     PIC X(08) VALUE 'CARDDEM0'.
         05 FILLER                     PIC X(08) VALUE '12345678'.
         05 FILLER                     PIC X(08) VALUE '87654321'.
         05 FILLER                     PIC X(08) VALUE 'ABCD1234'.
         05 FILLER                     PIC X(08) VALUE 'QWERTY12'.
         05 FILLER                     PIC X(08) VALUE 'LETMEIN1'.
         05 FILLER                     PIC X(08) VALUE 'WELCOME1'.
         05 FILLER                     PIC X(08) VALUE 'CHANGEME'.
       01 WS-WEAK-PASSWORD-TBL REDEFINES WS-WEAK-PASSWORD-LIST.
         05 WS-WEAK-PASSWORD           PIC X(08) OCCURS 12 TIMES
                                       INDEXED BY WS-WEAK-IDX.

       LINKAGE SECTION.
       COPY CSUTLPWY.

       PROCEDURE DIVISION USING CSUTLPWC-PARM.
       MAIN-PARA.

           MOVE '00'   TO CSUTLPWC-RESULT
           MOVE SPACES TO CSUTLPWC-MESSAGE
           MOVE FUNCTION UPPER-CASE(CSUTLPWC-PASSWORD) TO WS-PWD
           MOVE FUNCTION UPPER-CASE(CSUTLPWC-USER-ID)  TO WS-USER-ID

           MOVE ZERO TO WS-ALPHA-CNT WS-DIGIT-CNT WS-BLANK-CNT
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 8
               EVALUATE TRUE
                   WHEN WS-PWD(WS-IDX:1) = SPACE OR LOW-VALUE
                       ADD 1 TO WS-BLANK-CNT
                   WHEN WS-PWD(WS-IDX:1) IS NUMERIC
                       ADD 1 TO WS-DIGIT-CNT
                   WHEN WS-PWD(WS-IDX:1) IS ALPHABETIC-UPPER
                       ADD 1 TO WS-ALPHA-CNT
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM

           SET WS-WEAK-IDX TO 1
           SEARCH WS-WEAK-PASSWORD
               AT END
                   CONTINUE
               WHEN WS-WEAK-PASSWORD(WS-WEAK-IDX) = WS-PWD
                   MOVE '03' TO CSUTLPWC-RESULT
           END-SEARCH

           EVALUATE TRUE
               WHEN WS-BLANK-CNT > 0
                   MOVE '01' TO CSUTLPWC-RESULT
                   MOVE 'Password must be 8 non-blank characters'
                     TO CSUTLPWC-MESSAGE
               WHEN WS-PWD = WS-USER-ID
                   MOVE '02' TO CSUTLPWC-RESULT
                   MOVE 'Password must not be the same as the User ID'
                     TO CSUTLPWC-MESSAGE
               WHEN CSUTLPWC-RESULT = '03'
                   MOVE 'Password is a published or common default'
                     TO CSUTLPWC-MESSAGE
               WHEN WS-ALPHA-CNT = 0 OR WS-DIGIT-CNT = 0
                   MOVE '04' TO CSUTLPWC-RESULT
                   MOVE 'Password must contain letters and digits'
                     TO CSUTLPWC-MESSAGE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           GOBACK.
