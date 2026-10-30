      ******************************************************************
      * Program     : CBUSR01C.CBL
      * Application : CardDemo
      * Type        : BATCH COBOL Program
      * Function    : Validate the operator-supplied user security
      *               bootstrap file and build the USRSEC load file.
      *               Every record must carry a password that passes
      *               the CardDemo password policy (CSUTLPWC), at
      *               least one administrator is required, and every
      *               loaded account is flagged to change its password
      *               at first sign-on. Nothing is written and RC=12
      *               is returned if any record is rejected.
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
       PROGRAM-ID. CBUSR01C.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BOOTIN-FILE ASSIGN TO BOOTIN
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-BOOTIN-STATUS.
           SELECT USROUT-FILE ASSIGN TO USROUT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-USROUT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  BOOTIN-FILE
           RECORDING MODE IS F.
       01  BOOTIN-REC                  PIC X(80).

       FD  USROUT-FILE
           RECORDING MODE IS F.
       01  USROUT-REC                  PIC X(80).

       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-BOOTIN-STATUS           PIC X(02) VALUE SPACES.
         05 WS-USROUT-STATUS           PIC X(02) VALUE SPACES.
         05 WS-EOF-FLG                 PIC X(01) VALUE 'N'.
           88 BOOTIN-EOF                         VALUE 'Y'.
         05 WS-REC-CNT                 PIC 9(04) VALUE ZERO.
         05 WS-ADMIN-CNT               PIC 9(04) VALUE ZERO.
         05 WS-REJECT-CNT              PIC 9(04) VALUE ZERO.
         05 WS-IDX                     PIC 9(04) VALUE ZERO.
         05 WS-PREV-USER-ID            PIC X(08) VALUE LOW-VALUES.
         05 WS-REJECT-MSG              PIC X(60) VALUE SPACES.
         05 WS-MAX-USERS               PIC 9(04) VALUE 500.

       01 WS-USER-TABLE.
         05 WS-USER-ENTRY              PIC X(80) OCCURS 500 TIMES.

       COPY CSUSR01Y.

       COPY CSUTLPWY.

       PROCEDURE DIVISION.
       MAIN-PARA.

           DISPLAY 'START OF EXECUTION OF PROGRAM CBUSR01C'

           OPEN INPUT BOOTIN-FILE
           IF WS-BOOTIN-STATUS NOT = '00'
               DISPLAY 'CBUSR01C: UNABLE TO OPEN BOOTIN, STATUS '
                       WS-BOOTIN-STATUS
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF

           PERFORM READ-BOOTIN
           PERFORM UNTIL BOOTIN-EOF
               PERFORM VALIDATE-USER-RECORD
               PERFORM READ-BOOTIN
           END-PERFORM

           CLOSE BOOTIN-FILE

           IF WS-REC-CNT = ZERO
               DISPLAY 'CBUSR01C: BOOTIN CONTAINS NO USER RECORDS'
               ADD 1 TO WS-REJECT-CNT
           END-IF
           IF WS-REC-CNT > ZERO AND WS-ADMIN-CNT = ZERO
               DISPLAY 'CBUSR01C: BOOTIN CONTAINS NO ADMIN (TYPE A) '
                       'USER'
               ADD 1 TO WS-REJECT-CNT
           END-IF

           IF WS-REJECT-CNT > ZERO
               DISPLAY 'CBUSR01C: ' WS-REJECT-CNT
                       ' ERROR(S) - USRSEC LOAD FILE NOT WRITTEN'
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF

           PERFORM WRITE-USROUT

           DISPLAY 'CBUSR01C: ' WS-REC-CNT ' USER(S) ACCEPTED, '
                   WS-ADMIN-CNT ' ADMIN(S). PASSWORD CHANGE REQUIRED '
                   'AT FIRST SIGN-ON'
           DISPLAY 'END OF EXECUTION OF PROGRAM CBUSR01C'
           GOBACK.

      *----------------------------------------------------------------*
      *                      READ-BOOTIN
      *----------------------------------------------------------------*
       READ-BOOTIN.

           READ BOOTIN-FILE
               AT END
                   SET BOOTIN-EOF TO TRUE
           END-READ
           IF NOT BOOTIN-EOF AND WS-BOOTIN-STATUS NOT = '00'
               DISPLAY 'CBUSR01C: ERROR READING BOOTIN, STATUS '
                       WS-BOOTIN-STATUS
               ADD 1 TO WS-REJECT-CNT
               SET BOOTIN-EOF TO TRUE
           END-IF.

      *----------------------------------------------------------------*
      *                      VALIDATE-USER-RECORD
      *----------------------------------------------------------------*
       VALIDATE-USER-RECORD.

           ADD 1 TO WS-REC-CNT
           MOVE SPACES     TO WS-REJECT-MSG
           MOVE BOOTIN-REC TO SEC-USER-DATA
           MOVE FUNCTION UPPER-CASE(SEC-USR-ID)  TO SEC-USR-ID
           MOVE FUNCTION UPPER-CASE(SEC-USR-PWD) TO SEC-USR-PWD
           MOVE FUNCTION UPPER-CASE(SEC-USR-TYPE) TO SEC-USR-TYPE

           MOVE SEC-USR-ID  TO CSUTLPWC-USER-ID
           MOVE SEC-USR-PWD TO CSUTLPWC-PASSWORD
           CALL 'CSUTLPWC' USING CSUTLPWC-PARM

           EVALUATE TRUE
               WHEN WS-REC-CNT > WS-MAX-USERS
                   MOVE 'TOO MANY RECORDS IN BOOTIN' TO WS-REJECT-MSG
               WHEN SEC-USR-ID = SPACES OR LOW-VALUES
                   MOVE 'USER ID IS BLANK' TO WS-REJECT-MSG
               WHEN SEC-USR-ID NOT > WS-PREV-USER-ID
                   MOVE 'USER ID DUPLICATE OR NOT IN ASCENDING ORDER'
                     TO WS-REJECT-MSG
               WHEN SEC-USR-FNAME = SPACES OR LOW-VALUES
               WHEN SEC-USR-LNAME = SPACES OR LOW-VALUES
                   MOVE 'FIRST AND LAST NAME ARE REQUIRED'
                     TO WS-REJECT-MSG
               WHEN SEC-USR-TYPE NOT = 'A' AND NOT = 'U'
                   MOVE 'USER TYPE MUST BE A OR U' TO WS-REJECT-MSG
               WHEN NOT CSUTLPWC-PWD-OK
                   MOVE CSUTLPWC-MESSAGE TO WS-REJECT-MSG
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF WS-REJECT-MSG NOT = SPACES
               ADD 1 TO WS-REJECT-CNT
               DISPLAY 'CBUSR01C: RECORD ' WS-REC-CNT ' USER '
                       SEC-USR-ID ' REJECTED: '
                       FUNCTION TRIM(WS-REJECT-MSG)
           ELSE
               MOVE SEC-USR-ID TO WS-PREV-USER-ID
               IF SEC-USR-TYPE = 'A'
                   ADD 1 TO WS-ADMIN-CNT
               END-IF
               SET SEC-USR-PWD-CHG-REQ TO TRUE
               MOVE SPACES TO SEC-USR-FILLER
               MOVE SEC-USER-DATA TO WS-USER-ENTRY(WS-REC-CNT)
           END-IF.

      *----------------------------------------------------------------*
      *                      WRITE-USROUT
      *----------------------------------------------------------------*
       WRITE-USROUT.

           OPEN OUTPUT USROUT-FILE
           IF WS-USROUT-STATUS NOT = '00'
               DISPLAY 'CBUSR01C: UNABLE TO OPEN USROUT, STATUS '
                       WS-USROUT-STATUS
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF

           PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > WS-REC-CNT
               WRITE USROUT-REC FROM WS-USER-ENTRY(WS-IDX)
               IF WS-USROUT-STATUS NOT = '00'
                   DISPLAY 'CBUSR01C: ERROR WRITING USROUT, STATUS '
                           WS-USROUT-STATUS
                   MOVE 12 TO RETURN-CODE
                   CLOSE USROUT-FILE
                   GOBACK
               END-IF
           END-PERFORM

           CLOSE USROUT-FILE.
