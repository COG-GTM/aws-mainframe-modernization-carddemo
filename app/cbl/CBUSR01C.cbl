      ******************************************************************
      * Program     : CBUSR01C.CBL
      * Application : CardDemo
      * Type        : BATCH COBOL Program
      * Function    : Convert legacy 80-byte user security records
      *               (clear-text password in columns 57-64) into the
      *               200-byte USRSEC layout (CSUSR01Y) holding a
      *               salted password hash from CSPWHASH. Used by
      *               DUSRSECJ (initial load) and USRSECMG (migration
      *               of an existing 80-byte USRSEC file).
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
       PROGRAM-ID.    CBUSR01C.
       AUTHOR.        AWS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USRIN-FILE ASSIGN TO USRIN
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  FILE STATUS  IS USRIN-STATUS.
           SELECT USROUT-FILE ASSIGN TO USROUT
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  FILE STATUS  IS USROUT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  USRIN-FILE.
       01  USRIN-REC.
           05 IN-USR-ID                PIC X(08).
           05 IN-USR-FNAME             PIC X(20).
           05 IN-USR-LNAME             PIC X(20).
           05 IN-USR-PWD               PIC X(08).
           05 IN-USR-TYPE              PIC X(01).
           05 IN-USR-FILLER            PIC X(23).
       FD  USROUT-FILE.
       01  USROUT-REC                  PIC X(200).

       WORKING-STORAGE SECTION.
       COPY CSUSR01Y.
       COPY CSPWH01Y.

       01  USRIN-STATUS                PIC X(02).
       01  USROUT-STATUS               PIC X(02).
       01  WS-EOF                      PIC X(01) VALUE 'N'.
         88 END-OF-INPUT                         VALUE 'Y'.
       01  WS-COUNTS.
         05 WS-READ-CNT                PIC 9(07) VALUE 0.
         05 WS-HASHED-CNT              PIC 9(07) VALUE 0.
       01  WS-RANDOM                   PIC 9V9(9).
       01  WS-SEED                     PIC 9(09).
       01  WS-ENTROPY.
         05 WS-ENT-USER-ID             PIC X(08).
         05 WS-ENT-TIMESTAMP           PIC X(21).
         05 WS-ENT-COUNT               PIC 9(07).
         05 WS-ENT-RANDOM              PIC 9(10).
       01  ABCODE                      PIC S9(9) BINARY.
       01  TIMING                      PIC S9(9) BINARY.

       PROCEDURE DIVISION.
           DISPLAY 'START OF EXECUTION OF PROGRAM CBUSR01C'

           MOVE FUNCTION CURRENT-DATE(9:8) TO WS-SEED
           COMPUTE WS-RANDOM = FUNCTION RANDOM(WS-SEED)

           OPEN INPUT USRIN-FILE
           IF USRIN-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING USRIN, STATUS: ' USRIN-STATUS
               PERFORM ABEND-PROGRAM
           END-IF
           OPEN OUTPUT USROUT-FILE
           IF USROUT-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING USROUT, STATUS: ' USROUT-STATUS
               PERFORM ABEND-PROGRAM
           END-IF

           PERFORM UNTIL END-OF-INPUT
               READ USRIN-FILE
                   AT END
                       SET END-OF-INPUT TO TRUE
                   NOT AT END
                       ADD 1 TO WS-READ-CNT
                       PERFORM CONVERT-RECORD
               END-READ
               IF NOT END-OF-INPUT AND USRIN-STATUS NOT = '00'
                   DISPLAY 'ERROR READING USRIN, STATUS: '
                           USRIN-STATUS
                   PERFORM ABEND-PROGRAM
               END-IF
           END-PERFORM

           CLOSE USRIN-FILE
                 USROUT-FILE

           DISPLAY 'RECORDS READ    : ' WS-READ-CNT
           DISPLAY 'PASSWORDS HASHED: ' WS-HASHED-CNT
           DISPLAY 'END OF EXECUTION OF PROGRAM CBUSR01C'
           GOBACK.

       CONVERT-RECORD.
           MOVE SPACES         TO SEC-USER-DATA
           MOVE IN-USR-ID      TO SEC-USR-ID
           MOVE IN-USR-FNAME   TO SEC-USR-FNAME
           MOVE IN-USR-LNAME   TO SEC-USR-LNAME
           MOVE IN-USR-TYPE    TO SEC-USR-TYPE
           MOVE IN-USR-FILLER  TO SEC-USR-FILLER

           PERFORM HASH-LEGACY-PASSWORD
           ADD 1 TO WS-HASHED-CNT

           WRITE USROUT-REC FROM SEC-USER-DATA
           IF USROUT-STATUS NOT = '00'
               DISPLAY 'ERROR WRITING USROUT, STATUS: ' USROUT-STATUS
               PERFORM ABEND-PROGRAM
           END-IF.

       HASH-LEGACY-PASSWORD.
           IF IN-USR-PWD = SPACES OR LOW-VALUES
               DISPLAY 'USER ' IN-USR-ID ' HAS NO PASSWORD'
               PERFORM ABEND-PROGRAM
           END-IF

           COMPUTE WS-RANDOM = FUNCTION RANDOM
           MOVE IN-USR-ID             TO WS-ENT-USER-ID
           MOVE FUNCTION CURRENT-DATE TO WS-ENT-TIMESTAMP
           MOVE WS-READ-CNT           TO WS-ENT-COUNT
           COMPUTE WS-ENT-RANDOM = WS-RANDOM * 9999999999

           INITIALIZE PWH-PARMS
           SET PWH-FUNC-NEW TO TRUE
           MOVE FUNCTION UPPER-CASE(IN-USR-PWD) TO PWH-PASSWORD
           MOVE WS-ENTROPY TO PWH-ENTROPY
           CALL 'CSPWHASH' USING PWH-PARMS
           MOVE SPACES TO PWH-PASSWORD
                          IN-USR-PWD

           IF NOT PWH-RC-OK
               DISPLAY 'CSPWHASH FAILED FOR USER ' IN-USR-ID
                       ' RC: ' PWH-RETURN-CODE
               PERFORM ABEND-PROGRAM
           END-IF

           MOVE PWH-SCHEME     TO SEC-USR-PWD-SCHEME
           MOVE PWH-ITERATIONS TO SEC-USR-PWD-ITERATIONS
           MOVE PWH-SALT       TO SEC-USR-PWD-SALT
           MOVE PWH-HASH       TO SEC-USR-PWD-DIGEST.

       ABEND-PROGRAM.
           DISPLAY 'ABENDING PROGRAM'
           MOVE 0    TO TIMING
           MOVE 999  TO ABCODE
           CALL 'CEE3ABD' USING ABCODE, TIMING.
