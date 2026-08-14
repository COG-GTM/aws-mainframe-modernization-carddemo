       IDENTIFICATION DIVISION.
       PROGRAM-ID.    STMFIXTC.
       AUTHOR.        AWS.
      ******************************************************************
      * Program     : STMFIXTC.cbl
      * Application : CardDemo test suite
      * Type        : Test fixture
      * Function    : Creates small indexed (VSAM-like) test files
      *               for the CBSTM03B unit tests (TSTSTM1C).
      *               Record layouts match the FDs in CBSTM03B.
      *               File assignments (TRNXFILE, XREFFILE, CUSTFILE,
      *               ACCTFILE) are resolved through DD_* environment
      *               variables when run with GnuCOBOL, or through DD
      *               statements when run as a batch job on z/OS.
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
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRNX-FILE ASSIGN TO TRNXFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS FD-TRNXS-ID
                  FILE STATUS  IS TRNXFILE-STATUS.

           SELECT XREF-FILE ASSIGN TO   XREFFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS FD-XREF-CARD-NUM
                  FILE STATUS  IS XREFFILE-STATUS.

           SELECT CUST-FILE ASSIGN TO CUSTFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS FD-CUST-ID
                  FILE STATUS  IS CUSTFILE-STATUS.

           SELECT ACCT-FILE ASSIGN TO ACCTFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS FD-ACCT-ID
                  FILE STATUS  IS ACCTFILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TRNX-FILE.
       01  FD-TRNXFILE-REC.
           05 FD-TRNXS-ID.
              10  FD-TRNX-CARD                  PIC X(16).
              10  FD-TRNX-ID                    PIC X(16).
           05 FD-ACCT-DATA                      PIC X(318).

       FD  XREF-FILE.
       01  FD-XREFFILE-REC.
           05 FD-XREF-CARD-NUM                  PIC X(16).
           05 FD-XREF-DATA                      PIC X(34).

       FD  CUST-FILE.
       01  FD-CUSTFILE-REC.
           05 FD-CUST-ID                        PIC X(09).
           05 FD-CUST-DATA                      PIC X(491).

       FD  ACCT-FILE.
       01  FD-ACCTFILE-REC.
           05 FD-ACCT-ID                        PIC 9(11).
           05 FD-ACCT-DATA                      PIC X(289).

       WORKING-STORAGE SECTION.
       01  TRNXFILE-STATUS                      PIC X(2).
       01  XREFFILE-STATUS                      PIC X(2).
       01  CUSTFILE-STATUS                      PIC X(2).
       01  ACCTFILE-STATUS                      PIC X(2).
       01  WS-ERRORS                            PIC 9(2) VALUE 0.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-BUILD-TRNXFILE
              THRU 1000-EXIT
           PERFORM 2000-BUILD-XREFFILE
              THRU 2000-EXIT
           PERFORM 3000-BUILD-CUSTFILE
              THRU 3000-EXIT
           PERFORM 4000-BUILD-ACCTFILE
              THRU 4000-EXIT

           IF WS-ERRORS = 0
              DISPLAY 'STMFIXTC: test files created'
           ELSE
              DISPLAY 'STMFIXTC: errors creating test files: '
                      WS-ERRORS
           END-IF
           MOVE WS-ERRORS                TO RETURN-CODE
           GOBACK
           .

       1000-BUILD-TRNXFILE.
           OPEN OUTPUT TRNX-FILE
           MOVE '4111111111111111'       TO FD-TRNX-CARD
           MOVE '0000000000000001'       TO FD-TRNX-ID
           MOVE 'TRNX RECORD ONE'        TO FD-ACCT-DATA
                                            OF FD-TRNXFILE-REC
           WRITE FD-TRNXFILE-REC
           MOVE '4111111111111111'       TO FD-TRNX-CARD
           MOVE '0000000000000002'       TO FD-TRNX-ID
           MOVE 'TRNX RECORD TWO'        TO FD-ACCT-DATA
                                            OF FD-TRNXFILE-REC
           WRITE FD-TRNXFILE-REC
           CLOSE TRNX-FILE
           IF TRNXFILE-STATUS NOT = '00'
              ADD 1                      TO WS-ERRORS
              DISPLAY 'STMFIXTC TRNXFILE status: ' TRNXFILE-STATUS
           END-IF
           .
       1000-EXIT.
           EXIT
           .

       2000-BUILD-XREFFILE.
           OPEN OUTPUT XREF-FILE
           MOVE '4111111111111111'       TO FD-XREF-CARD-NUM
           MOVE '000000001 00000000001'  TO FD-XREF-DATA
           WRITE FD-XREFFILE-REC
           CLOSE XREF-FILE
           IF XREFFILE-STATUS NOT = '00'
              ADD 1                      TO WS-ERRORS
              DISPLAY 'STMFIXTC XREFFILE status: ' XREFFILE-STATUS
           END-IF
           .
       2000-EXIT.
           EXIT
           .

       3000-BUILD-CUSTFILE.
           OPEN OUTPUT CUST-FILE
           MOVE '000000001'              TO FD-CUST-ID
           MOVE 'JOHN DOE TEST CUSTOMER' TO FD-CUST-DATA
           WRITE FD-CUSTFILE-REC
           CLOSE CUST-FILE
           IF CUSTFILE-STATUS NOT = '00'
              ADD 1                      TO WS-ERRORS
              DISPLAY 'STMFIXTC CUSTFILE status: ' CUSTFILE-STATUS
           END-IF
           .
       3000-EXIT.
           EXIT
           .

       4000-BUILD-ACCTFILE.
           OPEN OUTPUT ACCT-FILE
           MOVE 1                        TO FD-ACCT-ID
           MOVE 'TEST ACCOUNT RECORD'    TO FD-ACCT-DATA
                                            OF FD-ACCTFILE-REC
           WRITE FD-ACCTFILE-REC
           CLOSE ACCT-FILE
           IF ACCTFILE-STATUS NOT = '00'
              ADD 1                      TO WS-ERRORS
              DISPLAY 'STMFIXTC ACCTFILE status: ' ACCTFILE-STATUS
           END-IF
           .
       4000-EXIT.
           EXIT
           .
