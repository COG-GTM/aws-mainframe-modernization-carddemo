       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBIMPORT.
       AUTHOR.        CARDDEMO TEAM.
      ******************************************************************
      * Program     : CBIMPORT.CBL
      * Application : CardDemo
      * Type        : BATCH COBOL Program
      * Function    : Import Customer Data from Branch Migration Export
      *               Reads multi-record export file and splits it into
      *               separate normalized target files (CUSTOMER, ACCOUNT,
      *               CARD-XREF, TRANSACTION) with validation
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
      * Business Use Case: Branch Migration Import
      * - Import complete customer profiles from export file
      * - Split multi-record layout into normalized target files
      * - Validate every record before any load file is written:
      *   field formats, duplicate keys, collisions with the live
      *   target files and customer/account/card references, which
      *   must resolve to records in the same export file
      * - Destination target files (CUSTFILE..CARDFILE) must be
      *   allocated; imported keys that already exist there are rejected
      * - Fail the import (RC=8, load files left empty) when any
      *   record is rejected; rejects are listed in the error file
      * - Generate import statistics and error reports
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EXPORT-INPUT ASSIGN TO EXPFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS EXPORT-INPUT-KEY
               FILE STATUS IS WS-EXPORT-STATUS.
               
           SELECT CUSTOMER-OUTPUT ASSIGN TO CUSTOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CUSTOMER-STATUS.
               
           SELECT ACCOUNT-OUTPUT ASSIGN TO ACCTOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ACCOUNT-STATUS.
               
           SELECT XREF-OUTPUT ASSIGN TO XREFOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-XREF-STATUS.
               
           SELECT TRANSACTION-OUTPUT ASSIGN TO TRNXOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-TRANSACTION-STATUS.
               
           SELECT CARD-OUTPUT ASSIGN TO CARDOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CARD-STATUS.
               
           SELECT ERROR-OUTPUT ASSIGN TO ERROUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ERROR-STATUS.

      * Work file of keys seen in the export file (validation only)
           SELECT KEY-WORK-FILE ASSIGN TO KEYWORK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS KW-KEY
               FILE STATUS IS WS-KEYWORK-STATUS.

      * Live target files, read only to detect key collisions
           SELECT OPTIONAL LIVE-CUSTOMER-FILE ASSIGN TO CUSTFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS LIVE-CUST-KEY
               FILE STATUS IS WS-LIVE-CUST-STATUS.

           SELECT OPTIONAL LIVE-ACCOUNT-FILE ASSIGN TO ACCTFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS LIVE-ACCT-KEY
               FILE STATUS IS WS-LIVE-ACCT-STATUS.

           SELECT OPTIONAL LIVE-XREF-FILE ASSIGN TO XREFFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS LIVE-XREF-KEY
               FILE STATUS IS WS-LIVE-XREF-STATUS.

           SELECT OPTIONAL LIVE-TRAN-FILE ASSIGN TO TRANSACT
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS LIVE-TRAN-KEY
               FILE STATUS IS WS-LIVE-TRAN-STATUS.

           SELECT OPTIONAL LIVE-CARD-FILE ASSIGN TO CARDFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS LIVE-CARD-KEY
               FILE STATUS IS WS-LIVE-CARD-STATUS.

       DATA DIVISION.
       FILE SECTION.
       
       FD  EXPORT-INPUT
           RECORDING MODE IS F
           RECORD CONTAINS 500 CHARACTERS.
      * Key position matches KEYS(4 28) in CBEXPORT.jcl
       01  EXPORT-INPUT-RECORD.
           05  FILLER                                  PIC X(28).
           05  EXPORT-INPUT-KEY                        PIC X(04).
           05  FILLER                                  PIC X(468).

       FD  CUSTOMER-OUTPUT
           RECORDING MODE IS F
           RECORD CONTAINS 500 CHARACTERS.
       COPY CVCUS01Y.

       FD  ACCOUNT-OUTPUT
           RECORDING MODE IS F
           RECORD CONTAINS 300 CHARACTERS.
       COPY CVACT01Y.

       FD  XREF-OUTPUT
           RECORDING MODE IS F
           RECORD CONTAINS 50 CHARACTERS.
       COPY CVACT03Y.

       FD  TRANSACTION-OUTPUT
           RECORDING MODE IS F
           RECORD CONTAINS 350 CHARACTERS.
       COPY CVTRA05Y.

       FD  CARD-OUTPUT
           RECORDING MODE IS F
           RECORD CONTAINS 150 CHARACTERS.
       COPY CVACT02Y.

       FD  ERROR-OUTPUT
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS.
       01  ERROR-OUTPUT-RECORD                        PIC X(132).

       FD  KEY-WORK-FILE.
       01  KEY-WORK-RECORD.
           05  KW-KEY.
               10  KW-REC-TYPE                         PIC X(01).
               10  KW-KEY-VALUE                        PIC X(16).
           05  KW-ACCT-ID                              PIC 9(11).
           05  KW-CUST-ID                              PIC 9(09).
           05  KW-SEQUENCE                             PIC 9(09).
           05  FILLER                                  PIC X(04).

       FD  LIVE-CUSTOMER-FILE.
       01  LIVE-CUSTOMER-RECORD.
           05  LIVE-CUST-KEY                           PIC X(09).
           05  FILLER                                  PIC X(491).

       FD  LIVE-ACCOUNT-FILE.
       01  LIVE-ACCOUNT-RECORD.
           05  LIVE-ACCT-KEY                           PIC X(11).
           05  FILLER                                  PIC X(289).

       FD  LIVE-XREF-FILE.
       01  LIVE-XREF-RECORD.
           05  LIVE-XREF-KEY                           PIC X(16).
           05  FILLER                                  PIC X(34).

       FD  LIVE-TRAN-FILE.
       01  LIVE-TRAN-RECORD.
           05  LIVE-TRAN-KEY                           PIC X(16).
           05  FILLER                                  PIC X(334).

       FD  LIVE-CARD-FILE.
       01  LIVE-CARD-RECORD.
           05  LIVE-CARD-KEY                           PIC X(16).
           05  FILLER                                  PIC X(134).

       WORKING-STORAGE SECTION.

       COPY CVEXPORT.

      * File Status Variables
       01  WS-FILE-STATUS-AREA.
           05  WS-EXPORT-STATUS                        PIC X(02).
               88  WS-EXPORT-EOF                       VALUE '10'.
               88  WS-EXPORT-OK                        VALUE '00'.
           05  WS-CUSTOMER-STATUS                      PIC X(02).
               88  WS-CUSTOMER-OK                      VALUE '00'.
           05  WS-ACCOUNT-STATUS                       PIC X(02).
               88  WS-ACCOUNT-OK                       VALUE '00'.
           05  WS-XREF-STATUS                          PIC X(02).
               88  WS-XREF-OK                          VALUE '00'.
           05  WS-TRANSACTION-STATUS                   PIC X(02).
               88  WS-TRANSACTION-OK                   VALUE '00'.
           05  WS-CARD-STATUS                          PIC X(02).
               88  WS-CARD-OK                          VALUE '00'.
           05  WS-ERROR-STATUS                         PIC X(02).
               88  WS-ERROR-OK                         VALUE '00'.
           05  WS-KEYWORK-STATUS                       PIC X(02).
               88  WS-KEYWORK-OK                       VALUE '00'.
               88  WS-KEYWORK-DUPLICATE                VALUE '22'.
               88  WS-KEYWORK-NOT-FOUND                VALUE '23'.
           05  WS-LIVE-CUST-STATUS                     PIC X(02).
           05  WS-LIVE-ACCT-STATUS                     PIC X(02).
           05  WS-LIVE-XREF-STATUS                     PIC X(02).
           05  WS-LIVE-TRAN-STATUS                     PIC X(02).
           05  WS-LIVE-CARD-STATUS                     PIC X(02).

      * Live target file availability ('Y' = opened, collisions checked)
       01  WS-LIVE-FILES-PRESENT.
           05  WS-LIVE-CUST-PRESENT                    PIC X(01).
           05  WS-LIVE-ACCT-PRESENT                    PIC X(01).
           05  WS-LIVE-XREF-PRESENT                    PIC X(01).
           05  WS-LIVE-TRAN-PRESENT                    PIC X(01).
           05  WS-LIVE-CARD-PRESENT                    PIC X(01).
       01  WS-LIVE-CHECK.
           05  WS-LIVE-STATUS                          PIC X(02).
           05  WS-LIVE-PRESENT                         PIC X(01).
           05  WS-LIVE-DD                              PIC X(08).

      * Import Control Variables
       01  WS-IMPORT-CONTROL.
           05  WS-IMPORT-DATE                          PIC X(10).
           05  WS-IMPORT-TIME                          PIC X(08).

      * Statistics Counters
       01  WS-IMPORT-STATISTICS.
           05  WS-TOTAL-RECORDS-READ                  PIC 9(09) VALUE 0.
           05  WS-CUSTOMER-RECORDS-IMPORTED           PIC 9(09) VALUE 0.
           05  WS-ACCOUNT-RECORDS-IMPORTED            PIC 9(09) VALUE 0.
           05  WS-XREF-RECORDS-IMPORTED               PIC 9(09) VALUE 0.
           05  WS-TRAN-RECORDS-IMPORTED           PIC 9(09) VALUE 0.
           05  WS-CARD-RECORDS-IMPORTED               PIC 9(09) VALUE 0.
           05  WS-ERROR-RECORDS-WRITTEN               PIC 9(09) VALUE 0.
           05  WS-UNKNOWN-RECORD-TYPE-COUNT           PIC 9(09) VALUE 0.
           05  WS-RECORDS-VALIDATED                   PIC 9(09) VALUE 0.
           05  WS-REJECT-COUNT                        PIC 9(09) VALUE 0.

      * Validation work areas
       01  WS-VALIDATION-AREA.
           05  WS-VALIDATION-RESULT                   PIC X(01)
                                                      VALUE 'N'.
               88  WS-VALIDATION-PASSED               VALUE 'Y'.
           05  WS-REJECT-REASON                       PIC X(50).
           05  WS-KEY-CUST-ID                         PIC 9(09).
           05  WS-KEY-ACCT-ID                         PIC 9(11).
           05  WS-DATE-CHECK.
               10  WS-DATE-YYYY                       PIC X(04).
               10  WS-DATE-SEP-1                      PIC X(01).
               10  WS-DATE-MM                         PIC X(02).
               10  WS-DATE-SEP-2                      PIC X(01).
               10  WS-DATE-DD                         PIC X(02).
           05  WS-DATE-NUM                            PIC 9(08).
           05  WS-DATE-NUM-R REDEFINES WS-DATE-NUM.
               10  WS-DATE-NUM-YYYY                   PIC X(04).
               10  WS-DATE-NUM-MM                     PIC X(02).
               10  WS-DATE-NUM-DD                     PIC X(02).
           05  WS-DATE-RESULT                         PIC X(01).
               88  WS-DATE-VALID                      VALUE 'Y'.



      * Error Record Layout
       01  WS-ERROR-RECORD.
           05  ERR-TIMESTAMP                           PIC X(26).
           05  FILLER                              PIC X(01) VALUE '|'. 
           05  ERR-RECORD-TYPE                         PIC X(01).
           05  FILLER                              PIC X(01) VALUE '|'. 
           05  ERR-SEQUENCE                            PIC 9(07).
           05  FILLER                              PIC X(01) VALUE '|'. 
           05  ERR-MESSAGE                             PIC X(50).
           05  FILLER                          PIC X(43) VALUE SPACES.

       PROCEDURE DIVISION.

      *****************************************************************
       0000-MAIN-PROCESSING.
      *****************************************************************
           PERFORM 1000-INITIALIZE
           PERFORM 3000-VALIDATE-IMPORT
           IF WS-VALIDATION-PASSED
               PERFORM 2000-PROCESS-EXPORT-FILE
           END-IF
           PERFORM 4000-FINALIZE
           GOBACK.

      *****************************************************************
       1000-INITIALIZE.
      *****************************************************************
           DISPLAY 'CBIMPORT: Starting Customer Data Import'
           
           MOVE FUNCTION CURRENT-DATE(1:4) TO WS-IMPORT-DATE(1:4)
           MOVE '-' TO WS-IMPORT-DATE(5:1)
           MOVE FUNCTION CURRENT-DATE(5:2) TO WS-IMPORT-DATE(6:2)
           MOVE '-' TO WS-IMPORT-DATE(8:1)
           MOVE FUNCTION CURRENT-DATE(7:2) TO WS-IMPORT-DATE(9:2)
           
           MOVE FUNCTION CURRENT-DATE(9:2) TO WS-IMPORT-TIME(1:2)
           MOVE ':' TO WS-IMPORT-TIME(3:1)
           MOVE FUNCTION CURRENT-DATE(11:2) TO WS-IMPORT-TIME(4:2)
           MOVE ':' TO WS-IMPORT-TIME(6:1)
           MOVE FUNCTION CURRENT-DATE(13:2) TO WS-IMPORT-TIME(7:2)
           
           PERFORM 1100-OPEN-FILES
           
           DISPLAY 'CBIMPORT: Import Date: ' WS-IMPORT-DATE
           DISPLAY 'CBIMPORT: Import Time: ' WS-IMPORT-TIME.

      *****************************************************************
       1100-OPEN-FILES.
      *****************************************************************
           OPEN INPUT EXPORT-INPUT
           IF NOT WS-EXPORT-OK
               DISPLAY 'ERROR: Cannot open EXPORT-INPUT, Status: '
                       WS-EXPORT-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           
           OPEN OUTPUT CUSTOMER-OUTPUT
           IF NOT WS-CUSTOMER-OK
               DISPLAY 'ERROR: Cannot open CUSTOMER-OUTPUT, Status: '
                       WS-CUSTOMER-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           
           OPEN OUTPUT ACCOUNT-OUTPUT
           IF NOT WS-ACCOUNT-OK
               DISPLAY 'ERROR: Cannot open ACCOUNT-OUTPUT, Status: '
                       WS-ACCOUNT-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           
           OPEN OUTPUT XREF-OUTPUT
           IF NOT WS-XREF-OK
               DISPLAY 'ERROR: Cannot open XREF-OUTPUT, Status: '
                       WS-XREF-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           
           OPEN OUTPUT TRANSACTION-OUTPUT
           IF NOT WS-TRANSACTION-OK
               DISPLAY 'ERROR: Cannot open TRANSACTION-OUTPUT, Status: '
                       WS-TRANSACTION-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           
           OPEN OUTPUT CARD-OUTPUT
           IF NOT WS-CARD-OK
               DISPLAY 'ERROR: Cannot open CARD-OUTPUT, Status: '
                       WS-CARD-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           
           OPEN OUTPUT ERROR-OUTPUT
           IF NOT WS-ERROR-OK
               DISPLAY 'ERROR: Cannot open ERROR-OUTPUT, Status: '
                       WS-ERROR-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF

           OPEN OUTPUT KEY-WORK-FILE
           IF NOT WS-KEYWORK-OK
               DISPLAY 'ERROR: Cannot open KEY-WORK-FILE, Status: '
                       WS-KEYWORK-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF

           OPEN INPUT LIVE-CUSTOMER-FILE
           MOVE WS-LIVE-CUST-STATUS TO WS-LIVE-STATUS
           MOVE 'CUSTFILE' TO WS-LIVE-DD
           PERFORM 1150-CHECK-LIVE-OPEN
           MOVE WS-LIVE-PRESENT TO WS-LIVE-CUST-PRESENT

           OPEN INPUT LIVE-ACCOUNT-FILE
           MOVE WS-LIVE-ACCT-STATUS TO WS-LIVE-STATUS
           MOVE 'ACCTFILE' TO WS-LIVE-DD
           PERFORM 1150-CHECK-LIVE-OPEN
           MOVE WS-LIVE-PRESENT TO WS-LIVE-ACCT-PRESENT

           OPEN INPUT LIVE-XREF-FILE
           MOVE WS-LIVE-XREF-STATUS TO WS-LIVE-STATUS
           MOVE 'XREFFILE' TO WS-LIVE-DD
           PERFORM 1150-CHECK-LIVE-OPEN
           MOVE WS-LIVE-PRESENT TO WS-LIVE-XREF-PRESENT

           OPEN INPUT LIVE-TRAN-FILE
           MOVE WS-LIVE-TRAN-STATUS TO WS-LIVE-STATUS
           MOVE 'TRANSACT' TO WS-LIVE-DD
           PERFORM 1150-CHECK-LIVE-OPEN
           MOVE WS-LIVE-PRESENT TO WS-LIVE-TRAN-PRESENT

           OPEN INPUT LIVE-CARD-FILE
           MOVE WS-LIVE-CARD-STATUS TO WS-LIVE-STATUS
           MOVE 'CARDFILE' TO WS-LIVE-DD
           PERFORM 1150-CHECK-LIVE-OPEN
           MOVE WS-LIVE-PRESENT TO WS-LIVE-CARD-PRESENT.

      *****************************************************************
       1150-CHECK-LIVE-OPEN.
      *****************************************************************
           EVALUATE WS-LIVE-STATUS
               WHEN '00'
                   MOVE 'Y' TO WS-LIVE-PRESENT
               WHEN '05'
      *            Collision checks are mandatory: missing DD fails
                   MOVE 'N' TO WS-LIVE-PRESENT
                   DISPLAY 'ERROR: ' WS-LIVE-DD ' not allocated'
                   MOVE SPACES TO EXPORT-REC-TYPE
                   MOVE 0 TO EXPORT-SEQUENCE-NUM
                   STRING 'Target file not allocated: ' WS-LIVE-DD
                       DELIMITED BY SIZE INTO WS-REJECT-REASON
                   PERFORM 3800-REJECT-RECORD
               WHEN OTHER
                   DISPLAY 'ERROR: Cannot open ' WS-LIVE-DD
                           ', Status: ' WS-LIVE-STATUS
                   PERFORM 9999-ABEND-PROGRAM
           END-EVALUATE.

      *****************************************************************
       2000-PROCESS-EXPORT-FILE.
      *****************************************************************
           PERFORM 2100-READ-EXPORT-RECORD
           
           PERFORM UNTIL WS-EXPORT-EOF
               ADD 1 TO WS-TOTAL-RECORDS-READ
               PERFORM 2200-PROCESS-RECORD-BY-TYPE
               PERFORM 2100-READ-EXPORT-RECORD
           END-PERFORM.

      *****************************************************************
       2100-READ-EXPORT-RECORD.
      *****************************************************************
           READ EXPORT-INPUT INTO EXPORT-RECORD
           
           IF NOT WS-EXPORT-OK AND NOT WS-EXPORT-EOF
               DISPLAY 'ERROR: Reading EXPORT-INPUT, Status: '
                       WS-EXPORT-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF.

      *****************************************************************
       2200-PROCESS-RECORD-BY-TYPE.
      *****************************************************************
           EVALUATE EXPORT-REC-TYPE
               WHEN 'C'
                   PERFORM 2300-PROCESS-CUSTOMER-RECORD
               WHEN 'A'
                   PERFORM 2400-PROCESS-ACCOUNT-RECORD
               WHEN 'X'
                   PERFORM 2500-PROCESS-XREF-RECORD
               WHEN 'T'
                   PERFORM 2600-PROCESS-TRAN-RECORD
               WHEN 'D'
                   PERFORM 2650-PROCESS-CARD-RECORD
               WHEN OTHER
                   PERFORM 2700-PROCESS-UNKNOWN-RECORD
           END-EVALUATE.

      *****************************************************************
       2300-PROCESS-CUSTOMER-RECORD.
      *****************************************************************
           INITIALIZE CUSTOMER-RECORD
           
      *    Map export fields to customer record
           MOVE EXP-CUST-ID TO CUST-ID
           MOVE EXP-CUST-FIRST-NAME TO CUST-FIRST-NAME
           MOVE EXP-CUST-MIDDLE-NAME TO CUST-MIDDLE-NAME
           MOVE EXP-CUST-LAST-NAME TO CUST-LAST-NAME
           MOVE EXP-CUST-ADDR-LINE(1) TO CUST-ADDR-LINE-1
           MOVE EXP-CUST-ADDR-LINE(2) TO CUST-ADDR-LINE-2
           MOVE EXP-CUST-ADDR-LINE(3) TO CUST-ADDR-LINE-3
           MOVE EXP-CUST-ADDR-STATE-CD TO CUST-ADDR-STATE-CD
           MOVE EXP-CUST-ADDR-COUNTRY-CD TO CUST-ADDR-COUNTRY-CD
           MOVE EXP-CUST-ADDR-ZIP TO CUST-ADDR-ZIP
           MOVE EXP-CUST-PHONE-NUM(1) TO CUST-PHONE-NUM-1
           MOVE EXP-CUST-PHONE-NUM(2) TO CUST-PHONE-NUM-2
           MOVE EXP-CUST-SSN TO CUST-SSN
           MOVE EXP-CUST-GOVT-ISSUED-ID TO CUST-GOVT-ISSUED-ID
           MOVE EXP-CUST-DOB-YYYY-MM-DD TO CUST-DOB-YYYY-MM-DD
           MOVE EXP-CUST-EFT-ACCOUNT-ID TO CUST-EFT-ACCOUNT-ID
           MOVE EXP-CUST-PRI-CARD-HOLDER-IND TO CUST-PRI-CARD-HOLDER-IND
           MOVE EXP-CUST-FICO-CREDIT-SCORE TO CUST-FICO-CREDIT-SCORE
           
           WRITE CUSTOMER-RECORD
           
           IF NOT WS-CUSTOMER-OK
               DISPLAY 'ERROR: Writing customer record, Status: '
                       WS-CUSTOMER-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           
           ADD 1 TO WS-CUSTOMER-RECORDS-IMPORTED.

      *****************************************************************
       2400-PROCESS-ACCOUNT-RECORD.
      *****************************************************************
           INITIALIZE ACCOUNT-RECORD
           
      *    Map export fields to account record
           MOVE EXP-ACCT-ID TO ACCT-ID
           MOVE EXP-ACCT-ACTIVE-STATUS TO ACCT-ACTIVE-STATUS
           MOVE EXP-ACCT-CURR-BAL TO ACCT-CURR-BAL
           MOVE EXP-ACCT-CREDIT-LIMIT TO ACCT-CREDIT-LIMIT
           MOVE EXP-ACCT-CASH-CREDIT-LIMIT TO ACCT-CASH-CREDIT-LIMIT
           MOVE EXP-ACCT-OPEN-DATE TO ACCT-OPEN-DATE
           MOVE EXP-ACCT-EXPIRAION-DATE TO ACCT-EXPIRAION-DATE
           MOVE EXP-ACCT-REISSUE-DATE TO ACCT-REISSUE-DATE
           MOVE EXP-ACCT-CURR-CYC-CREDIT TO ACCT-CURR-CYC-CREDIT
           MOVE EXP-ACCT-CURR-CYC-DEBIT TO ACCT-CURR-CYC-DEBIT
           MOVE EXP-ACCT-ADDR-ZIP TO ACCT-ADDR-ZIP
           MOVE EXP-ACCT-GROUP-ID TO ACCT-GROUP-ID
           
           WRITE ACCOUNT-RECORD
           
           IF NOT WS-ACCOUNT-OK
               DISPLAY 'ERROR: Writing account record, Status: '
                       WS-ACCOUNT-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           
           ADD 1 TO WS-ACCOUNT-RECORDS-IMPORTED.

      *****************************************************************
       2500-PROCESS-XREF-RECORD.
      *****************************************************************
           INITIALIZE CARD-XREF-RECORD
           
      *    Map export fields to xref record
           MOVE EXP-XREF-CARD-NUM TO XREF-CARD-NUM
           MOVE EXP-XREF-CUST-ID TO XREF-CUST-ID
           MOVE EXP-XREF-ACCT-ID TO XREF-ACCT-ID
           
           WRITE CARD-XREF-RECORD
           
           IF NOT WS-XREF-OK
               DISPLAY 'ERROR: Writing xref record, Status: '
                       WS-XREF-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           
           ADD 1 TO WS-XREF-RECORDS-IMPORTED.

      *****************************************************************
       2600-PROCESS-TRAN-RECORD.
      *****************************************************************
           INITIALIZE TRAN-RECORD
           
      *    Map export fields to transaction record
           MOVE EXP-TRAN-ID TO TRAN-ID
           MOVE EXP-TRAN-TYPE-CD TO TRAN-TYPE-CD
           MOVE EXP-TRAN-CAT-CD TO TRAN-CAT-CD
           MOVE EXP-TRAN-SOURCE TO TRAN-SOURCE
           MOVE EXP-TRAN-DESC TO TRAN-DESC
           MOVE EXP-TRAN-AMT TO TRAN-AMT
           MOVE EXP-TRAN-MERCHANT-ID TO TRAN-MERCHANT-ID
           MOVE EXP-TRAN-MERCHANT-NAME TO TRAN-MERCHANT-NAME
           MOVE EXP-TRAN-MERCHANT-CITY TO TRAN-MERCHANT-CITY
           MOVE EXP-TRAN-MERCHANT-ZIP TO TRAN-MERCHANT-ZIP
           MOVE EXP-TRAN-CARD-NUM TO TRAN-CARD-NUM
           MOVE EXP-TRAN-ORIG-TS TO TRAN-ORIG-TS
           MOVE EXP-TRAN-PROC-TS TO TRAN-PROC-TS
           
           WRITE TRAN-RECORD
           
           IF NOT WS-TRANSACTION-OK
               DISPLAY 'ERROR: Writing transaction record, Status: '
                       WS-TRANSACTION-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           
           ADD 1 TO WS-TRAN-RECORDS-IMPORTED.

      *****************************************************************
       2650-PROCESS-CARD-RECORD.
      *****************************************************************
           INITIALIZE CARD-RECORD
           
      *    Map export fields to card record
           MOVE EXP-CARD-NUM TO CARD-NUM
           MOVE EXP-CARD-ACCT-ID TO CARD-ACCT-ID
           MOVE EXP-CARD-CVV-CD TO CARD-CVV-CD
           MOVE EXP-CARD-EMBOSSED-NAME TO CARD-EMBOSSED-NAME
           MOVE EXP-CARD-EXPIRAION-DATE TO CARD-EXPIRAION-DATE
           MOVE EXP-CARD-ACTIVE-STATUS TO CARD-ACTIVE-STATUS
           
           WRITE CARD-RECORD
           
           IF NOT WS-CARD-OK
               DISPLAY 'ERROR: Writing card record, Status: '
                       WS-CARD-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           
           ADD 1 TO WS-CARD-RECORDS-IMPORTED.

      *****************************************************************
       2700-PROCESS-UNKNOWN-RECORD.
      *****************************************************************
           ADD 1 TO WS-UNKNOWN-RECORD-TYPE-COUNT
           
           MOVE FUNCTION CURRENT-DATE TO ERR-TIMESTAMP
           MOVE EXPORT-REC-TYPE TO ERR-RECORD-TYPE
           MOVE EXPORT-SEQUENCE-NUM TO ERR-SEQUENCE
           MOVE 'Unknown record type encountered' TO ERR-MESSAGE
           
           PERFORM 2750-WRITE-ERROR.

      *****************************************************************
       2750-WRITE-ERROR.
      *****************************************************************
           WRITE ERROR-OUTPUT-RECORD FROM WS-ERROR-RECORD
           
           IF NOT WS-ERROR-OK
               DISPLAY 'ERROR: Writing error record, Status: '
                       WS-ERROR-STATUS
           END-IF
           
           ADD 1 TO WS-ERROR-RECORDS-WRITTEN.

      *****************************************************************
       3000-VALIDATE-IMPORT.
      *****************************************************************
      *    Pass 1: field checks, duplicate keys, live-file collisions
           DISPLAY 'CBIMPORT: Validating export file'
           PERFORM 2100-READ-EXPORT-RECORD
           PERFORM UNTIL WS-EXPORT-EOF
               ADD 1 TO WS-RECORDS-VALIDATED
               PERFORM 3100-VALIDATE-RECORD
               PERFORM 2100-READ-EXPORT-RECORD
           END-PERFORM

           IF WS-RECORDS-VALIDATED = 0
               MOVE SPACES TO EXPORT-REC-TYPE
               MOVE 0 TO EXPORT-SEQUENCE-NUM
               MOVE 'Export file contains no records'
                 TO WS-REJECT-REASON
               PERFORM 3800-REJECT-RECORD
           END-IF

      *    Pass 2: references must resolve within the export file
           CLOSE KEY-WORK-FILE
           OPEN INPUT KEY-WORK-FILE
           IF NOT WS-KEYWORK-OK
               DISPLAY 'ERROR: Cannot reopen KEY-WORK-FILE, Status: '
                       WS-KEYWORK-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF

           PERFORM 3900-REOPEN-EXPORT-INPUT
           PERFORM 2100-READ-EXPORT-RECORD
           PERFORM UNTIL WS-EXPORT-EOF
               PERFORM 3500-VALIDATE-REFERENCES
               PERFORM 2100-READ-EXPORT-RECORD
           END-PERFORM

           IF WS-REJECT-COUNT = 0
               SET WS-VALIDATION-PASSED TO TRUE
               DISPLAY 'CBIMPORT: Import validation passed'
               PERFORM 3900-REOPEN-EXPORT-INPUT
           ELSE
               DISPLAY 'CBIMPORT: Import validation FAILED, '
                       'records rejected: ' WS-REJECT-COUNT
               DISPLAY 'CBIMPORT: No records written to load files'
           END-IF.

      *****************************************************************
       3100-VALIDATE-RECORD.
      *****************************************************************
           MOVE SPACES TO WS-REJECT-REASON

           EVALUATE EXPORT-REC-TYPE
               WHEN 'C'
                   PERFORM 3110-CHECK-CUSTOMER-FIELDS
               WHEN 'A'
                   PERFORM 3120-CHECK-ACCOUNT-FIELDS
               WHEN 'X'
                   PERFORM 3130-CHECK-XREF-FIELDS
               WHEN 'T'
                   PERFORM 3140-CHECK-TRAN-FIELDS
               WHEN 'D'
                   PERFORM 3150-CHECK-CARD-FIELDS
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-RECORD-TYPE-COUNT
                   MOVE 'Unknown record type encountered'
                     TO WS-REJECT-REASON
           END-EVALUATE

           IF WS-REJECT-REASON = SPACES
               PERFORM 3200-REGISTER-KEY
           END-IF

           IF WS-REJECT-REASON NOT = SPACES
               PERFORM 3800-REJECT-RECORD
           END-IF.

      *****************************************************************
       3110-CHECK-CUSTOMER-FIELDS.
      *****************************************************************
           EVALUATE TRUE
               WHEN EXP-CUST-ID = 0 OR EXP-CUST-ID > 999999999
                   MOVE 'Customer id invalid' TO WS-REJECT-REASON
               WHEN EXP-CUST-FIRST-NAME = SPACES
                 OR EXP-CUST-LAST-NAME = SPACES
                   MOVE 'Customer name is blank' TO WS-REJECT-REASON
               WHEN EXP-CUST-SSN NOT NUMERIC
                   MOVE 'Customer SSN is not numeric'
                     TO WS-REJECT-REASON
               WHEN EXP-CUST-FICO-CREDIT-SCORE NOT NUMERIC
                   MOVE 'Customer FICO score is not numeric'
                     TO WS-REJECT-REASON
               WHEN EXP-CUST-FICO-CREDIT-SCORE > 850
                   MOVE 'Customer FICO score out of range'
                     TO WS-REJECT-REASON
               WHEN EXP-CUST-PRI-CARD-HOLDER-IND NOT = 'Y'
                AND EXP-CUST-PRI-CARD-HOLDER-IND NOT = 'N'
                   MOVE 'Customer primary card holder flag invalid'
                     TO WS-REJECT-REASON
               WHEN OTHER
                   MOVE EXP-CUST-DOB-YYYY-MM-DD TO WS-DATE-CHECK
                   PERFORM 3190-CHECK-DATE
                   IF NOT WS-DATE-VALID
                       MOVE 'Customer date of birth invalid'
                         TO WS-REJECT-REASON
                   END-IF
           END-EVALUATE.

      *****************************************************************
       3120-CHECK-ACCOUNT-FIELDS.
      *****************************************************************
           EVALUATE TRUE
               WHEN EXP-ACCT-ID NOT NUMERIC
                   MOVE 'Account id is not numeric' TO WS-REJECT-REASON
               WHEN EXP-ACCT-ID = 0
                   MOVE 'Account id invalid' TO WS-REJECT-REASON
               WHEN EXP-ACCT-ACTIVE-STATUS NOT = 'Y'
                AND EXP-ACCT-ACTIVE-STATUS NOT = 'N'
                   MOVE 'Account active status invalid'
                     TO WS-REJECT-REASON
               WHEN EXP-ACCT-CURR-BAL NOT NUMERIC
                 OR EXP-ACCT-CURR-CYC-CREDIT NOT NUMERIC
                   MOVE 'Account balance is not numeric'
                     TO WS-REJECT-REASON
               WHEN EXP-ACCT-CREDIT-LIMIT NOT NUMERIC
                 OR EXP-ACCT-CASH-CREDIT-LIMIT NOT NUMERIC
                   MOVE 'Account credit limit is not numeric'
                     TO WS-REJECT-REASON
               WHEN EXP-ACCT-CREDIT-LIMIT < 0
                 OR EXP-ACCT-CASH-CREDIT-LIMIT < 0
                 OR EXP-ACCT-CASH-CREDIT-LIMIT > EXP-ACCT-CREDIT-LIMIT
                   MOVE 'Account credit limit out of range'
                     TO WS-REJECT-REASON
               WHEN OTHER
                   MOVE EXP-ACCT-OPEN-DATE TO WS-DATE-CHECK
                   PERFORM 3190-CHECK-DATE
                   IF WS-DATE-VALID
                       MOVE EXP-ACCT-EXPIRAION-DATE TO WS-DATE-CHECK
                       PERFORM 3190-CHECK-DATE
                   END-IF
                   IF WS-DATE-VALID
                       MOVE EXP-ACCT-REISSUE-DATE TO WS-DATE-CHECK
                       PERFORM 3190-CHECK-DATE
                   END-IF
                   IF NOT WS-DATE-VALID
                       MOVE 'Account date invalid' TO WS-REJECT-REASON
                   END-IF
           END-EVALUATE.

      *****************************************************************
       3130-CHECK-XREF-FIELDS.
      *****************************************************************
           EVALUATE TRUE
               WHEN EXP-XREF-CARD-NUM NOT NUMERIC
                   MOVE 'Cross-reference card number invalid'
                     TO WS-REJECT-REASON
               WHEN EXP-XREF-CUST-ID NOT NUMERIC
                 OR EXP-XREF-CUST-ID = 0
                   MOVE 'Cross-reference customer id invalid'
                     TO WS-REJECT-REASON
               WHEN EXP-XREF-ACCT-ID = 0
                 OR EXP-XREF-ACCT-ID > 99999999999
                   MOVE 'Cross-reference account id invalid'
                     TO WS-REJECT-REASON
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *****************************************************************
       3140-CHECK-TRAN-FIELDS.
      *****************************************************************
           EVALUATE TRUE
               WHEN EXP-TRAN-ID = SPACES
                   MOVE 'Transaction id is blank' TO WS-REJECT-REASON
               WHEN EXP-TRAN-TYPE-CD = SPACES
                   MOVE 'Transaction type is blank' TO WS-REJECT-REASON
               WHEN EXP-TRAN-CAT-CD NOT NUMERIC
                   MOVE 'Transaction category is not numeric'
                     TO WS-REJECT-REASON
               WHEN EXP-TRAN-AMT NOT NUMERIC
                   MOVE 'Transaction amount is not numeric'
                     TO WS-REJECT-REASON
               WHEN EXP-TRAN-CARD-NUM NOT NUMERIC
                   MOVE 'Transaction card number invalid'
                     TO WS-REJECT-REASON
               WHEN EXP-TRAN-ORIG-TS = SPACES
                   MOVE 'Transaction timestamp is blank'
                     TO WS-REJECT-REASON
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *****************************************************************
       3150-CHECK-CARD-FIELDS.
      *****************************************************************
           EVALUATE TRUE
               WHEN EXP-CARD-NUM NOT NUMERIC
                   MOVE 'Card number invalid' TO WS-REJECT-REASON
               WHEN EXP-CARD-ACCT-ID = 0
                 OR EXP-CARD-ACCT-ID > 99999999999
                   MOVE 'Card account id invalid' TO WS-REJECT-REASON
               WHEN EXP-CARD-CVV-CD > 999
                   MOVE 'Card CVV code invalid' TO WS-REJECT-REASON
               WHEN EXP-CARD-EMBOSSED-NAME = SPACES
                   MOVE 'Card embossed name is blank'
                     TO WS-REJECT-REASON
               WHEN EXP-CARD-ACTIVE-STATUS NOT = 'Y'
                AND EXP-CARD-ACTIVE-STATUS NOT = 'N'
                   MOVE 'Card active status invalid' TO WS-REJECT-REASON
               WHEN OTHER
                   MOVE EXP-CARD-EXPIRAION-DATE TO WS-DATE-CHECK
                   PERFORM 3190-CHECK-DATE
                   IF NOT WS-DATE-VALID
                       MOVE 'Card expiration date invalid'
                         TO WS-REJECT-REASON
                   END-IF
           END-EVALUATE.

      *****************************************************************
       3190-CHECK-DATE.
      *****************************************************************
      *    WS-DATE-CHECK must be a real calendar date in YYYY-MM-DD
           MOVE 'N' TO WS-DATE-RESULT
           IF  WS-DATE-YYYY IS NUMERIC
           AND WS-DATE-SEP-1 = '-'
           AND WS-DATE-MM IS NUMERIC
           AND WS-DATE-SEP-2 = '-'
           AND WS-DATE-DD IS NUMERIC
               MOVE WS-DATE-YYYY TO WS-DATE-NUM-YYYY
               MOVE WS-DATE-MM   TO WS-DATE-NUM-MM
               MOVE WS-DATE-DD   TO WS-DATE-NUM-DD
               IF FUNCTION TEST-DATE-YYYYMMDD(WS-DATE-NUM) = 0
                   MOVE 'Y' TO WS-DATE-RESULT
               END-IF
           END-IF.

      *****************************************************************
       3200-REGISTER-KEY.
      *****************************************************************
      *    Record the key in the work file (status 22 = duplicate in
      *    the export file) and reject keys already in a live file
           INITIALIZE KEY-WORK-RECORD
           MOVE EXPORT-REC-TYPE TO KW-REC-TYPE
           MOVE EXPORT-SEQUENCE-NUM TO KW-SEQUENCE

           EVALUATE EXPORT-REC-TYPE
               WHEN 'C'
                   MOVE EXP-CUST-ID TO WS-KEY-CUST-ID
                   MOVE WS-KEY-CUST-ID TO KW-KEY-VALUE
                   IF WS-LIVE-CUST-PRESENT = 'Y'
                       MOVE WS-KEY-CUST-ID TO LIVE-CUST-KEY
                       READ LIVE-CUSTOMER-FILE
                       MOVE WS-LIVE-CUST-STATUS TO WS-LIVE-STATUS
                       MOVE 'CUSTFILE' TO WS-LIVE-DD
                       PERFORM 3250-CHECK-LIVE-READ
                   END-IF
               WHEN 'A'
                   MOVE EXP-ACCT-ID TO WS-KEY-ACCT-ID
                   MOVE WS-KEY-ACCT-ID TO KW-KEY-VALUE
                   IF WS-LIVE-ACCT-PRESENT = 'Y'
                       MOVE WS-KEY-ACCT-ID TO LIVE-ACCT-KEY
                       READ LIVE-ACCOUNT-FILE
                       MOVE WS-LIVE-ACCT-STATUS TO WS-LIVE-STATUS
                       MOVE 'ACCTFILE' TO WS-LIVE-DD
                       PERFORM 3250-CHECK-LIVE-READ
                   END-IF
               WHEN 'X'
                   MOVE EXP-XREF-CARD-NUM TO KW-KEY-VALUE
                   MOVE EXP-XREF-ACCT-ID TO KW-ACCT-ID
                   MOVE EXP-XREF-CUST-ID TO KW-CUST-ID
                   IF WS-LIVE-XREF-PRESENT = 'Y'
                       MOVE EXP-XREF-CARD-NUM TO LIVE-XREF-KEY
                       READ LIVE-XREF-FILE
                       MOVE WS-LIVE-XREF-STATUS TO WS-LIVE-STATUS
                       MOVE 'XREFFILE' TO WS-LIVE-DD
                       PERFORM 3250-CHECK-LIVE-READ
                   END-IF
               WHEN 'T'
                   MOVE EXP-TRAN-ID TO KW-KEY-VALUE
                   IF WS-LIVE-TRAN-PRESENT = 'Y'
                       MOVE EXP-TRAN-ID TO LIVE-TRAN-KEY
                       READ LIVE-TRAN-FILE
                       MOVE WS-LIVE-TRAN-STATUS TO WS-LIVE-STATUS
                       MOVE 'TRANSACT' TO WS-LIVE-DD
                       PERFORM 3250-CHECK-LIVE-READ
                   END-IF
               WHEN 'D'
                   MOVE EXP-CARD-NUM TO KW-KEY-VALUE
                   MOVE EXP-CARD-ACCT-ID TO KW-ACCT-ID
                   IF WS-LIVE-CARD-PRESENT = 'Y'
                       MOVE EXP-CARD-NUM TO LIVE-CARD-KEY
                       READ LIVE-CARD-FILE
                       MOVE WS-LIVE-CARD-STATUS TO WS-LIVE-STATUS
                       MOVE 'CARDFILE' TO WS-LIVE-DD
                       PERFORM 3250-CHECK-LIVE-READ
                   END-IF
           END-EVALUATE

           WRITE KEY-WORK-RECORD
           EVALUATE TRUE
               WHEN WS-KEYWORK-OK
                   CONTINUE
               WHEN WS-KEYWORK-DUPLICATE
                   IF WS-REJECT-REASON = SPACES
                       MOVE 'Duplicate key in export file'
                         TO WS-REJECT-REASON
                   END-IF
               WHEN OTHER
                   DISPLAY 'ERROR: Writing KEY-WORK-FILE, Status: '
                           WS-KEYWORK-STATUS
                   PERFORM 9999-ABEND-PROGRAM
           END-EVALUATE.

      *****************************************************************
       3250-CHECK-LIVE-READ.
      *****************************************************************
           EVALUATE WS-LIVE-STATUS
               WHEN '00'
                   MOVE 'Key already exists in target file'
                     TO WS-REJECT-REASON
               WHEN '23'
                   CONTINUE
               WHEN OTHER
                   DISPLAY 'ERROR: Reading ' WS-LIVE-DD
                           ', Status: ' WS-LIVE-STATUS
                   PERFORM 9999-ABEND-PROGRAM
           END-EVALUATE.

      *****************************************************************
       3500-VALIDATE-REFERENCES.
      *****************************************************************
           MOVE SPACES TO WS-REJECT-REASON

           EVALUATE EXPORT-REC-TYPE
               WHEN 'X'
                   MOVE EXP-XREF-CUST-ID TO WS-KEY-CUST-ID
                   MOVE 'C' TO KW-REC-TYPE
                   MOVE WS-KEY-CUST-ID TO KW-KEY-VALUE
                   PERFORM 3590-READ-KEY-WORK
                   IF NOT WS-KEYWORK-OK
                       MOVE 'Cross-reference customer not in export'
                         TO WS-REJECT-REASON
                   ELSE
                       MOVE EXP-XREF-ACCT-ID TO WS-KEY-ACCT-ID
                       MOVE 'A' TO KW-REC-TYPE
                       MOVE WS-KEY-ACCT-ID TO KW-KEY-VALUE
                       PERFORM 3590-READ-KEY-WORK
                       IF NOT WS-KEYWORK-OK
                           MOVE 'Cross-reference account not in export'
                             TO WS-REJECT-REASON
                       ELSE
                           MOVE 'D' TO KW-REC-TYPE
                           MOVE EXP-XREF-CARD-NUM TO KW-KEY-VALUE
                           PERFORM 3590-READ-KEY-WORK
                           EVALUATE TRUE
                               WHEN NOT WS-KEYWORK-OK
                                   MOVE 'Xref card not in export'
                                     TO WS-REJECT-REASON
                               WHEN KW-ACCT-ID NOT = WS-KEY-ACCT-ID
                                   MOVE 'Xref account differs'
                                     TO WS-REJECT-REASON
                               WHEN OTHER
                                   CONTINUE
                           END-EVALUATE
                       END-IF
                   END-IF
               WHEN 'D'
                   MOVE EXP-CARD-ACCT-ID TO WS-KEY-ACCT-ID
                   MOVE 'A' TO KW-REC-TYPE
                   MOVE WS-KEY-ACCT-ID TO KW-KEY-VALUE
                   PERFORM 3590-READ-KEY-WORK
                   IF NOT WS-KEYWORK-OK
                       MOVE 'Card account not in export file'
                         TO WS-REJECT-REASON
                   ELSE
                       MOVE 'X' TO KW-REC-TYPE
                       MOVE EXP-CARD-NUM TO KW-KEY-VALUE
                       PERFORM 3590-READ-KEY-WORK
                       EVALUATE TRUE
                           WHEN NOT WS-KEYWORK-OK
                               MOVE 'Card has no cross-reference record'
                                 TO WS-REJECT-REASON
                           WHEN KW-ACCT-ID NOT = WS-KEY-ACCT-ID
                               MOVE 'Card account differs from xref'
                                 TO WS-REJECT-REASON
                           WHEN OTHER
                               CONTINUE
                       END-EVALUATE
                   END-IF
               WHEN 'T'
      *            Card must have both a D and an X record in the file
                   MOVE 'D' TO KW-REC-TYPE
                   MOVE EXP-TRAN-CARD-NUM TO KW-KEY-VALUE
                   PERFORM 3590-READ-KEY-WORK
                   IF WS-KEYWORK-OK
                       MOVE 'X' TO KW-REC-TYPE
                       PERFORM 3590-READ-KEY-WORK
                   END-IF
                   IF NOT WS-KEYWORK-OK
                       MOVE 'Transaction card not in export file'
                         TO WS-REJECT-REASON
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF WS-REJECT-REASON NOT = SPACES
               PERFORM 3800-REJECT-RECORD
           END-IF.

      *****************************************************************
       3590-READ-KEY-WORK.
      *****************************************************************
           READ KEY-WORK-FILE
           IF NOT WS-KEYWORK-OK AND NOT WS-KEYWORK-NOT-FOUND
               DISPLAY 'ERROR: Reading KEY-WORK-FILE, Status: '
                       WS-KEYWORK-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF.

      *****************************************************************
       3800-REJECT-RECORD.
      *****************************************************************
           ADD 1 TO WS-REJECT-COUNT
           MOVE FUNCTION CURRENT-DATE TO ERR-TIMESTAMP
           MOVE EXPORT-REC-TYPE TO ERR-RECORD-TYPE
           MOVE EXPORT-SEQUENCE-NUM TO ERR-SEQUENCE
           MOVE WS-REJECT-REASON TO ERR-MESSAGE
           PERFORM 2750-WRITE-ERROR.

      *****************************************************************
       3900-REOPEN-EXPORT-INPUT.
      *****************************************************************
           CLOSE EXPORT-INPUT
           OPEN INPUT EXPORT-INPUT
           IF NOT WS-EXPORT-OK
               DISPLAY 'ERROR: Cannot reopen EXPORT-INPUT, Status: '
                       WS-EXPORT-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF.

      *****************************************************************
       4000-FINALIZE.
      *****************************************************************
           CLOSE EXPORT-INPUT
           CLOSE CUSTOMER-OUTPUT
           CLOSE ACCOUNT-OUTPUT
           CLOSE XREF-OUTPUT
           CLOSE TRANSACTION-OUTPUT
           CLOSE CARD-OUTPUT
           CLOSE ERROR-OUTPUT
           CLOSE KEY-WORK-FILE
           CLOSE LIVE-CUSTOMER-FILE
           CLOSE LIVE-ACCOUNT-FILE
           CLOSE LIVE-XREF-FILE
           CLOSE LIVE-TRAN-FILE
           CLOSE LIVE-CARD-FILE
           
           DISPLAY 'CBIMPORT: Import completed'
           DISPLAY 'CBIMPORT: Total Records Read: '
           WS-TOTAL-RECORDS-READ
           DISPLAY 'CBIMPORT: Customers Imported: ' 
                   WS-CUSTOMER-RECORDS-IMPORTED
           DISPLAY 'CBIMPORT: Accounts Imported: ' 
                   WS-ACCOUNT-RECORDS-IMPORTED
           DISPLAY 'CBIMPORT: XRefs Imported: ' WS-XREF-RECORDS-IMPORTED
           DISPLAY 'CBIMPORT: Transactions Imported: ' 
                   WS-TRAN-RECORDS-IMPORTED
           DISPLAY 'CBIMPORT: Cards Imported: ' WS-CARD-RECORDS-IMPORTED
           DISPLAY 'CBIMPORT: Errors Written: ' WS-ERROR-RECORDS-WRITTEN
           DISPLAY 'CBIMPORT: Unknown Record Types: ' 
                   WS-UNKNOWN-RECORD-TYPE-COUNT
           DISPLAY 'CBIMPORT: Records Validated: ' WS-RECORDS-VALIDATED
           DISPLAY 'CBIMPORT: Records Rejected: ' WS-REJECT-COUNT

           IF WS-REJECT-COUNT > 0
               DISPLAY 'CBIMPORT: Import FAILED - see ERROUT'
               MOVE 8 TO RETURN-CODE
           END-IF.

      *****************************************************************
       9999-ABEND-PROGRAM.
      *****************************************************************
           DISPLAY 'CBIMPORT: ABENDING PROGRAM'
           CALL 'CEE3ABD'.      
      *
      * Ver: CardDemo_v2.0-44-gb6e9c27-254 Date: 2025-10-16 14:07:18 CDT
      *
