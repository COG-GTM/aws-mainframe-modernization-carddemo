      ******************************************************************
      * Program     : UNLDACCT.CBL
      * Purpose     : ORACLE HARNESS UTILITY - NOT PART OF THE LEGACY
      *               APPLICATION.
      *               Unloads the ACCTFILE INDEXED file (in ascending
      *               key order) back to a fixed-width ASCII sequential
      *               file so the post-run account master produced by
      *               CBACT04C can be diffed against the Java
      *               implementation's output.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    UNLDACCT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO ACCTFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS FD-ACCT-ID
                  FILE STATUS  IS ACCTFILE-STATUS.

           SELECT ACCTOUT-FILE ASSIGN TO ACCTOUT
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS ACCTOUT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       01  FD-ACCTFILE-REC.
           05 FD-ACCT-ID                        PIC 9(11).
           05 FD-ACCT-DATA                      PIC X(289).
       FD  ACCTOUT-FILE.
       01  ACCTOUT-REC                          PIC X(300).

       WORKING-STORAGE SECTION.
       01  ACCTFILE-STATUS         PIC XX.
       01  ACCTOUT-STATUS          PIC XX.
       01  WS-EOF                  PIC X VALUE 'N'.
       01  WS-COUNT                PIC 9(05) VALUE 0.

       PROCEDURE DIVISION.
           OPEN INPUT ACCOUNT-FILE
           OPEN OUTPUT ACCTOUT-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ ACCOUNT-FILE NEXT RECORD
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE FD-ACCTFILE-REC TO ACCTOUT-REC
                       WRITE ACCTOUT-REC
                       ADD 1 TO WS-COUNT
               END-READ
           END-PERFORM
           CLOSE ACCOUNT-FILE
           CLOSE ACCTOUT-FILE
           DISPLAY 'UNLDACCT RECORDS UNLOADED: ' WS-COUNT
           GOBACK.
