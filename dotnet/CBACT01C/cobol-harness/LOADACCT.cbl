      ******************************************************************
      * LOADACCT - builds the ACCTFILE KSDS (indexed) from the
      *            300-byte ASCII sample extract, like IDCAMS REPRO.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADACCT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO ACCTSEQ
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS IN-STATUS.
           SELECT KSDS-FILE ASSIGN TO ACCTFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS KSDS-ACCT-ID
                  FILE STATUS  IS KSDS-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC                  PIC X(300).
       FD  KSDS-FILE.
       01  KSDS-REC.
           05 KSDS-ACCT-ID         PIC 9(11).
           05 KSDS-ACCT-DATA       PIC X(289).
       WORKING-STORAGE SECTION.
       01  IN-STATUS               PIC XX.
       01  KSDS-STATUS             PIC XX.
       01  WS-COUNT                PIC 9(6) VALUE 0.
       PROCEDURE DIVISION.
           OPEN INPUT IN-FILE
           OPEN OUTPUT KSDS-FILE
           IF IN-STATUS NOT = '00'
               DISPLAY 'CANNOT OPEN ACCTSEQ ' IN-STATUS
               STOP RUN RETURNING 8
           END-IF
           PERFORM UNTIL IN-STATUS NOT = '00'
               READ IN-FILE
                   AT END CONTINUE
                   NOT AT END
                       MOVE IN-REC TO KSDS-REC
                       WRITE KSDS-REC
                       IF KSDS-STATUS NOT = '00'
                           DISPLAY 'LOAD ERROR ' KSDS-STATUS
                           STOP RUN RETURNING 8
                       END-IF
                       ADD 1 TO WS-COUNT
               END-READ
           END-PERFORM
           CLOSE IN-FILE KSDS-FILE
           DISPLAY 'LOADACCT: ' WS-COUNT ' RECORDS LOADED'
           STOP RUN.
