      ******************************************************************
      * LOADKSDS - parity harness utility, NOT part of the legacy app.
      * Loads a fixed-width ASCII sample (line sequential) into a
      * GnuCOBOL INDEXED file shaped like the KSDS CBSTM03B opens.
      * Usage: LOADKSDS <TRNX|XREF|CUST|ACCT>  (SEQIN = input path,
      *        TRNXFILE/XREFFILE/CUSTFILE/ACCTFILE = output path)
      * Key layouts copied from the FDs of app/cbl/CBSTM03B.CBL.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LOADKSDS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO SEQIN
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS SEQ-STATUS.
           SELECT TRNX-FILE ASSIGN TO TRNXFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS FD-TRNXS-ID
                  FILE STATUS  IS OUT-STATUS.
           SELECT XREF-FILE ASSIGN TO XREFFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS FD-XREF-CARD-NUM
                  FILE STATUS  IS OUT-STATUS.
           SELECT CUST-FILE ASSIGN TO CUSTFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS FD-CUST-ID
                  FILE STATUS  IS OUT-STATUS.
           SELECT ACCT-FILE ASSIGN TO ACCTFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS FD-ACCT-ID
                  FILE STATUS  IS OUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  SEQ-FILE.
       01  SEQ-REC                          PIC X(500).
       FD  TRNX-FILE.
       01  FD-TRNXFILE-REC.
           05 FD-TRNXS-ID                   PIC X(32).
           05 FILLER                        PIC X(318).
       FD  XREF-FILE.
       01  FD-XREFFILE-REC.
           05 FD-XREF-CARD-NUM              PIC X(16).
           05 FILLER                        PIC X(34).
       FD  CUST-FILE.
       01  FD-CUSTFILE-REC.
           05 FD-CUST-ID                    PIC X(09).
           05 FILLER                        PIC X(491).
       FD  ACCT-FILE.
       01  FD-ACCTFILE-REC.
           05 FD-ACCT-ID                    PIC 9(11).
           05 FILLER                        PIC X(289).
       WORKING-STORAGE SECTION.
       01  SEQ-STATUS                       PIC XX.
       01  OUT-STATUS                       PIC XX.
       01  WS-KIND                          PIC X(04).
       01  WS-COUNT                         PIC 9(06) VALUE 0.
       PROCEDURE DIVISION.
           ACCEPT WS-KIND FROM COMMAND-LINE.
           OPEN INPUT SEQ-FILE.
           EVALUATE WS-KIND
             WHEN 'TRNX' OPEN OUTPUT TRNX-FILE
             WHEN 'XREF' OPEN OUTPUT XREF-FILE
             WHEN 'CUST' OPEN OUTPUT CUST-FILE
             WHEN 'ACCT' OPEN OUTPUT ACCT-FILE
           END-EVALUATE.
           IF OUT-STATUS NOT = '00'
               DISPLAY 'OPEN ' WS-KIND ' FAILED ' OUT-STATUS
               STOP RUN RETURNING 8
           END-IF.
           PERFORM UNTIL SEQ-STATUS = '10'
               MOVE SPACES TO SEQ-REC
               READ SEQ-FILE
                 AT END CONTINUE
                 NOT AT END
                   EVALUATE WS-KIND
                     WHEN 'TRNX' WRITE FD-TRNXFILE-REC FROM SEQ-REC
                     WHEN 'XREF' WRITE FD-XREFFILE-REC FROM SEQ-REC
                     WHEN 'CUST' WRITE FD-CUSTFILE-REC FROM SEQ-REC
                     WHEN 'ACCT' WRITE FD-ACCTFILE-REC FROM SEQ-REC
                   END-EVALUATE
                   IF OUT-STATUS NOT = '00'
                       DISPLAY 'WRITE ' WS-KIND ' FAILED ' OUT-STATUS
                       STOP RUN RETURNING 8
                   END-IF
                   ADD 1 TO WS-COUNT
               END-READ
           END-PERFORM.
           CLOSE SEQ-FILE TRNX-FILE XREF-FILE CUST-FILE ACCT-FILE.
           DISPLAY 'LOADKSDS ' WS-KIND ' RECORDS: ' WS-COUNT.
           STOP RUN.
