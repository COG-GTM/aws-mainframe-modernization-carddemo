       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADKSDS.
      * Golden-master helper: loads a line-sequential ASCII dataset
      * into the GnuCOBOL indexed file CBSTM03B expects for one DD.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO INFILE
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRNX-FILE ASSIGN TO TRNXFILE
                  ORGANIZATION IS INDEXED ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS TRNX-KEY.
           SELECT XREF-FILE ASSIGN TO XREFFILE
                  ORGANIZATION IS INDEXED ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS XREF-KEY.
           SELECT CUST-FILE ASSIGN TO CUSTFILE
                  ORGANIZATION IS INDEXED ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS CUST-KEY.
           SELECT ACCT-FILE ASSIGN TO ACCTFILE
                  ORGANIZATION IS INDEXED ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS ACCT-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC                 PIC X(500).
       FD  TRNX-FILE.
       01  TRNX-REC.
           05 TRNX-KEY            PIC X(32).
           05 FILLER              PIC X(318).
       FD  XREF-FILE.
       01  XREF-REC.
           05 XREF-KEY            PIC X(16).
           05 FILLER              PIC X(34).
       FD  CUST-FILE.
       01  CUST-REC.
           05 CUST-KEY            PIC X(09).
           05 FILLER              PIC X(491).
       FD  ACCT-FILE.
       01  ACCT-REC.
           05 ACCT-KEY            PIC 9(11).
           05 FILLER              PIC X(289).
       WORKING-STORAGE SECTION.
       01  WS-WHICH               PIC X(08).
       01  WS-EOF                 PIC X VALUE 'N'.
       PROCEDURE DIVISION.
           ACCEPT WS-WHICH FROM COMMAND-LINE.
           OPEN INPUT IN-FILE.
           EVALUATE WS-WHICH
             WHEN 'TRNXFILE' OPEN OUTPUT TRNX-FILE
             WHEN 'XREFFILE' OPEN OUTPUT XREF-FILE
             WHEN 'CUSTFILE' OPEN OUTPUT CUST-FILE
             WHEN 'ACCTFILE' OPEN OUTPUT ACCT-FILE
           END-EVALUATE.
           PERFORM UNTIL WS-EOF = 'Y'
               MOVE SPACES TO IN-REC
               READ IN-FILE AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                 EVALUATE WS-WHICH
                   WHEN 'TRNXFILE' WRITE TRNX-REC FROM IN-REC
                   WHEN 'XREFFILE' WRITE XREF-REC FROM IN-REC
                   WHEN 'CUSTFILE' WRITE CUST-REC FROM IN-REC
                   WHEN 'ACCTFILE' WRITE ACCT-REC FROM IN-REC
                 END-EVALUATE
               END-READ
           END-PERFORM.
           CLOSE IN-FILE.
           EVALUATE WS-WHICH
             WHEN 'TRNXFILE' CLOSE TRNX-FILE
             WHEN 'XREFFILE' CLOSE XREF-FILE
             WHEN 'CUSTFILE' CLOSE CUST-FILE
             WHEN 'ACCTFILE' CLOSE ACCT-FILE
           END-EVALUATE.
           STOP RUN.
