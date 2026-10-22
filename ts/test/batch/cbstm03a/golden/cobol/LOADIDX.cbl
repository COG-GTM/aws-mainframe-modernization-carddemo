       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADIDX.
      * Golden-file helper: loads the line-sequential CREASTMT inputs
      * into GnuCOBOL indexed files (stand-ins for the VSAM KSDSs).
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRNX-IN ASSIGN TO TRNXIN
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT XREF-IN ASSIGN TO XREFIN
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CUST-IN ASSIGN TO CUSTIN
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ACCT-IN ASSIGN TO ACCTIN
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRNX-OUT ASSIGN TO TRNXFILE
                  ORGANIZATION IS INDEXED ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS TO-KEY.
           SELECT XREF-OUT ASSIGN TO XREFFILE
                  ORGANIZATION IS INDEXED ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS XO-KEY.
           SELECT CUST-OUT ASSIGN TO CUSTFILE
                  ORGANIZATION IS INDEXED ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS CO-KEY.
           SELECT ACCT-OUT ASSIGN TO ACCTFILE
                  ORGANIZATION IS INDEXED ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS AO-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD  TRNX-IN.
       01  TI-REC                 PIC X(350).
       FD  XREF-IN.
       01  XI-REC                 PIC X(50).
       FD  CUST-IN.
       01  CI-REC                 PIC X(500).
       FD  ACCT-IN.
       01  AI-REC                 PIC X(300).
       FD  TRNX-OUT.
       01  TO-REC.
           05 TO-KEY              PIC X(32).
           05 FILLER              PIC X(318).
       FD  XREF-OUT.
       01  XO-REC.
           05 XO-KEY              PIC X(16).
           05 FILLER              PIC X(34).
       FD  CUST-OUT.
       01  CO-REC.
           05 CO-KEY              PIC X(09).
           05 FILLER              PIC X(491).
       FD  ACCT-OUT.
       01  AO-REC.
           05 AO-KEY              PIC X(11).
           05 FILLER              PIC X(289).
       WORKING-STORAGE SECTION.
       01  WS-EOF                 PIC X VALUE 'N'.
       PROCEDURE DIVISION.
           OPEN INPUT TRNX-IN OUTPUT TRNX-OUT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ TRNX-IN AT END MOVE 'Y' TO WS-EOF
               NOT AT END WRITE TO-REC FROM TI-REC END-READ
           END-PERFORM
           CLOSE TRNX-IN TRNX-OUT
           OPEN INPUT XREF-IN OUTPUT XREF-OUT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ XREF-IN AT END MOVE 'Y' TO WS-EOF
               NOT AT END WRITE XO-REC FROM XI-REC END-READ
           END-PERFORM
           CLOSE XREF-IN XREF-OUT
           OPEN INPUT CUST-IN OUTPUT CUST-OUT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ CUST-IN AT END MOVE 'Y' TO WS-EOF
               NOT AT END WRITE CO-REC FROM CI-REC END-READ
           END-PERFORM
           CLOSE CUST-IN CUST-OUT
           OPEN INPUT ACCT-IN OUTPUT ACCT-OUT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ ACCT-IN AT END MOVE 'Y' TO WS-EOF
               NOT AT END WRITE AO-REC FROM AI-REC END-READ
           END-PERFORM
           CLOSE ACCT-IN ACCT-OUT
           STOP RUN.
