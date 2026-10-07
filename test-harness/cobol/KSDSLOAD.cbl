      ******************************************************************
      * KSDSLOAD - load the CardDemo ASCII sample files into GnuCOBOL
      * indexed files so the programs' VSAM KSDS reads behave as on the
      * mainframe (keyed random READ, sequential READ in key order).
      * Records may arrive in any order (ACCESS DYNAMIC); the KSDS keeps
      * them in key sequence like IDCAMS REPRO into a KSDS does.
      *
      * Usage:  KSDSLOAD <ACCT|XREF|CUST|CARD|TRAN>
      *   input : env LOADIN   (line-sequential ASCII sample file;
      *                         short lines are space padded)
      *   output: env DD_ACCTFILE / DD_XREFFILE / DD_CUSTFILE /
      *           DD_CARDFILE / DD_TRANFILE   (indexed file to create)
      * Keys and record lengths match the FDs in CBACT01C / CBTRN01C.
      * TRAN with an empty input creates an empty indexed TRANFILE
      * (CBTRN01C opens it but never reads it).
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. KSDSLOAD.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOADIN-FILE ASSIGN TO LOADIN
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS LOADIN-STATUS.
           SELECT ACCTFILE-FILE ASSIGN TO ACCTFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS FD-ACCT-ID
                  FILE STATUS IS OUT-STATUS.
           SELECT XREFFILE-FILE ASSIGN TO XREFFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS FD-XREF-CARD-NUM
                  FILE STATUS IS OUT-STATUS.
           SELECT CUSTFILE-FILE ASSIGN TO CUSTFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS FD-CUST-ID
                  FILE STATUS IS OUT-STATUS.
           SELECT CARDFILE-FILE ASSIGN TO CARDFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS FD-CARD-NUM
                  FILE STATUS IS OUT-STATUS.
           SELECT TRANFILE-FILE ASSIGN TO TRANFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS FD-TRANS-ID
                  FILE STATUS IS OUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  LOADIN-FILE.
       01  LOADIN-REC                           PIC X(500).
       FD  ACCTFILE-FILE.
       01  FD-ACCTFILE-REC.
           05 FD-ACCT-ID                        PIC 9(11).
           05 FD-ACCT-DATA                      PIC X(289).
       FD  XREFFILE-FILE.
       01  FD-XREFFILE-REC.
           05 FD-XREF-CARD-NUM                  PIC X(16).
           05 FD-XREF-DATA                      PIC X(34).
       FD  CUSTFILE-FILE.
       01  FD-CUSTFILE-REC.
           05 FD-CUST-ID                        PIC 9(09).
           05 FD-CUST-DATA                      PIC X(491).
       FD  CARDFILE-FILE.
       01  FD-CARDFILE-REC.
           05 FD-CARD-NUM                       PIC X(16).
           05 FD-CARD-DATA                      PIC X(134).
       FD  TRANFILE-FILE.
       01  FD-TRANFILE-REC.
           05 FD-TRANS-ID                       PIC X(16).
           05 FD-TRAN-DATA                      PIC X(334).
       WORKING-STORAGE SECTION.
       01  LOADIN-STATUS                        PIC XX.
       01  OUT-STATUS                           PIC XX.
       01  WS-WHICH                             PIC X(4).
       01  WS-EOF                               PIC X VALUE 'N'.
       01  WS-COUNT                             PIC 9(7) VALUE 0.
       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-WHICH FROM COMMAND-LINE.
           OPEN INPUT LOADIN-FILE.
           IF LOADIN-STATUS NOT = '00'
              DISPLAY 'KSDSLOAD: CANNOT OPEN LOADIN, STATUS '
                      LOADIN-STATUS
              MOVE 12 TO RETURN-CODE
              STOP RUN
           END-IF.
           EVALUATE WS-WHICH
             WHEN 'ACCT' OPEN OUTPUT ACCTFILE-FILE
             WHEN 'XREF' OPEN OUTPUT XREFFILE-FILE
             WHEN 'CUST' OPEN OUTPUT CUSTFILE-FILE
             WHEN 'CARD' OPEN OUTPUT CARDFILE-FILE
             WHEN 'TRAN' OPEN OUTPUT TRANFILE-FILE
             WHEN OTHER
               DISPLAY 'KSDSLOAD: UNKNOWN FILE TYPE ' WS-WHICH
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-EVALUATE.
           IF OUT-STATUS NOT = '00'
              DISPLAY 'KSDSLOAD: CANNOT OPEN OUTPUT, STATUS ' OUT-STATUS
              MOVE 12 TO RETURN-CODE
              STOP RUN
           END-IF.
           PERFORM UNTIL WS-EOF = 'Y'
              MOVE SPACES TO LOADIN-REC
              READ LOADIN-FILE
                 AT END MOVE 'Y' TO WS-EOF
                 NOT AT END PERFORM WRITE-ONE
              END-READ
           END-PERFORM.
           CLOSE LOADIN-FILE.
           EVALUATE WS-WHICH
             WHEN 'ACCT' CLOSE ACCTFILE-FILE
             WHEN 'XREF' CLOSE XREFFILE-FILE
             WHEN 'CUST' CLOSE CUSTFILE-FILE
             WHEN 'CARD' CLOSE CARDFILE-FILE
             WHEN 'TRAN' CLOSE TRANFILE-FILE
           END-EVALUATE.
           DISPLAY 'KSDSLOAD: ' WS-WHICH ' RECORDS LOADED: ' WS-COUNT.
           GOBACK.
       WRITE-ONE.
           EVALUATE WS-WHICH
             WHEN 'ACCT'
               MOVE LOADIN-REC(1:300) TO FD-ACCTFILE-REC
               WRITE FD-ACCTFILE-REC
             WHEN 'XREF'
               MOVE LOADIN-REC(1:50)  TO FD-XREFFILE-REC
               WRITE FD-XREFFILE-REC
             WHEN 'CUST'
               MOVE LOADIN-REC(1:500) TO FD-CUSTFILE-REC
               WRITE FD-CUSTFILE-REC
             WHEN 'CARD'
               MOVE LOADIN-REC(1:150) TO FD-CARDFILE-REC
               WRITE FD-CARDFILE-REC
             WHEN 'TRAN'
               MOVE LOADIN-REC(1:350) TO FD-TRANFILE-REC
               WRITE FD-TRANFILE-REC
           END-EVALUATE.
           IF OUT-STATUS NOT = '00'
              DISPLAY 'KSDSLOAD: WRITE FAILED, STATUS ' OUT-STATUS
                      ' RECORD ' WS-COUNT
              MOVE 12 TO RETURN-CODE
              STOP RUN
           END-IF.
           ADD 1 TO WS-COUNT.
