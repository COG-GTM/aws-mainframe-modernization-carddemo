       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LOADIDX.
      ******************************************************************
      * Program     : LOADIDX.cbl
      * Application : CBTRN04C test suite
      * Type        : Test utility (off-mainframe runs only)
      * Function    : Loads one of the six reference files read by
      *               CBTRN04C (TRANTYPE, TRANCATG, XREFFILE,
      *               ACCTFILE, TCATBALF, TRANFILE) from a text file
      *               with one record per line into a GnuCOBOL indexed
      *               file. On z/OS these
      *               files are VSAM KSDS built by IDCAMS REPRO
      *               (see app/jcl/TRANTYPE.jcl and friends); this
      *               utility plays that role for the test harness.
      *               Record layouts come from the same copybooks the
      *               production program uses.
      * Usage       : DD_LOADIN=<in> DD_<name>=<out> loadidx <name>
      *               where <name> is TRANTYPE, TRANCATG, XREFFILE,
      *               ACCTFILE, TCATBALF or TRANFILE.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOADIN-FILE ASSIGN TO LOADIN
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  FILE STATUS  IS LOADIN-STATUS.

           SELECT TRANTYPE-FILE ASSIGN TO TRANTYPE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS FD-TRAN-TYPE
                  FILE STATUS  IS TRANTYPE-STATUS.

           SELECT TRANCATG-FILE ASSIGN TO TRANCATG
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS FD-TRAN-CAT-KEY
                  FILE STATUS  IS TRANCATG-STATUS.

           SELECT XREF-FILE ASSIGN TO XREFFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS FD-XREF-CARD-NUM
                  FILE STATUS  IS XREFFILE-STATUS.

           SELECT ACCOUNT-FILE ASSIGN TO ACCTFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS FD-ACCT-ID
                  FILE STATUS  IS ACCTFILE-STATUS.

           SELECT TCATBAL-FILE ASSIGN TO TCATBALF
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS FD-TCATBAL-KEY
                  FILE STATUS  IS TCATBALF-STATUS.

           SELECT TRANSACT-FILE ASSIGN TO TRANFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS FD-TRANS-ID
                  FILE STATUS  IS TRANFILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  LOADIN-FILE.
       01  FD-LOADIN-REC                        PIC X(350).

       FD  TRANTYPE-FILE.
       01  FD-TRANTYPE-REC.
           05 FD-TRAN-TYPE                      PIC X(02).
           05 FD-TRAN-TYPE-DATA                 PIC X(58).

       FD  TRANCATG-FILE.
       01  FD-TRANCATG-REC.
           05 FD-TRAN-CAT-KEY.
              10 FD-TRAN-TYPE-CD                PIC X(02).
              10 FD-TRAN-CAT-CD                 PIC 9(04).
           05 FD-TRAN-CAT-DATA                  PIC X(54).

       FD  XREF-FILE.
       01  FD-XREFFILE-REC.
           05 FD-XREF-CARD-NUM                  PIC X(16).
           05 FD-XREF-DATA                      PIC X(34).

       FD  ACCOUNT-FILE.
       01  FD-ACCTFILE-REC.
           05 FD-ACCT-ID                        PIC 9(11).
           05 FD-ACCT-DATA                      PIC X(289).

       FD  TCATBAL-FILE.
       01  FD-TRAN-CAT-BAL-RECORD.
           05 FD-TCATBAL-KEY.
              10 FD-TCATBAL-ACCT-ID             PIC 9(11).
              10 FD-TCATBAL-TYPE-CD             PIC X(02).
              10 FD-TCATBAL-CD                  PIC 9(04).
           05 FD-TCATBAL-DATA                   PIC X(33).

       FD  TRANSACT-FILE.
       01  FD-TRANFILE-REC.
           05 FD-TRANS-ID                       PIC X(16).
           05 FD-TRANS-DATA                     PIC X(334).

       WORKING-STORAGE SECTION.
       COPY CVTRA03Y.
       COPY CVTRA04Y.
       COPY CVACT03Y.
       COPY CVACT01Y.
       COPY CVTRA01Y.
       COPY CVTRA05Y.

       01  WS-FILE-NAME                         PIC X(08).
       01  LOADIN-STATUS                        PIC X(02).
       01  TRANTYPE-STATUS                      PIC X(02).
       01  TRANCATG-STATUS                      PIC X(02).
       01  XREFFILE-STATUS                      PIC X(02).
       01  ACCTFILE-STATUS                      PIC X(02).
       01  TCATBALF-STATUS                      PIC X(02).
       01  TRANFILE-STATUS                      PIC X(02).
       01  WS-OUT-STATUS                        PIC X(02).
       01  WS-EOF                               PIC X(01) VALUE 'N'.
       01  WS-COUNT                             PIC 9(07) VALUE 0.

       PROCEDURE DIVISION.
           ACCEPT WS-FILE-NAME FROM COMMAND-LINE
           EVALUATE WS-FILE-NAME
               WHEN 'TRANTYPE'
                   OPEN OUTPUT TRANTYPE-FILE
                   MOVE TRANTYPE-STATUS TO WS-OUT-STATUS
               WHEN 'TRANCATG'
                   OPEN OUTPUT TRANCATG-FILE
                   MOVE TRANCATG-STATUS TO WS-OUT-STATUS
               WHEN 'XREFFILE'
                   OPEN OUTPUT XREF-FILE
                   MOVE XREFFILE-STATUS TO WS-OUT-STATUS
               WHEN 'ACCTFILE'
                   OPEN OUTPUT ACCOUNT-FILE
                   MOVE ACCTFILE-STATUS TO WS-OUT-STATUS
               WHEN 'TCATBALF'
                   OPEN OUTPUT TCATBAL-FILE
                   MOVE TCATBALF-STATUS TO WS-OUT-STATUS
               WHEN 'TRANFILE'
                   OPEN OUTPUT TRANSACT-FILE
                   MOVE TRANFILE-STATUS TO WS-OUT-STATUS
               WHEN OTHER
                   DISPLAY 'LOADIDX: UNKNOWN FILE NAME ' WS-FILE-NAME
                   MOVE 12 TO RETURN-CODE
                   GOBACK
           END-EVALUATE
           IF  WS-OUT-STATUS NOT = '00'
               DISPLAY 'LOADIDX: OPEN OUTPUT FAILED ' WS-FILE-NAME
                       ' STATUS ' WS-OUT-STATUS
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           OPEN INPUT LOADIN-FILE
           IF  LOADIN-STATUS NOT = '00'
               DISPLAY 'LOADIDX: OPEN INPUT FAILED STATUS '
                       LOADIN-STATUS
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
               READ LOADIN-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM 1000-WRITE-RECORD
               END-READ
           END-PERFORM
           CLOSE LOADIN-FILE
           EVALUATE WS-FILE-NAME
               WHEN 'TRANTYPE'  CLOSE TRANTYPE-FILE
               WHEN 'TRANCATG'  CLOSE TRANCATG-FILE
               WHEN 'XREFFILE'  CLOSE XREF-FILE
               WHEN 'ACCTFILE'  CLOSE ACCOUNT-FILE
               WHEN 'TCATBALF'  CLOSE TCATBAL-FILE
               WHEN 'TRANFILE'  CLOSE TRANSACT-FILE
           END-EVALUATE
           DISPLAY 'LOADIDX: ' WS-FILE-NAME ' RECORDS LOADED '
                   WS-COUNT
           GOBACK.

       1000-WRITE-RECORD.
           EVALUATE WS-FILE-NAME
               WHEN 'TRANTYPE'
                   MOVE FD-LOADIN-REC (1:60) TO TRAN-TYPE-RECORD
                   WRITE FD-TRANTYPE-REC FROM TRAN-TYPE-RECORD
                   MOVE TRANTYPE-STATUS TO WS-OUT-STATUS
               WHEN 'TRANCATG'
                   MOVE FD-LOADIN-REC (1:60) TO TRAN-CAT-RECORD
                   WRITE FD-TRANCATG-REC FROM TRAN-CAT-RECORD
                   MOVE TRANCATG-STATUS TO WS-OUT-STATUS
               WHEN 'XREFFILE'
                   MOVE FD-LOADIN-REC (1:50) TO CARD-XREF-RECORD
                   WRITE FD-XREFFILE-REC FROM CARD-XREF-RECORD
                   MOVE XREFFILE-STATUS TO WS-OUT-STATUS
               WHEN 'ACCTFILE'
                   MOVE FD-LOADIN-REC (1:300) TO ACCOUNT-RECORD
                   WRITE FD-ACCTFILE-REC FROM ACCOUNT-RECORD
                   MOVE ACCTFILE-STATUS TO WS-OUT-STATUS
               WHEN 'TCATBALF'
                   MOVE FD-LOADIN-REC (1:50) TO TRAN-CAT-BAL-RECORD
                   WRITE FD-TRAN-CAT-BAL-RECORD
                         FROM TRAN-CAT-BAL-RECORD
                   MOVE TCATBALF-STATUS TO WS-OUT-STATUS
               WHEN 'TRANFILE'
                   MOVE FD-LOADIN-REC (1:350) TO TRAN-RECORD
                   WRITE FD-TRANFILE-REC FROM TRAN-RECORD
                   MOVE TRANFILE-STATUS TO WS-OUT-STATUS
           END-EVALUATE
           IF  WS-OUT-STATUS = '00'
               ADD 1 TO WS-COUNT
           ELSE
               DISPLAY 'LOADIDX: WRITE FAILED ' WS-FILE-NAME
                       ' STATUS ' WS-OUT-STATUS
                       ' RECORD ' FD-LOADIN-REC
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           EXIT.
