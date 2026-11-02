      * Builds the KSDS stand-ins CBTRN03C reads (CARDXREF, TRANTYPE,
      * TRANCATG) as GnuCOBOL indexed files from line-sequential
      * extracts. Usage: IDXLOAD CARDXREF|TRANTYPE|TRANCATG, with
      * DD_INFILE and DD_<name> set.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IDXLOAD.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO INFILE
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT XREF-FILE ASSIGN TO CARDXREF
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS XREF-KEY.
           SELECT TYPE-FILE ASSIGN TO TRANTYPE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS TYPE-KEY.
           SELECT CATG-FILE ASSIGN TO TRANCATG
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CATG-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC                 PIC X(60).
       FD  XREF-FILE.
       01  XREF-REC.
           05  XREF-KEY           PIC X(16).
           05  FILLER             PIC X(34).
       FD  TYPE-FILE.
       01  TYPE-REC.
           05  TYPE-KEY           PIC X(02).
           05  FILLER             PIC X(58).
       FD  CATG-FILE.
       01  CATG-REC.
           05  CATG-KEY           PIC X(06).
           05  FILLER             PIC X(54).
       WORKING-STORAGE SECTION.
       01  WS-WHICH               PIC X(08).
       01  WS-EOF                 PIC X VALUE 'N'.
       01  WS-COUNT               PIC 9(06) VALUE 0.
       PROCEDURE DIVISION.
           ACCEPT WS-WHICH FROM COMMAND-LINE
           OPEN INPUT IN-FILE
           EVALUATE WS-WHICH
             WHEN 'CARDXREF' OPEN OUTPUT XREF-FILE
             WHEN 'TRANTYPE' OPEN OUTPUT TYPE-FILE
             WHEN 'TRANCATG' OPEN OUTPUT CATG-FILE
             WHEN OTHER
               DISPLAY 'usage: IDXLOAD CARDXREF|TRANTYPE|TRANCATG'
               STOP RUN RETURNING 12
           END-EVALUATE
           PERFORM UNTIL WS-EOF = 'Y'
             MOVE SPACES TO IN-REC
             READ IN-FILE
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                 ADD 1 TO WS-COUNT
                 EVALUATE WS-WHICH
                   WHEN 'CARDXREF'
                     MOVE IN-REC(1:50) TO XREF-REC
                     WRITE XREF-REC
                   WHEN 'TRANTYPE'
                     MOVE IN-REC TO TYPE-REC
                     WRITE TYPE-REC
                   WHEN 'TRANCATG'
                     MOVE IN-REC TO CATG-REC
                     WRITE CATG-REC
                 END-EVALUATE
             END-READ
           END-PERFORM
           CLOSE IN-FILE
           EVALUATE WS-WHICH
             WHEN 'CARDXREF' CLOSE XREF-FILE
             WHEN 'TRANTYPE' CLOSE TYPE-FILE
             WHEN 'TRANCATG' CLOSE CATG-FILE
           END-EVALUATE
           DISPLAY WS-WHICH ' records loaded: ' WS-COUNT
           STOP RUN.
