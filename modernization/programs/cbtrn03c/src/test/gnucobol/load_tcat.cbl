       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADtcat.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO INFILE
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO OUTFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS OUT-KEY
                  FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD IN-FILE.
       01 IN-REC PIC X(60).
       FD OUT-FILE.
       01 OUT-REC.
          05 OUT-KEY PIC X(6).
          05 OUT-DATA PIC X(54).
       WORKING-STORAGE SECTION.
       01 WS-ST PIC XX.
       01 WS-EOF PIC X VALUE 'N'.
       PROCEDURE DIVISION.
           OPEN INPUT IN-FILE OUTPUT OUT-FILE
           PERFORM UNTIL WS-EOF = 'Y'
             READ IN-FILE AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                 MOVE IN-REC TO OUT-REC
                 WRITE OUT-REC
                 IF WS-ST NOT = '00' DISPLAY 'WRITE ' WS-ST
                 END-IF
             END-READ
           END-PERFORM
           CLOSE IN-FILE OUT-FILE
           GOBACK.
