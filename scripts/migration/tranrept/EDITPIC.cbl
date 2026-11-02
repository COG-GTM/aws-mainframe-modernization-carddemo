      * Prints CVTRA07Y's two edited amount pictures for a list of
      * S9(09)V99 values read from DD_AMOUNTS (one per line, sign and
      * 11 digits, e.g. -00000004788): value|-ZZZ...|+ZZZ...
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDITPIC.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VALUES-FILE ASSIGN TO AMOUNTS
                  ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  VALUES-FILE.
       01  VALUES-REC.
           05  IN-SIGN            PIC X.
           05  IN-DIGITS          PIC 9(09)V99.
       WORKING-STORAGE SECTION.
       01  WS-EOF                 PIC X VALUE 'N'.
       01  WS-AMT                 PIC S9(09)V99.
       01  WS-DETAIL              PIC -ZZZ,ZZZ,ZZZ.ZZ.
       01  WS-TOTAL               PIC +ZZZ,ZZZ,ZZZ.ZZ.
       PROCEDURE DIVISION.
           OPEN INPUT VALUES-FILE
           PERFORM UNTIL WS-EOF = 'Y'
             READ VALUES-FILE
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                 MOVE IN-DIGITS TO WS-AMT
                 IF IN-SIGN = '-'
                   COMPUTE WS-AMT = 0 - WS-AMT
                 END-IF
                 MOVE WS-AMT TO WS-DETAIL
                 MOVE WS-AMT TO WS-TOTAL
                 DISPLAY VALUES-REC '|' WS-DETAIL '|' WS-TOTAL '|'
             END-READ
           END-PERFORM
           CLOSE VALUES-FILE
           STOP RUN.
