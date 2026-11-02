      ******************************************************************
      * RUNCB04 - calls CBACT04C the way //STEP15 EXEC PGM=CBACT04C,
      * PARM='2022071800' does: a halfword length followed by the text
      * (CBACT04C LINKAGE EXTERNAL-PARMS). PARM comes from the command
      * line and defaults to the INTCALC.jcl literal.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUNCB04.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ARG                  PIC X(10) VALUE SPACES.
       01  EXTERNAL-PARMS.
           05  PARM-LENGTH         PIC S9(04) COMP VALUE 10.
           05  PARM-DATE           PIC X(10) VALUE '2022071800'.
       PROCEDURE DIVISION.
           ACCEPT WS-ARG FROM COMMAND-LINE.
           IF WS-ARG NOT = SPACES
              MOVE WS-ARG TO PARM-DATE
           END-IF.
           CALL 'CBACT04C' USING EXTERNAL-PARMS.
           GOBACK.
