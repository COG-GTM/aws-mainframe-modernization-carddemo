      *****************************************************************
      * Passes a JCL EXEC PARM to a batch program the way z/OS does:  *
      * a halfword length followed by the PARM text.                  *
      *   usage: <exe> <program-id> <parm-text>                       *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JCLPARM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM                  PIC X(8).
       01  WS-PARM-TEXT                PIC X(100).
       01  WS-PARM-AREA.
           05  WS-PARM-LENGTH          PIC S9(4) COMP.
           05  WS-PARM-DATA            PIC X(100).
       PROCEDURE DIVISION.
           ACCEPT WS-PROGRAM   FROM ARGUMENT-VALUE
           ACCEPT WS-PARM-TEXT FROM ARGUMENT-VALUE
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-PARM-TEXT TRAILING))
             TO WS-PARM-LENGTH
           MOVE WS-PARM-TEXT TO WS-PARM-DATA
           CALL WS-PROGRAM USING WS-PARM-AREA
           STOP RUN.
