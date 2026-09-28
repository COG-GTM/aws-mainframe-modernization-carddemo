      * Driver that passes the INTCALC.jcl PARM to CBACT04C the way
      * z/OS does (halfword length + data).
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUNINTC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PARMS.
           05  PARM-LEN   PIC S9(4) COMP VALUE 10.
           05  PARM-DATE  PIC X(10) VALUE '2022071800'.
       PROCEDURE DIVISION.
           CALL 'CBACT04C' USING PARMS.
           GOBACK.
