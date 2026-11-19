      ******************************************************************
      * Test driver for CBACT04C.  On z/OS the JCL passes
      * PARM='2022071800' which LE presents as a halfword length plus
      * text in the LINKAGE SECTION.  GnuCOBOL main programs cannot
      * receive a PARM that way, so this driver builds the same
      * structure from the command line and CALLs CBACT04C.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUNACT04.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ARG                 PIC X(10).
       01  EXTERNAL-PARMS.
           05  PARM-LENGTH        PIC S9(04) COMP.
           05  PARM-DATE          PIC X(10).
       PROCEDURE DIVISION.
           ACCEPT WS-ARG FROM COMMAND-LINE
           MOVE 10     TO PARM-LENGTH
           MOVE WS-ARG TO PARM-DATE
           CALL 'CBACT04C' USING EXTERNAL-PARMS
           STOP RUN.
