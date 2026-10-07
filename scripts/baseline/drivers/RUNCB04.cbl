      ******************************************************************
      * RUNCB04 - batch driver that invokes CBACT04C the way JCL does:
      *   //STEP15 EXEC PGM=CBACT04C,PARM='2022071800'   (INTCALC.jcl)
      * z/OS passes PARM as a halfword length followed by the text;
      * CBACT04C declares LINKAGE EXTERNAL-PARMS (PARM-LENGTH S9(4)
      * COMP, PARM-DATE X(10)) and reads it with
      * PROCEDURE DIVISION USING EXTERNAL-PARMS.
      *
      * Pattern harvested from
      *   origin/devin/1790617863-batch @ e9658ca
      *     aws/batch/golden/RUNINTC.cbl
      *   origin/devin/1785783419-cbact04c-java-migration @ 2d5b9c4
      *     modernized/interest-calculator/oracle/RUNCB04.cbl
      * The PARM value is taken from the command line so the runner can
      * record it; it defaults to the JCL literal.
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
           DISPLAY 'RUNCB04: PARM=' PARM-DATE.
           CALL 'CBACT04C' USING EXTERNAL-PARMS.
           GOBACK.
