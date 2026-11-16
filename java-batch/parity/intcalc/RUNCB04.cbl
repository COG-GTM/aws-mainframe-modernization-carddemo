      ******************************************************************
      * Program     : RUNCB04.CBL
      * Purpose     : ORACLE HARNESS DRIVER - NOT PART OF THE LEGACY
      *               APPLICATION.
      *               Reproduces the z/OS JCL invocation
      *                   //STEP15 EXEC PGM=CBACT04C,PARM='2022071800'
      *               (app/jcl/INTCALC.jcl) by building the halfword
      *               length prefixed EXTERNAL-PARMS area that
      *               CBACT04C declares in its LINKAGE SECTION and
      *               CALLing the unmodified legacy program with it.
      *
      *               The run date is taken from argv[1] so the harness
      *               can be re-run for other PARM values.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RUNCB04.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  EXTERNAL-PARMS.
           05  PARM-LENGTH         PIC S9(04) COMP VALUE 10.
           05  PARM-DATE           PIC X(10)  VALUE SPACES.
       PROCEDURE DIVISION.
           ACCEPT PARM-DATE FROM ARGUMENT-VALUE
           DISPLAY 'RUNCB04 PARM-DATE=' PARM-DATE
           CALL 'CBACT04C' USING EXTERNAL-PARMS
           GOBACK.
