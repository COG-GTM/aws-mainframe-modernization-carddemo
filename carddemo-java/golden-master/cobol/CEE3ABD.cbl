      *****************************************************************
      * Stand-in for the Language Environment CEE3ABD abend service.  *
      * CBTRN02C, CBACT04C and CBTRN03C pass an abend code and timing *
      * flag; CBSTM03A calls it with no arguments. Either way the run *
      * unit ends with RC=16, the harness's stand-in for an abend.    *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CEE3ABD.
       DATA DIVISION.
       LINKAGE SECTION.
       01  LK-ABCODE                   PIC S9(9) BINARY.
       01  LK-TIMING                   PIC S9(9) BINARY.
       PROCEDURE DIVISION USING LK-ABCODE LK-TIMING.
           IF ADDRESS OF LK-ABCODE NOT = NULL
               DISPLAY 'CEE3ABD: ABEND CODE ' LK-ABCODE
           ELSE
               DISPLAY 'CEE3ABD: ABEND'
           END-IF
           MOVE 16 TO RETURN-CODE
           STOP RUN.
