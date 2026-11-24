      *****************************************************************
      * Stand-in for the Language Environment abend service CEE3ABD.  *
      * Ends the run unit with return code 16 so the harness can see  *
      * that a batch program abended.                                 *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CEE3ABD.
       DATA DIVISION.
       LINKAGE SECTION.
       01  LK-ABCODE                   PIC S9(9) BINARY.
       01  LK-TIMING                   PIC S9(9) BINARY.
       PROCEDURE DIVISION USING OPTIONAL LK-ABCODE
                                OPTIONAL LK-TIMING.
           IF ADDRESS OF LK-ABCODE = NULL
               DISPLAY 'CEE3ABD: ABEND REQUESTED'
           ELSE
               DISPLAY 'CEE3ABD: ABEND CODE ' LK-ABCODE
           END-IF
           STOP RUN RETURNING 16.
