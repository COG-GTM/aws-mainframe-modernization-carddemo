      ******************************************************************
      * Test double for the IBM Language Environment CEE3ABD service
      * (request abnormal termination).  On z/OS the step ends with an
      * ABEND (e.g. U0999); under GnuCOBOL we print the request and end
      * the run unit with RC=16 so tests can assert on it.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CEE3ABD.
       DATA DIVISION.
       LINKAGE SECTION.
       01  LK-ABCODE         PIC S9(9) BINARY.
       01  LK-TIMING         PIC S9(9) BINARY.
       PROCEDURE DIVISION USING OPTIONAL LK-ABCODE
                                OPTIONAL LK-TIMING.
           IF ADDRESS OF LK-ABCODE NOT = NULL
              DISPLAY 'CEE3ABD STUB: ABEND REQUESTED, CODE=' LK-ABCODE
           ELSE
              DISPLAY 'CEE3ABD STUB: ABEND REQUESTED'
           END-IF
           MOVE 16 TO RETURN-CODE
           STOP RUN.
