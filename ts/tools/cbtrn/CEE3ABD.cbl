       IDENTIFICATION DIVISION.
       PROGRAM-ID. CEE3ABD.
      * Stand-in for the LE abend service: end the run with RC 999.
       DATA DIVISION.
       LINKAGE SECTION.
       01  ABCODE                  PIC S9(9) BINARY.
       01  TIMING                  PIC S9(9) BINARY.
       PROCEDURE DIVISION USING ABCODE TIMING.
           DISPLAY 'CEE3ABD ABCODE=' ABCODE
           STOP RUN RETURNING 999.
