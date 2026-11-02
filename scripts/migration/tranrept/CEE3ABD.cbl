      * Stand-in for the LE service CEE3ABD that CBTRN03C calls in
      * 9999-ABEND-PROGRAM: ends the run with RC 16 (the Java ABEND RC).
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CEE3ABD.
       DATA DIVISION.
       LINKAGE SECTION.
       01  LK-ABCODE              PIC S9(9) BINARY.
       01  LK-TIMING              PIC S9(9) BINARY.
       PROCEDURE DIVISION USING LK-ABCODE LK-TIMING.
           DISPLAY 'CEE3ABD ABCODE=' LK-ABCODE
           STOP RUN RETURNING 16.
