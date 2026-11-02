      ******************************************************************
      * CEE3ABD - Language Environment abend stand-in: ends the run
      * with RETURN-CODE 16 (the CLI's abend code) after a message.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CEE3ABD.
       DATA DIVISION.
       LINKAGE SECTION.
       01  ABCODE                  PIC S9(9) BINARY.
       01  TIMING                  PIC S9(9) BINARY.
       PROCEDURE DIVISION USING ABCODE, TIMING.
           DISPLAY 'CEE3ABD: USER ABEND U' ABCODE.
           MOVE 16 TO RETURN-CODE.
           STOP RUN.
