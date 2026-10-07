      ******************************************************************
      * CEE3ABD - stand-in for the Language Environment abend service.
      * Mainframe: CEE3ABD(abcode, timing) terminates the enclave with
      * user abend U<abcode>.  Here: print the code and STOP RUN with
      * the abend code as the process return code so run scripts fail.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CEE3ABD.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RC                   PIC S9(9) BINARY.
       LINKAGE SECTION.
       01  ABCODE                  PIC S9(9) BINARY.
       01  TIMING                  PIC S9(9) BINARY.
       PROCEDURE DIVISION USING ABCODE, TIMING.
           DISPLAY 'CEE3ABD: USER ABEND U' ABCODE ' TIMING=' TIMING.
           MOVE ABCODE TO WS-RC.
           MOVE WS-RC TO RETURN-CODE.
           STOP RUN.
