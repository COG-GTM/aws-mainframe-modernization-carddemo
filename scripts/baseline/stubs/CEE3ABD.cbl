      ******************************************************************
      * CEE3ABD - GnuCOBOL stand-in for the z/OS Language Environment
      *           abend service.
      *
      * Harvested from origin/devin/cobol-safety-net @ 1c845c7
      *   test-harness/cobol/CEE3ABD.cbl
      * and hardened for callers that pass no arguments.
      *
      * Mainframe semantics: CEE3ABD(abcode, timing) terminates the
      * enclave with user abend U<abcode>; TIMING=1 requests a dump.
      * CardDemo callers:
      *   CBACT01C/02C/03C/04C, CBCUS01C, CBTRN01C/02C/03C
      *       CALL 'CEE3ABD' USING ABCODE, TIMING   (ABCODE=999)
      *   CBSTM03A, CBEXPORT, CBIMPORT
      *       CALL 'CEE3ABD'                        (no arguments)
      *
      * Baseline semantics: print the abend code and STOP RUN with the
      * abend code as the process return code so the runner records a
      * non-zero RC. A call without arguments is reported as such and
      * ends with RC 16 (there is no abend code to propagate).
      * The process exit status is RETURN-CODE modulo 256, so U0999
      * surfaces to the shell as 231.
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
           IF ADDRESS OF ABCODE = NULL
              DISPLAY 'CEE3ABD: USER ABEND (NO ABCODE PASSED)'
              MOVE 16 TO RETURN-CODE
              STOP RUN
           END-IF.
           IF ADDRESS OF TIMING = NULL
              DISPLAY 'CEE3ABD: USER ABEND U' ABCODE
           ELSE
              DISPLAY 'CEE3ABD: USER ABEND U' ABCODE
                      ' TIMING=' TIMING
           END-IF.
           MOVE ABCODE TO WS-RC.
           MOVE WS-RC TO RETURN-CODE.
           STOP RUN.
