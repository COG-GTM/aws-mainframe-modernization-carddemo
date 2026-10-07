      ******************************************************************
      * MVSWAIT - GnuCOBOL stand-in for app/asm/MVSWAIT.asm
      *           (HLASM timer wait called by COBSWAIT, job WAITSTEP).
      *
      * New for this baseline (no prior branch carried one).
      *
      * Mainframe semantics (see the .asm): R1 -> fullword address of
      * the delay; the value is stored in BINLBL and passed to the
      * ASMWAIT macro (STIMER WAIT, BINTVL) which suspends the task for
      * that many hundredths of a second (centiseconds); R15 is zeroed
      * on return. COBSWAIT passes PIC 9(8) COMP = 4-byte binary.
      *
      * Baseline semantics: display the requested delay, then sleep for
      * delay * 10 ms via CBL_OC_NANOSLEEP. Setting the environment
      * variable BASELINE_MVSWAIT_NOSLEEP=1 skips the sleep (used by
      * the runner's --fast mode); nothing else changes, so the
      * captured sysout is identical either way.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MVSWAIT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NANOSECONDS          PIC S9(18) BINARY.
       01  WS-NOSLEEP              PIC X(8) VALUE SPACES.
       LINKAGE SECTION.
       01  MVSWAIT-TIME            PIC 9(8) COMP.
       PROCEDURE DIVISION USING MVSWAIT-TIME.
           DISPLAY 'MVSWAIT: WAIT ' MVSWAIT-TIME ' CENTISECONDS'.
           ACCEPT WS-NOSLEEP
               FROM ENVIRONMENT 'BASELINE_MVSWAIT_NOSLEEP'.
           IF WS-NOSLEEP(1:1) NOT = '1'
              COMPUTE WS-NANOSECONDS = MVSWAIT-TIME * 10000000
              CALL 'CBL_OC_NANOSLEEP' USING WS-NANOSECONDS
           END-IF.
           MOVE 0 TO RETURN-CODE.
           GOBACK.
