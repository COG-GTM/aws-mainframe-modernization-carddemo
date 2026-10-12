       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CEE3PRM.
      ******************************************************************
      * Program     : CEE3PRM.cbl
      * Application : CBTRN04C test suite
      * Type        : Test stub (off-mainframe runs only)
      * Function    : Stand-in for the z/OS Language Environment
      *               CEE3PRM callable service (query PARM string) so
      *               CBTRN04C can be run with GnuCOBOL. The real
      *               service returns the EXEC PARM= value as an
      *               80-byte blank-padded string plus a 12-byte
      *               feedback code (CEE000 = success). This stub
      *               returns the value of environment variable
      *               CBTRN04C_PARM instead, blank when it is unset,
      *               which is what LE returns when no PARM is coded.
      *               NEVER deploy this program to z/OS: there the
      *               real LE service is used instead.
      *               Pattern adopted from the CEEDAYS stub on branch
      *               devin/1786708098-cobol-test-foundation.
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ENV-VALUE                         PIC X(80).

       LINKAGE SECTION.
       01  LS-PARM-STRING                       PIC X(80).
       01  LS-FEEDBACK-CODE.
           05 LS-FC-SEVERITY                    PIC S9(04) COMP.
           05 LS-FC-MSG-NO                      PIC S9(04) COMP.
           05 LS-FC-CASE-SEV-CTL                PIC X(01).
           05 LS-FC-FACILITY-ID                 PIC X(03).
           05 LS-FC-ISI                         PIC S9(09) COMP.

       PROCEDURE DIVISION USING LS-PARM-STRING LS-FEEDBACK-CODE.
           MOVE SPACES TO WS-ENV-VALUE
           ACCEPT WS-ENV-VALUE FROM ENVIRONMENT 'CBTRN04C_PARM'
           MOVE WS-ENV-VALUE TO LS-PARM-STRING
           MOVE 0           TO LS-FC-SEVERITY
           MOVE 0           TO LS-FC-MSG-NO
           MOVE LOW-VALUES  TO LS-FC-CASE-SEV-CTL
           MOVE LOW-VALUES  TO LS-FC-FACILITY-ID
           MOVE 0           TO LS-FC-ISI
           GOBACK.
