      * Stub for the LE CEEDAYS API: returns the feedback token chosen
      * by T_SEV / T_MSG / T_NEG so every EVALUATE branch is reachable.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CEEDAYS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SEV       PIC 9(4).
       01 WS-MSG       PIC 9(4).
       01 WS-NEG       PIC X.
       LINKAGE SECTION.
       01 LS-IN        PIC X(258).
       01 LS-PIC       PIC X(258).
       01 LS-LIL       PIC S9(9) BINARY.
       01 LS-FC.
          02 LS-SEV    PIC S9(4) BINARY.
          02 LS-MSG    PIC S9(4) BINARY.
          02 LS-CTL    PIC X(4).
          02 LS-ISI    PIC S9(9) BINARY.
       PROCEDURE DIVISION USING LS-IN LS-PIC LS-LIL LS-FC.
           ACCEPT WS-SEV FROM ENVIRONMENT 'T_SEV'
           ACCEPT WS-MSG FROM ENVIRONMENT 'T_MSG'
           ACCEPT WS-NEG FROM ENVIRONMENT 'T_NEG'
           IF WS-SEV = 0 AND WS-MSG = 0
              MOVE LOW-VALUES TO LS-FC
           ELSE
              MOVE WS-SEV TO LS-SEV
              IF WS-NEG = 'Y'
                 COMPUTE LS-SEV = 0 - WS-SEV
              END-IF
              MOVE WS-MSG TO LS-MSG
              MOVE X'59C3C5C5' TO LS-CTL
              MOVE 0 TO LS-ISI
           END-IF
           MOVE 0 TO LS-LIL
           GOBACK.
