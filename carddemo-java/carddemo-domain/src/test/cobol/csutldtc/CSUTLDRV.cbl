       IDENTIFICATION DIVISION.
       PROGRAM-ID. CSUTLDRV.
      * Calls app/cbl/CSUTLDTC.cbl the way CORPT00C and COTRN02C do and
      * prints LS-RESULT as hex, then RETURN-CODE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATE              PIC X(10).
       01 WS-DATE-FORMAT       PIC X(10).
       01 WS-RESULT            PIC X(80).
       01 WS-HEX               PIC X(160).
       01 WS-I                 PIC 9(3).
       01 WS-BYTE              PIC 9(3).
       01 WS-DIGITS            PIC X(16) VALUE '0123456789ABCDEF'.
       01 WS-RC                PIC 9(4).
       PROCEDURE DIVISION.
           ACCEPT WS-DATE FROM ENVIRONMENT 'CSUTLDTC_DATE'
           ACCEPT WS-DATE-FORMAT FROM ENVIRONMENT 'CSUTLDTC_FORMAT'
           MOVE SPACES TO WS-RESULT
           CALL 'CSUTLDTC' USING WS-DATE WS-DATE-FORMAT WS-RESULT
           MOVE RETURN-CODE TO WS-RC
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 80
              COMPUTE WS-BYTE = FUNCTION ORD(WS-RESULT(WS-I:1)) - 1
              MOVE WS-DIGITS(WS-BYTE / 16 + 1:1)
                TO WS-HEX(WS-I * 2 - 1:1)
              MOVE WS-DIGITS(FUNCTION MOD(WS-BYTE, 16) + 1:1)
                TO WS-HEX(WS-I * 2:1)
           END-PERFORM
           DISPLAY WS-HEX ' ' WS-RC
           MOVE 0 TO RETURN-CODE
           STOP RUN.
