      ******************************************************************
      * COBDATFT - GnuCOBOL stand-in for app/asm/COBDATFT.asm
      *            (HLASM date reformatter called by CBACT01C).
      *
      * Harvested from origin/devin/cobol-safety-net @ 1c845c7
      *   test-harness/cobol/COBDATFT.cbl
      *
      * Reproduces the assembler logic byte for byte (see the .asm):
      *   COINTYPE '1' (YYYYMMDD in)  : if COINPDT+4 = '-'  -> error
      *                                 if COOUTYPE = '2'   -> error
      *                                 else out = YYYY-MM-DD
      *   COINTYPE '2' (YYYY-MM-DD in): if COOUTYPE = '1'   -> error
      *                                 else out = YYYYMMDD
      *   any other COINTYPE          : error
      *   error  = MVC COERMSG,=C'INVALID INPUT' (output date untouched)
      * Only the bytes the assembler MVCs are written; the rest of
      * CODATECN-0UT-DATE is left exactly as the caller passed it.
      * Note the assembler never checks COOUTYPE for '2'/'1' positively:
      * for type 2 input any COOUTYPE other than '1' yields YYYYMMDD.
      * CBACT01C always calls with TYPE='2', OUTTYPE='2', so the reissue
      * date YYYY-MM-DD comes back as YYYYMMDD followed by the caller's
      * trailing bytes (the untouched tail of the 20-byte output area).
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBDATFT.
       DATA DIVISION.
       LINKAGE SECTION.
       01  CODATECN-REC.
           05  CODATECN-IN-REC.
               10  CODATECN-TYPE             PIC X.
               10  CODATECN-INP-DATE         PIC X(20).
           05  CODATECN-OUT-REC.
               10  CODATECN-OUTTYPE          PIC X.
               10  CODATECN-0UT-DATE         PIC X(20).
           05  CODATECN-ERROR-MSG            PIC X(38).
       PROCEDURE DIVISION USING CODATECN-REC.
       MAIN-PARA.
           EVALUATE CODATECN-TYPE
             WHEN '1'
               IF CODATECN-INP-DATE(5:1) = '-'
                  OR CODATECN-OUTTYPE = '2'
                  PERFORM GOTOERR
               ELSE
                  MOVE CODATECN-INP-DATE(1:4) TO CODATECN-0UT-DATE(1:4)
                  MOVE '-'                    TO CODATECN-0UT-DATE(5:1)
                  MOVE CODATECN-INP-DATE(5:2) TO CODATECN-0UT-DATE(6:2)
                  MOVE '-'                    TO CODATECN-0UT-DATE(8:1)
                  MOVE CODATECN-INP-DATE(7:2) TO CODATECN-0UT-DATE(9:2)
               END-IF
             WHEN '2'
               IF CODATECN-OUTTYPE = '1'
                  PERFORM GOTOERR
               ELSE
                  MOVE CODATECN-INP-DATE(1:4) TO CODATECN-0UT-DATE(1:4)
                  MOVE CODATECN-INP-DATE(6:2) TO CODATECN-0UT-DATE(5:2)
                  MOVE CODATECN-INP-DATE(9:2) TO CODATECN-0UT-DATE(7:2)
               END-IF
             WHEN OTHER
               PERFORM GOTOERR
           END-EVALUATE.
           GOBACK.
       GOTOERR.
      *    MVC COERMSG,=C'INVALID INPUT' moves 38 bytes starting at the
      *    13-byte literal; the assembler pads the rest from whatever
      *    follows the literal pool.  We pad with spaces.
           MOVE 'INVALID INPUT' TO CODATECN-ERROR-MSG.
