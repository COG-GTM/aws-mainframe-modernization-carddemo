      * Record areas of app/cbl/CBACT01C.cbl that are not in a shared copybook: the FILE SECTION records of
      * OUT-FILE (OUTFILE), ARRY-FILE (ARRYFILE) and the WORKING-STORAGE records written to VBRC-FILE
      * (VBRCFILE, RECORDING MODE V, VBR-REC PIC X(80), 12- and 39-byte records), same items and PICTUREs.
       01  OUT-ACCT-REC.
           05  OUT-ACCT-ID                 PIC 9(11).
           05  OUT-ACCT-ACTIVE-STATUS      PIC X(01).
           05  OUT-ACCT-CURR-BAL           PIC S9(10)V99.
           05  OUT-ACCT-CREDIT-LIMIT       PIC S9(10)V99.
           05  OUT-ACCT-CASH-CREDIT-LIMIT  PIC S9(10)V99.
           05  OUT-ACCT-OPEN-DATE          PIC X(10).
           05  OUT-ACCT-EXPIRAION-DATE     PIC X(10).
           05  OUT-ACCT-REISSUE-DATE       PIC X(10).
           05  OUT-ACCT-CURR-CYC-CREDIT    PIC S9(10)V99.
           05  OUT-ACCT-CURR-CYC-DEBIT     PIC S9(10)V99 COMP-3.
           05  OUT-ACCT-GROUP-ID           PIC X(10).
       01  ARR-ARRAY-REC.
           05  ARR-ACCT-ID                 PIC 9(11).
           05  ARR-ACCT-BAL OCCURS 5 TIMES.
               10  ARR-ACCT-CURR-BAL       PIC S9(10)V99.
               10  ARR-ACCT-CURR-CYC-DEBIT PIC S9(10)V99 COMP-3.
           05  ARR-FILLER                  PIC X(04).
       01  VBRC-REC1.
           05  VB1-ACCT-ID                 PIC 9(11).
           05  VB1-ACCT-ACTIVE-STATUS      PIC X(01).
       01  VBRC-REC2.
           05  VB2-ACCT-ID                 PIC 9(11).
           05  VB2-ACCT-CURR-BAL           PIC S9(10)V99.
           05  VB2-ACCT-CREDIT-LIMIT       PIC S9(10)V99.
           05  VB2-ACCT-REISSUE-YYYY       PIC X(04).
