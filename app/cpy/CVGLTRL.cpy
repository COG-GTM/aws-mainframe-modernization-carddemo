      ******************************************************************
      *    Ledger-summary trailer record (RECLN = 150)
      ******************************************************************
       01  GLSUM-TRAILER.
           05  GLT-REC-TYPE                         PIC X.
           05  GLT-DETAIL-COUNT                     PIC 9(9).
           05  GLT-TRAN-COUNT                       PIC 9(11).
           05  GLT-TOTAL-DEBITS                     PIC S9(15)V99
                                                    SIGN IS LEADING
                                                    SEPARATE CHARACTER.
           05  GLT-TOTAL-CREDITS                    PIC S9(15)V99
                                                    SIGN IS LEADING
                                                    SEPARATE CHARACTER.
           05  GLT-TOTAL-NET                        PIC S9(15)V99
                                                    SIGN IS LEADING
                                                    SEPARATE CHARACTER.
           05  GLT-HASH-TOTAL                       PIC 9(18).
           05  GLT-RECORD-COUNT                     PIC 9(9).
           05  FILLER                               PIC X(48).
