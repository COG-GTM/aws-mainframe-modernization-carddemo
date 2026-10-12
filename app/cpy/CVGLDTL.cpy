      ******************************************************************
      *    Ledger-summary detail record (RECLN = 150)
      ******************************************************************
       01  GLSUM-DETAIL.
           05  GLD-REC-TYPE                         PIC X.
           05  GLD-SEQ-NO                           PIC 9(9).
           05  GLD-ACCT-ID                          PIC 9(11).
           05  GLD-TYPE-CD                          PIC X(2).
           05  GLD-CAT-CD                           PIC 9(4).
           05  GLD-TRAN-COUNT                       PIC 9(9).
           05  GLD-DEBIT-AMT                        PIC S9(13)V99
                                                    SIGN IS LEADING
                                                    SEPARATE CHARACTER.
           05  GLD-CREDIT-AMT                       PIC S9(13)V99
                                                    SIGN IS LEADING
                                                    SEPARATE CHARACTER.
           05  GLD-NET-AMT                          PIC S9(13)V99
                                                    SIGN IS LEADING
                                                    SEPARATE CHARACTER.
           05  GLD-CATBAL-AMT                       PIC S9(13)V99
                                                    SIGN IS LEADING
                                                    SEPARATE CHARACTER.
           05  GLD-TIE-STATUS                       PIC X.
           05  FILLER                               PIC X(49).
