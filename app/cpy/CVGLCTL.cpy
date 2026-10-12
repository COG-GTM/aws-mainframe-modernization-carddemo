      ******************************************************************
      *    Ledger-summary control card (RECLN = 080)
      ******************************************************************
       01  GLPARM-RECORD.
           05  CTL-RUN-DATE                         PIC X(10).
           05  FILLER                               PIC X.
           05  CTL-SOURCE-SYS-ID                    PIC X(8).
           05  FILLER                               PIC X.
           05  CTL-EXPECTED-COUNT                   PIC 9(9).
           05  FILLER                               PIC X.
           05  CTL-EXPECTED-NET                     PIC S9(13)V99
                                                    SIGN IS LEADING
                                                    SEPARATE CHARACTER.
           05  FILLER                               PIC X(34).
