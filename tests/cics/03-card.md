# Card list / detail / update (CCLI / CCDL / CCUP)

Programs: `COCRDLIC` (COCRDLI), `COCRDSLC` (COCRDSL), `COCRDUPC` (COCRDUP).
File: CARDDAT (+ CARDAIX by account).

| ID | Steps | Expected |
|---|---|---|
| CRL-01 | CCLI as admin, no filter; ENTER | First page of cards (7 rows) |
| CRL-02 | PF8 / PF7 | Next / previous page; at end "no more records" style message |
| CRL-03 | Filter Account = existing account | Only that account's cards listed |
| CRL-04 | Filter Card = 15 digits or non-numeric | Card number must be 16 digits error |
| CRL-05 | Select a row with `S`; ENTER | XCTL to COCRDSLC with card in COMMAREA |
| CRL-06 | Select a row with `U`; ENTER | XCTL to COCRDUPC |
| CRL-07 | Two rows selected / code other than S,U | Error, no XCTL |
| CRD-01 | CCDL; Account + Card of an existing card | Card detail (name, expiry, status) |
| CRD-02 | Card not on file | Card not found error |
| CRU-01 | CCUP; change embossed name; ENTER, PF5 | Name updated in CARDDAT |
| CRU-02 | Name with digits/special chars | Name may contain only letters/spaces error |
| CRU-03 | Expiry month 13 or year < current | Expiry validation error |
| CRU-04 | Status other than Y/N | Validation error |
