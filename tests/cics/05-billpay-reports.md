# Bill payment and report request (CB00 / CR00)

Programs: `COBIL00C` (COBIL00), `CORPT00C` (CORPT00).

| ID | Steps | Expected |
|---|---|---|
| BIL-01 | CB00; Account with balance > 0; ENTER | Current balance displayed, confirm prompt |
| BIL-02 | Confirm = Y | Payment transaction (type 02) written to TRANSACT for full balance; account balance becomes 0; success message with tran id |
| BIL-03 | Account with balance 0 | "You have nothing to pay" style message, nothing written |
| BIL-04 | Account not on file / blank | Error message |
| BIL-05 | Confirm = N or invalid value | No payment; invalid value shows error |
| RPT-01 | CR00; Monthly = X; ENTER; confirm Y | JCL (TRANREPT) submitted via TDQ JOBS; "report submitted" message |
| RPT-02 | Yearly = X | Same, for current year |
| RPT-03 | Custom; start 2022-01-01, end 2022-07-06 | Job submitted with those dates in DATEPARM (compare with batch test CBTRN03C) |
| RPT-04 | Custom start 2022-02-30 | CSUTLDTC date error, no submit |
| RPT-05 | Custom start month 13 / non-numeric | Validation error |
| RPT-06 | No option selected | "Select a report type" style error |
