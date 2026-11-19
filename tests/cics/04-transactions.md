# Transaction list / view / add (CT00 / CT01 / CT02)

Programs: `COTRN00C` (COTRN00), `COTRN01C` (COTRN01), `COTRN02C` (COTRN02).
File: TRANSACT; COTRN02C also reads CXACAIX/CCXREF and calls `CSUTLDTC`.

| ID | Steps | Expected |
|---|---|---|
| TRL-01 | CT00; ENTER | First 10 transactions in id order |
| TRL-02 | PF8 / PF7 | Paging; past end shows "bottom of page" style message |
| TRL-03 | Search Tran ID = existing id | List starts at that id |
| TRL-04 | Search Tran ID non-numeric | Validation error |
| TRL-05 | `S` next to a row; ENTER | XCTL to COTRN01C for that id |
| TRV-01 | CT01; Tran ID existing | All transaction fields displayed |
| TRV-02 | Tran ID not on file | "Transaction ID NOT found" style error |
| TRA-01 | CT02; Account or Card valid, Type 01, Cat 0001, Source, Desc, Amount `+00000100.00`, Orig/Proc date `2024-06-01`, merchant fields; ENTER; Confirm = Y | New record written with next sequential id; success message shows id |
| TRA-02 | Both Account and Card blank | "Account or Card Number must be entered" style error |
| TRA-03 | Card not in xref | Invalid card error |
| TRA-04 | Amount `100` (bad format) | Amount format error (-99999999.99) |
| TRA-05 | Orig date `2023-02-29` | Date invalid (CSUTLDTC returns `Datevalue error`; see tests/unit) |
| TRA-06 | Orig date `2024-13-01` | Date invalid (`Invalid month`) |
| TRA-07 | Confirm = N | Nothing written |
| TRA-08 | PF4 (clear) / PF5 (copy last tran) | Fields cleared / prefilled from last transaction |
