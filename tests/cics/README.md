# CICS online test specifications

The online tier (`CO*` programs, BMS maps in `app/bms/`) runs under CICS
(EXEC CICS SEND/RECEIVE MAP, XCTL, READ/STARTBR on VSAM files, COMMAREA
`COCOM01Y`). GnuCOBOL has no CICS translator or runtime, so these programs
are **not** executed by `tests/run_all.sh`. The files here are structured
manual/emulator test cases to run on a CICS region (IBM z/OS, IBM ZD&T,
Micro Focus Enterprise Server, etc.) with the standard CardDemo data loaded
(`app/data`, sign-on users from `app/jcl/DUSRSECJ.jcl`).

| Spec | Transactions | Programs |
|---|---|---|
| [01-signon-menus.md](01-signon-menus.md) | CC00, CM00, CA00 | COSGN00C, COMEN01C, COADM01C |
| [02-account.md](02-account.md) | CAVW, CAUP | COACTVWC, COACTUPC |
| [03-card.md](03-card.md) | CCLI, CCDL, CCUP | COCRDLIC, COCRDSLC, COCRDUPC |
| [04-transactions.md](04-transactions.md) | CT00, CT01, CT02 | COTRN00C, COTRN01C, COTRN02C |
| [05-billpay-reports.md](05-billpay-reports.md) | CB00, CR00 | COBIL00C, CORPT00C |
| [06-user-admin.md](06-user-admin.md) | CU00-CU03 | COUSR00C-COUSR03C |

Case format: **ID**, preconditions, steps (field = value, then key),
expected result (screen, message line `ERRMSG`, data changes). Exact message
texts are in the cited program; where a case says "error message" check the
program source for the literal. Status: **PENDING - no CICS environment**.

Common keys: ENTER = process, PF3 = back/exit, PF7/PF8 = page up/down,
PF5 = save/confirm (update screens), PF12 = cancel. Invalid AID keys show an
"invalid key" message without changing data.
