# Online user administration (Session 9)

| COBOL | TS | Tran | Map | Function |
|---|---|---|---|---|
| `COUSR00C` | `cousr00c.ts` | CU00 | COUSR0A | List USRSEC, 10 rows/page, PF7/PF8 paging, row select `U`/`D` (XCTL to COUSR02C/03C) |
| `COUSR01C` | `cousr01c.ts` | CU01 | COUSR1A | Add user (WRITE; DUPKEY/DUPREC -> `User ID already exist...`), PF4 clear |
| `COUSR02C` | `cousr02c.ts` | CU02 | COUSR2A | Fetch (READ UPDATE), PF5 update (REWRITE), PF3 save+exit, PF4 clear, PF12 admin menu |
| `COUSR03C` | `cousr03c.ts` | CU03 | COUSR3A | Fetch (READ UPDATE), PF5 delete (DELETE), PF3 exit, PF4 clear, PF12 admin menu |

Registered in `src/online/areas/user-admin.ts`. `common.ts` holds two local helpers:
`UserAdminCommarea` (COCOM01Y + the 34-byte `CDEMO-CUxx-INFO` extension the programs append) and
`SecUserData` (CSUSR01Y working storage over `SecUserDataLayout`). Tests: `test/online/user-admin/`.

## Bug-for-bug behaviour kept

- COUSR00C: an invalid selection message is overwritten when the re-read list hits ENDFILE on the look-ahead READNEXT (e.g. exactly 10 users -> `You have reached the bottom of the page...`).
- COUSR00C: a search key past the last user gives STARTBR NOTFND, then an unconditional ENDBR with no browse -> INVREQ -> abend AEIP.
- COUSR01C: on DUPREC the cursor stays on First Name (FNAMEL was already -1; SEND MAP CURSOR uses the first L=-1 field).
- COUSR02C/03C: PF5 continues after a failed READ UPDATE; the REWRITE/DELETE then fails with INVREQ (no lock) -> `Unable to Update User...` (also the delete program's text). PF3 in COUSR02C runs the update and XCTLs regardless of its outcome.
- Records written by COUSR01C carry LOW-VALUES in SEC-USR-FILLER (the WORKING-STORAGE record has no VALUE).

## LIMITATIONS

- CICS programs cannot run under GnuCOBOL; expected values in the tests were derived by tracing the COBOL source (no golden files).
- `DISPLAY 'RESP:' ... 'REAS:'` diagnostics (job log) are not reproduced.
- `CDEMO-CU00-PAGE-NUM PIC 9(08)`: non-digit bytes are read as 0 (on z/OS this would be a data exception or undefined).
- The S6 test `test/online/menu.test.ts` "routes to a registered admin program" stubbed `COUSR00C`, which is now registered (duplicate registration); it now stubs the never-ported Db2 program `COTRTLIC` (option 5) instead.
