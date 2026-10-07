# 08 - UI map: BMS maps → React pages

`modernization/carddemo-ui/` replaces the 3270 maps of `app/bms` with one React route per map (ADR-0022). The field
inventory of each page is the symbolic map in `app/cpy-bms/<MAP>.CPY` (the `...I` input fields); every page renders
those fields with `data-bms="<FIELD>"`, and `src/test/bmsFields.test.ts` fails if a page loses one. Routing follows the
API's `NavigationContext.toProgram` (`src/programs.ts`), never menu option numbers.

Common to every page (rendered by `components/Screen.tsx`): `TRNNAME`/`PGMNAME` (transaction id and program of the
page), `TITLE01`/`TITLE02`, `CURDATE`/`CURTIME` (from the API `ScreenHeader` when the screen has one), the message
area `ERRMSG`/`INFOMSG` with the API's message text, and the PF-key line (`FKEYS`...) as buttons with keyboard
shortcuts (F-keys; Esc = F3). `APPLID`/`SYSID` (COSGN00) come from the sign-on screen header.
`CRDSTPn` (COCRDLI) only carries the protect attribute of empty list rows and has no UI element.
API paths are relative to `/api/v1`; `n` = repeated row/option fields (`SEL0001..SEL0010`, `OPTN001..OPTN012`...).

| BMS map | Tran / program | Route | Title | Fields (`app/cpy-bms`) | API calls | PF keys |
| --- | --- | --- | --- | --- | --- | --- |
| COSGN00 | CC00 / COSGN00C | `/signon` | Sign On | USERID PASSWD | POST /auth/login<br>POST /auth/logout | ENTER Sign-on, F3 Exit |
| COMEN01 | CM00 / COMEN01C | `/menu` | Main Menu | OPTNn OPTION | POST /auth/logout<br>POST /menu/{menu}/selection<br>POST /menu/{menu}/exit | ENTER Continue, F3 Exit |
| COADM01 | CA00 / COADM01C | `/admin` (ADMIN) | Admin Menu | OPTNn OPTION | POST /auth/logout<br>POST /menu/{menu}/selection<br>POST /menu/{menu}/exit | ENTER Continue, F3 Exit |
| COACTVW | CAVW / COACTVWC | `/accounts/view` | View Account | ACCTSID ACSTTUS ADTOPEN ACRDLIM AEXPDT ACSHLIM AREISDT ACURBAL ACRCYCR AADDGRP ACRCYDB ACSTNUM ACSTSSN ACSTDOB ACSTFCO ACSFNAM ACSMNAM ACSLNAM ACSADL1 ACSSTTE ACSADL2 ACSZIPC ACSCITY ACSCTRY ACSPHN1 ACSGOVT ACSPHN2 ACSEFTC ACSPFLG | GET /accounts/{id} | ENTER Fetch, F3 Exit |
| COACTUP | CAUP / COACTUPC | `/accounts/update` | Update Account | ACCTSID ACSTTUS OPNYEAR OPNMON OPNDAY ACRDLIM EXPYEAR EXPMON EXPDAY ACSHLIM RISYEAR RISMON RISDAY ACURBAL ACRCYCR AADDGRP ACRCYDB ACSTNUM ACTSSN1 ACTSSN2 ACTSSN3 DOBYEAR DOBMON DOBDAY ACSTFCO ACSFNAM ACSMNAM ACSLNAM ACSADL1 ACSSTTE ACSADL2 ACSZIPC ACSCITY ACSCTRY ACSPH1A ACSPH1B ACSPH1C ACSGOVT ACSPH2A ACSPH2B ACSPH2C ACSEFTC ACSPFLG | GET /accounts/{id}<br>PUT /accounts/{id} | ENTER Process, F3 Exit, F5 Save, F12 Cancel |
| COCRDLI | CCLI / COCRDLIC | `/cards` | List Credit Cards | PAGENO ACCTSID CARDSID CRDSELn ACCTNOn CRDNUMn CRDSTSn | GET /cards<br>POST /cards/selection | ENTER Continue, F3 Exit, F7 Backward, F8 Forward |
| COCRDSL | CCDL / COCRDSLC | `/cards/view` | View Credit Card Detail | ACCTSID CARDSID CRDNAME CRDSTCD EXPMON EXPYEAR | GET /cards/{cardRef} | ENTER Search Cards, F3 Exit |
| COCRDUP | CCUP / COCRDUPC | `/cards/update` | Update Credit Card Details | ACCTSID CARDSID CRDNAME CRDSTCD EXPMON EXPYEAR EXPDAY | GET /cards/{cardRef}<br>PUT /cards/{cardRef} | ENTER Process, F3 Exit, F5 Save, F12 Cancel |
| COTRN00 | CT00 / COTRN00C | `/transactions` | List Transactions | PAGENUM TRNIDIN SELn TRNIDn TDATEn TDESCn TAMTn | GET /transactions<br>POST /transactions/selection | ENTER Continue, F3 Back, F7 Backward, F8 Forward |
| COTRN01 | CT01 / COTRN01C | `/transactions/view` | View Transaction | TRNIDIN TRNID CARDNUM TTYPCD TCATCD TRNSRC TDESC TRNAMT TORIGDT TPROCDT MID MNAME MCITY MZIP | GET /transactions/{tranId} | ENTER Fetch, F3 Back, F4 Clear, F5 Browse Tran. |
| COTRN02 | CT02 / COTRN02C | `/transactions/add` | Add Transaction | ACTIDIN CARDNIN TTYPCD TCATCD TRNSRC TDESC TRNAMT TORIGDT TPROCDT MID MNAME MCITY MZIP CONFIRM | POST /transactions | ENTER Continue, F3 Back, F4 Clear, F5 Copy Last Tran. |
| COBIL00 | CB00 / COBIL00C | `/bill-payment` | Bill Payment | ACTIDIN CURBAL CONFIRM | POST /accounts/{id}/bill-payment | ENTER Continue, F3 Back, F4 Clear |
| CORPT00 | CR00 / CORPT00C | `/reports` | Transaction Reports | MONTHLY YEARLY CUSTOM SDTMM SDTDD SDTYYYY EDTMM EDTDD EDTYYYY CONFIRM | POST /reports/transactions<br>GET /reports/transactions/{executionId}<br>GET /reports/transactions/{executionId}/report | ENTER Continue, F3 Back |
| COUSR00 | CU00 / COUSR00C | `/admin/users` (ADMIN) | List Users | PAGENUM USRIDIN SELn USRIDn FNAMEn LNAMEn UTYPEn | GET /users<br>POST /users/selection | ENTER Continue, F3 Back, F7 Backward, F8 Forward |
| COUSR01 | CU01 / COUSR01C | `/admin/users/add` (ADMIN) | Add User | FNAME LNAME USERID PASSWD USRTYPE | POST /users | ENTER Add User, F3 Back, F4 Clear, F12 Exit |
| COUSR02 | CU02 / COUSR02C | `/admin/users/update` (ADMIN) | Update User | USRIDIN FNAME LNAME PASSWD USRTYPE | GET /users/{id}<br>PUT /users/{id} | ENTER Fetch, F3 Save&Exit, F4 Clear, F5 Save, F12 Cancel |
| COUSR03 | CU03 / COUSR03C | `/admin/users/delete` (ADMIN) | Delete User | USRIDIN FNAME LNAME USRTYPE | DELETE /users/{id} | ENTER Fetch, F3 Back, F4 Clear, F5 Delete |

COSGN00 also calls `GET /auth/login` for its header. The menus load `GET /menu/{main|admin}` and show only what the API
lists; an option whose program is not installed (e.g. COPAUS0C, COTRTLIC) gets the API's message. A USER who reaches an
`(ADMIN)` route sees `No access - Admin Only option...` without any API call; the API enforces the same with 403
`NOTAUTH`, which every page renders in the message area.

## Not tested

- Browser coverage is Chromium only (Playwright and the recordings); Firefox/Safari are not exercised.
- Menu options whose programs are not part of this port (COPAUS0C pending authorizations, COTRTLIC/COTRTUPC Db2
  transaction types) are only checked for the API message, not as screens (no map/program in scope).
- 409 `CHANGED` (concurrent account/card/user update) and the report `FAILED`/503 queue-full paths are covered by the
  API ITs and by message-area unit tests, not by an end-to-end browser run.
- Token expiry after 1 h is tested through a mocked 401 (`focus.test.tsx`), not by waiting out a real token.
- Accessibility (screen reader, contrast) and responsive layouts beyond desktop widths were not audited.
