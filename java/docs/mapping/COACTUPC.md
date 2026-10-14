# COACTUPC - Account update (transaction CAUP)

| | |
|---|---|
| COBOL | `app/cbl/COACTUPC.cbl` (CICS online, 4,236 lines) |
| BMS | `app/bms/COACTUP.bms` (mapset COACTUP, map CACTUPA); symbolic map `app/cpy-bms/COACTUP.CPY` |
| COPY | CVCRD01Y, CSLKPCDY, DFHBMSCA, DFHAID, COTTL01Y, COACTUP, CSDAT01Y, CSMSG01Y, CSMSG02Y, CSUSR01Y, CVACT01Y, CVACT03Y, CVCUS01Y, COCOM01Y, CSUTLDWY, CSUTLDPY, CSSTRPFY, CSSETATY |
| Files | `CXACAIX` (AIX path over CCXREF on XREF-ACCT-ID, read), `ACCTDAT` (READ, READ UPDATE, REWRITE), `CUSTDAT` (READ, READ UPDATE, REWRITE) |
| XCTL targets | `CDEMO-FROM-PROGRAM` or `COMEN01C` (PF3) - returned as `nextProgram`, not executed |
| Java | `com.carddemo.online.coactupc.Coactupc` (+ `AcupRecords`, `LookupCodes`, `DisplayNumerics`) |
| REST | `POST /api/cics/CAUP` or `POST /api/cics/programs/COACTUPC` |

## Paragraph mapping

All methods are on `Coactupc.Task` (one instance per CICS task = fresh WORKING-STORAGE).

| COBOL paragraph (lines) | Java |
|---|---|
| 0000-MAIN (859-1022) | `main()`; `Coactupc#execute` adds the HANDLE ABEND LABEL (line 862) |
| COMMON-RETURN (1007-1020) | `commonReturn()` |
| 1000-PROCESS-INPUTS (1025-1037) | `processInputs1000()` |
| 1100-RECEIVE-MAP (1039-1427) | `receiveMap1100()`, `receiveField()`, `receiveMoney()` |
| 1200-EDIT-MAP-INPUTS (1429-1679) | `editMapInputs1200()` |
| 1205-COMPARE-OLD-NEW (1681-1778) | `compareOldNew1205()` |
| 1210-EDIT-ACCOUNT (1783-1821) | `editAccount1210()` |
| 1215-EDIT-MANDATORY (1824-1853) | `editMandatory1215()` |
| 1220-EDIT-YESNO (1856-1895) | `editYesno1220()` |
| 1225-EDIT-ALPHA-REQD (1898-1952) | `editAlphaReqd1225()` |
| 1230-EDIT-ALPHANUM-REQD (1955-2010) | `editAlphanumReqd1230()` (never PERFORMed) |
| 1235-EDIT-ALPHA-OPT (2012-2058) | `editAlphaOpt1235()` |
| 1240-EDIT-ALPHANUM-OPT (2061-2106) | `editAlphanumOpt1240()` (never PERFORMed) |
| 1245-EDIT-NUM-REQD (2109-2177) | `editNumReqd1245()` |
| 1250-EDIT-SIGNED-9V2 (2180-2222) | `editSigned9v2_1250()` |
| 1260-EDIT-US-PHONE-NUM (2225-2245) | `editUsPhoneNum1260()` |
| EDIT-AREA-CODE (2246-2315) | `editAreaCode()` |
| EDIT-US-PHONE-PREFIX (2316-2369) | `editUsPhonePrefix()` |
| EDIT-US-PHONE-LINENUM (2370-2423) | `editUsPhoneLinenum()` |
| EDIT-US-PHONE-EXIT / 1260-EDIT-US-PHONE-NUM-EXIT (2424-2428) | end of `editUsPhoneNum1260()` |
| 1265-EDIT-US-SSN (2431-2490) | `editUsSsn1265()` |
| 1270-EDIT-US-STATE-CD (2493-2512) | `editUsStateCd1270()` |
| 1275-EDIT-FICO-SCORE (2514-2532) | `editFicoScore1275()` |
| 1280-EDIT-US-STATE-ZIP-CD (2536-2559) | `editUsStateZipCd1280()` |
| 2000-DECIDE-ACTION (2562-2644) | `decideAction2000()` |
| 3000-SEND-MAP (2649-2665) | `sendMap3000()` |
| 3100-SCREEN-INIT (2668-2695) | `screenInit3100()` |
| 3200-SETUP-SCREEN-VARS (2698-2728) | `setupScreenVars3200()` |
| 3201-SHOW-INITIAL-VALUES (2731-2784) | `showInitialValues3201()` |
| 3202-SHOW-ORIGINAL-VALUES (2787-2868) | `showOriginalValues3202()` |
| 3203-SHOW-UPDATED-VALUES (2870-2952) | `showUpdatedValues3203()`, `showMoney()` |
| 3250-SETUP-INFOMSG (2955-2984) | `setupInfomsg3250()` |
| 3300-SETUP-SCREEN-ATTRS (2986-3438) | `setupScreenAttrs3300()`, `positionCursor()` (EVALUATE 3008-3205), `csSetAty()` (COPY CSSETATY x39, 3208-3436) |
| 3310-PROTECT-ALL-ATTRS (3441-3497) | `protectAllAttrs3310()` |
| 3320-UNPROTECT-FEW-ATTRS (3500-3563) | `unprotectFewAttrs3320()` |
| 3390-SETUP-INFOMSG-ATTRS (3566-3585) | `setupInfomsgAttrs3390()` |
| 3400-SEND-SCREEN (3589-3604) | `sendScreen3400()` |
| 9000-READ-ACCT (3608-3648) | `readAcct9000()` |
| 9200-GETCARDXREF-BYACCT (3650-3699) | `getCardxrefByacct9200()` (`ctx.readPath("CXACAIX", ...)`) |
| 9300-GETACCTDATA-BYACCT (3701-3749) | `getAcctdataByacct9300()` |
| 9400-GETCUSTDATA-BYCUST (3752-3798) | `getCustdataBycust9400()` |
| 9500-STORE-FETCHED-DATA (3801-3886) | `storeFetchedData9500()` |
| 9600-WRITE-PROCESSING (3888-4106) | `writeProcessing9600()` (`ctx.readForUpdate`, `ctx.rewrite`, `ctx.syncpointRollback`) |
| 9700-CHECK-CHANGE-IN-REC (4109-4194) | `checkChangeInRec9700()` |
| ABEND-ROUTINE (4203-4227) | `abendRoutine()` |
| CSSTRPFY YYYY-STORE-PFKEY | `yyyyStorePfkey()` -> runtime `PfKeys.storePfKey` |
| CSUTLDPY EDIT-DATE-CCYYMMDD (18-24) / -EXIT (329) | `editDateCcyymmdd()` (the paragraphs fall through in order) |
| CSUTLDPY EDIT-YEAR-CCYY (25-89) | `editYearCcyy()` |
| CSUTLDPY EDIT-MONTH (91-146) | `editMonth()` |
| CSUTLDPY EDIT-DAY (150-206) | `editDay()` |
| CSUTLDPY EDIT-DAY-MONTH-YEAR (209-281) | `editDayMonthYear()` (false = GO TO EDIT-DATE-CCYYMMDD-EXIT) |
| CSUTLDPY EDIT-DATE-LE (284-324) | `editDateLe()` -> runtime `CobolDates.ceedays` (CALL 'CSUTLDTC' / CEEDAYS) |
| CSUTLDPY EDIT-DATE-OF-BIRTH (341-371) | `editDateOfBirth()` -> runtime `CobolDates.integerOfDate` |

## Copybook -> Java

New shared records in `carddemo-runtime` `com.carddemo.common.copybook` (static `LAYOUT`, COBOL names in Javadoc):

| Copybook | Class | Notes |
|---|---|---|
| CVACT01Y ACCOUNT-RECORD (300) | `AccountRecord` | ACCT-ID 9(11) zoned; ACCT-CURR-BAL / CREDIT-LIMIT / CASH-CREDIT-LIMIT / CURR-CYC-CREDIT / CURR-CYC-DEBIT S9(10)V99 zoned (overpunch sign); dates X(10); ACCT-EXPIRAION-DATE spelling kept; ACCT-ADDR-ZIP X(10) at 102; ACCT-GROUP-ID X(10) at 112; FILLER X(178) |
| CVCUS01Y CUSTOMER-RECORD (500) | `CustomerRecord` | CUST-ID 9(9), names X(25), address lines X(50), CUST-SSN 9(9), CUST-FICO-CREDIT-SCORE 9(3) at 329, FILLER X(168) |
| CVACT03Y CARD-XREF-RECORD (50) | `CardXrefRecord` | XREF-CARD-NUM X(16) (key), XREF-CUST-ID 9(9), XREF-ACCT-ID 9(11) at 25 (CXACAIX key, NONUNIQUE), FILLER X(14) |
| CSMSG02Y ABEND-DATA (134) | `AbendData` | ABEND-CODE X(4), ABEND-CULPRIT X(8), ABEND-REASON X(50), ABEND-MSG X(72); all VALUE SPACES |

Program-private (package `com.carddemo.online.coactupc`):

| COBOL | Java |
|---|---|
| WS-THIS-PROGCOMMAREA (652-858): ACUP-CHANGE-ACTION + ACUP-OLD-DETAILS + ACUP-NEW-DETAILS (873 bytes) with every REDEFINES (`-N` numerics, date parts, phone parts, SSN parts) | `AcupRecords.PROG_COMMAREA`, accessed by COBOL name through `AcupRecords.Rec` |
| WS-COMMAREA X(2000) = CARDDEMO-COMMAREA + WS-THIS-PROGCOMMAREA + spaces | `AcupRecords.WS_COMMAREA` (also the `commareaLayout()` used to decode REST responses) |
| ACCT-UPDATE-RECORD (418-433) / CUST-UPDATE-RECORD (434-456) | `AcupRecords.ACCT_UPDATE` / `CUST_UPDATE` |
| ACUP-CHANGE-ACTION 88s | chars: LOW-VALUES/SPACES = DETAILS-NOT-FETCHED, `S` SHOW-DETAILS, `E` CHANGES-NOT-OK, `N` CHANGES-OK-NOT-CONFIRMED, `C` OKAYED-AND-DONE, `L` LOCK-ERROR, `F` FAILED; `changesMade()` = E/N/C/L/F |
| WS-NON-KEY-FLAGS (191-385) + WS-EDIT-US-SSN-FLGS | `Task.wsNonKeyFlags` / `wsEditUsSsnFlgs` (LOW-VALUES = valid, `0` = not ok, `B` = blank; status flags hold `Y`/`N`) |
| WS-INFO-MSG / WS-RETURN-MSG 88s | constants `FOUND_ACCOUNT_DATA` ... `LOCKED_BUT_UPDATE_FAILED` (verbatim) |
| CVCRD01Y CC-WORK-AREA | `Task.ccardAid`, `ccAcctId`, ... |
| CSLKPCDY | `LookupCodes` (generated verbatim: 410 general-purpose area codes, 56 state codes, 240 state+zip2 combos) |
| CSUTLDWY | `Task.wsEditDateCcyymmdd`, `wsEditDateFlgs`, `wsSeverity`, `wsMsgNo`, `wsDivBy`/`wsDividend`/`wsRemainder` |
| COACTUP symbolic map | `SymbolicMap.of("COACTUP", "CACTUPA")` |

Generic runtime additions used: `PfKeys`, `BmsAttributes`, `CobolNumval`, `NumericEdited` (`+ZZZ,ZZZ,ZZZ.99`), `CobolDates`,
`FileControl.register(AlternateIndexPath)` / `path` / `readPath` / `ksdsFiles`, `CicsContext.readForUpdate` / `readPath` /
`rewrite` / `write` / `syncpoint` / `syncpointRollback`, dispatcher backout on ABEND, `RecordLayout.describe` skipping
fields beyond a shorter record (XCTL commarea of 160 bytes vs the 2000-byte layout), `CardDemoAsciiSeed` + ASCII
resources (acctdata/custdata/cardxref) in the runtime pom; `OnlineRegionConfig` registers ACCTDAT, CUSTDAT, CCXREF
and the CXACAIX path.

## Expected-value derivation

Tests: `CoactupcTest` (pseudo-conversations through `CicsDispatcher`), `CoactupcParagraphsTest` (one test per
edit paragraph), `CoactupcRestTest` (HTTP + H2), runtime `CoactupcRuntimeAdditionsTest`.

* **Seed data** (app/data/ASCII): account 00000000001 -> cardxref.txt line 49 (card 9680294154603697, customer
  000000001); acctdata.txt line 1 balance `00000001940{` = +194.00 (overpunch `{` = +0), credit limit `00000020200{`
  = 2020.00, cash `00000010200{` = 1020.00, group id blank (the `A000000000` is ACCT-ADDR-ZIP). custdata.txt line 1:
  Immanuel Madeline Kessler, NC 12546, phones (908)119-8310 / (373)693-8684, SSN 020973888, DOB 1961-06-08, FICO 274.
  Account 18 / customer 18 (AP 96003-5867, FICO 340, phones 303/520) passes every edit except the state (AP is in
  the zip-combo list but not in VALID-US-STATE-CODE), so change tests start from it with state CA / zip 90210.
* **Currency display**: MOVE to `+ZZZ,ZZZ,ZZZ.99` (3202/3203): 194.00 -> `+        194.00`; 0 -> `+           .00`
  (Z suppression stops at the point because of the `99`).
* **Messages**: STRING of `FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)` and the paragraph literal, only when
  WS-RETURN-MSG-OFF (first error wins). WS-EDIT-VARIABLE-NAME is X(25), so `Current Cycle Credit Limit` prints as
  `Current Cycle Credit Limi`. File errors: `RESP` / `RESP2` are `PIC S9(9) COMP` moved to `X(10)` -> 9 digits + a
  space (`Resp:000000013  Reas:...`).
* **Cursor**: first true WHEN of the 3300 EVALUATE (3008-3205), e.g. a state/zip mismatch puts the cursor on ACSSTTE
  even when an earlier message (phone) was reported.
* **Dates** (CSUTLDPY): the fall-through order YEAR -> MONTH -> DAY -> DAY-MONTH-YEAR -> LE gives the flag patterns in
  `CoactupcParagraphsTest#editDateCcyymmdd` (e.g. 2023-02-29 -> all three NOT-OK, ":Not a leap year..."; 1900-02-29
  uses the /400 rule because YY = 00). DOB is "in the future" when INTEGER-OF-DATE(dob) >= today (2024-03-05 fixed clock).
* **Update path**: after F5 the ACCTDAT record is ACCT-UPDATE-RECORD (300 bytes) - see bug list; CUSTDAT phone is
  `STRING '(' A ')' B '-' C`.
* **Rollback**: `customerRewriteFailureRollsBackAccountRewrite` forces REWRITE CUSTDAT to fail (CUST-UPDATE-ID from
  the screen != the held key -> INVREQ) and checks ACCTDAT is back to its before-image (SYNCPOINT ROLLBACK, 4085-4103).

### Bug-for-bug behaviour (kept, not fixed)

1. ACCT-UPDATE-RECORD has no ACCT-ADDR-ZIP: ACCT-UPDATE-GROUP-ID is written at offset 102 (over ACCT-ADDR-ZIP) and
   its FILLER X(188) is never initialised (INITIALIZE skips FILLER), so bytes 112-299 of ACCTDAT become LOW-VALUES.
2. Blank screen fields become LOW-VALUES (1100); every seed account has a blank ACCT-GROUP-ID, and 1205 compares
   `TRIM(LOW-VALUES)` with `TRIM(SPACES)`, so "No change detected" can never be reached for unmodified seed accounts.
3. With ACUP-CHANGES-OK-NOT-CONFIRMED, 1200 skips all edits (line 1466), so data altered between ENTER and F5 is saved
   unvalidated.
4. COULD-NOT-LOCK-CUST-FOR-UPDATE has no WHEN in 2000: WHEN OTHER sets ACUP-CHANGES-OKAYED-AND-DONE, the screen says
   "Changes committed to database" with ERRMSG "Could not lock customer record for update".
5. REWRITE ACCTDAT failure sets LOCKED-BUT-UPDATE-FAILED without SYNCPOINT ROLLBACK; only a CUSTDAT failure rolls back.
6. The all-blank phone test checks `NUMA = SPACES` instead of `NUMC = SPACES` (line ~2232); unobservable through the
   map because blank inputs are already LOW-VALUES.
7. DID-NOT-FIND-ACCT-IN-ACCTDAT / DID-NOT-FIND-CUST-IN-CUSTDAT are never set (9300/9400 build their own messages),
   so 9000 always continues; an account missing from ACCTDAT still reaches ACUP-SHOW-DETAILS when the customer exists.
8. ZIP is edited and saved from the 5-byte screen field (ZIP+4 data is truncated on save); JUSTIFY=RIGHT date parts
   (e.g. month ` 5`) pass the edits but are saved as typed (`2014- 5-20`).
9. DUPKEY on CXACAIX is treated as a file error (WHEN OTHER) although the record was read.
10. Lower-case Y/N in Account Status / Primary Card Holder is rejected; state codes are case-sensitive.

## LIMITATIONS

* **No GnuCOBOL golden run**: COACTUPC is a CICS/BMS program (EXEC CICS, CEEDAYS via CSUTLDTC); GnuCOBOL has no CICS
  translator, so all expected values are hand-traced from the COBOL source and app/data/ASCII.
* **CEEDAYS** is emulated (`CobolDates.ceedays`, Gregorian, picture YYYYMMDD / YYYY-MM-DD, Lilian 1582-10-15 = 1).
  Only the feedback severity and message number are reproduced (CEE2EB 2507, CEE2EC 2508, CEE2EH 2513, CEE2EM 2518,
  CEE2EO 2520, severity 3); the full 12-byte token, LE national-language messages and the CEEDAYS century window
  are not. CSUTLDPY rejects every invalid date before CEEDAYS is called, so in practice only the valid path runs.
* **Invalid numeric DISPLAY data** (e.g. the PIC 9 REDEFINES of an X screen field holding spaces or letters) is
  evaluated digit-by-digit from the EBCDIC low nibble (spaces/LOW-VALUES -> 0); a nibble > 9 raises a data
  exception routed to ABEND-ROUTINE (HANDLE ABEND, abend 9999). Real behaviour depends on compiler options
  (NUMPROC, NUMCHECK, ARCH) and was not verified on z/OS.
* **NUMVAL-C / TEST-NUMVAL-C** follow the IBM rules for the default currency sign `$` and decimal point `.`; the exact
  non-zero position returned by TEST-NUMVAL(-C) is approximate (only zero / non-zero is used by COACTUPC).
* **Terminal behaviour**: MDT/FSET, field colours (`setColor` overrides are kept in the symbolic map but not exposed in
  `BmsScreen`), bright/dark rendering, FREEKB and the 3270 data stream are not emulated; the REST client is assumed
  to send every unprotected field back (all are FSET by 3300).
* **DFHCOMMAREA shorter than 160 + 873 bytes** (e.g. XCTL from COMEN01C with 160 bytes): bytes beyond EIBCALEN are
  undefined storage in CICS; modelled as LOW-VALUES (ACUP-DETAILS-NOT-FETCHED).
* **Locking / recovery**: `CicsContext.readForUpdate` takes a region-wide record lock (`RecordLockTable`) held until
  SYNCPOINT / task end, so a second task's READ UPDATE of the same record waits; after 5 s it gets RECORDBUSY
  (COACTUPC then reports "Could not lock ... for update") instead of CICS's DTIMOUT / AKCS. Backout (SYNCPOINT
  ROLLBACK, ABEND or any unexpected exception) restores before-images of changes made through
  `CicsContext.rewrite/write`; each store write is still its own JDBC transaction (no log stream / 2-phase commit),
  so a JVM crash between writes is not recovered.
* **Security**: like the COBOL, COACTUPC performs no authorization check (CICS transaction/resource security is
  external to the program). The REST adapter has no authentication; it must not be exposed without one.
* **EBCDIC collation**: comparisons use Java char order (ASCII); none of COACTUPC's comparisons are order-dependent.
* PF-key validity, abend codes and XCTL navigation are verified; COMEN01C / COCRDLIC etc. are not migrated here and
  are only returned as `nextProgram`.
