# COACTUPC - Account update (transaction CAUP)

| | |
|---|---|
| COBOL | `app/cbl/COACTUPC.cbl` (4236 lines) |
| BMS | `app/bms/COACTUP.bms` (mapset COACTUP, map CACTUPA), symbolic map `app/cpy-bms/COACTUP.CPY` |
| Copybooks | COCOM01Y, COACTUP, COTTL01Y, CSDAT01Y, CSMSG01Y, CSMSG02Y, CSUSR01Y, CVACT01Y, CVACT03Y, CVCUS01Y, CVCRD01Y, CSLKPCDY, CSUTLDWY, CSUTLDPY, CSSETATY, CSSTRPFY, DFHAID, DFHBMSCA |
| Files | ACCTDAT (key 11, LRECL 300), CUSTDAT (key 9, LRECL 500), CXACAIX (path over CCXREF, AIX key XREF-ACCT-ID offset 25 length 11, LRECL 50) |
| Java | `carddemo-online` `com.carddemo.online.coactupc.Coactupc` (+ `AcupCommarea`, `Cslkpcdy`) |
| REST | `POST /api/cics/CAUP` |
| Navigation | PF3 -> XCTL `CDEMO-FROM-PROGRAM` or `COMEN01C` (returned as `nextProgram`, not migrated yet) |

## Paragraph mapping

All methods are on `Coactupc.Task` (per-task WORKING-STORAGE).

| COBOL paragraph (lines) | Java |
|---|---|
| WORKING-STORAGE (30-650) | `Task` fields (`wsReturnMsg`, `inputFlag`, `acctFilterFlag`, edit flags, `editVariableName`, `editDate`, ...), `cdemo` (COCOM01Y), `acup` (`AcupCommarea`, WS-THIS-PROGCOMMAREA 652-849), `map`, `accountRecord`, `xrefRecord`, `customerRecord`, `abendData`, `acctUpdate` (`AcctUpdateRecord`, ACCT-UPDATE-RECORD 418-433), `custUpdate` (CUST-UPDATE-RECORD) |
| `0000-MAIN` (859-1006) | `main0000`, `mainBody` |
| `COMMON-RETURN` (1007-1020) | `commonReturn` |
| `1000-PROCESS-INPUTS` (1025-1035) | `processInputs1000` |
| `1100-RECEIVE-MAP` (1039-1425) | `receiveMap1100` (`in`, `money` helpers for the `'*'`/SPACES -> LOW-VALUES and NUMVAL-C moves) |
| `1200-EDIT-MAP-INPUTS` (1429-1677) | `editMapInputs1200` |
| `1205-COMPARE-OLD-NEW` (1681-1776) | `compareOldNew1205` |
| `1210-EDIT-ACCOUNT` (1783-1819) | `editAccount1210` |
| `1215-EDIT-MANDATORY` (1824-1851) | `editMandatory1215` |
| `1220-EDIT-YESNO` (1856-1893) | `editYesno1220` |
| `1225-EDIT-ALPHA-REQD` (1898-1950) | `editAlphaReqd1225` |
| `1230-EDIT-ALPHANUM-REQD` (1955-2008) | `editAlphanumReqd1230` |
| `1235-EDIT-ALPHA-OPT` (2012-2056) | `editAlphaOpt1235` |
| `1240-EDIT-ALPHANUM-OPT` (2061-2104) | `editAlphanumOpt1240` |
| `1245-EDIT-NUM-REQD` (2109-2175) | `editNumReqd1245` |
| `1250-EDIT-SIGNED-9V2` (2180-2220) | `editSigned9v2_1250` |
| `1260-EDIT-US-PHONE-NUM`, `EDIT-AREA-CODE`, `EDIT-US-PHONE-PREFIX`, `EDIT-US-PHONE-LINENUM`, `EDIT-US-PHONE-EXIT` (2225-2426) | `editUsPhoneNum1260` (the three sub-paragraphs are sequential sections of the method; `phoneFlags[0..2]`) |
| `1265-EDIT-US-SSN` (2431-2488) | `editUsSsn1265` |
| `1270-EDIT-US-STATE-CD` (2493-2510) | `editUsStateCd1270` |
| `1275-EDIT-FICO-SCORE` (2514-2530) | `editFicoScore1275` |
| `1280-EDIT-US-STATE-ZIP-CD` (2536-2557) | `editUsStateZipCd1280` |
| CSUTLDPY `EDIT-DATE-CCYYMMDD`, `EDIT-YEAR-CCYY`, `EDIT-MONTH`, `EDIT-DAY`, `EDIT-DAY-MONTH-YEAR`, `EDIT-DATE-LE` (CSUTLDPY 18-329) | `editDateCcyymmdd` (sections in source order) |
| CSUTLDTC / `CEEDAYS` (called from `EDIT-DATE-LE`) | `Task.ceedays` (emulation, see LIMITATIONS) |
| CSUTLDPY `EDIT-DATE-OF-BIRTH` (341-370) | `editDateOfBirth` |
| `2000-DECIDE-ACTION` (2562-2642) | `decideAction2000` |
| `3000-SEND-MAP` (2649-2663) | `sendMap3000` |
| `3100-SCREEN-INIT` (2668-2693) | `screenInit3100` |
| `3200-SETUP-SCREEN-VARS` (2698-2726) | `setupScreenVars3200` |
| `3201-SHOW-INITIAL-VALUES` (2731-2782) | `showInitialValues3201` |
| `3202-SHOW-ORIGINAL-VALUES` (2787-2866) | `showOriginalValues3202` (`editCurrency` = the `+ZZZ,ZZZ,ZZZ.99` edited move) |
| `3203-SHOW-UPDATED-VALUES` (2870-2950) | `showUpdatedValues3203` |
| `3250-SETUP-INFOMSG` (2955-2982) | `setupInfomsg3250` |
| `3300-SETUP-SCREEN-ATTRS` (2986-3436, incl. 39 `COPY CSSETATY`) | `setupScreenAttrs3300` |
| `3310-PROTECT-ALL-ATTRS` (3441-3495) | `protectAllAttrs3310` |
| `3320-UNPROTECT-FEW-ATTRS` (3500-3561) | `unprotectFewAttrs3320` |
| `3390-SETUP-INFOMSG-ATTRS` (3566-3583) | `setupInfomsgAttrs3390` |
| `3400-SEND-SCREEN` (3589-3602) | `sendScreen3400` |
| `9000-READ-ACCT` (3608-3646) | `readAcct9000` |
| `9200-GETCARDXREF-BYACCT` (3650-3697) | `getCardXrefByAcct9200` |
| `9300-GETACCTDATA-BYACCT` (3701-3747) | `getAcctDataByAcct9300` |
| `9400-GETCUSTDATA-BYCUST` (3752-3796) | `getCustDataByCust9400` |
| `9500-STORE-FETCHED-DATA` (3801-3884) | `storeFetchedData9500` |
| `9600-WRITE-PROCESSING` (3888-4104) | `writeProcessing9600` |
| `9700-CHECK-CHANGE-IN-REC` (4109-4192) | `checkChangeInRec9700` |
| CSSTRPFY `YYYY-STORE-PFKEY` (4199) | `yyyyStorePfkey` -> `PfKeys.storePfKey` |
| `ABEND-ROUTINE` (4203-4225) | `abendRoutine` |

## Copybook -> Java

| Copybook field | Java |
|---|---|
| CVACT01Y `ACCOUNT-RECORD` (300) | `AccountRecord` (`LAYOUT`, `LENGTH`, `FILE_NAME`=ACCTDAT) |
| `ACCT-ID` 9(11), `ACCT-ACTIVE-STATUS` X(1) | `ACCT_ID`, `ACTIVE_STATUS` |
| `ACCT-CURR-BAL`, `-CREDIT-LIMIT`, `-CASH-CREDIT-LIMIT`, `-CURR-CYC-CREDIT`, `-CURR-CYC-DEBIT` S9(10)V99 | `CURR_BAL`, `CREDIT_LIMIT`, `CASH_CREDIT_LIMIT`, `CURR_CYC_CREDIT`, `CURR_CYC_DEBIT` (signed zoned, scale 2) |
| `ACCT-OPEN-DATE`, `-EXPIRAION-DATE`, `-REISSUE-DATE` X(10) | `OPEN_DATE`, `EXPIRAION_DATE`, `REISSUE_DATE` |
| `ACCT-ADDR-ZIP` X(10), `ACCT-GROUP-ID` X(10), FILLER X(178) | `ADDR_ZIP`, `GROUP_ID` |
| CVCUS01Y `CUSTOMER-RECORD` (500) | `CustomerRecord` (`FILE_NAME`=CUSTDAT) |
| `CUST-ID` 9(9), `CUST-FIRST/MIDDLE/LAST-NAME` X(25), `CUST-ADDR-LINE-1/2/3` X(50) | `CUST_ID`, `FIRST_NAME`, `MIDDLE_NAME`, `LAST_NAME`, `ADDR_LINE_1/2/3` |
| `CUST-ADDR-STATE-CD` X(2), `-COUNTRY-CD` X(3), `-ZIP` X(10), `CUST-PHONE-NUM-1/2` X(15) | `ADDR_STATE_CD`, `ADDR_COUNTRY_CD`, `ADDR_ZIP`, `PHONE_NUM_1/2` |
| `CUST-SSN` 9(9), `CUST-GOVT-ISSUED-ID` X(20), `CUST-DOB-YYYY-MM-DD` X(10), `CUST-EFT-ACCOUNT-ID` X(10), `CUST-PRI-CARD-HOLDER-IND` X(1), `CUST-FICO-CREDIT-SCORE` 9(3), FILLER X(168) | `SSN`, `GOVT_ISSUED_ID`, `DOB_YYYY_MM_DD`, `EFT_ACCOUNT_ID`, `PRI_CARD_HOLDER_IND`, `FICO_CREDIT_SCORE` |
| CVACT03Y `CARD-XREF-RECORD` (50): `XREF-CARD-NUM` X(16), `XREF-CUST-ID` 9(9), `XREF-ACCT-ID` 9(11), FILLER X(14) | `CardXrefRecord` `CARD_NUM`, `CUST_ID`, `ACCT_ID`; `ACCT_PATH_NAME`=CXACAIX, `ACCT_AIX_OFFSET` 25, `ACCT_AIX_LENGTH` 11 |
| CSMSG02Y `ABEND-DATA`: `ABEND-CODE` X(4), `-CULPRIT` X(8), `-REASON` X(50), `-MSG` X(72) | `AbendData` `CODE`, `CULPRIT`, `REASON`, `MSG` |
| WS-THIS-PROGCOMMAREA `ACUP-CHANGE-ACTION` + 88s, `ACUP-OLD-DETAILS`, `ACUP-NEW-DETAILS` (incl. the `-X`/`-N` REDEFINES of money, dates, SSN, phone, FICO) | `AcupCommarea` `CHANGE_ACTION`, `OLD`, `NEW` (`Details` fields keep the COBOL suffixes: `acctIdX`, `currBal`, `openDate`, `ssn1..3`, `phone1a..c`, `ficoScoreX`, ...) |
| CSLKPCDY `VALID-PHONE-AREA-CODE`, `VALID-GENERAL-PURP-CODE`, `VALID-EASY-RECOG-AREA-CODE`, `VALID-US-STATE-CODE`, `VALID-US-STATE-ZIP-CD2-COMBO` | `Cslkpcdy` sets of the same names (checked against the copybook by `CslkpcdyTest`) |
| CSUTLDWY `WS-EDIT-DATE-*`, `WS-EDIT-DATE-FLGS`, CSUTLDTC `FEEDBACK` | `Task.editDate`, `dateFlags[3]`, `ceedays` result `{severity, msgNo}` |
| COACTUP `CACTUPAI/O` | `SymbolicMap.of("COACTUP","CACTUPA")` |
| DFHBMSCA `DFHBMPRF`, `DFHBMFSE`, `DFHRED`, `DFHBMDAR`, ... | `com.carddemo.runtime.bms.Dfhbmsca` |

## Expected-value derivation

Tests: `CoactupcEditTest` (95 cases, every 12xx edit paragraph + CSUTLDPY + CEEDAYS), `CoactupcFlowTest` (23 pseudo-conversations over the real seed records), `CslkpcdyTest`, and runtime tests `CobolNumvalTest`, `IbmDisplayNumericTest`, `PfKeysTest`, `UnitOfWorkTest`.

* Messages are the literals / STRING statements at the cited lines (e.g. `'Account Number if supplied must be a 11 digit Non-Zero Number'` 1807-1808, info messages `'Changes validated.Press F5 to save'` / `'Changes committed to database'` / `'Changes unsuccessful. Please try again'` 473-477, `DATA-WAS-CHANGED-BEFORE-UPDATE` set in 9700). Field-specific messages are `FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)` + the literal, as built by each 12xx paragraph.
* Flow fixtures use the first records of `app/data/ASCII/acctdata.txt` (account `00000000001`, balance 194.00, credit limit 2020.00, cash limit 1020.00, opened 2014-11-20, zip `A000000000`, group blank), `custdata.txt` (customer `000000001` Immanuel Madeline Kessler, NC 12546, phones (908)119-8310 / (373)693-8684, SSN 020973888, DOB 1961-06-08, FICO 274) and `cardxref.txt` (card 9680294154603697 -> cust 1 -> acct 1).
* Screen money fields: `+ZZZ,ZZZ,ZZZ.99` edited move (3202) -> `+      2,020.00`; NUMVAL-C on receive (1100) -> S9(10)V99 zoned -> `00000020200{` (ASCII overpunch).
* Boundaries: FICO 300/850 valid, 299/851 invalid (88 `FICO-RANGE-IS-VALID VALUES 300 THRU 850`); SSN part 1 `000`, `666`, `900`-`999` invalid (88 `INVALID-SSN-PART1`); leap years 2024-02-29 / 2000-02-29 valid, 2023-02-29 / 1900-02-29 invalid (EDIT-DAY-MONTH-YEAR); century 19/20 only; date of birth must be strictly before the fixed clock date 2024-03-05.
* READ UPDATE / REWRITE: 9600 reads ACCTDAT and CUSTDAT UPDATE, 9700 compares every fetched field with `ACUP-OLD-*`, then REWRITEs account then customer; a customer REWRITE failure issues `SYNCPOINT ROLLBACK` (line 4100). The CSD defines ACCTDAT and CUSTDAT with `RECOVERY(NONE)` (`app/csd/CARDDEMO.CSD`), so in the configured region (`OnlineRegionConfig`) the account REWRITE is *not* backed out; `CoactupcFlowTest` covers both that case and a recoverable account file, where the rollback restores it. 0000-MAIN issues `SYNCPOINT` (line 953) before XCTL on PF3.

### Bug-for-bug behaviour (kept, not fixed)

* **Blank group id is never "unchanged"**: 1100 (1213-1218) moves LOW-VALUES for a blank AADDGRP, while ACUP-OLD-GROUP-ID holds SPACES; `FUNCTION TRIM` (1697-1700) only strips spaces, so 1205 always sees a change for accounts with a blank group (all edits then run; customer 1's FICO 274 then fails 1275).
* **ACCT-ADDR-ZIP is overwritten**: ACCT-UPDATE-RECORD (418-433) has no ADDR-ZIP field, so ACCT-UPDATE-GROUP-ID is written at ACCT-ADDR-ZIP's offset and the original group position gets FILLER. Account 1's zip `A000000000` becomes the received group id (LOW-VALUES).
* **Customer lock failure reports success**: the 2000 EVALUATE (2607-2614) has no WHEN for COULD-NOT-LOCK-CUST-FOR-UPDATE, so WHEN OTHER sets ACUP-CHANGES-OKAYED-AND-DONE: the info line says "Changes committed to database" while the error line says the customer could not be locked (and nothing was rewritten).
* **Blank account id message**: 1210 sets "Account number not provided", but 1200 (1441-1442) then SETs NO-SEARCH-CRITERIA-RECEIVED, so the screen shows "No input received".
* **ABEND message stays blank**: CSMSG02Y gives ABEND-MSG `VALUE SPACES`, but ABEND-ROUTINE (4205) only defaults it when it equals LOW-VALUES, so an unexpected abend sends a blank message.
* PF5 before validation and unknown AIDs are treated as ENTER (0000-MAIN AID check); PF13-PF24 fold to PF1-PF12 (CSSTRPFY).
* Customer 1 cannot be re-saved unchanged: its seed values (FICO 274, NC/12546, area code 373) fail 1275/1280/1260.

## LIMITATIONS

* **CICS runtime**: pseudo-conversation, EIB, RESP codes and SYNCPOINT / SYNCPOINT ROLLBACK are emulated by `carddemo-runtime` (`UnitOfWork`), not a real CICS. Rollback backs out only files registered as recoverable (ACCTDAT/CUSTDAT are `RECOVERY(NONE)` in the CSD, so in the region nothing is backed out, as on CICS); an explicit SYNCPOINT commits; an ABEND rolls back. RESP2 values are approximations.
* **Locking**: READ UPDATE holds are per task; there is no cross-task ENQ, so the "record changed by someone else" path (9700) is only reachable when another task REWRITEs between the fetch and the PF5 pseudo-conversation (as tested), not by lock contention.
* **CEEDAYS (CSUTLDTC)**: Language Environment is not available. `Task.ceedays` emulates the `YYYYMMDD` picture only: non-numeric -> severity 3 / msg 2520, invalid calendar date -> 3 / 2508, before 1582-10-15 (Lilian day 1) -> 3 / 2513, otherwise 0 / 0 (message numbers chosen to match the documented CEEDAYS conditions, not verified on z/OS). Other CEEDAYS feedback codes, Lilian day numbers and upper-bound (9999-12-31) checks are not emulated; CSUTLDPY only uses the severity, and its own edits already reject most inputs before CEEDAYS is reached.
* **Terminal/BMS**: 3270 attributes (PROT/UNPROT, FSET, colour, DRK, cursor) are reported as metadata / attribute bytes only. What the terminal transmits (MDT) is decided by the REST client; blank fields must be sent as spaces or omitted, and the program receives omitted fields as LOW-VALUES.
* **EBCDIC**: zoned-decimal fields are stored ASCII-overpunched (`{`, `A`-`I`, `}`, `J`-`R`) as in `app/data/ASCII`. `IbmDisplayNumeric` models the z/OS low-nibble arithmetic on DISPLAY data that is not strictly numeric (e.g. `'1 '` -> 10); collation is ISO-8859-1, not EBCDIC (no browse in this program, so no visible effect). `FUNCTION UPPER-CASE`, `NUMVAL-C` / `TEST-NUMVAL-C` (`CobolNumval`) follow IBM Enterprise COBOL rules for the default currency sign `$` and decimal point `.` only.
* **Uninitialised storage**: COBOL fields without VALUE (including FILLER in ACCT-UPDATE-RECORD / CUST-UPDATE-RECORD) are assumed LOW-VALUES; on z/OS their content depends on compiler/CICS options.
* **XCTL targets** (COMEN01C or CDEMO-FROM-PROGRAM) are not migrated; the result only reports `nextProgram` and the commarea.
* **FUNCTION CURRENT-DATE** (date-of-birth edit, header) uses the JVM `Clock`.
* **Security**: like the COBOL (which relies on CICS transaction security), COACTUPC does not check that the caller signed on; the REST adapter has no authentication. Not for exposure beyond a local/demo environment.
* **Concurrency**: a rollback restores a before-image only if the record still holds this task's after-image, so it never overwrites another task's later update; true record-level locking across tasks is not emulated.
* GnuCOBOL golden files are not used (CICS commands, CEEDAYS and BMS cannot run under GnuCOBOL); all expected values are hand-traced from the source and seed data.
