# COACTVWC - Account view (transaction CAVW)

Source: `app/cbl/COACTVWC.cbl`, map `app/bms/COACTVW.bms` (symbolic map `app/cpy-bms/COACTVW.CPY`).
Java: `carddemo-online` `com.carddemo.online.coactvwc.Coactvwc` (Spring `@Component`, `CicsProgram`).
REST: `POST /api/cics/CAVW` or `POST /api/cics/programs/COACTVWC` (generic adapter).

Files (registered in `OnlineRegionConfig.fileControl`, seeded from `app/data/ASCII` when empty):

| CICS file | Dataset (CSD) | Record | Key | Seed |
|---|---|---|---|---|
| ACCTDAT | AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS | `AccountRecord` (300) | ACCT-ID 0/11 | acctdata.txt |
| CUSTDAT | AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS | `CustomerRecord` (500) | CUST-ID 0/9 | custdata.txt |
| CCXREF | AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS | `CardXrefRecord` (50) | XREF-CARD-NUM 0/16 | cardxref.txt |
| CXACAIX | PATH over CCXREF (XREFFILE.jcl: KEYS(11,25) NONUNIQUEKEY) | `CardXrefRecord` | XREF-ACCT-ID 25/11 | - |

## Paragraph map

| COBOL paragraph (lines) | Java |
|---|---|
| 0000-MAIN (262-392) | `Coactvwc.Task#main0000` |
| 0000-MAIN WHEN CCARD-AID-PFK03 (320-344) | `Coactvwc.Task#pf3Exit` |
| COMMON-RETURN (394-404) | `Coactvwc.Task#commonReturn` |
| 1000-SEND-MAP (416-428) | `Coactvwc.Task#sendMap1000` |
| 1100-SCREEN-INIT (431-458) | `Coactvwc.Task#screenInit1100` |
| 1200-SETUP-SCREEN-VARS (460-540) | `Coactvwc.Task#setupScreenVars1200` |
| 1300-SETUP-SCREEN-ATTRS (541-576) | `Coactvwc.Task#setupScreenAttrs1300` |
| 1400-SEND-SCREEN (577-594) | `Coactvwc.Task#sendScreen1400` |
| 2000-PROCESS-INPUTS (596-608) | `Coactvwc.Task#processInputs2000` |
| 2100-RECEIVE-MAP (610-621) | `Coactvwc.Task#receiveMap2100` |
| 2200-EDIT-MAP-INPUTS (622-648) | `Coactvwc.Task#editMapInputs2200` |
| 2210-EDIT-ACCOUNT (649-686) | `Coactvwc.Task#editAccount2210` |
| 9000-READ-ACCT (687-722) | `Coactvwc.Task#readAcct9000` |
| 9200-GETCARDXREF-BYACCT (723-773) | `Coactvwc.Task#getCardXrefByAcct9200` |
| 9300-GETACCTDATA-BYACCT (774-824) | `Coactvwc.Task#getAcctDataByAcct9300` |
| 9400-GETCUSTDATA-BYCUST (825-870) | `Coactvwc.Task#getCustDataByCust9400` |
| SEND-PLAIN-TEXT (877-890) | `Coactvwc.Task#sendPlainText` |
| SEND-LONG-TEXT (891-909, never performed) | `Coactvwc.Task#sendLongText` |
| YYYY-STORE-PFKEY (COPY CSSTRPFY) | `com.carddemo.runtime.cics.PfKeys#storePfKey` |
| ABEND-ROUTINE (916-933) | `Coactvwc.Task#abendRoutine` (via `Coactvwc#execute` catch = HANDLE ABEND) |

## Copybooks

| Copybook | Java |
|---|---|
| CVACT01Y ACCOUNT-RECORD | `com.carddemo.common.copybook.AccountRecord` (new) |
| CVACT02Y CARD-RECORD | `com.carddemo.common.copybook.CardRecord` (new; not used by COACTVWC, shared by later programs) |
| CVACT03Y CARD-XREF-RECORD | `com.carddemo.common.copybook.CardXrefRecord` (new) |
| CVCUS01Y CUSTOMER-RECORD | `com.carddemo.common.copybook.CustomerRecord` (new) |
| CSMSG02Y ABEND-DATA | `com.carddemo.common.copybook.AbendData` (new) |
| COCOM01Y | `CardDemoCommarea` (existing) |
| COTTL01Y / CSDAT01Y / CSMSG01Y | `ScreenTitles` / `DateTimeFields` / `CommonMessages` (existing) |
| CVCRD01Y CC-WORK-AREA | `Task#ccardAid`, `Task#ccAcctId` (only items used) |
| COACTVW (BMS) | `SymbolicMap.of("COACTVW","CACTVWA")` built from app/bms/COACTVW.bms |
| CSSTRPFY | `PfKeys` |
| DFHAID / DFHBMSCA | `AidKey` / constants in `Coactvwc` (DFHBMFSE X'C8', DFHDFCOL X'00', DFHRED X'F2', DFHNEUTR X'F7', DFHBMDAR X'4C') |

New record fields (field-for-field, COBOL name in each constant's Javadoc):

- `AccountRecord`: ACCT_ID 9(11) @0, ACTIVE_STATUS X @11, CURR_BAL S9(10)V99 @12, CREDIT_LIMIT @24, CASH_CREDIT_LIMIT @36, OPEN_DATE X(10) @48, EXPIRAION_DATE @58, REISSUE_DATE @68, CURR_CYC_CREDIT @78, CURR_CYC_DEBIT @90, ADDR_ZIP X(10) @102, GROUP_ID X(10) @112, FILLER X(178).
- `CardRecord`: CARD_NUM X(16), ACCT_ID 9(11), CVV_CD 9(3), EMBOSSED_NAME X(50), EXPIRAION_DATE X(10), ACTIVE_STATUS X, FILLER X(59).
- `CardXrefRecord`: CARD_NUM X(16), CUST_ID 9(9), ACCT_ID 9(11) @25, FILLER X(14).
- `CustomerRecord`: CUST_ID 9(9), FIRST/MIDDLE/LAST_NAME X(25), ADDR_LINE_1..3 X(50), ADDR_STATE_CD X(2), ADDR_COUNTRY_CD X(3), ADDR_ZIP X(10), PHONE_NUM_1/2 X(15), SSN 9(9), GOVT_ISSUED_ID X(20), DOB_YYYY_MM_DD X(10), EFT_ACCOUNT_ID X(10), PRI_CARD_HOLDER_IND X, FICO_CREDIT_SCORE 9(3) @329, FILLER X(168).
- `AbendData`: CODE X(4), CULPRIT X(8), REASON X(50), MSG X(72), all VALUE SPACES (`AbendData.initial()`).

## Runtime additions (additive only)

- `runtime.cics.PfKeys#storePfKey` (CSSTRPFY).
- `runtime.vsam.FileControl#register(AlternateIndexPath)` / `#path`; `FileControl#read` falls back to a registered PATH when no base file has that name.
- `runtime.bms.SymbolicMap#color(name)` getter.
- `runtime.cobol.NumericEdited#format` (MOVE to numeric-edited: 9, Z, comma, period, fixed +/-).
- `common.copybook.AsciiSeedData` (loads app/data/ASCII seeds, packaged by carddemo-runtime/pom.xml).

## Expected-value derivation (CoactvwcTest)

- Header: 1100-SCREEN-INIT lines 433-456 with fixed clock 2024-03-05T14:07:09.25Z.
- Account 00000000001: acctdata.txt line 1 (`Y 00000001940{ 00000020200{ 00000010200{ 2014-11-20 2025-05-20 2025-05-20 00000000000{ 00000000000{`; `{` = positive 0 overpunch). PICOUT `+ZZZ,ZZZ,ZZZ.99` gives `+        194.00`, `+      2,020.00`, `+      1,020.00`, `+           .00`.
- Xref: cardxref.txt line 49 (`9680294154603697 000000001 00000000001`), moved to CDEMO-CARD-NUM / CDEMO-CUST-ID (lines 744-745).
- Customer 000000001: custdata.txt line 1 (Immanuel Madeline Kessler, SSN 020973888 -> `020-97-3888` via STRING line 497-505, FICO 274, ...). Phone and ZIP truncated to the map lengths (13 / 5).
- Messages: literals from lines 117-150, 672, 750-753, 799-802, 849-852. RESP/RESP2 MOVE S9(9) COMP -> X(10) gives `000000013 ` / `000000080 `; STRING into WS-RETURN-MSG X(75) truncates (e.g. `...Resp:000000013  Reas:0000`). File errors: WS-FILE-ERROR-MESSAGE (80 bytes, lines 87-105) truncated to 75. DISABLED = 84/50, NOTOPEN = 19/60, DUPKEY = 15/0 (runtime codes).

## Bug-for-bug behaviour preserved

1. Line 283 tests `CDEMO-FROM-PROGRAM = 'COMEN01C'` before the commarea is copied, so only EIBCALEN = 0 initializes the commarea.
2. A blank account id gets `Account number not provided`; the following `IS NOT NUMERIC` test (not an ELSE) resets the flag to NOT-OK, so 2200's `No input received` and 1300's red `*` (lines 562-566) are unreachable.
3. 88-levels SEARCHED-ACCT-ZEROES / SEARCHED-ACCT-NOT-NUMERIC are never used; the shown text is `Account Filter must  be a non-zero 11 digit number` (two spaces).
4. 9000 tests DID-NOT-FIND-ACCT-IN-ACCTDAT (line 710), which no paragraph sets, so CUSTDAT is still read after an ACCTDAT NOTFND and FOUND-CUST makes 1200 move the never-read ACCOUNT-RECORD.
5. WS-INFO-MSG is always `Enter or update id of account to display` (WS-INFORM-OUTPUT is never set), so INFOMSG is always DFHNEUTR.
6. PF3 sets CDEMO-USRTYP-USER (line 336) even for an admin caller.
7. ABEND-ROUTINE checks ABEND-MSG for LOW-VALUES but CSMSG02Y gives it VALUE SPACES, so `UNEXPECTED ABEND OCCURRED.` is never moved.
8. Any AID other than ENTER/PF3/PF15 (incl. PA3 and CLEAR) is treated as ENTER.

## LIMITATIONS

- CICS/BMS runtime behaviour is emulated by carddemo-runtime (`CicsContext`, `SymbolicMap`); no real CICS region or 3270 terminal was used, and GnuCOBOL golden runs do not apply to a CICS program. Expected values are hand-traced from the COBOL.
- Terminal-side field validation (BMS `NUM`/`MUSTFILL` on ACCTSID) is not enforced; non-numeric or short input reaches the program as typed and is rejected by 2210 as COBOL would.
- Colours and attributes (DFHRED, DFHBMFSE, DFHNEUTR, DFHBMDAR) are kept in the symbolic map and asserted at task level; the REST screen payload does not render them.
- MOVE of invalid zoned data (a never-read, LOW-VALUES ACCOUNT-RECORD after an ACCTDAT NOTFND) to a numeric-edited field is implementation dependent on z/OS; Java uses each byte's low nibble (LOW-VALUES -> 0, shown as `+           .00`).
- DFHCOMMAREA bytes 161-172 beyond EIBCALEN are undefined storage on CICS; Java treats them as spaces.
- RESP2 values and the DUPKEY response for the non-unique CXACAIX path come from the runtime's VSAM emulation; EBCDIC collation is not involved (keys are numeric).
- XCTL targets (COMEN01C or the caller in CDEMO-FROM-PROGRAM) are not implemented; navigation is returned as `nextProgram` plus the 160-byte commarea.
- ASCII seeds are used (app/data/ASCII); their overpunch signs are decoded by `ZonedDecimal`. The EBCDIC datasets were not cross-checked byte for byte.
- CURRENT-DATE comes from the injected `Clock`.
