# COACTVWC - Account view (transaction CAVW)

| | |
|---|---|
| COBOL | `app/cbl/COACTVWC.cbl` |
| BMS | `app/bms/COACTVW.bms` (mapset COACTVW, map CACTVWA), symbolic map `app/cpy-bms/COACTVW.CPY` |
| Copybooks | CVCRD01Y, COCOM01Y, DFHBMSCA, DFHAID, COTTL01Y, COACTVW, CSDAT01Y, CSMSG01Y, CSMSG02Y, CSUSR01Y, CVACT01Y, CVACT02Y, CVACT03Y, CVCUS01Y, CSSTRPFY |
| Files | `CXACAIX` (AIX path over CCXREF / `AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS`, `KEYS(11,25) NONUNIQUEKEY`), `ACCTDAT` (key 11, LRECL 300), `CUSTDAT` (key 9, LRECL 500) |
| Java | `carddemo-online` `com.carddemo.online.coactvwc.Coactvwc` |
| REST | `POST /api/cics/CAVW` |
| Navigation | PF3: XCTL to `CDEMO-FROM-PROGRAM` (or `COMEN01C` when blank) - returned as `nextProgram`, not migrated yet |

## Paragraph mapping

| COBOL paragraph (lines) | Java |
|---|---|
| WORKING-STORAGE (35-254) | `Coactvwc.Task` fields (`ws*`, `ccard*`/`cc*` for CVCRD01Y, `commarea`, `thisProgCommarea`, `wsCommarea`, `cactvwa`, `abendData`, `accountRecord`, `cardXrefRecord`, `customerRecord`) |
| `0000-MAIN` (262-393) | `Coactvwc.Task#main` (+ `Coactvwc#execute` for `HANDLE ABEND LABEL(ABEND-ROUTINE)`) |
| `COMMON-RETURN` (394-407) | `Coactvwc.Task#commonReturn` |
| `0000-MAIN-EXIT` (408-413, duplicated label) | - (EXIT) |
| `1000-SEND-MAP` (416-429) | `Coactvwc.Task#sendMap1000` |
| `1100-SCREEN-INIT` (431-459) | `Coactvwc.Task#screenInit1100` |
| `1200-SETUP-SCREEN-VARS` (460-539) | `Coactvwc.Task#setupScreenVars1200` |
| `1300-SETUP-SCREEN-ATTRS` (541-576) | `Coactvwc.Task#setupScreenAttrs1300` |
| `1400-SEND-SCREEN` (577-594) | `Coactvwc.Task#sendScreen1400` |
| `2000-PROCESS-INPUTS` (596-609) | `Coactvwc.Task#processInputs2000` |
| `2100-RECEIVE-MAP` (610-621) | `Coactvwc.Task#receiveMap2100` |
| `2200-EDIT-MAP-INPUTS` (622-647) | `Coactvwc.Task#editMapInputs2200` |
| `2210-EDIT-ACCOUNT` (649-685) | `Coactvwc.Task#editAccount2210` |
| `9000-READ-ACCT` (687-722) | `Coactvwc.Task#readAcct9000` |
| `9200-GETCARDXREF-BYACCT` (723-773) | `Coactvwc.Task#getCardXrefByAcct9200` |
| `9300-GETACCTDATA-BYACCT` (774-823) | `Coactvwc.Task#getAcctDataByAcct9300` |
| `9400-GETCUSTDATA-BYCUST` (825-872) | `Coactvwc.Task#getCustDataByCust9400` |
| `SEND-PLAIN-TEXT` (877-890) | `Coactvwc.Task#sendPlainText` |
| `SEND-LONG-TEXT` (896-909) | `Coactvwc.Task#sendLongText` (never performed; all callers are commented out) |
| `YYYY-STORE-PFKEY` (CSSTRPFY, 913) | `Coactvwc.Task#yyyyStorePfkey` -> `PfKeys#storePfKey` (runtime) |
| `ABEND-ROUTINE` (916-937) | `Coactvwc.Task#abendRoutine` |

## Copybook -> Java

| Copybook field | Java |
|---|---|
| CVACT01Y `ACCOUNT-RECORD` (300): `ACCT-ID` 9(11), `ACCT-ACTIVE-STATUS` X, `ACCT-CURR-BAL`/`-CREDIT-LIMIT`/`-CASH-CREDIT-LIMIT` S9(10)V99, `ACCT-OPEN-DATE`/`-EXPIRAION-DATE`/`-REISSUE-DATE` X(10), `ACCT-CURR-CYC-CREDIT`/`-DEBIT` S9(10)V99, `ACCT-ADDR-ZIP` X(10), `ACCT-GROUP-ID` X(10), FILLER X(178) | `AccountRecord` `ACCT_ID`, `ACTIVE_STATUS`, `CURR_BAL`, `CREDIT_LIMIT`, `CASH_CREDIT_LIMIT`, `OPEN_DATE`, `EXPIRAION_DATE`, `REISSUE_DATE`, `CURR_CYC_CREDIT`, `CURR_CYC_DEBIT`, `ADDR_ZIP`, `GROUP_ID` (zoned DISPLAY, overpunch sign) |
| CVCUS01Y `CUSTOMER-RECORD` (500): `CUST-ID` 9(9), names X(25) x3, `CUST-ADDR-LINE-1..3` X(50), `-STATE-CD` X(2), `-COUNTRY-CD` X(3), `-ZIP` X(10), `CUST-PHONE-NUM-1/2` X(15), `CUST-SSN` 9(9), `CUST-GOVT-ISSUED-ID` X(20), `CUST-DOB-YYYY-MM-DD` X(10), `CUST-EFT-ACCOUNT-ID` X(10), `CUST-PRI-CARD-HOLDER-IND` X, `CUST-FICO-CREDIT-SCORE` 9(3), FILLER X(168) | `CustomerRecord` `CUST_ID`, `FIRST_NAME`, `MIDDLE_NAME`, `LAST_NAME`, `ADDR_LINE_1..3`, `ADDR_STATE_CD`, `ADDR_COUNTRY_CD`, `ADDR_ZIP`, `PHONE_NUM_1/2`, `SSN`, `GOVT_ISSUED_ID`, `DOB_YYYY_MM_DD`, `EFT_ACCOUNT_ID`, `PRI_CARD_HOLDER_IND`, `FICO_CREDIT_SCORE` |
| CVACT03Y `CARD-XREF-RECORD` (50): `XREF-CARD-NUM` X(16), `XREF-CUST-ID` 9(9), `XREF-ACCT-ID` 9(11), FILLER X(14) | `CardXrefRecord` `CARD_NUM`, `CUST_ID`, `ACCT_ID` (+ `ACCT_AIX_OFFSET` 25 / `ACCT_AIX_LENGTH` 11) |
| CSMSG02Y `ABEND-DATA` (134): `ABEND-CODE` X(4), `ABEND-CULPRIT` X(8), `ABEND-REASON` X(50), `ABEND-MSG` X(72), all VALUE SPACES | `AbendData` `ABEND_CODE`, `ABEND_CULPRIT`, `ABEND_REASON`, `ABEND_MSG` |
| CVCRD01Y `CC-WORK-AREA` (`CCARD-AID` + 88s, `CCARD-NEXT-PROG/MAPSET/MAP`, `CCARD-ERROR-MSG`, `CCARD-RETURN-MSG`, `CC-ACCT-ID` X(11) / `-N` 9(11), `CC-CARD-NUM`, `CC-CUST-ID`) | program-private `Coactvwc.Task` strings `ccardAid`, `ccardNextProg`, `ccardNextMapset`, `ccardNextMap`, `ccardErrorMsg`, `ccardReturnMsg`, `ccAcctId`, `ccCardNum`, `ccCustId`; 88s via `ccardAidIs(..)` |
| WS-THIS-PROGCOMMAREA (213-216) `CA-FROM-PROGRAM` X(8), `CA-FROM-TRANID` X(4) | `Coactvwc.THIS_PROGCOMMAREA`, `CA_FROM_PROGRAM`, `CA_FROM_TRANID` |
| `WS-COMMAREA` X(2000) | `Task.wsCommarea` (RETURN TRANSID passes all 2000 bytes: 160 commarea + 12 + 1828 spaces) |
| 88s `INPUT-OK/ERROR`, `PFK-VALID/INVALID`, `FLG-ACCTFILTER-*`, `FLG-CUSTFILTER-*`, `FOUND-ACCT/CUST-IN-MASTER`, `WS-NO-INFO-MESSAGE`, `WS-RETURN-MSG-OFF` | `Task#inputError`, `wsPfkFlag`, `flgAcctfilterNotOk/Blank`, `wsEditCustFlag`, `foundAcctInMaster/foundCustInMaster`, `wsNoInfoMessage`, `wsReturnMsgOff` |
| `WS-FILE-ERROR-MESSAGE` (86-105) | `Task#wsFileErrorMessage` (FILLER literals + `errorOpname/File/Resp/Resp2`) |
| COACTVW `CACTVWAI/O` | `SymbolicMap.of("COACTVW","CACTVWA")`; `ACCTSIDA` -> `setAttribute`, `xxxC` -> `setColor`/`color`, `xxxL = -1` -> `setCursor` |
| DFHBMSCA `DFHBMFSE`, `DFHBMDAR`, `DFHDFCOL`, `DFHRED`, `DFHNEUTR` | program-private `Dfhbmsca` (EBCDIC byte values) |
| CVACT02Y `CARD-RECORD` | COPYed but never referenced by the procedure division: not mapped |

## Expected-value derivation (tests in `CoactvwcTest`)

* Data: `app/data/ASCII/acctdata.txt` line 1 (account `00000000001`: `Y`, `00000001940{` = +194.00, `00000020200{` =
  +2020.00, `00000010200{` = +1020.00, dates 2014-11-20 / 2025-05-20 / 2025-05-20, cycle amounts 0, group blank),
  `custdata.txt` line 1 (customer `000000001`, Immanuel Madeline Kessler, SSN `020973888`, FICO `274`, ...),
  `cardxref.txt` line 49 (card `9680294154603697` -> cust `000000001`, acct `00000000001`). The xref file has one card
  per account (`seedHasOneXrefPerAccount`), so DUPKEY needs an injected second card.
* Amounts: `ACURBALO` etc. are `PIC +ZZZ,ZZZ,ZZZ.99` (COACTVW.CPY 326): fixed `+`/`-`, zero suppression with commas,
  9 integer positions for a 10-digit source (high digit dropped, e.g. 9876543210.99 -> `+876,543,210.99`).
* SSN: `STRING CUST-SSN(1:3) '-' (4:2) '-' (6:4)` (lines 496-504) = 11 bytes into X(12); byte 12 keeps the
  LOW-VALUE of `MOVE LOW-VALUES TO CACTVWAO` (shown blank).
* Truncations: `CUST-ADDR-ZIP` X(10) -> X(5), `CUST-PHONE-NUM-n` X(15) -> X(13), WS-RETURN-MSG X(75) -> ERRMSG X(78) padded,
  WS-INFO-MSG X(40) -> INFOMSG X(45) padded.
* NOTFND messages: `STRING ... DELIMITED BY SIZE INTO WS-RETURN-MSG` (75 bytes). `ERROR-RESP` = `MOVE WS-RESP-CD`
  (S9(9) COMP) `TO PIC X(10)` = 9 unsigned digits + space (`000000013 `); NOTFND RESP2 = 80. The xref message is
  8+11+13+23+10+6+10 = 81 characters, so it ends `Reas:0000`; ACCTDAT the same; CUSTDAT 7+9+10+26+10+6+10 = 78 -> `REAS:0000000`.
* File errors (WHEN OTHER): `WS-FILE-ERROR-MESSAGE` is 80 bytes (`File Error: ` + `READ    ` + ` on ` + file X(9) +
  ` returned RESP ` + resp + `,RESP2 ` + resp2 + 5 spaces) moved to X(75). DISABLED = RESP 84 / RESP2 50, DUPKEY = 15 / 0,
  FILENOTFOUND = 12 / 1 (runtime values).
* Header: fixed clock 2024-03-05T14:07:09.25Z -> `03/05/24`, `14:07:09`; this map has no APPLID/SYSID.
* Cursor: lines 546-552 always `MOVE -1 TO ACCTSIDL`.

### Bug-for-bug behaviour (kept, not fixed)

* Lines 282-284 test `CDEMO-FROM-PROGRAM = 'COMEN01C'` *before* DFHCOMMAREA is copied, i.e. on uninitialised
  WORKING-STORAGE (LOW-VALUES): never true, so an incoming commarea is always copied (never INITIALIZEd).
* `DFHCOMMAREA(161:12)` is moved even when EIBCALEN is 160 (reads past the commarea).
* 2210 sets `'Account number not provided'`, then 2200 (lines 640-642) overwrites it with `'No input received'`.
* The 2210 message literal is `'Account Filter must  be a non-zero 11 digit number'` (two spaces); the 88-level
  `SEARCHED-ACCT-NOT-NUMERIC` text is never used.
* `DID-NOT-FIND-ACCT-IN-ACCTDAT` / `-CUST-IN-CUSTDAT` are never SET (lines 792, 842 commented out), so the exits at
  lines 704 / 713 never fire: after an ACCTDAT NOTFND or file error, CUSTDAT is still read and, if the customer
  exists, the never-read ACCOUNT-RECORD is displayed (amounts `+           .00`, other fields blank).
* `WS-INFORM-OUTPUT` is never SET: 9000 blanks WS-INFO-MSG and 1200 restores the prompt, so INFOMSG always shows
  `Enter or update id of account to display` (and is NEUTRAL, never dark).
* `MOVE DFHBMDAR TO INFOMSGC` (line 568) moves an attribute constant into the colour byte (unreachable anyway).
* PF3 sets `CDEMO-USRTYP-USER` (line 344) even for admin users.
* A non-unique CXACAIX key (account with two cards) returns DUPKEY, which the EVALUATE treats as a file error.
* 9400's WHEN OTHER message is not guarded by `WS-RETURN-MSG-OFF` (overwrites an earlier message), unlike the NOTFND branches.
* `ABEND-MSG` has `VALUE SPACES`, never LOW-VALUES, so `'UNEXPECTED ABEND OCCURRED.'` is never used; WHEN OTHER of
  0000-MAIN fills ABEND-CODE/-CULPRIT but sends `UNEXPECTED DATA SCENARIO` and RETURNs without abending.
* The `IF INPUT-ERROR` block after the main EVALUATE (lines 387-392) is unreachable.

## LIMITATIONS

* **CICS runtime**: pseudo-conversation, EIB, HANDLE ABEND and RESP codes are emulated by `carddemo-runtime`. RESP values
  follow CICS numbering; RESP2 values (80 NOTFND, 50 DISABLED, ...) are approximations, and they appear verbatim in the messages.
* **Uninitialised storage**: items without VALUE (CARDDEMO-COMMAREA, ACCOUNT-RECORD, symbolic map) start as LOW-VALUES; on
  z/OS their content depends on compiler/LE/CICS options. This decides the COMEN01C test above and the "never-read
  ACCOUNT-RECORD" display; reading `DFHCOMMAREA` past EIBCALEN is undefined in CICS and is modelled as spaces.
* **Data exceptions**: a LOW-VALUES zoned amount is edited as zero (PACK + ED of X'00' digits); any other invalid numeric
  throws and runs ABEND-ROUTINE (as an S0C7 / ASRA would). Exact z/OS behaviour depends on compiler options (NUMPROC, ZONECHECK).
* **ABEND-ROUTINE**: `EXEC CICS SEND FROM(ABEND-DATA)` (no map) is reported as a text output without ERASE.
* **Terminal/BMS**: colour/attribute bytes are recorded in the symbolic map (`color`, `attributeByte`) but not rendered; `PICIN`
  and `VALIDN=(MUSTFILL)` on ACCTSID are terminal-side validations and are not enforced (the program's own edits are).
  What the terminal transmits (MDT) is decided by the REST client.
* **EBCDIC collation**: AIX ordering uses ISO-8859-1 byte order; only keyed READs are used here, so no visible effect.
* **XCTL targets** (COMEN01C or the caller in `CDEMO-FROM-PROGRAM`) are not migrated; only `nextProgram` + commarea are returned.
* GnuCOBOL golden files are not used for this online program (CICS commands cannot run under GnuCOBOL); expected values are
  hand-traced from the source and the app/data/ASCII records.
