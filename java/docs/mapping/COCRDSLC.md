# COCRDSLC - Credit card detail view (transaction CCDL)

| | |
|---|---|
| COBOL | `app/cbl/COCRDSLC.cbl` |
| BMS | `app/bms/COCRDSL.bms` (mapset COCRDSL, map CCRDSLA), symbolic map `app/cpy-bms/COCRDSL.CPY` |
| Copybooks | CVCRD01Y, COCOM01Y, DFHBMSCA, DFHAID, COTTL01Y, COCRDSL, CSDAT01Y, CSMSG01Y, CSMSG02Y, CSUSR01Y, CVACT02Y, CVCUS01Y, CSSTRPFY |
| Files | CARDDAT (CSD `DEFINE FILE(CARDDAT)`, DSN `AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS`, key 16, LRECL 150), CARDAIX (path on CARD-ACCT-ID; only in dead paragraph 9150) |
| Java | `carddemo-online` `com.carddemo.online.cocrdslc.Cocrdslc` |
| REST | `POST /api/cics/CCDL` (fields `ACCTSID`, `CARDSID`) or `POST /api/cics/programs/COCRDSLC` (XCTL from COCRDLIC / COMEN01C) |
| Navigation | PF3 -> XCTL `CDEMO-FROM-PROGRAM` (COCRDLIC when called from the card list), else `COMEN01C`; returned as `nextProgram`, not migrated yet |

## Paragraph mapping

| COBOL paragraph (lines) | Java |
|---|---|
| WORKING-STORAGE (35-240) | `Cocrdslc.Task` fields (`wsInputFlag`, `wsEditAcctFlag`, `wsEditCardFlag`, `wsReturnMsg`, `wsInfoMsg`, `ccAcctId`, `ccCardNum`, `commarea`, `wsThisProgcommarea`, `ccrdsla`, `cardRecord`, `abendData`) and `Cocrdslc` `LIT_*` / message constants |
| `EXEC CICS HANDLE ABEND LABEL(ABEND-ROUTINE)` (250-252) | `Cocrdslc.Task#run` |
| `0000-MAIN` (248-392) | `Cocrdslc.Task#mainPara` |
| PF3 branch of `0000-MAIN` (305-334) | `Cocrdslc.Task#xctlToCallerOrMenu` |
| `COMMON-RETURN` (394-407) | `Cocrdslc.Task#commonReturn` |
| `1000-SEND-MAP` (412-421) | `Cocrdslc.Task#sendMap1000` |
| `1100-SCREEN-INIT` (427-451) | `Cocrdslc.Task#screenInit1100` |
| `1200-SETUP-SCREEN-VARS` (457-497) | `Cocrdslc.Task#setupScreenVars1200` |
| `1300-SETUP-SCREEN-ATTRS` (502-558) | `Cocrdslc.Task#setupScreenAttrs1300` (+ `cameFromCardList`) |
| `1400-SEND-SCREEN` (563-577) | `Cocrdslc.Task#sendScreen1400` |
| `2000-PROCESS-INPUTS` (582-591) | `Cocrdslc.Task#processInputs2000` |
| `2100-RECEIVE-MAP` (596-603) | `Cocrdslc.Task#receiveMap2100` |
| `2200-EDIT-MAP-INPUTS` (608-641) | `Cocrdslc.Task#editMapInputs2200` |
| `2210-EDIT-ACCOUNT` (647-679) | `Cocrdslc.Task#editAccount2210` |
| `2220-EDIT-CARD` (685-720) | `Cocrdslc.Task#editCard2220` |
| `9000-READ-DATA` (726-730) | `Cocrdslc.Task#readData9000` |
| `9100-GETCARD-BYACCTCARD` (736-773) | `Cocrdslc.Task#getcardByacctcard9100` |
| `9150-GETCARD-BYACCT` (779-809, never PERFORMed) | `Cocrdslc.Task#getcardByacct9150` |
| `SEND-LONG-TEXT` (820-830, never PERFORMed) | `Cocrdslc.Task#sendLongText` |
| `SEND-PLAIN-TEXT` (838-848) | `Cocrdslc.Task#sendPlainText` |
| `YYYY-STORE-PFKEY` (CSSTRPFY, line 855) | `Cocrdslc.Task#yyyyStorePfkey` -> `PfKeys#storePfKey` |
| `ABEND-ROUTINE` (857-878) | `Cocrdslc.Task#abendRoutine` |

## Copybook -> Java

| Copybook field | Java |
|---|---|
| CVACT02Y `CARD-RECORD` (150): `CARD-NUM` X(16), `CARD-ACCT-ID` 9(11), `CARD-CVV-CD` 9(3), `CARD-EMBOSSED-NAME` X(50), `CARD-EXPIRAION-DATE` X(10), `CARD-ACTIVE-STATUS` X(1), FILLER X(59) | `com.carddemo.common.copybook.CardRecord` `NUM`, `ACCT_ID`, `CVV_CD`, `EMBOSSED_NAME`, `EXPIRAION_DATE`, `ACTIVE_STATUS` (`LAYOUT`, `KEY_*`, `ACCT_ID_OFFSET/LENGTH` for CARDAIX) |
| CSMSG02Y `ABEND-DATA` (134): `ABEND-CODE` X(4), `ABEND-CULPRIT` X(8), `ABEND-REASON` X(50), `ABEND-MSG` X(72) | `com.carddemo.common.copybook.AbendData` `CODE`, `CULPRIT`, `REASON`, `MSG` |
| CSSTRPFY `YYYY-STORE-PFKEY` | `com.carddemo.runtime.cics.PfKeys#storePfKey` (returns the CCARD-AID value; `null` = no WHEN matched) |
| CVCRD01Y `CC-WORK-AREA`: `CCARD-AID` X(5) + 88s, `CCARD-NEXT-PROG/-MAPSET/-MAP`, `CCARD-ERROR-MSG`, `CCARD-RETURN-MSG`, `CC-ACCT-ID` X(11) / `-N` 9(11), `CC-CARD-NUM` X(16) / `-N` 9(16), `CC-CUST-ID` | program-private `Task` fields `ccardAid` ("ENTER", "PFK03", ...), `ccardNextProg`, `ccardNextMapset`, `ccardNextMap`, `ccardErrorMsg`, `ccardReturnMsg`, `ccAcctId`, `ccCardNum`, `ccCustId` |
| `WS-INFO-MSG` 88s `FOUND-CARDS-FOR-ACCOUNT`, `WS-PROMPT-FOR-INPUT` | `FOUND_CARDS_FOR_ACCOUNT`, `WS_PROMPT_FOR_INPUT` |
| `WS-RETURN-MSG` 88s | `WS_PROMPT_FOR_ACCT`, `WS_PROMPT_FOR_CARD`, `NO_SEARCH_CRITERIA_RECEIVED`, `DID_NOT_FIND_ACCT_IN_CARDXREF`, `DID_NOT_FIND_ACCTCARD_COMBO` (unused 88s `SEARCHED-*`, `XREF-READ-ERROR`, `CODING-TO-BE-DONE` omitted) |
| `WS-FILE-ERROR-MESSAGE` (80) | `Task#wsFileErrorMessage` (`errorOpname`, `errorFile`, `errorResp`, `errorResp2`) |
| `WS-THIS-PROGCOMMAREA` (`CA-FROM-PROGRAM` X(8), `CA-FROM-TRANID` X(4)) | `Task#wsThisProgcommarea` (12-char string, carried at commarea bytes 161-172) |
| `WS-COMMAREA` X(2000) | built in `Task#commonReturn` |
| COCRDSL `CCRDSLAI/O` | `SymbolicMap.of("COCRDSL","CCRDSLA")`; `xxxC` colour bytes recorded in `Task#colors` |
| DFHBMSCA `DFHBMPRF`/`DFHBMFSE`/`DFHBMDAR`/`DFHDFCOL`/`DFHRED`/`DFHNEUTR` | `Cocrdslc` constants (EBCDIC byte values X'61', X'C1', X'4C', X'00', X'F2', X'F7') |
| CVCUS01Y, CSUSR01Y, CSMSG01Y | COPYed but not referenced: not mapped |

## Expected-value derivation (tests in `CocrdslcTest`, `CocrdslcRestTest`)

* Card data: `app/data/ASCII/carddata.txt` (50 records) line 1 = `0500024453765740` / account `00000000050` / CVV 747 /
  `Aniya Von` / `2023-03-09` / `Y`, so EXPMON `03`, EXPYEAR `2023` (REDEFINES at lines 85-90, moves at 477-484).
  Every account has exactly one card in the sample data; the DUPKEY case adds a second card in the test.
* Header: fixed clock 2024-03-05T14:07:09.25Z -> CURDATE `03/05/24`, CURTIME `14:07:09` (lines 437-449, CURTIME is 8 bytes here).
* Messages: literals at lines 130-154, 670, 711, 377; INFOMSG is 40 bytes, ERRMSG 80 (WS-RETURN-MSG 75 padded).
* File error message: `'File Error: '` + `READ    ` + `' on '` + `CARDDAT  ` + `' returned RESP '` + `000000084 ` + `',RESP2 '` +
  `000000050 ` = 75 bytes after truncation into WS-RETURN-MSG (80-byte group, last 5 FILLER bytes dropped). DISABLED = RESP 84 / RESP2 50; DUPKEY = 15.
  `MOVE S9(9) COMP TO X(10)` gives 9 unsigned digits + a space.
* Cursor (lines 515-524): first of ACCTSID (account NOT-OK or BLANK), CARDSID (card NOT-OK or BLANK), else ACCTSID.
* Colours / '*' (lines 527-557): red for NOT-OK; '*' + red for BLANK only when `CDEMO-PGM-REENTER`; default colour when
  coming from the list (`CDEMO-LAST-MAPSET = 'COCRDLI'` and `CDEMO-FROM-PROGRAM = 'COCRDLIC'`), which also protects both fields.
* Commarea: 2000 bytes = CARDDEMO-COMMAREA (160, PGM-CONTEXT 1 after 1400) + WS-THIS-PROGCOMMAREA (12) + SPACES.

### Bug-for-bug behaviour (kept, not fixed)

* Lines 268-270 test `CDEMO-FROM-PROGRAM`/`CDEMO-PGM-REENTER` on the WORKING-STORAGE commarea before DFHCOMMAREA is
  copied, so the "coming from the menu" re-initialisation never happens when EIBCALEN > 0 (test `menuCommareaIsNotReinitialisedBugForBug`).
* 9100 reads CARDDAT by card number only (`MOVE CC-ACCT-ID-N` is commented out at line 739): the account typed is
  validated as an 11-digit number but never checked against the card (test `accountIsNotCheckedAgainstTheCard`).
* An all-zero account / card number is treated as "not provided" (lines 653, 693).
* After a non-numeric filter, `CDEMO-xxx = 0`, so 1200 blanks the field: the typed value is not echoed back.
* PF3 always sets `CDEMO-USRTYP-USER` (line 326): an admin user is returned to the caller as a regular user.
* `DFHBMDAR` (an attribute value) is moved into the INFOMSG colour byte (line 554).
* `CDEMO-LAST-MAPSET`/`-MAP` are only updated on PF3, not when the screen is sent.
* 9150 (CARDAIX) treats DUPKEY (several cards on the account, normal for a non-unique AIX) as a file error. It is dead code.
* `ABEND-MSG` has `VALUE SPACES`, so the `= LOW-VALUES` test in ABEND-ROUTINE never fires and the default text is never used.
* A commarea shorter than 172 bytes (XCTL from COCRDLIC/COMEN01C passes 160) is still read at bytes 161-172
  (beyond EIBCALEN); the Java port pads with SPACES.

## LIMITATIONS

* **CICS runtime**: pseudo-conversation, EIB, RESP codes, XCTL and RETURN are emulated by `carddemo-runtime`; RESP2 values are approximations.
* **HANDLE ABEND**: emulated by catching Java program checks (`NumberFormatException`, `ArithmeticException`,
  `IndexOutOfBoundsException`) around `0000-MAIN`; real ASRA/AICA abends and `HANDLE ABEND CANCEL` are not reproduced.
  `EXEC CICS SEND FROM(ABEND-DATA)` is reported as an unformatted text output without ERASE.
* **Terminal/BMS**: 3270 attributes (PROT/FSET, colour, DRK) are metadata only; colour bytes moved by the program are
  not part of the REST `screen` payload (unit tests check them through `Task#colors`). MDT/FSET semantics are up to
  the REST client (send the fields the user modified; no fields = MAPFAIL).
* **Numeric comparisons on invalid data**: `CDEMO-ACCT-ID = 0`, `CC-ACCT-ID-N EQUAL ZEROS`, `CDEMO-PGM-ENTER` etc. on
  non-digit bytes follow z/OS PACK + sign fix-up (low nibble of each EBCDIC byte, so SPACES/LOW-VALUES compare as zero);
  a nibble above 9 is treated as "not equal" instead of raising S0C7. Actual results depend on compiler options (NUMPROC).
* **Uninitialised storage**: COBOL fields without VALUE are assumed LOW-VALUES; DFHCOMMAREA beyond EIBCALEN is assumed SPACES.
* **EBCDIC collation**: keys compare as ISO-8859-1 bytes; COCRDSLC only does keyed READs, so ordering does not matter.
* **CARDAIX**: there is no CSD-level PATH in the file control table; 9150 opens the path over the CARDDAT cluster in code.
* **XCTL targets** COCRDLIC and COMEN01C are not migrated; only `nextProgram` and the commarea are returned.
* **Security**: the REST adapter has no authentication and trusts the posted commarea (see COSGN00C.md).
* **FUNCTION CURRENT-DATE** uses the JVM clock/time zone.
* GnuCOBOL golden files are not applicable (online CICS program); expected values are hand-traced from the source.
