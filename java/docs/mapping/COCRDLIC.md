# COCRDLIC - List credit cards (transaction CCLI)

| | |
|---|---|
| COBOL | `app/cbl/COCRDLIC.cbl` |
| BMS | `app/bms/COCRDLI.bms` (mapset COCRDLI, map CCRDLIA), symbolic map `app/cpy-bms/COCRDLI.CPY` |
| Copybooks | CVCRD01Y, COCOM01Y, DFHBMSCA, DFHAID, COTTL01Y, COCRDLI, CSDAT01Y, CSMSG01Y, CSUSR01Y (unused), CVACT02Y, CSSTRPFY |
| File | CARDDAT (CSD `DEFINE FILE(CARDDAT)`, DSN `AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS`, key CARD-NUM 16, LRECL 150), seeded from `app/data/ASCII/carddata.txt`. `LIT-CARD-FILE-ACCT-PATH` (CARDAIX) is declared but never used. |
| Java | `carddemo-online` `com.carddemo.online.cocrdlic.Cocrdlic` (+ `ThisProgCommarea`, `Dfhbmsca`) |
| REST | `POST /api/cics/CCLI` |
| Navigation | PF3 -> XCTL `COMEN01C`; `S` -> XCTL `COCRDSLC`; `U` -> XCTL `COCRDUPC` (returned as `nextProgram`, not migrated here) |

## Paragraph mapping

| COBOL paragraph (lines) | Java |
|---|---|
| WORKING-STORAGE (41-290) | `Cocrdlic.Task` fields (`wsInputFlag`, `wsEditAcctFlag`, `wsEditCardFlag`, `wsEditSelect`, `wsEditSelectErrorFlags`, `i`, `iSelected`, `wsInfoMsg`, `wsErrorMsg`, `wsCardRidCardnum`, `wsScrnCounter`, `ccAcctId`, `ccCardNum`, `commarea`, `prog`, `ccrdlia`, `cardRecord`, ...); constants in `Cocrdlic` |
| `0000-MAIN` (298-602) | `Cocrdlic.Task#main` (EVALUATE branches inline; lines 517-569 in `#transferToSelectedCard`) |
| `COMMON-RETURN` (604-620) | `Cocrdlic.Task#commonReturn` |
| `0000-MAIN-EXIT` (621-623) | - (EXIT) |
| `1000-SEND-MAP` (624-641) | `Cocrdlic.Task#sendMap1000` |
| `1100-SCREEN-INIT` (642-676) | `Cocrdlic.Task#screenInit1100` |
| `1200-SCREEN-ARRAY-INIT` (678-747) | `Cocrdlic.Task#screenArrayInit1200` |
| `1250-SETUP-ARRAY-ATTRIBS` (748-836) | `Cocrdlic.Task#setupArrayAttribs1250` |
| `1300-SETUP-SCREEN-ATTRS` (837-892) | `Cocrdlic.Task#setupScreenAttrs1300` |
| `1400-SETUP-MESSAGE` (895-935) | `Cocrdlic.Task#setupMessage1400` |
| `1500-SEND-SCREEN` (938-950) | `Cocrdlic.Task#sendScreen1500` |
| `2000-RECEIVE-MAP` (951-961) | `Cocrdlic.Task#receiveMap2000` |
| `2100-RECEIVE-SCREEN` (962-983) | `Cocrdlic.Task#receiveScreen2100` |
| `2200-EDIT-INPUTS` (985-1001) | `Cocrdlic.Task#editInputs2200` |
| `2210-EDIT-ACCOUNT` (1003-1034) | `Cocrdlic.Task#editAccount2210` |
| `2220-EDIT-CARD` (1036-1071) | `Cocrdlic.Task#editCard2220` |
| `2250-EDIT-ARRAY` (1073-1121) | `Cocrdlic.Task#editArray2250` |
| `9000-READ-FORWARD` (1123-1263) | `Cocrdlic.Task#readForward9000` (READNEXT via `#readnext`, ENDBR via `#endbr`, error text via `#fileErrorMessage`) |
| `9100-READ-BACKWARDS` (1264-1380) | `Cocrdlic.Task#readBackwards9100` (READPREV via `#readprev`) |
| `9500-FILTER-RECORDS` (1382-1411) | `Cocrdlic.Task#filterRecords9500` |
| `YYYY-STORE-PFKEY` (CSSTRPFY, 1416) | `Cocrdlic.Task#yyyyStorePfkey` -> `com.carddemo.runtime.cics.PfKeys#storePfKey` |
| `SEND-PLAIN-TEXT` (1422-1435) | `Cocrdlic.Task#sendPlainText` (never performed) |
| `SEND-LONG-TEXT` (1441-1454) | `Cocrdlic.Task#sendLongText` (never performed) |

## Copybook -> Java

| Copybook field | Java |
|---|---|
| CVACT02Y `CARD-RECORD` (150): `CARD-NUM` X(16), `CARD-ACCT-ID` 9(11), `CARD-CVV-CD` 9(3), `CARD-EMBOSSED-NAME` X(50), `CARD-EXPIRAION-DATE` X(10), `CARD-ACTIVE-STATUS` X(1), FILLER X(59) | `com.carddemo.common.copybook.CardRecord` `NUM`, `ACCT_ID`, `CVV_CD`, `EMBOSSED_NAME`, `EXPIRAION_DATE`, `ACTIVE_STATUS` (`LAYOUT`, key 0/16); seed `CardDataSeed` |
| `WS-THIS-PROGCOMMAREA` (229-260, 254 bytes): `WS-CA-LAST-CARD-NUM` X(16), `-LAST-CARD-ACCT-ID` 9(11), `-FIRST-CARD-NUM` X(16), `-FIRST-CARD-ACCT-ID` 9(11), `WS-CA-SCREEN-NUM` 9(1), `WS-CA-LAST-PAGE-DISPLAYED` 9(1), `WS-CA-NEXT-PAGE-IND` X(1), `WS-RETURN-FLAG` X(1), `WS-SCREEN-DATA`/`WS-ALL-ROWS` X(196) REDEFINED as 7 x (`WS-ROW-ACCTNO` X(11), `WS-ROW-CARD-NUM` X(16), `WS-ROW-CARD-STATUS` X(1)) | `ThisProgCommarea` `LAST_CARD_NUM`, `LAST_CARD_ACCT_ID`, `FIRST_CARD_NUM`, `FIRST_CARD_ACCT_ID`, `SCREEN_NUM`, `LAST_PAGE_DISPLAYED`, `NEXT_PAGE_IND`, `RETURN_FLAG`, `ALL_ROWS`, `EACH_CARD[]`, `ROW_ACCTNO[]`, `ROW_CARD_NUM[]`, `ROW_CARD_STATUS[]` |
| 88 `CA-FIRST-PAGE` (1), `CA-LAST-PAGE-SHOWN` (0) / `-NOT-SHOWN` (9), `CA-NEXT-PAGE-EXISTS` ('Y') / `-NOT-EXISTS` (LOW-VALUES) | `Cocrdlic.CA_FIRST_PAGE`, `CA_LAST_PAGE_SHOWN`, `CA_LAST_PAGE_NOT_SHOWN`, `CA_NEXT_PAGE_EXISTS`, `CA_NEXT_PAGE_NOT_EXISTS` (`Condition88`) |
| `WS-COMMAREA` X(2000) | `ThisProgCommarea.WS_COMMAREA_LAYOUT` (CARDDEMO-COMMAREA + WS-THIS-PROGCOMMAREA + FILLER) - also the JSON `commareaFields` layout |
| CVCRD01Y `CC-WORK-AREA`: `CCARD-AID` X(5) + 88s, `CCARD-NEXT-PROG/-MAPSET/-MAP`, `CCARD-ERROR-MSG`, `CC-ACCT-ID` X(11) / `-N` 9(11), `CC-CARD-NUM` X(16) / `-N` 9(16) | `Task.ccardAid` (`aid("PFK07")` = `CCARD-AID-PFK07`), `ccardNextProg`, `ccardNextMapset`, `ccardNextMap`, `ccardErrorMsg`, `ccAcctId`, `ccCardNum` (program-private) |
| `WS-EDIT-SELECT-FLAGS` X(7) REDEFINED as `WS-EDIT-SELECT` OCCURS 7 + 88 `SELECT-OK` (S,U), `VIEW-REQUESTED-ON` (S), `UPDATE-REQUESTED-ON` (U), `SELECT-BLANK` (space, LOW-VALUES) | `Task.wsEditSelect` (char[7]), `viewRequestedOn(i)`, `updateRequestedOn(i)` |
| `WS-EDIT-SELECT-ERROR-FLAGS` X(7) / `WS-ROW-CRDSELECT-ERROR` | `Task.wsEditSelectErrorFlags` |
| `WS-FILE-ERROR-MESSAGE` (153-171) | `Task#fileErrorMessage` |
| CSSTRPFY | `PfKeys.storePfKey(AidKey, String)` (runtime) |
| DFHBMSCA `DFHBMPRO` X'60', `DFHBMPRF` X'61', `DFHBMFSE` X'C1', `DFHBMDAR` X'4C', `DFHRED` X'F2', `DFHNEUTR` X'F7` | `Dfhbmsca` (program package) |
| COCRDLI `CCRDLIAI/O` | `SymbolicMap.of("COCRDLI","CCRDLIA")`: `data()`, `setCursor()` (`xxxL = -1`), `setAttribute()` (`xxxA`), `setColor()`/`color()` (`xxxC`) |
| COCOM01Y, COTTL01Y, CSDAT01Y | `CardDemoCommarea`, `ScreenTitles`, `DateTimeFields` (existing) |

## Expected-value derivation (tests in `CocrdlicTest`)

* Data: `app/data/ASCII/carddata.txt` (50 records, one card per account, all status `Y`) loaded into an in-memory
  `KsdsFile`. Sorted by CARD-NUM, page n shows records 7n-6..7n (page 1 starts `0500024453765740` / acct
  `00000000050`, page 2 `1561409106491600`, page 3 `3766281984155154`, page 8 only `9805583408996588`).
* First entry: lines 315-325 initialise the commarea; the EVALUATE falls to WHEN OTHER (572-582), 9000 browses
  GTEQ from SPACES, fills 7 rows, then the read-ahead (1197-1214) finds record 8 -> `CA-NEXT-PAGE-EXISTS`,
  `WS-CA-LAST-CARD-NUM` = record 8. 1400 WHEN `WS-NO-INFO-MESSAGE` -> `TYPE S FOR DETAIL, U TO UPDATE ANY RECORD`.
* Commarea: 2000 bytes = CARDDEMO-COMMAREA (1:160) + WS-THIS-PROGCOMMAREA (161:254) + SPACES (INITIALIZE
  WS-COMMAREA, line 302). XCTLs pass only CARDDEMO-COMMAREA (160).
* Header: fixed clock 2024-03-05T14:07:09Z -> `03/05/24`, `14:07:09`; `PAGENO` = `WS-CA-SCREEN-NUM` 9(1) in X(3).
* Messages: literals at lines 116-126, 1022, 1058, 1219/1239, 903, 908; ERRMSG is 78 bytes (75-byte WS-ERROR-MSG).
* File error text: `'File Error:'` X(12) + `READ` X(8) + `' on '` + `CARDDAT ` in X(9) + `' returned RESP '` +
  RESP S9(9) COMP moved to X(10) (9 digits) + `',RESP2 '` + RESP2 = exactly 75 bytes (final FILLER X(5) truncated).
  READPREV past the first record -> ENDFILE RESP 20 / RESP2 90.
* Attributes/colours: 1250/1300 lines quoted in each test; colours are read from the symbolic map (`task.ccrdlia.color`).

### Bug-for-bug behaviour (kept, not fixed)

* PF7 from page 2 lands on page 1 but shows `NO PREVIOUS PAGES TO DISPLAY` (1400 tests CA-FIRST-PAGE after the SUBTRACT).
* An invalid/multiple selection (INPUT-ERROR with valid filters) re-reads with `WS-CARD-RID-CARDNUM` still SPACES, so
  the list restarts at the first card while `PAGENO` keeps the old page number.
* The read-ahead (line 1197) is not filtered: with a filter, `CA-NEXT-PAGE-EXISTS` can be set when no further match exists.
* A filter is applied only from the current page's first card onwards (WHEN OTHER browses from `WS-CA-FIRST-CARD-NUM`);
  off page 1 an empty result says `NO MORE RECORDS TO SHOW`, not `NO RECORDS FOUND...`.
* PF7 with a filter that runs out of records before 7 matches hits start-of-file: READPREV ENDFILE is reported as a
  `File Error` message and only the bottom rows are filled.
* `WS-CA-SCREEN-NUM` is 9(1): page 9 + PF8 truncates to 0 and READ-FORWARD turns it into 1; SUBTRACT from 0 stores 1.
* Row 1 uses DFHBMPRF (others DFHBMPRO) when protected, and an erroneous row 1 never gets the cursor (rows 2-7 do).
* Line 790 has a stray `I`, making it `MOVE DFHBMPRO TO CRDSEL4A OF CCRDLIAI I`; I becomes 0 (no visible effect).
* `INFOMSGC` receives `DFHBMDAR` (an attribute constant) as a colour.
* Selecting S/U sets `CDEMO-USRTYP-USER`: an admin continues as a regular user.
* Re-entry with `CDEMO-PGM-REENTER` from another program INITIALIZEs CARDDEMO-COMMAREA (user id lost).
* Entry with `CDEMO-PGM-ENTER` from a program other than COMEN01C shows `CDEMO-ACCT-ID`/`CDEMO-CARD-NUM` in the filter
  fields without applying them.
* `VIEW-REQUESTED-ON(I-SELECTED)` is evaluated with `I-SELECTED = 0` when nothing was received (subscript out of range:
  the byte before the table, X'0C' of WS-EDIT-SELECT-COUNTER) - never S/U, emulated as false.
* Lines 586-601 after the EVALUATE are unreachable; `SEND-PLAIN-TEXT`, `SEND-LONG-TEXT`, CSUSR01Y, `CARDAIX` and
  `WS-RETURN-FLAG` are unused.
* ENDBR is coded without RESP, so a browse that never started (file disabled/undefined) abends the task
  (AEIP / AEIL) instead of showing the error message.

## LIMITATIONS

* CICS/BMS runtime behaviour is emulated, not executed: terminal attribute bytes and colours are checked on the symbolic
  map / `BmsScreen.Field.attributeByte`; colour overrides are not part of the REST screen JSON. FREEKB, ERASE and real
  3270 cursor addressing are modelled only as flags/field names.
* Default abend codes for conditions raised without RESP (AEIL, AEIP, ...) follow the IBM AEIx table; the RESP2 values
  of failed browse commands come from the runtime `KsdsFile` (e.g. INVREQ 41, ENDFILE 90, DISABLED 50).
* Non-numeric data in numeric items is evaluated the way PACK sees it on the mainframe (low nibble of each CP037
  byte): SPACES/LOW-VALUES count as zero for `= 0` / `= ZEROS` tests and ADD/SUBTRACT; data exceptions (S0C7) are not emulated. 88-levels on numeric items
  use the runtime convention (non-numeric -> false), so a LOW-VALUES CDEMO-PGM-CONTEXT is neither ENTER nor REENTER.
* Bytes of DFHCOMMAREA beyond EIBCALEN (e.g. a 160-byte commarea from the menu) are read as SPACES; on CICS they are
  undefined storage (the program re-initialises that area in the menu/re-entry cases anyway).
* Collation is ISO-8859-1 byte order, not EBCDIC; identical for the all-digit CARD-NUM keys.
* No GnuCOBOL golden run: COCRDLIC is a CICS program (golden files apply to batch programs); expected values are
  hand-traced from the COBOL as cited above.
* REST terminal emulation (shared `CicsTransactionController`, not program logic):
  * There is no FSET/MDT retransmission. A 3270 resends ACCTSID/CARDSID because 1300 sets `DFHBMFSE`. A REST client
    must resend those fields (those with `attributeByte` X'C1' or BMS `FSET`) with PF7/PF8/ENTER, or the filters are
    cleared, as they would be on a terminal where the user erased them.
  * Field protection is not enforced. A selection posted for a protected empty row reaches 2250 and, like the COBOL,
    is accepted. A 3270 cannot send it.
  * The commarea is held by the client (base64), so its contents are trusted the way CICS trusts its own storage.
    There is no authentication: COCRDLIC itself has no security check and relies on CICS sign-on and transaction
    security. Both need a session/security layer in the adapter, outside this program's scope.

