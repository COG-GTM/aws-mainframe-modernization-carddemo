# COCRDLIC - List credit cards (transaction CCLI)

| | |
|---|---|
| COBOL | `app/cbl/COCRDLIC.cbl` |
| BMS | `app/bms/COCRDLI.bms` (mapset COCRDLI, map CCRDLIA), symbolic map `app/cpy-bms/COCRDLI.CPY` |
| Copybooks | CVCRD01Y, COCOM01Y, DFHBMSCA, DFHAID, COTTL01Y, COCRDLI, CSDAT01Y, CSMSG01Y, CSUSR01Y (unused), CVACT02Y, CSSTRPFY |
| File | CARDDAT (CSD `DEFINE FILE(CARDDAT)`, DSN `AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS`, key CARD-NUM 16, LRECL 150), seeded from `app/data/ASCII/carddata.txt`. `LIT-CARD-FILE-ACCT-PATH` (CARDAIX) is declared but never used. |
| Java | `carddemo-online` `com.carddemo.online.cocrdlic.Cocrdlic` (+ `CardWorkArea`, `ThisProgCommarea`) |
| REST | `POST /api/cics/CCLI` (fields `ACCTSID`, `CARDSID`, `CRDSEL1`..`CRDSEL7`; aid `ENTER`/`PF3`/`PF7`/`PF8`) |
| Navigation | XCTL `COMEN01C` (PF3), `COCRDSLC` (S), `COCRDUPC` (U) - returned as `nextProgram` + 160-byte commarea, not migrated here |

## Paragraph mapping

| COBOL paragraph (lines) | Java |
|---|---|
| WORKING-STORAGE (41-290) | `Cocrdlic.Task` fields (`work`, `wsEditSelect`, `wsEditSelectErrors`, `wsErrorMsg`, `wsInfoMsg`, flags, `commarea`, `prog`, `ccrdlia`, `cardRecord`) |
| `0000-MAIN` (298-602) | `Cocrdlic.Task#mainPara` (S/U branches 517-569 share `transferWithSelectedRow`) |
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
| `9000-READ-FORWARD` (1123-1263) | `Cocrdlic.Task#readForward9000` |
| `9100-READ-BACKWARDS` (1264-1372) | `Cocrdlic.Task#readBackwards9100` |
| `9100-READ-BACKWARDS-EXIT` (1374-1380) | `Cocrdlic.Task#readBackwardsExit9100` (ENDBR) |
| `9500-FILTER-RECORDS` (1382-1411) | `Cocrdlic.Task#filterRecords9500` |
| `YYYY-STORE-PFKEY` (COPY CSSTRPFY, 1416) | `Cocrdlic.Task#yyyyStorePfkey` -> `PfKeys.storePfKey` |
| `SEND-PLAIN-TEXT` (1422-1435) | `Cocrdlic.Task#sendPlainText` (never performed) |
| `SEND-LONG-TEXT` (1441-1454) | `Cocrdlic.Task#sendLongText` (never performed) |
| `*-EXIT` paragraphs | - (EXIT) |
| Unreachable tail of 0000-MAIN (585-601) | not translated: every EVALUATE branch ends in GO TO COMMON-RETURN or XCTL |

## Copybook -> Java

| Copybook field | Java |
|---|---|
| CVACT02Y `CARD-RECORD` (150): `CARD-NUM` X(16), `CARD-ACCT-ID` 9(11), `CARD-CVV-CD` 9(3), `CARD-EMBOSSED-NAME` X(50), `CARD-EXPIRAION-DATE` X(10), `CARD-ACTIVE-STATUS` X(1), FILLER X(59) | `com.carddemo.common.copybook.CardRecord` `NUM`, `ACCT_ID`, `CVV_CD`, `EMBOSSED_NAME`, `EXPIRAION_DATE`, `ACTIVE_STATUS` (`KEY_OFFSET` 0, `KEY_LENGTH` 16) |
| CVCRD01Y `CC-WORK-AREA`: `CCARD-AID` X(5) + 88s, `CCARD-NEXT-PROG/-MAPSET/-MAP`, `CCARD-ERROR-MSG`, `CCARD-RETURN-MSG`, `CC-ACCT-ID` X(11) / `-N` 9(11), `CC-CARD-NUM` X(16) / `-N` 9(16), `CC-CUST-ID` X(9) / `-N` 9(9) | `cocrdlic.CardWorkArea` (program-private; REDEFINES as overlapping fields) |
| CSSTRPFY `YYYY-STORE-PFKEY` | `com.carddemo.runtime.cics.PfKeys#storePfKey` |
| `WS-THIS-PROGCOMMAREA` (229-260, 254 bytes): `WS-CA-LAST-CARDKEY`, `WS-CA-FIRST-CARDKEY`, `WS-CA-SCREEN-NUM` 9(1) (88 `CA-FIRST-PAGE`), `WS-CA-LAST-PAGE-DISPLAYED` 9(1) (88 `CA-LAST-PAGE-SHOWN`=0 / `-NOT-SHOWN`=9), `WS-CA-NEXT-PAGE-IND` (88 `CA-NEXT-PAGE-NOT-EXISTS`=LOW-VALUES / `-EXISTS`='Y'), `WS-RETURN-FLAG`, `WS-SCREEN-DATA` (`WS-ROW-ACCTNO/-CARD-NUM/-CARD-STATUS` OCCURS 7) | `cocrdlic.ThisProgCommarea` (`LAST_CARD_NUM`, `FIRST_CARD_NUM`, `SCREEN_NUM`, `LAST_PAGE_DISPLAYED`, `NEXT_PAGE_IND`, `ROW_*[7]`, `EACH_CARD[7]`, 88s as `Condition88`) |
| `WS-COMMAREA` X(2000) | `Cocrdlic.COMMAREA_LAYOUT` = CARDDEMO-COMMAREA (0-159) + WS-THIS-PROGCOMMAREA (160-413) + SPACES |
| `WS-EDIT-SELECT-FLAGS` / `WS-EDIT-SELECT(7)` (88 `SELECT-OK`, `VIEW-REQUESTED-ON`, `UPDATE-REQUESTED-ON`, `SELECT-BLANK`) | `Task.wsEditSelect` (`char[7]`), `selectIs(n, c)` |
| `WS-EDIT-SELECT-ERROR-FLAGS` / `WS-ROW-CRDSELECT-ERROR(7)` | `Task.wsEditSelectErrors` |
| `WS-ERROR-MSG` X(75) + 88s, `WS-INFO-MSG` X(45) + 88s | `Task.wsErrorMsg`, `Task.wsInfoMsg`; message literals `Cocrdlic.MSG_*` |
| `WS-FILE-ERROR-MESSAGE` (153-171) | `Task#setFileErrorMessage` |
| COCRDLI `CCRDLIAI/O` (`xxxL`, `xxxA`, `xxxC`, `xxxI/O`) | `SymbolicMap.of("COCRDLI","CCRDLIA")`: `data`, `setCursor`, `setAttribute`, `setColor`/`color` |
| DFHBMSCA `DFHBMPRO` X'60', `DFHBMPRF` X'61', `DFHBMFSE` X'C1', `DFHBMDAR` X'4C', `DFHRED` X'F2', `DFHNEUTR` X'F7` | `Cocrdlic.Dfhbmsca` |

## Expected-value derivation (tests in `CocrdlicTest`, `CicsTransactionControllerTest#cardListConversation`)

* Data: `app/data/ASCII/carddata.txt`, 50 records, one card per account, all status `Y`. Sorted by CARD-NUM
  (KSDS key order) page 1 = records 1-7 (`0500024453765740` ... `1142167692878931`), page 2 = 8-14, page 8 = record 50
  (`9805583408996588`, acct `00000000040`). The table is in the test class Javadoc.
* First entry: lines 315-325 (EIBCALEN 0) set CA-FIRST-PAGE; WHEN OTHER (572-582) reads forward from
  WS-CARD-RID-CARDNUM = SPACES (INITIALIZE, line 301) = first record. Lines 1191-1214: after 7 rows one more
  READNEXT; its key becomes WS-CA-LAST-CARD-NUM and CA-NEXT-PAGE-EXISTS.
* PF8 (486-497): STARTBR GTEQ the look-ahead key, so page n+1 starts with it; ADD 1 to the PIC 9 page number
  (10 truncates to 0 and line 1177 resets it to 1).
* Last page: READNEXT ENDFILE (1233-1240) -> 'NO MORE RECORDS TO SHOW'; 1400 (910-916) sets CA-LAST-PAGE-SHOWN on
  that PF8, so the next PF8 (WHEN OTHER re-read) gives 'NO MORE PAGES TO DISPLAY' (905-909).
* PF7 (501-513, 9100): first READPREV returns the current first record (counter 8 -> 7, not stored), the next 7
  READPREVs fill rows 7..1; WS-CA-LAST-CARDKEY = old WS-CA-FIRST-CARDKEY (1268).
* Filters: 2210/2220; RECEIVE MAP space-pads, so short numbers are NOT NUMERIC. 9500 compares CARD-ACCT-ID to the
  11 typed characters and CARD-NUM to CC-CARD-NUM-N (alphanumeric comparisons).
* Selections: 2250 tallies S/U (I > 1 -> MORE-THAN-1 message and error flags via INSPECT REPLACING); any other
  non-blank code -> 'INVALID ACTION CODE'. S/U with ENTER XCTLs with WS-ROW-ACCTNO/WS-ROW-CARD-NUM(I-SELECTED)
  taken from WS-SCREEN-DATA in the incoming commarea.
* Attributes: 1250 - row 1 protected with DFHBMPRF, rows 2-7 with DFHBMPRO; error rows DFHRED, rows 2-7 also get
  the cursor (L = -1). The cursor goes to the first map field with L = -1 (map order), else the IC field ACCTSID.
* File error text: `'File Error:'` X(12) + `ERROR-OPNAME` X(8) + `' on '` + `ERROR-FILE` X(9) + `' returned RESP '` +
  RESP as 9 digits in X(10) + `',RESP2 '` + RESP2 in X(10), truncated to WS-ERROR-MSG X(75). RESP values are CICS
  DFHRESP numbers (ENDFILE 20, INVREQ 16, DISABLED 84); RESP2 values come from the runtime (see LIMITATIONS).
* Header: fixed clock 2024-03-05T14:07:09.25Z -> CURDATE `03/05/24`, CURTIME `14:07:09`; PAGENO is the 9(1) digit
  moved to X(3) (`1  `).

### Bug-for-bug behaviour (kept, not fixed)

* Header comment says non-admins only see their account's cards; the code never checks CDEMO-USER-TYPE: everyone sees all cards.
* PF3 (and S/U transfers) `SET CDEMO-USRTYP-USER`: an admin's commarea comes back as user type `U`.
* Returning to page 1 with PF7 shows 'NO PREVIOUS PAGES TO DISPLAY' (page number is decremented before 1400 tests CA-FIRST-PAGE).
* A selection error (multiple selections / invalid code) re-reads from WS-CARD-RID-CARDNUM = SPACES: the screen shows the first (filtered) page under the old page number.
* A filter typed on page n starts the filtered browse at page n's first card and keeps the page number; no match there gives 'NO MORE RECORDS TO SHOW' instead of 'NO RECORDS FOUND...'.
* With a filter, CA-NEXT-PAGE-EXISTS is set by any record after the 7th match, even one the filter would exclude.
* PF7 that hits the start of the file before 7 rows are filled shows `File Error: READ ... RESP 000000020 ...` with the rows bottom-aligned.
* After returning from COCRDSLC/COCRDUPC the account/card fields show CDEMO-ACCT-ID / CDEMO-CARD-NUM but the list is not filtered by them.
* Row 1 uses DFHBMPRF (others DFHBMPRO) and gets no cursor on error; its `'*'` marker (line 758) can never appear because error flags are only set for non-blank selections.
* Line 790: a stray `I` makes `MOVE DFHBMPRO TO CRDSEL4A OF CCRDLIAI I` also move into the subscript I; no visible effect (I is not used afterwards).
* STARTBR RESP is not checked and ENDBR has no RESP: an empty or unusable CARDDAT ends in an AEIP abend after READNEXT fails.
* The S/U code typed with PF8 is echoed onto the next page's rows (WS-EDIT-SELECT is not cleared).
* PF3 when CDEMO-FROM-PROGRAM is another program (line 458) INITIALIZEs CARDDEMO-COMMAREA, losing the user id.
* `CDEMO-PGM-CONTEXT` is never set to REENTER by this program.

## LIMITATIONS

* **CICS runtime**: pseudo-conversation, EIB, browse and RESP codes are emulated by `carddemo-runtime`. RESP values follow DFHRESP numbering; RESP2 values (e.g. 90 for ENDFILE, 41 for a READNEXT without browse, 50 for DISABLED) are runtime approximations, so the digits after `RESP2` in the file-error message may differ on a real CICS.
* **Abend codes**: unhandled ENDBR conditions map to AEIP (INVREQ), AEIL (DSIDERR/FILENOTFOUND), AEIM (NOTOPEN); real CICS codes for other conditions were not verified.
* **Terminal/BMS**: attribute (`xxxA`) bytes are returned as `attributeByte`; colour overrides (`xxxC`, e.g. DFHRED on error rows, DFHBMDAR moved into INFOMSGC) are kept in the symbolic map but not exposed in the REST screen payload. FSET/MDT is up to the client: send `ACCTSID`/`CARDSID` again on PF7/PF8 to keep a filter, as the 3270 would.
* **DFHCOMMAREA shorter than 414 bytes** (e.g. the 160-byte commarea from COMEN01C): CICS would copy whatever storage follows the commarea into WS-THIS-PROGCOMMAREA; Java pads with SPACES. The PGM-ENTER-from-another-program path re-initializes it anyway.
* **Invalid numeric data**: `CC-ACCT-ID-N = ZEROS` etc. on non-digit data is compiler-dependent (PACK ignores zones on z/OS, e.g. `0` followed by spaces could compare equal to zero); Java treats non-digit content as not zero, so such input gets the "must be a 11 digit number" error. Non-digit commarea numerics are also treated as non-zero, and same-size 9(n) moves copy bytes unchanged.
* **Uninitialised storage**: CARD-RECORD and the symbolic map start as LOW-VALUES (README convention).
* **EBCDIC collation**: keys compare as ISO-8859-1 bytes; CARDDAT keys are all digits, so the order matches EBCDIC.
* **Level-number oddity**: `05 WS-SCREEN-DATA` after `10`-level items is taken as part of WS-THIS-PROGCOMMAREA (the rows travel in the commarea, which the S/U transfer relies on).
* **XCTL targets** COMEN01C, COCRDSLC, COCRDUPC are not migrated; only `nextProgram` and the commarea are returned.
* **GnuCOBOL**: not used - this is a CICS online program (EXEC CICS cannot run under GnuCOBOL); expected values are hand-traced from the source and the seed data.
