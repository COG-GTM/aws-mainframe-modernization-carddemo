# COCRDSLC - Card detail view (transaction CCDL)

Source: `app/cbl/COCRDSLC.cbl`, BMS `app/bms/COCRDSL.bms` (mapset COCRDSL, map CCRDSLA), symbolic map
`app/cpy-bms/COCRDSL.CPY`. Java: `carddemo-online/src/main/java/com/carddemo/online/cocrdslc/Cocrdslc.java`
(`@Component`, `CicsProgram`, transaction `CCDL`, exposed as `POST /api/cics/CCDL`).

Reached from the card list (COCRDLIC, 'S' on a row: commarea carries account + card, FROM-PROGRAM COCRDLIC,
LAST-MAPSET COCRDLI) or the main menu (COMEN01C option). Reads `CARDDAT` by card number and shows embossed name,
status and expiry. PF3 XCTLs back to the caller (or COMEN01C); XCTL targets are not migrated and are returned as
`outcome=XCTL, nextProgram=..., commarea=...`. Every other key behaves as ENTER.

## Paragraph mapping

| COBOL paragraph (lines) | Java |
|---|---|
| 0000-MAIN (248-392), HANDLE ABEND (250-252) | `Cocrdslc.Task#mainPara` (try/catch -> `abendRoutine`), `#main0000` |
| INITIALIZE CARDDEMO-COMMAREA (271) | `Task#initializeCommarea` |
| COMMON-RETURN (394-407) | `Task#commonReturn` |
| 0000-MAIN-EXIT (408-410) | n/a (EXIT) |
| 1000-SEND-MAP (412-421) | `Task#sendMap1000` |
| 1100-SCREEN-INIT (427-451) | `Task#screenInit1100` |
| 1200-SETUP-SCREEN-VARS (457-497) | `Task#setupScreenVars1200` |
| 1300-SETUP-SCREEN-ATTRS (502-558) | `Task#setupScreenAttrs1300` |
| 1400-SEND-SCREEN (563-577) | `Task#sendScreen1400` |
| 2000-PROCESS-INPUTS (582-591) | `Task#processInputs2000` |
| 2100-RECEIVE-MAP (596-603) | `Task#receiveMap2100` |
| 2200-EDIT-MAP-INPUTS (608-641) | `Task#editMapInputs2200` |
| 2210-EDIT-ACCOUNT (647-679) | `Task#editAccount2210` |
| 2220-EDIT-CARD (685-720) | `Task#editCard2220` |
| 9000-READ-DATA (726-730) | `Task#readData9000` |
| 9100-GETCARD-BYACCTCARD (736-773) | `Task#getcardByacctcard9100` (+ `#fileError` for WS-FILE-ERROR-MESSAGE) |
| 9150-GETCARD-BYACCT (779-809) | `Task#getcardByacct9150` (dead code in COBOL, kept and unit-tested) |
| SEND-LONG-TEXT (820-830) | `Task#sendLongText` (dead code) |
| SEND-PLAIN-TEXT (838-848) | `Task#sendPlainText` |
| YYYY-STORE-PFKEY (CSSTRPFY, 855) | `com.carddemo.runtime.cics.PfKeys#storePfKey` |
| ABEND-ROUTINE (857-878) | `Task#abendRoutine` |

88-levels: `INPUT-OK/ERROR` -> `wsInputFlag` + `inputError()`; `FLG-ACCTFILTER-*`/`FLG-CARDFILTER-*` -> `wsEditAcctFlag`
/`wsEditCardFlag` + predicates; `WS-RETURN-MSG-OFF` -> `wsReturnMsgOff()`; `WS-NO-INFO-MESSAGE` -> `wsNoInfoMessage()`;
`CDEMO-PGM-ENTER/REENTER` -> `cdemoPgmEnter()/cdemoPgmReenter()`; message 88s -> `MSG_*`/`INFO_*` constants (verbatim).

## Copybook -> Java

| Copybook | Java | Notes |
|---|---|---|
| CVACT02Y `CARD-RECORD` | `com.carddemo.common.copybook.CardRecord` (new, shared) | CARD-NUM X(16) `NUM`; CARD-ACCT-ID 9(11) `ACCT_ID`; CARD-CVV-CD 9(3) `CVV_CD`; CARD-EMBOSSED-NAME X(50) `EMBOSSED_NAME`; CARD-EXPIRAION-DATE X(10) `EXPIRAION_DATE` (sic); CARD-ACTIVE-STATUS X(1) `ACTIVE_STATUS`; FILLER X(59). 150 bytes, key 0/16, CARDAIX alt key 16/11 |
| CSMSG02Y `ABEND-DATA` | `com.carddemo.common.copybook.AbendData` (new, shared) | ABEND-CODE X(4), ABEND-CULPRIT X(8), ABEND-REASON X(50), ABEND-MSG X(72); VALUE SPACES -> constructor fills spaces |
| CVCRD01Y `CC-WORK-AREA` | `cocrdslc.CardWorkArea` (program-private) | CCARD-AID, CCARD-NEXT-PROG/MAPSET/MAP, CCARD-ERROR-MSG, CCARD-RETURN-MSG, CC-ACCT-ID(-N), CC-CARD-NUM(-N), CC-CUST-ID(-N) with REDEFINES |
| COCOM01Y | `CardDemoCommarea` (existing) | |
| COCRDSL (symbolic map) | `SymbolicMap.of("COCRDSL", "CCRDSLA")` | fields TRNNAME, TITLE01/02, CURDATE, CURTIME, PGMNAME, ACCTSID, CARDSID, CRDNAME, CRDSTCD, EXPMON, EXPYEAR, INFOMSG, ERRMSG |
| COTTL01Y / CSDAT01Y | `ScreenTitles` / `DateTimeFields` (existing) | |
| DFHBMSCA | `cocrdslc.Dfhbmsca` (program-private, EBCDIC byte values) | DFHBMPRF X'61', DFHBMFSE X'C1', DFHBMDAR X'4C', DFHDFCOL X'00', DFHRED X'F2', DFHNEUTR X'F7' |
| WS-MISC-STORAGE | `Cocrdslc.Task` fields | CICS-OUTPUT-EDIT-VARS: only CARD-EXPIRAION-DATE-X year(1:4)/month(6:2) is used |
| CSMSG01Y, CSUSR01Y, CVCUS01Y, DFHAID | not referenced by the procedure division | CVCUS01Y `CustomerRecord` is left to the first session that reads CUSTDAT |

Files: `OnlineRegionConfig.fileControl` registers `CARDDAT` (`JdbcKsdsStore`, table for
`AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS`) seeded from `app/data/ASCII/carddata.txt` via `CardDataSeed` (50 records).
`CARDAIX` is a PATH over CARDDAT; it is opened on demand with `KsdsFile#path("CARDAIX", 16, 11, false)` because
`FileControl` has no path registry and the only CARDAIX reader (9150) is never PERFORMed.

Runtime additions (additive only): `CardRecord`, `AbendData`, `CardDataSeed`, `PfKeys`, `SymbolicMap#color(String)`
getter, and the `carddata.txt` resource in `carddemo-runtime/pom.xml`.

## Expected-value derivation (tests in `CocrdslcTest`)

- Card 1 of `carddata.txt`: `0500024453765740` / account `00000000050` / CVV 747 / `Aniya Von` / `2023-03-09` /
  `Y` -> CRDNAME `Aniya Von` (padded 50), EXPMON `03`, EXPYEAR `2023`, CRDSTCD `Y` (1200 lines 474-485).
- INFOMSG `" Displaying requested details"` keeps the leading space of 88 FOUND-CARDS-FOR-ACCOUNT (lines 129-130).
- Prompt `Please enter Account and Card Number` whenever no info message is set (1200 lines 459-460, 490-491).
- Error messages verbatim from lines 136-158 and 670/711, padded to ERRMSG length 80. Only the first error is
  kept (`IF WS-RETURN-MSG-OFF` guards) except `No input received`, which overwrites when both filters are blank (637-639).
- Cursor (1300 lines 515-524): account filter not-ok/blank -> ACCTSID; else card not-ok/blank -> CARDSID; else ACCTSID.
- Colours (527-557): DFHDFCOL when coming from the list, DFHRED for not-ok filters and for blank filters on re-entry
  (`*` shown), INFOMSG always DFHNEUTR (the prompt is set first).
- File error (762-771): READ on an undefined CARDDAT -> RESP 12 (FILENOTFOUND) / RESP2 1 ->
  `File Error: READ     on CARDDAT   returned RESP 000000012 ,RESP2 000000001 ` (ERROR-RESP is the 9-digit DISPLAY
  image of the S9(9) COMP left-justified in X(10)).
- PF3 (305-334): TO-TRANID/TO-PROGRAM = FROM-* or CM00/COMEN01C when SPACES/LOW-VALUES; FROM = CCDL/COCRDSLC;
  USER-TYPE 'U'; PGM-CONTEXT 0; LAST-MAPSET `COCRDSL` (8-char literal truncated to X(7)); LAST-MAP `CCRDSLA`.
- RETURN TRANSID (394-406) passes 2000 bytes: CARDDEMO-COMMAREA (160) + WS-THIS-PROGCOMMAREA (12) + SPACES.

### Bug-for-bug behaviour (kept, not fixed)

1. Line 269 tests `CDEMO-FROM-PROGRAM = LIT-MENUPGM` on the WORKING-STORAGE commarea before it is loaded
   (LOW-VALUES), so the clause is never true: an entry from COMEN01C keeps the menu's commarea (FROM-PROGRAM stays
   COMEN01C) instead of being initialised; only `EIBCALEN = 0` initialises it.
2. 9100 reads CARDDAT by card number only; the account entered is never checked against CARD-ACCT-ID, so any
   valid 11-digit account plus an existing card shows that card.
3. A non-numeric filter sets the commarea id to 0, so 1200 moves LOW-VALUES and the bad value disappears from
   the (red) field; blank/zero filters show `*` only on re-entry.
4. `CC-ACCT-ID-N EQUAL ZEROS` (653) runs before the NUMERIC test, so `00000000000` and `0` followed by spaces
   all give `Account number not provided` rather than the numeric message (same for the card).
5. ABEND-ROUTINE only fills ABEND-MSG when it is LOW-VALUES, but ABEND-DATA is `VALUE SPACES`, so the abend text
   never contains `UNEXPECTED ABEND OCCURRED.`.
6. Lines 386-391 (after END-EVALUATE) and 9150 / SEND-LONG-TEXT are unreachable.
7. Entry from the list keeps LAST-MAPSET `COCRDLI` and FROM-PROGRAM `COCRDLIC` on every re-entry (only PF3 changes
   them), so the filters stay protected (DFHBMPRF, MDT on) and are re-sent with each ENTER.

## LIMITATIONS

- CICS/BMS behaviour is emulated, not executed: no GnuCOBOL golden files (EXEC CICS cannot run under GnuCOBOL);
  expected values are hand-traced from the COBOL lines cited above.
- Attribute/colour bytes are the EBCDIC DFHBMSCA values; real 3270 rendering (protect, MDT, colour, DFHBMDAR moved
  into a colour byte) is not verified. `BmsScreen` exposes the attribute byte; colours are checked via
  `SymbolicMap#color` in unit tests only (not in the REST payload).
- Comparisons of PIC 9 DISPLAY items holding non-digits (`CDEMO-ACCT-ID = 0`, `CC-ACCT-ID-N = ZEROS`,
  CDEMO-PGM-CONTEXT 88s) use the digit nibble of each byte; Enterprise COBOL may instead raise S0C7 depending on
  NUMPROC/compile options.
- HANDLE ABEND is approximated by catching unexpected Java exceptions; real CICS abend codes (e.g. AEIx for
  unhandled conditions) are not produced. The ABEND-DATA SEND (terminal control) is modelled as a SEND TEXT.
- RECEIVE MAP RESP (MAPFAIL etc.) is not tested by the COBOL and is ignored, as in the source. A DFHCOMMAREA
  shorter than 172 bytes is padded with spaces where the COBOL would read beyond EIBCALEN.
- CARDAIX is not a registered FCT entry (no path registry in `FileControl`); it is only reachable through dead
  paragraph 9150. EBCDIC key collation is not relevant (keyed READ only).
- COCRDLIC / COMEN01C are not implemented here; PF3 returns `XCTL` with the target in `nextProgram`.
