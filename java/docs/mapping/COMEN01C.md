# COMEN01C - Main menu for regular users (transaction CM00)

| | |
|---|---|
| COBOL | `app/cbl/COMEN01C.cbl` |
| BMS | `app/bms/COMEN01.bms` (mapset COMEN01, map COMEN1A), symbolic map `app/cpy-bms/COMEN01.CPY` |
| Copybooks | COCOM01Y, COMEN02Y, COMEN01, COTTL01Y, CSDAT01Y, CSMSG01Y, CSUSR01Y (copied, unused), DFHAID, DFHBMSCA |
| Files | none (`WS-USRSEC-FILE` is declared but never used) |
| Java | `carddemo-online` `com.carddemo.online.comen01c.Comen01c`, `MenuOptions` (COMEN02Y) |
| REST | `POST /api/cics/CM00` (CSD `DEFINE TRANSACTION(CM00) PROGRAM(COMEN01C)`) |
| Navigation | XCTL `COSGN00C` (PF3 / no commarea; migrated) and the 11 menu programs `COACTVWC` ... `COPAUS0C` - returned as `nextProgram` with the commarea; none of the menu programs is migrated yet |

## Paragraph mapping

| COBOL paragraph (lines) | Java |
|---|---|
| WORKING-STORAGE (35-58) | `Comen01c.Task` fields (`wsPgmname`, `wsTranid`, `wsMessage`, `wsUsrsecFile`, `wsErrFlg`, `wsRespCd`, `wsReasCd`, `wsOptionX`, `wsOption`, `wsIdx`, `wsMenuOptTxt`, `commarea`, `menu`, `comen1a`) |
| `MAIN-PARA` (75-110) | `Comen01c.Task#mainPara` |
| `PROCESS-ENTER-KEY` (115-191) | `Comen01c.Task#processEnterKey` (+ `moveToUnsignedDisplay` for line 124, `stringInto`/`delimitedBy` for the STRINGs) |
| `RETURN-TO-SIGNON-SCREEN` (196-203) | `Comen01c.Task#returnToSignonScreen` |
| `SEND-MENU-SCREEN` (208-220) | `Comen01c.Task#sendMenuScreen` |
| `RECEIVE-MENU-SCREEN` (225-233) | `Comen01c.Task#receiveMenuScreen` |
| `POPULATE-HEADER-INFO` (238-257) | `Comen01c.Task#populateHeaderInfo` |
| `BUILD-MENU-OPTIONS` (262-303) | `Comen01c.Task#buildMenuOptions` |

## Copybook -> Java

| Copybook field | Java |
|---|---|
| COMEN02Y `CDEMO-MENU-OPT-COUNT` 9(2) VALUE 11 | `MenuOptions.OPT_COUNT`, `count()` |
| COMEN02Y `CDEMO-MENU-OPTIONS-DATA` (11 x 46 VALUE'd FILLERs) redefined as `CDEMO-MENU-OPT OCCURS 12` | `MenuOptions.LAYOUT` (`CDEMO-MENU-OPT-*(1..12)`), `COMEN02Y_ENTRIES`, `comen02y()`; `of(count, entries)` for other tables |
| `CDEMO-MENU-OPT-NUM(i)` 9(2), `-NAME(i)` X(35), `-PGMNAME(i)` X(8), `-USRTYPE(i)` X(1) | `num(i)`, `name(i)`, `pgmName(i)`, `usrType(i)` (byte-offset addressing, any subscript) |
| COMEN01 `COMEN1AI/O`: `TRNNAME`, `TITLE01/02`, `CURDATE`, `CURTIME`, `PGMNAME`, `OPTN001`-`OPTN012` (40), `OPTION` (2, JUSTIFY RIGHT), `ERRMSG` (78), `ERRMSGC` | `SymbolicMap.of("COMEN01","COMEN1A")`: `data()`/`setData()`, `setColor("ERRMSG", Dfhbmsca.DFHRED/DFHGREEN)` |
| COCOM01Y, COTTL01Y, CSDAT01Y, CSMSG01Y | existing `CardDemoCommarea`, `ScreenTitles`, `DateTimeFields`, `CommonMessages` (see COSGN00C.md) |
| DFHBMSCA `DFHRED`, `DFHGREEN` (and the other colours) | new runtime `com.carddemo.runtime.bms.Dfhbmsca` |
| `EXEC CICS INQUIRE PROGRAM(...) NOHANDLE` + `EIBRESP = DFHRESP(NORMAL)` | new runtime `CicsContext#inquireProgram` over `InstalledPrograms` (Spring bean: `app/csd/CARDDEMO.CSD` + `carddemo.cics.installed-programs`) |

## Expected-value derivation

Tests: `Comen01cTest` (paragraph/business-rule tests through `CicsDispatcher`), `Comen01cHttpTest` (CM00 over HTTP),
`Comen01cGnuCobolGoldenTest` (pure-COBOL statements vs GnuCOBOL).

* Header: lines 240-255 with a fixed clock 2024-03-05T14:07:09.25Z -> CURDATE `03/05/24`, CURTIME `14:07:09` (8-byte field
  in COMEN01, unlike COSGN00's 9); TRNNAME `CM00`, PGMNAME `COMEN01C`; titles from COTTL01Y.
* Menu lines: lines 264-272, `STRING num '. ' name DELIMITED BY SIZE` into 40 bytes = 2 + 2 + 35 characters + 1 space, using
  COMEN02Y lines 23-89 (e.g. `01. Account View` + 28 spaces). Only 11 options (COMEN02Y line 22), so `OPTN012` is not sent.
* Option parsing (lines 117-125), validity (127-129), the two message STRINGs (163-167, 172-176) and the menu STRING (269-272)
  are run under GnuCOBOL 3.1.2 by `carddemo-online/tools/gnucobol/comen01c/run.sh`, which copies those lines verbatim around the
  real COMEN02Y copybook; output in `src/test/resources/golden/comen01c/comen01c-golden.txt` (14 option inputs, 12 x 3 strings).
* Messages are the literals at lines 132, 140, 163-166, 172-175 and CSMSG01Y `CCDA-MSG-INVALID-KEY`; ERRMSG is 78 bytes so the
  80-byte WS-MESSAGE is truncated.
* Cursor: no `-1` length and no `CURSOR` on SEND MAP, so the `IC` field (OPTION, row 20 col 42) holds it.
* Commarea: on XCTL to a menu program only `CDEMO-FROM-TRANID` (`CM00`), `CDEMO-FROM-PROGRAM` (`COMEN01C`) and
  `CDEMO-PGM-CONTEXT` (0) change (lines 153-155, 178-182); on first entry only `CDEMO-PGM-CONTEXT` becomes 1 (line 88).
* COPAUS0C: `app/csd/CARDDEMO.CSD` has no `DEFINE PROGRAM(COPAUS0C)` (it is in
  `app/app-authorization-ims-db2-mq/csd/CRDDEMO2.csd`), so with the default configuration option 11 shows
  `This option Pending Authorization View is not installed...` in red; with `carddemo.cics.installed-programs=COPAUS0C` it XCTLs.
* Admin-only and DUMMY paths: every COMEN02Y entry is user type `U` with a real program name, so these branches are tested with
  tables built by `MenuOptions.of(...)` (package-private `Comen01c(MenuOptions)` constructor).

### Bug-for-bug behaviour (kept, not fixed)

* `MOVE WS-OPTION-X TO WS-OPTION` (X(2) -> 9(2)) follows GnuCOBOL: digits are read left to right, `+`/`-`/`,` skipped, `.` or any
  other character ends the number. So `1A` -> 01 (a valid option), `A1` -> 00 (invalid), `-1` and `1.` -> 01. The converted value is
  also what `MOVE WS-OPTION TO OPTIONO` echoes back.
* The invalid-option IF (127-134) and the admin-only IF (136-143) are independent: an option > count whose (spare) table entry is
  `A` sends the menu twice (invalid message, then admin message). With COMEN02Y this cannot happen.
* Line 137 subscripts with WS-OPTION even when it is invalid: `USRTYPE(0)` is the second digit of `CDEMO-MENU-OPT-COUNT`,
  13-99 are outside the table (LOW-VALUES here). Non-numeric WS-OPTION never reaches here after the GnuCOBOL MOVE.
* "coming soon": `name DELIMITED BY SPACE` keeps only the first word and there is no space before `is`
  (`This option Cardis coming soon ...`). "not installed" uses `DELIMITED BY '  '`, so multi-word names survive.
* A commarea shorter than 160 bytes is space-padded (line 86); `CDEMO-PGM-CONTEXT` = space is neither ENTER nor REENTER, so the
  first-entry path runs.
* PF3 moves `COSGN00C` to `CDEMO-TO-PROGRAM`, and every XCTL to the sign-on passes no COMMAREA (line 200-202).
* `CDEMO-FROM-PROGRAM` is moved twice (lines 179-180); `WS-USRSEC-FILE`, CSUSR01Y, `WS-RESP-CD`/`WS-REAS-CD` are never used.
* The first-entry path ignores EIBAID and any received fields.

## LIMITATIONS

* **CICS runtime**: pseudo-conversation, EIB and RESP codes are emulated by `carddemo-runtime`, not a real CICS. SEND MAP resets
  EIBRESP, as the RESP of the last command.
* **INQUIRE PROGRAM** only answers installed / not installed from the CSD extract plus configuration; program status
  (ENABLED/DISABLED), autoinstall and LIBRARY lookup are not emulated.
* **Terminal/BMS**: colour (incl. `ERRMSGC` red/green overrides), brightness, underline and NUM are metadata only. The 3270 numeric
  lock (NUM) is not enforced: REST clients can send letters, which the 3270 would refuse. `JUSTIFY=(RIGHT,ZERO)` right-justifies but
  pads with spaces, not zeros (existing runtime behaviour); COMEN01C replaces those spaces by zeros itself, so only the raw OPTIONI
  differs. MAPFAIL is returned when no fields are sent.
* **Alphanumeric -> numeric MOVE**: emulated as GnuCOBOL does it (golden file). IBM Enterprise COBOL moves the bytes and may fix
  the last byte's zone depending on NUMPROC, so for non-digit input (`1A`, `-1`, `A1`) the z/OS result can differ. Digit/space input
  (all a 3270 NUM field normally sends, except `-`/`.`) is unaffected.
* **Out-of-table subscripts** (option 0 or > 12 in the admin check) read storage outside COMEN02Y on z/OS; here it is the count digit
  / LOW-VALUES. No subscript checking (SSRANGE) is emulated.
* **GnuCOBOL scope**: the full program cannot run under GnuCOBOL (EXEC CICS); only the statements listed above are verified against it.
  COMEN02Y needs `-frelax-syntax-checks` because the OCCURS 12 REDEFINES is larger than the 11-entry data it redefines.
* **Uninitialised storage**: COBOL fields without VALUE (commarea, map) are assumed LOW-VALUES.
* **XCTL targets** COACTVWC, COACTUPC, COCRDLIC, COCRDSLC, COCRDUPC, COTRN00C, COTRN01C, COTRN02C, CORPT00C, COBIL00C and COPAUS0C
  are not migrated; the result only reports `nextProgram` and the commarea.
* **Security**: the commarea is trusted (as in CICS); a client can post a forged commarea, e.g. with user type `A`, to
  `/api/cics/CM00`. No authentication on the REST adapter.
* **FUNCTION CURRENT-DATE** uses the JVM clock/time zone (`Clock` bean).
