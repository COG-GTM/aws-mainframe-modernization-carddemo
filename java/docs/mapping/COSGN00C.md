# COSGN00C - Sign-on (transaction CC00)

| | |
|---|---|
| COBOL | `app/cbl/COSGN00C.cbl` |
| BMS | `app/bms/COSGN00.bms` (mapset COSGN00, map COSGN0A), symbolic map `app/cpy-bms/COSGN00.CPY` |
| Copybooks | COCOM01Y, COSGN00, COTTL01Y, CSDAT01Y, CSMSG01Y, CSUSR01Y, DFHAID, DFHBMSCA |
| File | USRSEC (CSD `DEFINE FILE(USRSEC)`, DSN `AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS`, key 8, LRECL 80) |
| Java | `carddemo-online` `com.carddemo.online.cosgn00c.Cosgn00c` |
| REST | `POST /api/cics/CC00` |
| Navigation | XCTL `COADM01C` (admin) / `COMEN01C` (user) - returned as `nextProgram`, not migrated yet |

## Paragraph mapping

| COBOL paragraph (lines) | Java |
|---|---|
| WORKING-STORAGE (35-58) | `Cosgn00c.Task` fields (`wsMessage`, `wsErrFlg`, `wsUserId`, `wsUserPwd`, `commarea`, `cosgn0a`, `secUserData`) |
| `MAIN-PARA` (73-102) | `Cosgn00c.Task#mainPara` |
| `PROCESS-ENTER-KEY` (108-140) | `Cosgn00c.Task#processEnterKey` |
| `SEND-SIGNON-SCREEN` (145-157) | `Cosgn00c.Task#sendSignonScreen` |
| `SEND-PLAIN-TEXT` (162-172) | `Cosgn00c.Task#sendPlainText` |
| `POPULATE-HEADER-INFO` (177-204) | `Cosgn00c.Task#populateHeaderInfo` |
| `READ-USER-SEC-FILE` (209-257) | `Cosgn00c.Task#readUserSecFile` |

## Copybook -> Java

| Copybook field | Java |
|---|---|
| COCOM01Y `CARDDEMO-COMMAREA` (160 bytes) | `CardDemoCommarea` (`LAYOUT`, `LENGTH`) |
| `CDEMO-FROM-TRANID` X(4) / `-FROM-PROGRAM` X(8) / `-TO-TRANID` X(4) / `-TO-PROGRAM` X(8) | `FROM_TRANID`, `FROM_PROGRAM`, `TO_TRANID`, `TO_PROGRAM` |
| `CDEMO-USER-ID` X(8), `CDEMO-USER-TYPE` X(1) + 88 `CDEMO-USRTYP-ADMIN`/`-USER` | `USER_ID`, `USER_TYPE`, `USRTYP_ADMIN`/`USRTYP_USER` (`isAdmin()`/`isUser()`) |
| `CDEMO-PGM-CONTEXT` 9(1) + 88 `CDEMO-PGM-ENTER`/`-REENTER` | `PGM_CONTEXT` (zoned), `PGM_ENTER`/`PGM_REENTER` |
| `CDEMO-CUST-*`, `CDEMO-ACCT-*`, `CDEMO-CARD-NUM`, `CDEMO-LAST-MAP(SET)` | `CUST_ID`, `CUST_FNAME/MNAME/LNAME`, `ACCT_ID`, `ACCT_STATUS`, `CARD_NUM`, `LAST_MAP`, `LAST_MAPSET` (+ group views) |
| CSUSR01Y `SEC-USER-DATA` (80): `SEC-USR-ID` X(8), `-FNAME` X(20), `-LNAME` X(20), `-PWD` X(8), `-TYPE` X(1), `-FILLER` X(23) | `UserSecurityRecord` `USR_ID`, `USR_FNAME`, `USR_LNAME`, `USR_PWD`, `USR_TYPE`, `USR_FILLER` |
| COTTL01Y `CCDA-TITLE01/02`, `CCDA-THANK-YOU` | `ScreenTitles` |
| CSMSG01Y `CCDA-MSG-THANK-YOU`, `CCDA-MSG-INVALID-KEY` | `CommonMessages` |
| CSDAT01Y `WS-CURDATE-*`, `WS-CURTIME-*`, `WS-CURDATE-MM-DD-YY`, `WS-CURTIME-HH-MM-SS` | `DateTimeFields` |
| COSGN00 `COSGN0AI/O` (`xxxL`, `xxxF/A`, `xxxI/O`) | `SymbolicMap.of("COSGN00","COSGN0A")`: `data()`, `length()`/`setCursor()`, `flag()` |

## Expected-value derivation (tests in `Cosgn00cTest`)

* Users: `app/data/EBCDIC/AWS.M2.CARDDEMO.USRSEC.PS` decoded CP037 (identical to DUSRSECJ.jcl lines 35-44;
  `CopybookTest.seedMatchesDusrsecjInstreamData` proves it). `PASSWORDA` = `SEC-USR-PWD` `PASSWORD` + `SEC-USR-TYPE` `A`.
  app/data/ASCII has no USRSEC file.
* Header: lines 179-196 with a fixed clock 2024-03-05T14:07:09.25Z -> CURDATE `03/05/24`, CURTIME `14:07:09 ` (9-byte field);
  APPLID/SYSID from ASSIGN (defaults `CARDDEMO` / `CICS`); titles copied from COTTL01Y.
* Messages are the literals at lines 120, 125, 242, 249, 254 and CSMSG01Y; ERRMSG is 78 bytes so the 80-byte WS-MESSAGE is truncated.
* Cursor: `MOVE -1 TO xxxL` + `SEND MAP ... CURSOR` (lines 82, 121, 126, 244, 250, 255); otherwise the `IC` field (USERID, row 19 col 44).
* Commarea: only fields set by lines 132-134 and 224-228 change; everything else is LOW-VALUES (uninitialised WORKING-STORAGE).

### Bug-for-bug behaviour (kept, not fixed)

* DFHCOMMAREA is never copied into CARDDEMO-COMMAREA: any re-entry returns a commarea that is LOW-VALUES except `CDEMO-USER-ID`.
* After the "Please enter User ID/Password" errors, lines 132-136 still run, so `CDEMO-USER-ID` gets the (upper-cased) input.
* Wrong password does not set `WS-ERR-FLG` (no visible effect).
* Entered password is upper-cased, stored password is not: a lower-case `SEC-USR-PWD` can never match.
* User type other than `A` goes to COMEN01C.
* Characters beyond 8 in USERID/PASSWD are dropped by the map (LENGTH=8).
* `ASSIGN SYSID` returns 4 bytes into 8-byte SYSIDO; bytes 5-8 keep their previous (LOW-VALUE) content.

## LIMITATIONS

* **CICS runtime**: pseudo-conversation, EIB and RESP codes are emulated by `carddemo-runtime`, not a real CICS. `WS-RESP-CD`/`WS-REAS-CD` follow CICS RESP numbering (NORMAL 0, NOTFND 13, DISABLED 84, ...), but RESP2 values are approximations.
* **Terminal/BMS**: 3270 attributes (colour, brightness, DRK, FSET/MDT, PROT) are reported as metadata only; DRK fields are blanked in `screenLines`. What the terminal transmits (MDT) is decided by the REST client: send only the fields the user modified. MAPFAIL is returned when no fields are sent.
* **Uninitialised storage**: COBOL fields without VALUE are assumed LOW-VALUES; on z/OS their initial content depends on compiler/CICS options.
* **EBCDIC collation**: keys are compared as ISO-8859-1 unsigned bytes, not EBCDIC; ordering differs for mixed letters/digits (no effect on COSGN00C, which only does keyed READs).
* **XCTL targets** COADM01C and COMEN01C are not migrated; the result only reports `nextProgram` and the commarea.
* **Security**: passwords are plain text and seeded with the source's demo values (`PASSWORD`); no RACF/ESM sign-on is emulated and the REST adapter has no authentication. As in CICS, the commarea is trusted: a client can post a forged commarea to `/api/cics/programs/{program}`. Not for exposure beyond a local/demo environment.
* **Task scope**: READ UPDATE holds and browse cursors are per task (thread) and released when the task ends (`TaskFileState`); there is no record-level locking across tasks (no ENQ), so two tasks can READ UPDATE the same record.
* **FUNCTION CURRENT-DATE** uses the JVM clock/time zone (`Clock` bean).
* GnuCOBOL golden files are not used for this online program (CICS commands cannot run under GnuCOBOL); expected values are hand-traced from the source.
