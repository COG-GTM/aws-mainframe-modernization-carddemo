# COSGN00C — Sign-on screen (transaction CC00, map COSGN0A / mapset COSGN00)

Source: `app/cbl/COSGN00C.cbl`. Entry point of the online application. Data access: `USRSEC` (VSAM KSDS,
key = 8-byte user id, READ only). Commarea: `CARDDEMO-COMMAREA` (copybook `COCOM01Y`).

Each rule is numbered **R-n** and written as *given → then*; message texts are verbatim from the source
(80-byte `WS-MESSAGE` shown in field `ERRMSG`). "Cursor" means the `-1` length trick (`MOVE -1 TO xxxL`).

## MAIN-PARA

| # | Given | Then |
|---|---|---|
| R-1 | Program entered with `EIBCALEN = 0` (first entry, no commarea) | Map `COSGN0A` is cleared (`LOW-VALUES`), cursor placed on `USERID`, screen sent (`SEND-SIGNON-SCREEN`), then `RETURN TRANSID('CC00')` with the commarea so the next AID re-enters this program. |
| R-2 | `EIBCALEN > 0` and `EIBAID = DFHENTER` | `PROCESS-ENTER-KEY` is performed (R-5 ff.). |
| R-3 | `EIBAID = DFHPF3` | `CCDA-MSG-THANK-YOU` (`COTTL01Y`: `Thank you for using CardDemo application...      `) is sent as plain text (`SEND TEXT … ERASE FREEKB`) and the task ends with a bare `EXEC CICS RETURN` (no TRANSID → user is logged off to CICS). |
| R-4 | Any other AID (PF1, PF2, PF4–PF24, PA, CLEAR, …) | `WS-ERR-FLG = 'Y'`, message `CCDA-MSG-INVALID-KEY` (`Invalid key pressed. Please see below...       `) and the sign-on screen is re-sent. |

## PROCESS-ENTER-KEY

| # | Given | Then |
|---|---|---|
| R-5 | `USERIDI` is `SPACES` or `LOW-VALUES` | Error; message `Please enter User ID ...`; cursor on `USERID`; screen re-sent. The password is **not** inspected. |
| R-6 | User id present but `PASSWDI` is `SPACES` or `LOW-VALUES` | Error; message `Please enter Password ...`; cursor on `PASSWD`; screen re-sent. |
| R-7 | Both present | `WS-USER-ID` and `CDEMO-USER-ID` ← `UPPER-CASE(USERIDI)`; `WS-USER-PWD` ← `UPPER-CASE(PASSWDI)`; `READ-USER-SEC-FILE` is performed. Credentials are therefore **case-insensitive** on input and compared upper-cased against the file. |
| R-8 | Error flag already set (R-5/R-6) | The USRSEC read is skipped (`IF NOT ERR-FLG-ON`). |

## READ-USER-SEC-FILE

`EXEC CICS READ DATASET('USRSEC  ') INTO(SEC-USER-DATA) RIDFLD(WS-USER-ID) KEYLENGTH(8)` — full-key read, record layout `CSUSR01Y`
(`SEC-USR-ID X(8)`, `SEC-USR-FNAME X(20)`, `SEC-USR-LNAME X(20)`, `SEC-USR-PWD X(8)`, `SEC-USR-TYPE X(1)`, filler X(23)).

| # | Given | Then |
|---|---|---|
| R-9 | `RESP = 0` (NORMAL) and `SEC-USR-PWD = WS-USER-PWD` (exact 8-byte compare, upper-cased input vs stored) | Commarea set: `CDEMO-FROM-TRANID='CC00'`, `CDEMO-FROM-PROGRAM='COSGN00C'`, `CDEMO-USER-ID`, `CDEMO-USER-TYPE=SEC-USR-TYPE`, `CDEMO-PGM-CONTEXT=0`. |
| R-10 | R-9 and `CDEMO-USER-TYPE = 'A'` (`CDEMO-USRTYP-ADMIN`) | `XCTL PROGRAM('COADM01C')` with the commarea (admin menu). |
| R-11 | R-9 and user type is anything else (`'U'`) | `XCTL PROGRAM('COMEN01C')` with the commarea (main menu). |
| R-12 | `RESP = 0` but password differs | Message `Wrong Password. Try again ...`; cursor on `PASSWD`; screen re-sent. **Note:** `WS-ERR-FLG` is *not* set in this branch (only the message is). |
| R-13 | `RESP = 13` (NOTFND) | `WS-ERR-FLG='Y'`; message `User not found. Try again ...`; cursor on `USERID`; screen re-sent. |
| R-14 | Any other RESP (I/O error, file closed, …) | `WS-ERR-FLG='Y'`; message `Unable to verify the User ...`; cursor on `USERID`; screen re-sent. |

## SEND-SIGNON-SCREEN / POPULATE-HEADER-INFO

| # | Given | Then |
|---|---|---|
| R-15 | Every send | Header fields: `TITLE01`/`TITLE02` from `COTTL01Y`, `TRNNAME='CC00'`, `PGMNAME='COSGN00C'`, `CURDATE` as `MM/DD/YY` (from `FUNCTION CURRENT-DATE`, 2-digit year = positions 3:2), `CURTIME` as `HH:MM:SS`, `APPLID` and `SYSID` via `EXEC CICS ASSIGN`. `ERRMSG` ← `WS-MESSAGE`. Sent with `ERASE CURSOR`. |

## Non-functional / modernization notes

- No lockout, no attempt counter, no password hashing: plain 8-byte compare (R-9).
- Blank user id is detected before blank password (R-5 before R-6); both messages end with ` ...`.
- Successful sign-on leaves `CDEMO-PGM-CONTEXT = 0` and the *from* fields pointing at `COSGN00C`; the menu programs use these to detect "fresh from sign-on".
- PF3 ends the pseudo-conversation without a TRANSID (R-3); every other path returns with `TRANSID('CC00')`.
