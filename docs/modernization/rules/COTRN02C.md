# COTRN02C — Transaction add (transaction CT02, map COTRN2A / mapset COTRN02)

Source: `app/cbl/COTRN02C.cbl`. Data access:
`CXACAIX` `READ` by `XREF-ACCT-ID X(11)`; `CCXREF` `READ` by `XREF-CARD-NUM X(16)` (`CVACT03Y`);
`TRANSACT` `STARTBR`/`READPREV`/`ENDBR` from HIGH-VALUES and `WRITE` by `TRAN-ID X(16)` (`CVTRA05Y`).
Calls `CSUTLDTC` (date validator, format `YYYY-MM-DD`). Fields: `ACTIDIN`(11), `CARDNIN`(16), `TTYPCD`(2), `TCATCD`(4),
`TRNSRC`(10), `TRNAMT`(12), `TDESC`(60), `TORIGDT`(10), `TPROCDT`(10), `MID`(9), `MNAME`(30), `MCITY`(25), `MZIP`(10), `CONFIRM`(1).

**Important:** `SEND-TRNADD-SCREEN` ends with `EXEC CICS RETURN TRANSID('CT02')`, so the **first** failing validation sends
its message and ends the task; later rules are not evaluated.

## MAIN-PARA

| # | Given | Then |
|---|---|---|
| R-1 | `EIBCALEN = 0` | `CDEMO-TO-PROGRAM='COSGN00C'`; `RETURN-TO-PREV-SCREEN`. |
| R-2 | First entry | Set re-enter; clear map; if `CDEMO-CT02-TRN-SELECTED` non-blank → copied to `CARDNINI` and `PROCESS-ENTER-KEY`; send. |
| R-3 | Re-entry + `DFHENTER` | `PROCESS-ENTER-KEY`. |
| R-4 | Re-entry + `DFHPF3` | `CDEMO-TO-PROGRAM` = `CDEMO-FROM-PROGRAM` if non-blank else `COMEN01C`; return. |
| R-5 | Re-entry + `DFHPF4` | `CLEAR-CURRENT-SCREEN` (all 14 fields + message blank, cursor `ACTIDIN`). |
| R-6 | Re-entry + `DFHPF5` | `COPY-LAST-TRAN-DATA` (R-27). |
| R-7 | Other AID | `Invalid key pressed. Please see below...`; send. |

## PROCESS-ENTER-KEY = VALIDATE-INPUT-KEY-FIELDS → VALIDATE-INPUT-DATA-FIELDS → confirm

### VALIDATE-INPUT-KEY-FIELDS

| # | Given | Then |
|---|---|---|
| R-8 | `ACTIDINI` non-blank, not numeric | `Account ID must be Numeric...`; cursor `ACTIDIN`. |
| R-9 | `ACTIDINI` numeric | `XREF-ACCT-ID ← NUMVAL(ACTIDINI)` as 11 digits (echoed back zero-padded); `READ-CXACAIX-FILE`; on success `CARDNINI ← XREF-CARD-NUM`. Account takes precedence over card when both are entered. |
| R-10 | `ACTIDINI` blank, `CARDNINI` non-blank, not numeric | `Card Number must be Numeric...`; cursor `CARDNIN`. |
| R-11 | `CARDNINI` numeric | `XREF-CARD-NUM ← NUMVAL(CARDNINI)` 16 digits; `READ-CCXREF-FILE`; on success `ACTIDINI ← XREF-ACCT-ID`. |
| R-12 | Both blank | `Account or Card Number must be entered...`; cursor `ACTIDIN`. |
| R-13 | `READ-CXACAIX-FILE` | `NOTFND` → `Account ID NOT found...`; other → `Unable to lookup Acct in XREF AIX file...` (cursor `ACTIDIN`). |
| R-14 | `READ-CCXREF-FILE` | `NOTFND` → `Card Number NOT found...`; other → `Unable to lookup Card # in XREF file...` (cursor `CARDNIN`). |

### VALIDATE-INPUT-DATA-FIELDS (presence, in this order)

| # | Given | Then |
|---|---|---|
| R-15 | `TTYPCDI` blank | `Type CD can NOT be empty...` |
| R-16 | `TCATCDI` blank | `Category CD can NOT be empty...` |
| R-17 | `TRNSRCI` blank | `Source can NOT be empty...` |
| R-18 | `TDESCI` blank | `Description can NOT be empty...` |
| R-19 | `TRNAMTI` blank | `Amount can NOT be empty...` |
| R-20 | `TORIGDTI` blank | `Orig Date can NOT be empty...` |
| R-21 | `TPROCDTI` blank | `Proc Date can NOT be empty...` |
| R-22 | `MIDI` / `MNAMEI` / `MCITYI` / `MZIPI` blank | `Merchant ID can NOT be empty...` / `Merchant Name can NOT be empty...` / `Merchant City can NOT be empty...` / `Merchant Zip can NOT be empty...` |

### Format rules (each cursor on its own field)

| # | Given | Then |
|---|---|---|
| R-23 | `TTYPCDI` not numeric | `Type CD must be Numeric...`; `TCATCDI` not numeric → `Category CD must be Numeric...`. |
| R-24 | `TRNAMTI` not matching `[+-]99999999.99` exactly (pos 1 ∈ {`-`,`+`}, pos 2–9 digits, pos 10 `.`, pos 11–12 digits) | `Amount should be in format -99999999.99`. |
| R-25 | `TORIGDTI` / `TPROCDTI` not `9999-99-99` shape | `Orig Date should be in format YYYY-MM-DD` / `Proc Date should be in format YYYY-MM-DD`. |
| R-26 | Shape OK | `WS-TRAN-AMT-N ← NUMVAL-C(TRNAMTI)` and re-displayed as `+99999999.99`. Then `CALL 'CSUTLDTC'(date,'YYYY-MM-DD',result)` for each date: accepted when `CSUTLDTC-RESULT-SEV-CD='0000'` **or** `RESULT-MSG-NUM='2513'` (CEEDAYS "insufficient data" – lenient); otherwise `Orig Date - Not a valid date...` / `Proc Date - Not a valid date...`. Finally `MIDI` not numeric → `Merchant ID must be Numeric...`. |

### Confirm

| # | Given | Then |
|---|---|---|
| R-27a | `CONFIRMI` = `Y`/`y` | `ADD-TRANSACTION` (R-28). |
| R-27b | `N`/`n`/blank | `ERR-FLG='Y'`; `Confirm to add this transaction...`; cursor `CONFIRM`. |
| R-27c | Other | `Invalid value. Valid values are (Y/N)...`; cursor `CONFIRM`. |

## ADD-TRANSACTION

| # | Given | Then |
|---|---|---|
| R-28 | Id assignment | `TRAN-ID ← HIGH-VALUES`; `STARTBR`; `READPREV` (last record; `ENDFILE` → `TRAN-ID ← 0`); `ENDBR`; `WS-TRAN-ID-N ← TRAN-ID + 1` (16-digit, zero-padded). `STARTBR NOTFND` → `Transaction ID NOT found...`; other browse error → `Unable to lookup Transaction...`. |
| R-29 | Record build | `INITIALIZE TRAN-RECORD`; `TRAN-TYPE-CD←TTYPCDI`, `TRAN-CAT-CD←TCATCDI`, `TRAN-SOURCE←TRNSRCI`, `TRAN-DESC←TDESCI`, `TRAN-AMT←NUMVAL-C(TRNAMTI)` (S9(9)V99), `TRAN-CARD-NUM←CARDNINI`, `TRAN-MERCHANT-ID←MIDI`, `-NAME←MNAMEI`, `-CITY←MCITYI`, `-ZIP←MZIPI`, `TRAN-ORIG-TS←TORIGDTI`, `TRAN-PROC-TS←TPROCDTI` (10-char date moved into the 26-char timestamp; rest spaces). No account balance update is made (contrast COBIL00C). |
| R-30 | `WRITE` `NORMAL` | All fields cleared; `ERRMSGC=DFHGREEN`; `Transaction added successfully.  Your Tran ID is <id>.` (two spaces before *Your*). |
| R-31 | `DUPKEY`/`DUPREC` | `Tran ID already exist...`. Other → `Unable to Add Transaction...`. |

## COPY-LAST-TRAN-DATA (PF5)

| # | Given | Then |
|---|---|---|
| R-32 | PF5 | `VALIDATE-INPUT-KEY-FIELDS` (account/card must be valid first, R-8..R-14); last transaction read via HIGH-VALUES `READPREV`; if no error its type, category, source, amount (edited), description, orig/proc TS, merchant id/name/city/zip are copied into the input fields, then `PROCESS-ENTER-KEY` runs (so the copied data is validated and, with `CONFIRM` blank, R-27b `Confirm to add this transaction...` is shown). |

## RETURN-TO-PREV-SCREEN / SEND-TRNADD-SCREEN

| # | Given | Then |
|---|---|---|
| R-33 | Return | Blank target → `COSGN00C`; from-fields `CT02`/`COTRN02C`, context 0; `XCTL ... COMMAREA`. |
| R-34 | Send | Standard header (`CT02`, `COTRN02C`); `SEND MAP('COTRN2A') MAPSET('COTRN02') ERASE CURSOR` **then `RETURN TRANSID('CT02') COMMAREA`** (task ends). |

## Java port notes (UNT51-20, `POST /api/v1/transactions`)

- One request runs the dialogue: keys (R-8..R-14) → presence (R-15..R-22) → format (R-23..R-26) → confirm (R-27).
  `confirm` blank/`N` = validate only (200 `VALIDATED`), `Y` = write (201 `ADDED`, `Location` header), other = 400.
  `copyLast=true` is PF5 (R-32). Errors are the first failing edit, in source order, on its field.
- Addition required by the migration plan (not in the COBOL): after R-26 the type code must exist in TRANTYPE
  (`Type CD not found in TRANTYPE...`) and the type/category pair in TRANCATG
  (`Category CD not found in TRANCATG for this Type CD...`), so online rows always resolve in TRANREPT.
- **Race-safe id (R-28).** COBOL reads the last key (HIGH-VALUES `READPREV`) and adds one, which two concurrent tasks can
  both do. The port takes `pg_advisory_xact_lock(TransactionRepository.TRAN_ID_LOCK)` first, then reads
  `max(tran_id)` (`findFirstByOrderByTranIdDesc`), adds one (16 digits, zero-padded; empty table → `0000000000000001`;
  wraps at 10^16 like `PIC 9(16)`) and inserts, all in the request's transaction; the lock is released at commit or
  rollback. COBIL00C uses the same lock, so online adds and bill payments are serialised on id assignment only. A
  database sequence was not used because it would not continue from ids written by POSTTRAN/repro and leaves gaps
  on rollback. A duplicate key that still happens (a writer outside the lock) is 409 `DUPREC` `Tran ID already exist...`.
- R-29: rows go to the shared `transaction` table in the TRANSACT layout, so the TRANREPT job stream reports them
  (`TransactionApiIT`). `orig_ts`/`proc_ts` hold the 10-character date as typed (the COBOL trailing spaces are not
  stored; fixed-width unloads pad them back). TRANREPT selects on `proc_ts(1:10)`.
- Role: no ownership check — COTRN02C has none (ADR-0020 §4).
