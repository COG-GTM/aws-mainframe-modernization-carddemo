# CBSTM03B — File service for CBSTM03A (job CREASTMT, step STEP040)

Source: `app/cbl/CBSTM03B.CBL` (called subprogram, `PROCEDURE DIVISION USING LK-M03B-AREA`); caller `CBSTM03A`
(`CBSTM03A.md`). Java: `com.carddemo.batch.creastmt.Cbstm03b` — a file-service class with one method per call, keyed
like the COBOL by DD name and operation code; it is not an emulation of the linkage area or of `CALL`.

## Interface

| COBOL (`LK-M03B-AREA`) | Java |
|---|---|
| `LK-M03B-DD X(08)` — `TRNXFILE`, `XREFFILE`, `CUSTFILE`, `ACCTFILE` | `dd` argument (`Cbstm03b.TRNXFILE` …) |
| `LK-M03B-OPER X(01)` — `O` open, `C` close, `R` read next, `K` read by key, `W` write, `Z` rewrite (level 88s `M03B-…`) | `Cbstm03b.Operation` `OPEN('O')`, `CLOSE('C')`, `READ('R')`, `READ_KEY('K')`, `WRITE('W')`, `REWRITE('Z')`; `Operation.of(char)` |
| `LK-M03B-KEY X(25)`, `LK-M03B-KEY-LN S9(4)` | `key`, `keyLength` of `call(dd, operation, key, keyLength)` |
| `LK-M03B-RC X(02)` | `Response.returnCode()` |
| `LK-M03B-FLDT X(1000)` (`READ … INTO`) | `Response.record()` — the record read, empty when nothing was read |

## Files

| DD | Dataset | Organisation / access | Record | Operations implemented |
|---|---|---|---|---|
| `TRNXFILE` | TRXFL KSDS | indexed, **sequential**, key `FD-TRNXS-ID` = card X(16) + transaction id X(16) | 350 | `O` (`OPEN INPUT`), `R`, `C` |
| `XREFFILE` | CARDXREF KSDS | indexed, sequential, key card number X(16) | 50 | `O`, `R`, `C` |
| `CUSTFILE` | CUSTDATA KSDS | indexed, **random**, key `FD-CUST-ID X(09)` | 500 | `O`, `K`, `C` |
| `ACCTFILE` | ACCTDATA KSDS | indexed, random, key `FD-ACCT-ID 9(11)` | 300 | `O`, `K`, `C` |

Java binds each DD to the harness: TRNXFILE / XREFFILE are `KsdsInput`s (a KSDS unload file or the loaded TRXFL
generation / `card_xref` table, read in key order); CUSTFILE / ACCTFILE are `KeyedDataset`s (`customer` / `account` table
or the unload file).

## Rules

| # | Given | Then |
|---|---|---|
| B-1 | Dispatch (`EVALUATE LK-M03B-DD`) | One paragraph per DD; `WHEN OTHER` → `GOBACK` without touching the area (RC stays what the caller put there). No caller passes another DD; Java rejects an unknown DD with `IllegalArgumentException` (programming error, not a file condition). |
| B-2 | `O` | `OPEN INPUT` the DD's file. RC = its FILE STATUS (`00`; `35` missing file; …). |
| B-3 | `R` on TRNXFILE / XREFFILE | `READ <file> INTO LK-M03B-FLDT` — next record in key order, moved left-aligned into the 1000-byte area, rest spaces (Java: the record, the caller pads to its own record length). At end: status `10`, the area keeps its previous content (CBSTM03A clears it with `MOVE SPACES` before every read). Read before open: status `47` (Java harness `FileStatus`). |
| B-4 | `K` on CUSTFILE / ACCTFILE | `MOVE LK-M03B-KEY (1:LK-M03B-KEY-LN) TO FD-<dd>-ID`, `READ <file> INTO LK-M03B-FLDT` (random). Found `00`; not found `23`. ACCTFILE's key is `9(11)`: the caller passes the 11-digit text of `XREF-ACCT-ID`. Java takes the first `keyLength` characters of `key`; a key that is empty or not all digits finds no record (`23`), as no numeric key can equal it. Read before open: `42` in the Java harness (random read of an unopened file). |
| B-5 | `C` | `CLOSE <file>`; RC = FILE STATUS. |
| B-6 | An operation the DD does not implement (`W` and `Z` everywhere, `K` on a sequential DD, `R` on a random DD) | None of the `IF M03B-…` tests matches; control falls into `n900-EXIT`, `MOVE <dd>-STATUS TO LK-M03B-RC`: **no I/O and the RC is the DD's last FILE STATUS** (e.g. `00` after a successful open, so the caller cannot tell it did nothing). Before any I/O the status field is uninitialised (spaces on GnuCOBOL); Java returns `"  "`. `W`/`Z` are declared but never implemented — CBSTM03A only reads. |
| B-7 | Return code | Always the two-character FILE STATUS (`LK-M03B-RC X(02)`), never a numeric RC. The caller decides: open/close accept `00`/`04`; the first TRNXFILE read accepts `00`/`04`, later reads `00` and `10`; XREF reads `00`/`10`; keyed reads only `00` (CBSTM03A R-2/R-3/R-5/R-6/R-13). |
| B-8 | State | Files stay open between calls (the COBOL subprogram is not `INITIAL` and is never `CANCEL`ed); one `Cbstm03b` instance per step run holds the four files and their last statuses. |

## The 13 calls CBSTM03A makes

TRNXFILE: `O`, `R` (first), `R` (loop), `C`; XREFFILE: `O`, `R`, `C`; CUSTFILE: `O`, `K`, `C`; ACCTFILE: `O`, `K`, `C`.

## Verification

`Cbstm03bTest`: sequential `O`/`R`/EOF `10`/`C` on TRNXFILE and XREFFILE, keyed `K` hits (`00`) and misses (`23`) on
CUSTFILE / ACCTFILE, B-6 (`W` before any I/O → spaces, `K` on a sequential DD after open → `00`, `R` on a random DD →
spaces, no record), read before open (`47` / `42`), unknown operation code and unknown DD. The baseline and equivalence runs exercise every call
through CBSTM03A (`CBSTM03A.md`, Verification).
