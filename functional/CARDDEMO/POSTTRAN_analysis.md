# POSTTRAN stream analysis (BATCH)

Produced by `!mf_stream_analysis` on 2026-11-19. Scope = STOP B (D-002): `app/jcl/POSTTRAN.jcl` STEP15 → `CBTRN02C`, hard stop `GOBACK` (`app/cbl/CBTRN02C.cbl:234`). Nothing past `GOBACK` is analysed (WAITSTEP and later CA-7 jobs are context only). All cites are `app/cbl/CBTRN02C.cbl:<line>` unless stated. Behaviour marked **[ORACLE]** was observed in `.migration/07_runbook.md`.

## 1. Trigger and runtime contract

| Item | Value | Cite |
|---|---|---|
| Trigger | CA-7 triggers POSTTRAN on completion of CBPAUP0J; POSTTRAN completion triggers WAITSTEP | `app/scheduler/CardDemo.ca7:70,97` |
| Step | `STEP15 EXEC PGM=CBTRN02C`, no PARM, no COND | `POSTTRAN.jcl:23` |
| Inputs | DALYTRAN (PS), XREFFILE, ACCTFILE, TCATBALF (KSDS) | `POSTTRAN.jcl:28-43` |
| Outputs | TRANFILE (KSDS, recreated), ACCTFILE / TCATBALF (updated in place), DALYREJS (new GDG generation, LRECL 430) | `POSTTRAN.jcl:28,34-39` |
| Return code | 0 if no reject; 4 if ≥ 1 reject; abend U999 on any unexpected I/O status (GnuCOBOL RC 231) | `:229-231`, `:707-711`; [ORACLE] R-04, R-06 |
| Online window | CLOSEFIL closes CICS files TRANSACT, CCXREF, ACCTDAT, CXACAIX, USRSEC before the chain; OPENFIL reopens them | `app/jcl/CLOSEFIL.jcl:26-30`, `app/jcl/OPENFIL.jcl:26-30` |

## 2. Control flow (paragraph level)

```
PROCEDURE DIVISION (:192-234)
  DISPLAY 'START OF EXECUTION OF PROGRAM CBTRN02C'
  0000..0500 open DALYTRAN(INPUT) TRANSACT(OUTPUT :256) XREF(INPUT) DALYREJS(OUTPUT) ACCT(I-O) TCATBAL(I-O)
      any status <> '00' -> 9999-ABEND-PROGRAM
  loop until EOF (:202-219)
     1000-DALYTRAN-GET-NEXT (:345-369): '00' ok | '10' EOF | other -> abend
     count += 1; reason := 0; desc := spaces
     1500-VALIDATE-TRAN (:370-378)
        1500-A-LOOKUP-XREF (:380-391): READ XREF by card num; INVALID KEY -> 100
        if reason = 0: 1500-B-LOOKUP-ACCT (:393-420)
            READ ACCT by XREF-ACCT-ID; INVALID KEY -> 101
            else: temp = CYC-CREDIT - CYC-DEBIT + AMT; if CREDIT-LIMIT < temp -> 102
                  if EXPIRAION-DATE < ORIG-TS(1:10) -> 103   (evaluated after 102: overwrites it)
     reason = 0 -> 2000-POST-TRANSACTION (:424-444)
                     build TRAN-RECORD from DALYTRAN, PROC-TS := Z-GET-DB2-FORMAT-TIMESTAMP
                     2700-UPDATE-TCATBAL -> 2800-UPDATE-ACCOUNT-REC -> 2900-WRITE-TRANSACTION-FILE
     else          -> rejects += 1; 2500-WRITE-REJECT-REC (:446-465)
  9000..9500 close all six (any status <> '00' -> abend; 9300 displays XREFFILE-STATUS instead of DALYREJS-STATUS :649)
  DISPLAY counts (:227-228); rejects > 0 -> RETURN-CODE 4 (:229-231)
  DISPLAY 'END OF EXECUTION OF PROGRAM CBTRN02C'; GOBACK (:234)  <- hard stop
```

## 3. Business rules

| Id | Rule | Cite |
|---|---|---|
| BR-01 | Card must exist in CARDXREF, else reject 100 `INVALID CARD NUMBER FOUND` | `:380-391` |
| BR-02 | Account (from XREF-ACCT-ID) must exist, else reject 101 `ACCOUNT RECORD NOT FOUND`; account checks skipped when BR-01 failed | `:372-376,393-399` |
| BR-03 | Over-limit: reject 102 `OVERLIMIT TRANSACTION` when `ACCT-CREDIT-LIMIT < ACCT-CURR-CYC-CREDIT - ACCT-CURR-CYC-DEBIT + DALYTRAN-AMT` | `:403-412` |
| BR-04 | Expiry: reject 103 `TRANSACTION RECEIVED AFTER ACCT EXPIRATION` when `ACCT-EXPIRAION-DATE < DALYTRAN-ORIG-TS(1:10)` (text compare, `YYYY-MM-DD`); checked after BR-03 and overwrites 102 | `:414-419`; [ORACLE] R-07 |
| BR-05 | Valid record → TCATBAL key (acct, type, cat): if absent, DISPLAY `TCATBAL record not found for key : <17-char key>.. Creating.` and WRITE a new record with balance = AMT; else ADD AMT and REWRITE | `:467-543` |
| BR-06 | Account update: `CURR-BAL += AMT`; `AMT >= 0` → `CURR-CYC-CREDIT += AMT` else `CURR-CYC-DEBIT += AMT`; REWRITE | `:545-554` |
| BR-07 | TRANSACT record = DALYTRAN fields copied 1:1 + `TRAN-PROC-TS` = current timestamp `YYYY-MM-DD-HH.MM.SS.hh0000` | `:424-438,692-703` |
| BR-08 | Reject record = original 350-byte DALYTRAN record + trailer: reason `9(4)` + description `X(76)` | `:176-182,446-451` |
| BR-09 | Posting order per record: TCATBAL, ACCOUNT, TRANSACT | `:440-442` |
| BR-10 | RC 4 when at least one reject, else 0 | `:229-231` |

## 4. Data access

| File | Open | Ops | Key | Status handling |
|---|---|---|---|---|
| DALYTRAN | INPUT | READ next | - | 00 ok, 10 EOF, else abend (`:345-369`) |
| XREF | INPUT | READ random | card num | INVALID KEY → 100 (`:383-388`) |
| ACCOUNT | I-O | READ, REWRITE | acct id | READ INVALID KEY → 101; REWRITE INVALID KEY → sets 109 only (`:555-558`) |
| TCATBAL | I-O | READ, WRITE, REWRITE | acct+type+cat (17) | READ 00/23 ok else abend; WRITE/REWRITE ≠ 00 → abend |
| TRANSACT | **OUTPUT** | WRITE | tran id | ≠ 00 (e.g. 22 duplicate) → abend (`:562-579`); [ORACLE] R-06 |
| DALYREJS | OUTPUT | WRITE | - | ≠ 00 → abend (`:446-465`) |

## 5. Quirks preserved for parity (not fixed)

| Id | Quirk | Cite | Consequence |
|---|---|---|---|
| Q-01 | `OPEN OUTPUT` on TRANSACT: each run starts from an empty TRANSACT; previous content is replaced by this run's postings | `:256`; `app/jcl/TRANFILE.jcl:30-49` reloads TRANSACT separately | A new run replaces the master; a rerun of the same DALYTRAN reposts ACCT/TCATBAL |
| Q-02 | 103 overwrites 102 | `:403-419` | Trailer shows 103 when both fail [ORACLE] R-07 |
| Q-03 | REWRITE INVALID KEY on ACCOUNT sets reason 109 after the decision to post: no reject, TRANSACT still written, RC unaffected | `:555-558` | Unreachable in practice (account just read); target logs it, no reject |
| Q-04 | Abend after TCATBAL/ACCOUNT updates but before TRANSACT write leaves partial updates (no rollback in VSAM) | `:440-442` | Target improves to one DB transaction per record (D-011); parity cases never abend mid-record except R-06, compared on RC only |
| Q-05 | Close of DALYREJS reports XREFFILE-STATUS | `:649` | Message text only |
| Q-06 | TRAN-PROC-TS centiseconds come from the clock | `:693-700` | Golden compare masks cols 325-326 (R-05) |

## 6. Boundaries touched

B-01..B-09 (`.migration/04_boundary_register.md`). Decisions in `POSTTRAN_migration_plan.md` §4.

## 7. Observed volumes [ORACLE R-04]

300 input records → 262 posted, 38 rejected (all 102), RC 4; TCATBALF 50 → 100 rows (50 created); ACCTDATA 50 rows updated in place.
