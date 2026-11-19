# CBTRN02C program functional requirement

Produced by `!mf_program_fr_generation` on 2026-11-19. Program: `app/cbl/CBTRN02C.cbl` (only in-scope program, inventory §3). Scope ends at `GOBACK` (`:234`); nothing is delegated past it. Cites are `CBTRN02C.cbl:<line>`; `FR-POSTTRAN-nn` = stream FR; Wave 1 target element from plan §2.

## 1. Identity

| Item | Value |
|---|---|
| Type | Batch COBOL, no CICS/SQL/DL-I |
| Invoked by | `POSTTRAN.jcl:23` STEP15, no PARM |
| Calls | `CEE3ABD` only, on abend (`:711`) |
| Target | Job `POSTTRAN` / step `STEP15` in `carddemo-app`, package `com.carddemo.transaction` |

## 2. Inputs and outputs

| DD | Copybook | Mode | Target |
|---|---|---|---|
| DALYTRAN | CVTRA06Y (350) | read seq (`:29-32,345-369`) | file `--dd=DALYTRAN` |
| XREFFILE | CVACT03Y (50) | read random by card | `card.card_xref` |
| ACCTFILE | CVACT01Y (300) | read + rewrite | `account.accounts` |
| TCATBALF | CVTRA01Y (50) | read + write + rewrite | `reference.tran_cat_balances` |
| TRANFILE | CVTRA05Y (350) | OUTPUT, write (`:256,562-579`) | `transaction.transactions` |
| DALYREJS | 350 + 80 trailer (`:176-182`) | OUTPUT, write | file `--dd=DALYREJS` |

## 3. Program requirements

| Id | Requirement | Source | Stream FR | Target element | Acceptance |
|---|---|---|---|---|---|
| PR-CBTRN02C-01 | Display `START OF EXECUTION OF PROGRAM CBTRN02C`; open the six files; any open failure displays `ERROR OPENING <file>` + status and abends | `:193-200,236-343` | FR-13, FR-14 | `PosttranJobConfig`, `SysoutWriter` | Missing DALYTRAN file → exit 16 |
| PR-CBTRN02C-02 | Read DALYTRAN to EOF (status 10); other read failure abends; count each record read | `:202-206,345-369` | FR-01, FR-15 | `DailyTransactionReader` | R-04 count 300; R-08 count 0 |
| PR-CBTRN02C-03 | Reset reason to 0 and description to spaces before validating each record | `:207-209` | FR-06 | `DailyTransactionValidator` | Trailer of a later reject never carries an earlier reason |
| PR-CBTRN02C-04 | Lookup xref by `DALYTRAN-CARD-NUM`; not found → 100 `INVALID CARD NUMBER FOUND`; account checks skipped | `:370-391` | FR-02 | `DailyTransactionValidator` | R-07 case 1 |
| PR-CBTRN02C-05 | Lookup account by `XREF-ACCT-ID`; not found → 101 `ACCOUNT RECORD NOT FOUND` | `:393-399` | FR-03 | same | golden `acct-missing` |
| PR-CBTRN02C-06 | Over-limit check `CREDIT-LIMIT >= CYC-CREDIT - CYC-DEBIT + AMT`, else 102 | `:400-412` | FR-04 | same | R-04 (38 × 0102) |
| PR-CBTRN02C-07 | Expiry check `EXPIRAION-DATE >= ORIG-TS(1:10)` (text), else 103, overwriting 102 | `:414-419` | FR-05 | same | R-07 cases 2-3 |
| PR-CBTRN02C-08 | Reject: count it; write 350-byte record + reason `9(4)` + desc `X(76)`; write failure → `ERROR WRITING TO REJECTS FILE` + abend | `:214-216,446-465` | FR-06, FR-13 | `DailyRejectWriter` | DALYREJS byte compare |
| PR-CBTRN02C-09 | Post: build transaction from DALYTRAN fields, PROC-TS `YYYY-MM-DD-HH.MM.SS.hh0000` | `:424-438,692-703` | FR-09 | `DailyTransactionPostingService` | TRANSACT.unl compare (cols 325-326 masked) |
| PR-CBTRN02C-10 | TCATBAL: read key acct+type+cat; 23 → display `TCATBAL record not found for key : <key>.. Creating.` and insert with balance = AMT; found → add AMT and update; any other status abends | `:467-543` | FR-07 | same | TCATBALF.unl + SYSOUT compare |
| PR-CBTRN02C-11 | Account: `CURR-BAL += AMT`; `AMT >= 0` → `CYC-CREDIT += AMT` else `CYC-DEBIT += AMT`; rewrite; rewrite not-found sets 109 only (logged, D-027) | `:545-559` | FR-08 | same | ACCTDATA.unl compare |
| PR-CBTRN02C-12 | Insert transaction; failure (e.g. duplicate id) → `ERROR WRITING TO TRANSACTION FILE` + abend | `:562-579` | FR-13 | same | golden `dup-id` exit 16 |
| PR-CBTRN02C-13 | Steps 10-12 run in order TCATBAL → account → transaction in one DB transaction | `:440-442` | FR-10 | same (`@Transactional`) | Failure-injection test |
| PR-CBTRN02C-14 | New job instance empties the transaction master; restart does not | `:256` | FR-11 | `TransactionMasterResetListener` | Two-run + restart tests |
| PR-CBTRN02C-15 | Close all files; close failure abends | `:221-226,581-690` | FR-13 | step end | n/a (no target equivalent beyond resource close) |
| PR-CBTRN02C-16 | Display `TRANSACTIONS PROCESSED :` and `TRANSACTIONS REJECTED  :` with 9-digit counts; RC 4 if rejects > 0, else 0; display end line; `GOBACK` | `:227-234` | FR-12, FR-14 | `JclExitCodeMapper`, `SysoutWriter` | R-04 RC 4, R-07 syn_ok RC 0, SYSOUT compare |
| PR-CBTRN02C-17 | Abend: display `ABENDING PROGRAM`, abend U999 (no further processing) | `:707-711` | FR-13 | `PosttranAbendException` → exit 16 | R-06 (RC 231 ↔ 16) |

## 4. Error, restart and rerun

- Errors: every unexpected status → PR-17; validation failures are rejects, never errors.
- Restart (failed instance): resume after the last committed record; DALYREJS truncated to committed offset (D-012); transaction master kept.
- Rerun (new instance): master reset (D-024); account and balance tables are not reset, so re-feeding the same DALYTRAN reposts amounts, as COBOL does.

## 5. Hard stop

`GOBACK` at `:234`. No delegation: the next job (WAITSTEP) is triggered by the scheduler on the exit code (B-08), outside this program.

## 6. Acceptance summary

All golden cases (R-04, R-07 syn_all/syn_ok, R-08, R-09, `acct-missing`, `dup-id`): DALYREJS bytes, four table unloads, SYSOUT lines and exit code equal to the oracle; plus unit tests named after PR-CBTRN02C-nn.
