# CBTRN02C — Daily transaction posting (job POSTTRAN, Java STEP15)

Source: `app/cbl/CBTRN02C.cbl`; JCL `app/jcl/POSTTRAN.jcl` (`STEP15 EXEC PGM=CBTRN02C`). Files: `DALYTRAN` sequential
input (`CVTRA06Y`, 350); `TRANFILE` KSDS opened **OUTPUT** (`CVTRA05Y`, 350, key `TRAN-ID X(16)`); `XREFFILE` KSDS
input (`CVACT03Y`, key card number); `DALYREJS` new GDG generation `(+1)`, `RECFM=F LRECL=430`; `ACCTFILE` KSDS I-O
(`CVACT01Y`, key `9(11)`); `TCATBALF` KSDS I-O (`CVTRA01Y`, key `9(11)` + `X(02)` + `9(04)`). All amounts are signed
zoned decimal with 2 decimals (no COMP-3, no `ROUNDED`: `BigDecimal`, `RoundingMode.DOWN`, ADR-0004/0005).
Harvested from the prior-work business spec (`devin/1790171286-posttran-business-spec`,
`docs/specs/POSTTRAN-CBTRN02C-business-spec.md`, see `docs/modernization/00-prior-work.md` #30) and re-checked
against the source. Java: `com.carddemo.batch.posttran.Cbtrn02c`, job `cbtrn02c`, `PosttranJobConfiguration`.

## Main loop (l.202–231)

| # | Given | Then |
|---|---|---|
| R-1 | Start | `START OF EXECUTION OF PROGRAM CBTRN02C`; open DALYTRAN, TRANFILE (OUTPUT: the transaction master is **replaced**, not appended), XREFFILE, DALYREJS, ACCTFILE, TCATBALF; a non-`00` status → `ERROR OPENING …` + `FILE STATUS IS: NNNN…` + `ABENDING PROGRAM`, abend U999 (Java RC 16). |
| R-2 | Each DALYTRAN record (status `00`; `10` = end; other → abend) | Count it; reason ← 0, description ← spaces; validate (R-3..R-7); reason 0 → post (R-8..R-11), else count a reject and write it (R-12). |
| R-3 | End of file | Close the six files; `TRANSACTIONS PROCESSED :` and `TRANSACTIONS REJECTED  :` as `9(09)`; `END OF EXECUTION OF PROGRAM CBTRN02C`; **RC 4 if any reject, else 0**. |

## Validation (1500-VALIDATE-TRAN, l.370–420)

| # | Given | Then |
|---|---|---|
| R-4 | `READ XREFFILE` by `DALYTRAN-CARD-NUM` INVALID KEY | Reason **100** `INVALID CARD NUMBER FOUND`; the account checks are skipped. |
| R-5 | `READ ACCTFILE` by `XREF-ACCT-ID` INVALID KEY | Reason **101** `ACCOUNT RECORD NOT FOUND`; limit/expiry checks skipped. |
| R-6 | `WS-TEMP-BAL S9(09)V99 = ACCT-CURR-CYC-CREDIT - ACCT-CURR-CYC-DEBIT + DALYTRAN-AMT` (high-order digits beyond 9 are truncated, as the COMPUTE target); `ACCT-CREDIT-LIMIT >= WS-TEMP-BAL` | Passes; **equal to the limit passes**. Otherwise reason **102** `OVERLIMIT TRANSACTION`, and validation continues. |
| R-7 | `ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS(1:10)` (character comparison) | Passes; **expiring on the transaction date passes**. Otherwise reason **103** `TRANSACTION RECEIVED AFTER ACCT EXPIRATION`, **overwriting 102** when both fail. `ACCT-ACTIVE-STATUS` is not checked. |

## Posting (2000-POST-TRANSACTION, l.424–444, 467–579) — Java: one database transaction per record

| # | Given | Then |
|---|---|---|
| R-8 | Accepted record | `TRAN-RECORD` ← every DALYTRAN field (same id, type, category, source, description, amount, merchant, card, original timestamp); `TRAN-PROC-TS` ← `YYYY-MM-DD-HH.MM.SS.hh0000` from `CURRENT-DATE` (hundredths, then `0000`; Java: injected `Clock`, `golden` = 2022-07-06T00:00, ADR-0014). |
| R-9 | 2700: `READ TCATBALF` by (`XREF-ACCT-ID`, type, category); status `00`/`23` accepted, other → abend | Not found: `TCATBAL record not found for key : <17-char key>.. Creating.`; new record with balance = amount (`WRITE`). Found: balance + amount (`REWRITE`). Signed, no netting. |
| R-10 | 2800: account from R-5 | `ACCT-CURR-BAL` + amount; amount `>= 0` → `ACCT-CURR-CYC-CREDIT` + amount; amount `< 0` → `ACCT-CURR-CYC-DEBIT` + amount (the debit bucket accumulates **negative** values); `REWRITE`. INVALID KEY sets reason 109 but nothing tests it (dead code). |
| R-11 | 2900: `WRITE TRANFILE` | Status not `00` (e.g. `22` duplicate `TRAN-ID`) → `ERROR WRITING TO TRANSACTION FILE` + abend. COBOL leaves the TCATBALF/ACCTFILE updates of that record applied; Java table mode rolls back the whole record (ticket s4.2: one posting = one database transaction), earlier records stay committed. File mode has no transaction: on any abend the KSDS files are written with every update made so far (VSAM writes are durable when issued), so they match COBOL. |

## Rejects (2500-WRITE-REJECT-REC, l.446–465)

| # | Given | Then |
|---|---|---|
| R-12 | Rejected record | `DALYREJS` record = the original 350-byte DALYTRAN image + 80-byte trailer (`9(04)` reason + `X(76)` description); a write error → `ERROR WRITING TO REJECTS FILE` + abend. |
| R-13 | `DISP=(NEW,CATLG,DELETE)` | The generation is catalogued only when the step ends normally. Java: `DatedOutputFiles` writes `DALYREJS/DALYREJS.<business-date>.<job-execution-id>` + a `batch_output_file` row (ADR-0012) after the program ends; an abend catalogues nothing. `--DALYREJS=<path>` writes a plain file instead. |
| R-14 | Rerun after an abend | COBOL POSTTRAN cannot be restarted: earlier postings are already in ACCTFILE/TCATBALF and TRANFILE is opened OUTPUT again, so a plain rerun posts them twice. Recovery is restore ACCTDATA/TCATBALF, then rerun. Java: `cbtrn02c` is `preventRestart()`, so relaunching a failed instance (same `--run.id`) is refused; after restoring the data, run a new instance. |

## Deviations

| # | Legacy | Java | Why / test |
|---|---|---|---|
| D-1 | CICS files were closed (`CLOSEFIL`) while POSTTRAN ran, so no online add could race a TRANFILE write. | Table mode keeps the online API up, so the CBTRN02C step holds `TransactionRepository.TRAN_ID_LOCK` as a PostgreSQL session lock from before the TRANFILE OPEN OUTPUT clear to step end (`TransactionIdStepLock`); online `TransactionIds` takes the transaction-scoped form of the same key, so online adds wait for the posting instead of computing `max(tran_id)+1` between two postings (DALYTRAN ids ascend, a per-record lock would allow a duplicate). No change to what is posted (golden set unchanged). | s6.4 hardening; `TransactionIdLockIT` (a held online lock blocks the posting; 20 online adds racing the step all succeed with ids above every posted id). Volume cost and batch window caveats: `volume-smoke.md`, `10-runbook-nightly-cycle.md` §7. |

## FILLER and record areas

`READ … INTO` copies the whole record into working storage, `INITIALIZE` and field MOVEs never touch FILLER, so a
WRITE/REWRITE carries the FILLER of the last record read on that file (TCATBALF records created by R-9 inherit the
`0…0` FILLER of the sample). File-mode DDs reproduce this byte for byte (`KeyedDataset`); tables do not store FILLER
(ADR-0011), so table-mode comparisons exclude it and report the count.

Baseline (`docs/validation/baseline/POSTTRAN`): 300 processed, 38 rejected (all reason 102), 262 posted, RC 4; 50
created TCATBALF rows (100 total). Reproduced with zero differences in file mode and, apart from the documented
ACCTDATA record 49 ZIP input difference and the unpersisted FILLER, in table mode (`scripts/batch/run_posttran.sh`).
