# POSTTRAN stream functional requirements

Produced by `!mf_stream_fr_generation` on 2026-11-19 from `POSTTRAN_analysis.md` (BR/Q ids) and the source. Scope ends at `CBTRN02C` `GOBACK` (`app/cbl/CBTRN02C.cbl:234`). Each requirement has an acceptance check against the GnuCOBOL oracle (`.migration/07_runbook.md`).

## 1. Stream summary

Daily posting: read every DALYTRAN record, validate it against CARDXREF and ACCTDATA, post valid ones to TCATBALF, ACCTDATA and TRANSACT, write invalid ones to a new DALYREJS generation, and end with RC 0 (no rejects) or 4 (rejects).

## 2. Requirements

| Id | Requirement | Source | Acceptance |
|---|---|---|---|
| FR-POSTTRAN-01 | The job `POSTTRAN` runs one step `STEP15` that processes the DALYTRAN file sequentially to EOF, one record at a time, in file order | `POSTTRAN.jcl:23`; `:202-219` | R-04: 300 processed |
| FR-POSTTRAN-02 | A record whose card number is not in CARDXREF is rejected with reason `0100 INVALID CARD NUMBER FOUND`; no account check is made | BR-01, BR-02 | R-07 syn_all case 1 |
| FR-POSTTRAN-03 | A record whose xref account is not in ACCTDATA is rejected with `0101 ACCOUNT RECORD NOT FOUND` | BR-02 | Phase 1 golden case `acct-missing` (new oracle case) |
| FR-POSTTRAN-04 | A record is rejected with `0102 OVERLIMIT TRANSACTION` when `credit_limit < cyc_credit - cyc_debit + amount` (exact decimal arithmetic, scale 2) | BR-03 | R-04: 38 × 0102 |
| FR-POSTTRAN-05 | A record is rejected with `0103 TRANSACTION RECEIVED AFTER ACCT EXPIRATION` when the account expiration date (text `YYYY-MM-DD`) is lower than the first 10 chars of the origin timestamp; 0103 replaces 0102 when both apply | BR-04, Q-02 | R-07 syn_all cases 2-3 |
| FR-POSTTRAN-06 | Each reject writes one 430-byte DALYREJS record: the original 350 input bytes unchanged + reason (4 digits, zero padded) + description (76 chars, space padded) | BR-08 | DALYREJS byte-identical to R-04 / R-07 |
| FR-POSTTRAN-07 | A valid record adds the amount to the transaction-category balance (account, type, category); if the balance row does not exist it is created with the amount and the line `TCATBAL record not found for key : <acct11><type2><cat4>.. Creating.` is written to SYSOUT | BR-05 | R-04 TCATBALF table = TCATBALF.unl; SYSOUT lines equal |
| FR-POSTTRAN-08 | A valid record adds the amount to the account current balance, and to cycle credit when amount ≥ 0, else to cycle debit | BR-06 | R-04 accounts table = ACCTDATA.unl |
| FR-POSTTRAN-09 | A valid record inserts a transaction with all DALYTRAN fields copied unchanged and processing timestamp `YYYY-MM-DD-HH.MM.SS.hh0000` from the job clock | BR-07 | R-04 transactions table = TRANSACT.unl (centiseconds masked) |
| FR-POSTTRAN-10 | Posting a record updates balance, account and transaction atomically (all or nothing) | BR-09, Q-04, D-011 | Failure-injection test: abend on transaction insert leaves balance and account unchanged |
| FR-POSTTRAN-11 | A new job instance starts with an empty transaction master and loads only this run's postings; a restart of a failed instance keeps what was committed | Q-01 (`:256`) | Two consecutive runs: transactions table = run-2 postings only; restart test |
| FR-POSTTRAN-12 | Job ends with exit code 4 when at least one record was rejected, else 0 | BR-10 | R-04 RC 4, R-07 syn_ok RC 0, R-08 RC 0 |
| FR-POSTTRAN-13 | Any unexpected I/O or database failure (including a duplicate transaction id) stops the job with a failure (exit 16; COBOL abend U999), after the SYSOUT error line | `:707-711`; R-06 | Duplicate-id case: exit 16, `ERROR WRITING TO TRANSACTION FILE` |
| FR-POSTTRAN-14 | SYSOUT shows start line, TCATBAL creation lines, `TRANSACTIONS PROCESSED :<9 digits>`, `TRANSACTIONS REJECTED  :<9 digits>`, end line | `:193,227-228,233,476` | SYSOUT equal to oracle |
| FR-POSTTRAN-15 | Empty DALYTRAN: no posting, empty DALYREJS, counts 0, exit 0 | `:202-219` | R-08 |
| FR-POSTTRAN-16 | DALYTRAN may be ASCII (ISO-8859-1) or EBCDIC CP037 fixed 350-byte records; both give the same result | D-005 | R-09 |
| FR-POSTTRAN-17 | DALYREJS is a new file per run; the job refuses to overwrite an existing one | `POSTTRAN.jcl:34-39`; D-016 | Launch test with existing file: exit 12, nothing posted |

## 3. Non-functional

| Id | Requirement | Source |
|---|---|---|
| NFR-01 | Commit interval 1; restartable from the last committed record | USER; D-012 |
| NFR-02 | Money as `BigDecimal` scale 2, never `double` | USER |
| NFR-03 | Parity is byte/row exact against the oracle for every golden case | USER parity rule |
| NFR-04 | Card numbers masked (last 4) in technical logs; SYSOUT business lines unchanged | D-007 |

## 4. Out of scope

Anything after `GOBACK` (WAITSTEP, OPENFIL), online programs that share the files (B-03..B-05, B-09), the Q-03 reason 109 path (logged only), Q-05 message text.

## 5. Traceability

BR-01→FR-02; BR-02→FR-02/03; BR-03→FR-04; BR-04→FR-05; BR-05→FR-07; BR-06→FR-08; BR-07→FR-09; BR-08→FR-06; BR-09→FR-10; BR-10→FR-12; Q-01→FR-11; Q-02→FR-05; Q-04→FR-10; Q-06→FR-09.
