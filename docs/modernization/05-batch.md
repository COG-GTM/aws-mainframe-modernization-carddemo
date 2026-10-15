# 05 - Batch modernization

| JCL | Program | Spring Batch job | Inputs, then outputs | Trigger on AWS |
|---|---|---|---|---|
| POSTTRAN | CBTRN02C | `POSTTRAN` | daily_transaction (PENDING), then card_transaction, tran_cat_balance, account, daily_transaction_reject | S3 PUT of DALYTRAN, or nightly |
| INTCALC | CBACT04C | `INTCALC` (`date` = PARM-DATE) | tran_cat_balance, disclosure_group, account, then card_transaction, account | Nightly after POSTTRAN RC <= 4 |
| TRANREPT | CBTRN03C (+ the SORT step) | `TRANREPT` (`startDate`, `endDate`) | card_transaction, then report text | On demand (CR00 request) |
| CREASTMT | CBSTM03A/B | `CREASTMT` | card_xref, customer, account, card_transaction, then statements | Monthly |

**Return codes:** each job reports the JCL return code (`returnCode`, also stored as the step exit description
`RC=n`). POSTTRAN returns 4 when it writes rejects, the same as CBTRN02C. The Step Functions chain continues on RC <= 4,
like the `COND=(4,LT)` logic in the JCL.

**Files:** DALYTRAN is a fixed-width 350-byte file. It is accepted unchanged (EBCDIC or ASCII) through
`POST /api/v1/batch/daily-transactions` or an S3 event, and decoded by the same `RecordDecoder` the loader uses.
The DALYREJS layout (record + 80-byte reason) becomes rows in `daily_transaction_reject` with `reason_code` and
`reason_desc`. Report and statement text is returned by the API and stored on `report_request.output`. On AWS it is
written to S3.

**Scheduling:** locally, `BatchScheduler` processes queued CR00 report requests every 30 seconds. On AWS, set
`carddemo.batch.scheduler.enabled=false` and use EventBridge Scheduler to start a Step Functions state machine:
`POSTTRAN, then (Choice RC <= 4), then INTCALC, then (monthly) CREASTMT`. Each state calls the batch-service job API
(`POST /api/v1/batch/jobs/{name}`, through an API Gateway / HTTP task with an admin client-credentials token) and
branches on `returnCode`. batch-service runs as a long-lived ECS service (`spring.batch.job.enabled=false`), so no
one-shot task mode is needed.

**Rerun protection:** INTCALC refuses a second run for a date it already posted (RC=8, no balances touched),
because the interest transaction IDs are `PARM-DATE + sequence`, as in CBACT04C.

**Restartability:** the Spring Batch job repository tables are created automatically. POSTTRAN marks every daily
record POSTED or REJECTED in the same transaction as its postings, so a rerun only picks up records still PENDING.
That is safer than rerunning CBTRN02C over the same DALYTRAN file.
