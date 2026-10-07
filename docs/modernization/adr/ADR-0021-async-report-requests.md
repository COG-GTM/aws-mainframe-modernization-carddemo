# ADR-0021: Online report requests run the batch `tranrept` stream asynchronously

- Status: Accepted (UNT51-21, 2026-10-07); extends ADR-0015 (batch harness) and ADR-0019 (error body)
- Applies to: `com.carddemo.batch.report`, `com.carddemo.web.report`, Flyway `V5__report_request.sql`

## Context
CORPT00C (CR00) validates a Monthly / Yearly / Custom date range and writes an inline JCL stream (TRANREPT with the
range in `PARM-START-DATE` / `PARM-END-DATE`) to the `JOBS` extra-partition TDQ, i.e. the internal reader. The
terminal only learns "submitted for printing"; the report itself is a SYSOUT/GDG the user never sees on the screen.
The batch estate is already ported (`tranrept` stream: STEP05 repro, STEP10 sort/filter, STEP15 CBTRN03C, output a
dated `TRANREPT` generation catalogued in `batch_output_file`, executions in `batch_run`). The acceptance criterion is
that the online path and `--job=tranrept` produce identical report content for the same range.

## Decision
1. **No second report implementation.** `TransactionReportLauncher` runs the same `JobStream` that `--job=tranrept`
   resolves, with the same job parameters the CLI takes (`run-date`, `encoding`, `PARM-START-DATE`,
   `PARM-END-DATE`, every DD defaulted exactly as on the CLI) plus `report.request-id` for correlation.
2. **Internal reader = bounded in-process queue.** `ReportExecutor` is a single worker thread with a bounded queue
   (`carddemo.reports.async.queue-capacity`, default 20), registered only in a web application with
   `carddemo.reports.async.enabled=true` (default; `false` under `test`/`golden` so tests are deterministic, and
   report ITs opt back in). A full queue is the TDQ write failure: 503 `NOSPACE` `Unable to Write TDQ (JOBS)...`,
   and the request row is marked FAILED. One worker means report jobs never run concurrently with each other.
3. **`report_request` table (Flyway V5)** is the execution id the API returns: QUEUED → RUNNING → COMPLETED / FAILED,
   with the stream's job execution ids, the STEP15 execution, the JCL return code (max of the steps, ADR-0015) and
   the `batch_output_file` id of the exact `TRANREPT` generation STEP15 wrote. `GET .../{executionId}` reads it with
   the `batch_run` rows; `GET .../{executionId}/report` serves the catalogued bytes unchanged (EBCDIC by default,
   like the CLI) and the JSON status embeds the decoded lines.
4. **Visibility.** CR00 is on the main menu (COMEN01C), so any signed-on user may submit. An execution is visible to
   the user who requested it and to any ADMIN; for anyone else it is 404 `NOTFND` (not 403), so ids do not leak.
5. **Start ≤ end is enforced** (`Start Date can NOT be after End Date...`). CORPT00C does not check it (a reversed
   range yields an empty report); the step definition requires it. Recorded as a deviation in the CORPT00C rules doc.

## Consequences
- Report requests survive neither a JVM restart while QUEUED/RUNNING (the row stays in that state; no resubmission)
  nor a scale-out beyond one instance (each instance has its own queue). Both are acceptable for the current
  single-instance deployment; a durable queue is the follow-up if that changes.
- The online report is byte-identical to the CLI report for the same range and run date (`TransactionReportApiIT`).
- Generations are retained per the TRANREPT GDG limit; a status poll for a pruned generation answers 404 with
  `Report NOT available: generation no longer retained...`.
