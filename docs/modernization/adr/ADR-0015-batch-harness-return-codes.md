# ADR-0015: Batch CLI, DD parameters and JCL condition codes

- Status: Accepted (UNT51-11, 2026-10-07)
- Applies to: `modernization/carddemo-app`, package `com.carddemo.batch` (harness in `com.carddemo.batch.harness`)

## Context
JCL jobs are launched by the scheduler, receive their datasets through `DD` statements and symbolics, and report a
condition code (RC) that later steps test with `COND=`. Spring Batch has none of these: a Boot app launched with
`--spring.batch.job.name` exits 0 even when the job FAILS, `ExitStatus` is a string, and job parameters are typed
values without any notion of a dataset. Golden-set validation (decision `d-verify`) needs the Java jobs to run from
the command line with the same inputs/outputs as the GnuCOBOL baseline, and job chaining needs the RCs.

## Decision
- **CLI.** `java -jar carddemo-app.jar --job=<name> [--run-date=YYYY-MM-DD] [--<param>=<value> ...]`
  (`--spring.batch.job.name` is an alias) runs one job without a web server and exits with its RC
  (`CardDemoApplication.runBatch`); 16 if the context cannot start. Job names match case-insensitively (`READACCT` =
  `readacct`). `run-date` becomes a `LocalDate` parameter (default: today on the injected clock, ADR-0014); every
  other non-Spring option becomes a string parameter; an identifying `run.id` (epoch millis unless `--run.id=` is
  given, for restarts) makes each invocation a new job instance. `CommandLineJobParameters` adapts the raw
  parameters for a job (e.g. `initial-load` adds `source-dir`/`mode`/`source-sha256`).
  Malformed requests (bare `--job`, bad `--run-date`/`--run.id`, credential-like names such as `--*password*`,
  `--*token*`) end RC 16 with an `ABANDONED` `batch_run` row; credentials come from the environment, never from job
  parameters (they would land in the job repository, `batch_run` and the log). `spring.batch.job.enabled=true`
  (Boot's own runner) cannot be combined with the CLI → RC 16.
- **DD statements → parameters named after the DD.** `--ACCTFILE=<path>` reads/writes that file;
  `--ACCTFILE=table` (the default for KSDS inputs) reads the PostgreSQL table (ADR-0011) in key order through a
  keyset browse. Output DDs default to `<carddemo.batch.output-dir>/<DSN>`; `--SYSOUT=<path>` receives the DISPLAY
  lines (default `<output-dir>/SYSOUT/<job>.<run-date>.<execution id>.txt`). `--encoding=EBCDIC|ASCII` (default EBCDIC) is the
  code page of file datasets; `--record-prefix=ZOS_RDW|GNUCOBOL_VARSEQ_0|GNUCOBOL_VARSEQ|NONE` (default ZOS_RDW)
  frames RECFM=V output. Fixed-width records go through the `common` codec, so outputs compare byte for byte.
- **Return codes.** `ReturnCode` = 0 OK, 4 WARNING, 8 ERROR, 12 SEVERE, 16 TERMINAL. A step raises its RC with
  `ReturnCode.set(stepExecution, rc)` (`MOVE n TO RETURN-CODE`) and completes; a failed step maps its exception:
  `AbendException` (ADR-0013) → 16 and counts as an abend, `ReturnCodeException` → its RC, `RecordFormatException` →
  8, anything else → 12. Job RC = highest step RC (at least 8 when the job FAILED, 16 when STOPPED/ABANDONED); a
  launch that never ran (unknown job, bad parameters) is 16. The RC is appended to the Spring Batch exit
  description as `RC=nnnn`.
- **`batch_run` (Flyway V4).** One row per job execution (`step_name` null) plus one per step: status, exit code,
  RC, read/write/skip/filter counts, times, parameters, failure message; written by a job listener on every job,
  plus an `ABANDONED`/16 row for launches that failed before a job execution existed.
- **Chaining.** `JobChain` runs jobs as JCL steps with `JclCond` (`COND=(code,op[,step])`, lists, `EVEN`, `ONLY`):
  a step is bypassed when any test is true, or after an abend unless `EVEN`/`ONLY`. The chain RC is the highest RC.

## Consequences
- The in-app scheduler (decision `d-scheduler`) and the REST trigger reuse `BatchJobLauncher`/`JobChain`, so a
  scheduled flow and a CLI run produce the same `batch_run` rows and RCs.
- Report/print jobs port their `DISPLAY` statements to `Sysout.display` and are validated against
  `docs/validation/baseline/<JOB>/` by `scripts/batch/compare_print_jobs.py` (CI job `batch-equivalence`).
