# ADR-0014: Reproduce the baseline clock pin and JCL date parameters in Java

- Status: Accepted (UNT51-5, 2026-10-07)
- Applies to: `modernization/carddemo-app`

## Context
The GnuCOBOL baseline (UNT51-4, `scripts/baseline/`, outputs in `docs/validation/baseline/<JOB>/`) is deterministic
because it freezes time and pins the JCL parameters (`scripts/baseline/README.md`, "Fixed values"):

| Baseline input | Where | Value |
| --- | --- | --- |
| `COB_CURRENT_DATE` (feeds `FUNCTION CURRENT-DATE`, `ACCEPT ... FROM DATE/TIME`) | `baseline.py` `FROZEN_CLOCK` | `2022-07-06 00:00:00.00` |
| INTCALC `EXEC PGM=CBACT04C,PARM=` | `app/jcl/INTCALC.jcl` STEP15 | `2022071800` |
| TRANREPT date window (`PARM-START-DATE`/`PARM-END-DATE`, `DATEPARM`) | `app/jcl/TRANREPT.jcl` | `2022-01-01` .. `2022-07-06` |
| WAITSTEP `SYSIN` | `app/jcl/WAITSTEP.jcl` | `00003600` |

Java code that calls `LocalDate.now()` would produce different transaction timestamps, interest dates and report
headers than the baseline, and the golden set (decision `d-verify`) could never match.

## Decision
- All application code obtains time from the injected `java.time.Clock` bean (`common.time.ClockConfiguration`),
  never from no-arg `now()`. `carddemo.clock.fixed` (ISO local date-time) + `carddemo.clock.zone` (default UTC) pin it.
- The `golden` Spring profile (`application-golden.yml`) sets `carddemo.clock.fixed=2022-07-06T00:00:00` and the JCL
  parameters as `carddemo.baseline.*` (`BaselineRunProperties`): `intcalc-parm-date=2022071800`,
  `tranrept-start-date=2022-01-01`, `tranrept-end-date=2022-07-06`, `waitstep-centiseconds=3600`.
- Batch jobs take these as Spring Batch job parameters; the `golden` profile supplies the defaults, so a golden run is
  `--spring.profiles.active=golden` plus the job name. Production runs pass real parameters and an unpinned clock.
- WAITSTEP is not reproduced as a real sleep in golden runs (the baseline also runs with `--fast`).
- If a baseline value changes in `scripts/baseline/`, change `application-golden.yml` in the same PR;
  `GoldenProfileTest` asserts the values.
