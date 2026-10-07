# ADR-0013: `CALL 'CEE3ABD'` → unchecked `AbendException` with the original code

- Status: Accepted (UNT51-5, 2026-10-07)
- Applies to: `modernization/carddemo-app`

## Context
Ten core programs abend through Language Environment: `MOVE 999 TO ABCODE`, `MOVE 0 TO TIMING`,
`CALL 'CEE3ABD' USING ABCODE, TIMING`, usually after `DISPLAY` of the file status (e.g. `9999-ABEND-PROGRAM` in
CBACT04C).

## Decision
- Throw `com.carddemo.common.AbendException` (unchecked) with the code the COBOL moved to `ABCODE`
  (`AbendException.carddemo(message, cause)` for 999). The message keeps the COBOL `DISPLAY` text; the I/O status
  goes in the message or cause.
- `abendLabel()` renders the code as `U0999`, as the job log would.
- Batch: let it propagate out of the step. The job execution ends `FAILED` with exit description `USER ABEND U0999`;
  the scheduler stops the flow like a `COND` check would. Do not catch it to continue.
- Online: `CicsResponseExceptionHandler` returns 500 `cicsResp=ABEND` with `abendCode`, and logs the stack.
- Never convert an abend into `System.exit` or a checked exception; never swallow it.
