# ADR-0019: Uniform online error body `{code, field, message}` and `NavigationContext`

- Status: Accepted (UNT51-17, 2026-10-07); refines ADR-0009 (status mapping unchanged) and ADR-0007 (navigation)
- Applies to: every online endpoint (`com.carddemo.web..`), `com.carddemo.common.web`

## Context
A 3270 screen reports an error with one message in `ERRMSG`, often with the cursor on the field that failed the edit.
ADR-0009 already renders CICS conditions as RFC 7807 problems; the UI needs one stable shape for all of them,
including Spring Security and Spring MVC failures.

## Decision
- Every error is `application/problem+json` with the RFC 7807 members (`status`, `title`, `detail`, `instance`)
  plus `code`, `field`, `message` (`ApiErrors.problem/decorate`, OpenAPI schema `ApiError`):
  - `code`: CICS condition (`NOTFND`, `INVREQ`, `DUPREC`, `CHANGED`, `ABEND`, `OTHER`, `NOTAUTH`) or a program reason
    (`WRONG_PASSWORD`, `SIGNON_REQUIRED`, `INVALID_KEY`). `cicsResp` is kept for ADR-0009 conditions.
  - `field`: the request field the edit failed on (the BMS field the COBOL put the cursor on), else `null`; always
    present.
  - `message`: the COBOL text exactly (right-trimmed, ADR-0003); `detail` repeats it.
- An HTTP method a resource does not support is the AID the program does not handle: 405, `INVALID_KEY`,
  `Invalid key pressed. Please see below...`.
- A non-error that the COBOL shows in `ERRMSG` (e.g. `This option is not installed ...`) is a 200 response with
  `message` and `messageColor`, not a problem.
- **`NavigationContext`** (`com.carddemo.web`) carries the COMMAREA navigation fields
  (`fromTranId`, `fromProgram`, `toTranId`, `toProgram`, `pgmContext`, `custId`, `acctId`, `cardNum`) returned by any
  endpoint that performs an `XCTL` and sent back by the UI to the target screen. It never carries user id or type.
