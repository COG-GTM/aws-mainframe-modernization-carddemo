# ADR-0009: CICS RESP conditions → HTTP status

- Status: Accepted (UNT51-5, 2026-10-07)
- Applies to: `modernization/carddemo-app`

## Decision
Domain services signal file-control outcomes with the exceptions in `com.carddemo.common`; the shared
`CicsResponseExceptionHandler` renders an RFC 7807 `ProblemDetail` whose `cicsResp` property keeps the condition name.

| CICS RESP (`DFHRESP(...)`) | Exception | HTTP |
| --- | --- | --- |
| `NORMAL` | (none) | 200 / 201 / 204 |
| `NOTFND` | `RecordNotFoundException` | 404 |
| `DUPREC`, `DUPKEY` | `DuplicateRecordException` (`Condition.DUPREC` / `DUPKEY`, reported as `cicsResp`) | 409 |
| `INVREQ`, and map edit errors | `InvalidRequestException` (or Bean Validation) | 400 |
| `ENDFILE` while browsing | not an error: empty/last page | 200 |
| `OTHER` / anything else | `AbendException` or unexpected exception | 500 |

- The `detail` text is the COBOL message from the program's `WS-MESSAGE` / `ERRMSG` (e.g. COACTVWC
  "Did not find this account in account card xref file") so the UI can show what the 3270 screen showed. The exact strings per
  program are in `docs/modernization/rules/<PGM>.md`.
- Field-level edit errors keep the COBOL message and add the offending field name.
- Concurrent update conflict is 409 with `cicsResp=CHANGED` (ADR-0010).
