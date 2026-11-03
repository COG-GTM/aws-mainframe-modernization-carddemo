# Online screen-flow tests (UNT149-22)

Integration tests that walk each CICS transaction's screen flow through the REST API. They run under
`mvn -B verify` in the service modules, on Testcontainers PostgreSQL 16 with the five service schemas
migrated by their own Flyway scripts and the sample data loaded by `data-loader` (`SampleDataLoader`
on `app/data`).

The online COBOL cannot run on this VM (no CICS), so the expected messages and behavior come from
reading the COBOL source and BMS maps (scope §4.2). This is weaker evidence than the batch golden set.

## Naming and coverage

Every flow test carries `@CobolFlow(program, paragraph, key)` and its method name starts with
`<PROGRAM>_<PARAGRAPH>_`, e.g. `COSGN00C_READ_USER_SEC_FILE_wrongPasswordIsRefused`. The coverage matrix
(s6.3) can be generated from the method names or the annotation.

`OnlineScreens` (common test sources) lists the 17 online programs with the screen keys that reach the
server: ENTER, PF5 (save / confirm), PF7/PF8 (page). PF3, PF4 and PF12 are client navigation or clear,
so they have no server call. Each service has a `*FlowCoverageTest` that fails when a listed key has no
flow test, when a test name lacks its program/paragraph prefix, or when a program is tested in the
wrong module.

Messages are enumerated from the COBOL: `CobolMessages` reads the `MOVE '...' TO WS-MESSAGE` lines of a
program, and the coverage tests fail when a literal is neither asserted by a flow test nor listed as
not produced by the API with a reason (Y/N confirmation steps, page-edge texts, CICS I/O errors).
This check is textual (a quoted literal in the test source); the assertions that run are what prove the
message, so a literal left in a comment or an unused case would pass the check.
COACTUPC builds its texts with `STRING`, so its test checks every message of the 25
`AccountUpdateValidator.RULES` and that each suffix is a COACTUPC/CSUTLDPY literal. Card update messages
are checked against `CardMessages`; CORPT00C's two-line MOVEs are listed in its test.

| Service | Programs | Test classes |
|---|---|---|
| auth-service | COSGN00C, COMEN01C, COADM01C, COUSR00C–03C | `auth/flow/SignonAndMenuFlowTest`, `UserAdministrationFlowTest` |
| account-service | COACTVWC, COACTUPC | `account/flow/AccountFlowTest`, `AccountUpdateMessagesFlowTest` |
| card-service | COCRDLIC, COCRDSLC, COCRDUPC | `card/flow/CardFlowTest` |
| transaction-service | COTRN00C–02C, COBIL00C | `transaction/flow/TransactionFlowTest` |
| batch-service | CORPT00C | `batch/flow/ReportSubmissionFlowTest` |

## Cross-service calls: stubbed at the HTTP boundary

Account view/update, transaction add, bill payment and report submission call other services. The
tests do not start those services. A `ServiceStandIn` (JDK `HttpServer`) answers the remote endpoints
from the other schemas of the same flow database, so the real REST clients, JSON mapping and token
forwarding run. The stand-in answers 401 to a call without a bearer token, like the real services:

- account-service: card-service `GET /api/v1/card-xrefs/by-account/{id}`, customer-service
  `GET`/`PUT /api/v1/customers/{id}` (with the version check).
- transaction-service: account-service `GET /api/v1/accounts/{id}` and the `BILL_PAYMENT` posting
  (balance only), card-service xref lookups.
- batch-service: card-service xref, transaction-service types, categories and transactions.

## Data notes

- TRANSACT is empty after a load. transaction-service tests copy DALYTRAN into TRANSACT as the start
  state; batch-service serves DALYTRAN as TRANSACT and stamps TRAN-PROC-TS (blank in the sample file)
  with one POSTTRAN run date.
- Sample customer 1 fails three COACTUPC edits as stored (NC with ZIP 12546, area code 373, FICO 274).
  The update tests correct those three fields first. Some sample ZIPs are longer than the 5-character
  CAUP ZIP field (e.g. `19852-6716`), so those accounts cannot be resubmitted unchanged.
- Card `7026637615032277` (Lucious O'Connell) cannot change status without a rename: the COBOL name
  edit rejects the apostrophe. The test asserts this as parity.
