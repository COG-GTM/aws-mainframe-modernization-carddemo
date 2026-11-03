# Residual risks and deliberate deviations

Ticket UNT149-23. What the Java does differently from the COBOL reference, what is not proven, and what a
reviewer has to accept or decide before `migration/carddemo-java` goes to `main`. Coverage per program is in
[coverage-matrix.md](coverage-matrix.md). Facts about COBOL behavior come from reading `app/`; anything that
was not observed in a run is marked **Hypothesis**.

**Decision needed** marks rows where the Java keeps a COBOL behavior on purpose and a human should confirm or
change it.

## 1. Parity defects kept on purpose

| # | Risk | Detail | Evidence | Action |
|---|---|---|---|---|
| P1 | CBACT04C never rewrites the last TCATBAL account | The account is rewritten on an account break; the end-of-file branch skips `1050-UPDATE-ACCOUNT`, so the last account's interest is written to SYSTRAN but its balance is not updated. Java (`InterestCalculationJobConfig`) does the same so the golden set matches. | GnuCOBOL run in the golden-set chain, `IntcalcCobolParityTest`, [notes/INTCALC.md](../../modernization/verification/notes/INTCALC.md) | **Decision needed**: the fix is one line (update the pending account at end of input); taking it makes ACCTFILE differ from the COBOL golden set by design. |
| P2 | INTCALC refuses a rerun with the same PARM | A second run with the same `PARM` date would generate the same TRAN-IDs (`PARM-DATE` + suffix); Java refuses it instead of creating duplicates. **Hypothesis:** on z/OS a rerun with the same date writes a new GDG generation and the duplicate keys surface downstream. | `IntcalcParmRolloverTest`; notes/INTCALC.md | Accept, or rerun with `--parm=<new date>`. |
| P3 | COCRDUPC `1230-EDIT-NAME` rejects apostrophes | Only letters and spaces pass, so the embossed name of sample customer `Lucious O'Connell` cannot be saved unchanged. Java keeps the rule. | `CardFlowTest.COCRDUPC_1230_EDIT_NAME_*` | **Decision needed**: keep for parity or allow `'`. |
| P4 | Sample customer 1 fails three COACTUPC edits as stored | An update that does not touch them is rejected until they are corrected, exactly as COACTUPC would. | `AccountUpdateMessagesFlowTest`, [account-view-update.md](account-view-update.md) §3 | Accept (data, not code). |
| P5 | COACTUPC defects reported, not reproduced | `ACCT-UPDATE-RECORD` writes the group id into the zip bytes; Java writes `group_id`. The phone all-blank test that never checks the line number **is** reproduced. | account-view-update.md §4 | Accept. |

## 2. Deliberate deviations (modernization, not parity)

| # | Deviation | Detail |
|---|---|---|
| D1 | TRAN-ID from a database sequence (d-tran-id) | COBOL took the highest key + 1 (STARTBR HIGH-VALUES, READPREV). Java uses `transaction.tran_id_seq` (`TransactionIdGenerator`), zero-padded to 16 digits and lifted past the highest stored numeric id, so concurrent replicas never collide. Ids are not gap-free and are not the COBOL's on a concurrent system. Batch ids (POSTTRAN keeps DALYTRAN's id, INTCALC builds `PARM-DATE`+suffix) follow the COBOL. |
| D2 | BCrypt instead of `SEC-USR-PWD` | Plaintext 8-char passwords are not carried over; the loader hashes them (sample value `PASSWORD`). COSGN00C upper-cases user id and password before comparing; Java upper-cases the user id and accepts the password as typed or upper-cased. |
| D3 | Stateless 8-hour tokens, no revocation | `POST /api/v1/auth/signon` returns a signed token (`CARDDEMO_TOKEN_TTL`, default PT8H). Deleting a user or changing their type does not end tokens already issued; there is no refresh or logout. |
| D4 | Role-only authorization from CSUSR01Y | Only `SEC-USR-TYPE` A/U is checked, as in the COBOL; there are no per-account ownership checks (the COBOL had none either). COMEN02Y has an admin-only option branch that no option uses, so it is unreachable in COBOL and Java. |
| D5 | API breaking changes vs the pre-migration branch | `PIC 9(n)` ids are zero-padded strings (not JSON numbers); transaction add requires the dates and merchant fields COTRN02C requires; update DTOs carry `version` (optimistic lock replaces `READ ... UPDATE`, 409 on conflict). See [api-style.md](api-style.md), [conventions.md](conventions.md) §12. |
| D6 | Range checks instead of silent truncation | Where COBOL `MOVE`/`COMPUTE` would drop high-order digits (e.g. `NUMVAL-C` into `S9(10)V99`), Java rejects the input with the field's "is not valid" message. Extra decimals are still truncated. |
| D7 | 3270/BMS behavior not reproduced | Attribute bytes (protect/unprotect, colors, cursor `-1` lengths), PF3/PF12 navigation, PF4 clear and screen-state COMMAREA fields are client concerns; the REST API returns data and `ApiError` messages. CICS RESP/RESP2 suffixes are dropped from messages. |
| D8 | Extension modules return "not available" | Menu option 11 and admin options 5–6 point to the IMS/DB2/MQ extensions, which are out of scope. |

## 3. Consistency across services

| # | Risk | Detail | Detection |
|---|---|---|---|
| C1 | Bill payment is two database transactions | COBIL00C holds ACCTDAT with `READ UPDATE` and CICS backs out the TRANSACT write if the REWRITE fails. Java takes a per-account advisory lock, writes and flushes the TRANSACT row, posts the debit to account-service (`BILL_PAYMENT`, TRAN-ID as idempotency key, up to 3 retries) and rolls the row back if the posting is rejected or never answered. Remaining window: account-service applied the posting but every answer was lost, or the local commit fails after a successful posting; then the account is debited with no TRANSACT row. No automatic repair. | An `account.account_postings` row whose `tran_id` is missing from `transaction.transactions` (architecture.md §6). |
| C2 | POSTTRAN posting crosses services the same way | Each daily transaction is posted to account-service (`DAILY_TRANSACTION`) and written locally; if the local writes fail after the posting, the account is updated without a TRANSACT row until POSTTRAN is rerun, which reposts idempotently by `tran_id` (`PostingService`). | Same query on `account_postings`; README "Cross service batch retries". |
| C3 | COACTUPC account + customer rewrite | Two services, two commits: a failure after the customer PUT succeeded leaves the customer updated without the account. | account-view-update.md §4 |

## 4. Security and operations

| # | Risk | Detail |
|---|---|---|
| S1 | Unauthenticated batch launch | `java -jar ... --job=` needs no credentials (it signs downstream calls as `BATCH`/`U`); whoever can run the jar on the host can post, calculate interest or import. Accepted for this checkout; production needs the scheduler's own access control. The HTTP launch endpoints (`/api/v1/batch/**`) need a bearer token like every `/api/**` call, but no admin role: any signed-on user can launch a job. |
| S2 | Dev-only default `CARDDEMO_TOKEN_SECRET` | `docker-compose.yml` falls back to `local-dev-only-carddemo-token-secret-not-for-prod`. Any deployment must set its own secret, or anyone can mint tokens. |
| S3 | Demo-only defaults | Sample users `ADMIN001` … `USER0005` with password `PASSWORD`; database credentials `carddemo`/`carddemo`; no TLS between services; Compose publishes the service ports on every host interface. Local demo only (architecture.md §6). |
| S4 | INTCALC with a new PARM charges again | Running interest again with a new date charges unchanged category balances again; CBACT04C opens TCATBAL input-only and behaves the same. Once-per-period is a scheduling decision. |
| S5 | Test configuration leaks | transaction-service's `@ComponentScan(com.carddemo.transaction, com.carddemo.common)` also picks up any annotated test configuration on the test classpath (e.g. `StubGateways`), so a test can silently run against stubs it did not ask for. batch-service's test `application.yml` replaces (not overlays) the main one, so main settings such as `spring.flyway.default-schema: batch` are not exercised by its tests. |

## 5. Verification limits

| # | Limit | Detail |
|---|---|---|
| V1 | CICS not runnable here | Online expectations (messages, paging, edits) are read from the COBOL source and checked through the REST API by the `@CobolFlow` tests; no CICS region produced them. |
| V2 | DateValidation not checked against LE | `CSUTLDTC` calls the LE service CEEDAYS; `DateValidation` reproduces the documented feedback codes/messages. No LE runtime was available, so the mapping of edge dates to codes is a **Hypothesis**. |
| V3 | Golden-set accepted differences | (a) `TRAN-PROC-TS` is wall-clock in Java, frozen in the COBOL reference run; (b) FILLER is written as spaces where the source held zeros/low-values; (c) `DALYTRAN-PROC-TS` is blank in the input and stamped by POSTTRAN. The harness accepts exactly these and fails on anything else ([verification/README.md](../../modernization/verification/README.md)). CREASTMT/TRANREPT HTML escaping and the interest-row provenance are documented in the per-job notes. |
| V4 | Source-data discrepancies | ACCTDATA record 49 ZIP and DISCGRP record 34 rate differ between the ASCII and EBCDIC sample files (ASCII loaded); `ACCT-GROUP-ID` is blank in the sample accounts, so DISCGRP falls back to `DEFAULT`; `DALYTRAN.PS.INIT` is not loaded (the golden set feeds `dailytran.txt` to POSTTRAN). [validation/sample-data-reconciliation.md](validation/sample-data-reconciliation.md) |
| V5 | Not yet built | PRTCATBL and TRANBKP have no Java job; COMBTRAN takes TRANSACT.BKUP(0) from `--dd` or INTCALC's generation. |
