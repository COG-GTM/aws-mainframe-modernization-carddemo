# Contract: Messaging (IBM MQ / CICS TD → Amazon SQS)

Status: **v1 (Discovery session)**. Owners: online-services (producers/consumers), infra (queues, DLQs,
IAM), frontend (none — only REST). Source programs: `app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl`,
`app/app-vsam-mq/cbl/COACCT01.cbl`, `app/app-vsam-mq/cbl/CODATE01.cbl`, CICS TD queue `JOBS` used by
`app/cbl/CORPT00C.cbl`.

## 1. General rules

| Rule | Value |
|---|---|
| Queue type | SQS **standard** queues (legacy processing has no cross-message ordering dependency: each request is handled independently, `MQGMO-NO-SYNCPOINT`) |
| Name | `${SQS_QUEUE_PREFIX}<logical-name>`; default prefix `carddemo-`; infra may add env (`carddemo-dev-…`) via the prefix |
| DLQ | Every consumer queue has `<name>-dlq`, redrive `maxReceiveCount = 5`, DLQ retention 14 days |
| Body | UTF-8 JSON, schemas below; `schemaVersion` field = `"1"` |
| Correlation | Request carries `messageId` (UUID). Reply carries `correlationId` = request `messageId` (replaces `MQMD-MSGID` → `MQMD-CORRELID` copy done in all three MQ programs). Same value also as SQS message attribute `correlationId` (String) for filtering |
| Reply routing | Request attribute `replyTo` (queue name) replaces `MQMD-REPLYTOQ`. If absent, consumer uses the default reply queue in §2. `replyTo` MUST be in the consumer's configured allowlist (default: only the flow's own reply queue from §2, prefixed with `SQS_QUEUE_PREFIX`); any other value → no reply sent, message logged to `carddemo-error` and deleted. Consumer IAM `sqs:SendMessage` is limited to those queues |
| Expiry | Legacy authorization reply: `MQMD-EXPIRY = 50` (tenths of a second = 5 s), non-persistent. Target: reply attribute `expiresAt` (ISO-8601 = send time + 5 s); requesters ignore late replies. Reply queues `MessageRetentionPeriod = 60` s (SQS minimum) |
| Visibility timeout | 30 s (consumer processing is a handful of DB reads/writes) |
| Long polling | `WaitTimeSeconds = 5` (replaces `MQGMO-WAITINTERVAL = 5000` ms in `COPAUA0C`) |
| Batch limit | Consumer loop processes at most **500** messages per invocation before yielding (`WS-REQSTS-PROCESS-LIMIT = 500` in `COPAUA0C`); in a long-running Spring listener this is advisory |
| Money in JSON | Decimal **string** with 2 decimals, e.g. `"125.40"` (avoid float) |
| Errors | Unprocessable / infrastructure errors → structured error message to `carddemo-error` (§5) and the original message is deleted; unexpected exceptions → not deleted → redrive to DLQ |
| Trigger | MQ trigger monitor / CICS `RETRIEVE` of `MQTM` (start of `COPAUA0C`, `COACCT01`, `CODATE01`) → SQS listener (Spring Cloud AWS `@SqsListener`) in the online-services deployable; no CICS transaction ids needed |

## 2. Queue catalogue

| Logical SQS queue | DLQ | Replaces (legacy) | Producer | Consumer | Message |
|---|---|---|---|---|---|
| `carddemo-pauth-request` | `carddemo-pauth-request-dlq` | `AWS.M2.CARDDEMO.PAUTH.REQUEST` (README of authorization sub-app; name delivered to `COPAUA0C` in the trigger message) | external authorization simulator / tests | authorization service (`COPAUA0C` replacement) | §4.1 |
| `carddemo-pauth-reply` | `carddemo-pauth-reply-dlq` | `AWS.M2.CARDDEMO.PAUTH.REPLY` (legacy reply goes to request `MQMD-REPLYTOQ`) | authorization service | requester | §4.2 |
| `carddemo-acct-inquiry-request` | `carddemo-acct-inquiry-request-dlq` | triggered input queue of `COACCT01` (name from `MQTM-QNAME`; transaction `CDRA`) | external client | inquiry service | §3.1 |
| `carddemo-acct-inquiry-reply` | `carddemo-acct-inquiry-reply-dlq` | `CARD.DEMO.REPLY.ACCT` (hard-coded in `COACCT01`) | inquiry service | client | §3.2 |
| `carddemo-date-inquiry-request` | `carddemo-date-inquiry-request-dlq` | triggered input queue of `CODATE01` (transaction `CDRD`) | external client | inquiry service | §3.3 |
| `carddemo-date-inquiry-reply` | `carddemo-date-inquiry-reply-dlq` | `CARD.DEMO.REPLY.DATE` (hard-coded in `CODATE01`) | inquiry service | client | §3.4 |
| `carddemo-error` | `carddemo-error-dlq` | `CARD.DEMO.ERROR` (hard-coded in `COACCT01`/`CODATE01`); CICS TD `CSSL` error log writes of `COPAUA0C` | all consumers | ops (CloudWatch alarm on depth) | §5 |
| `carddemo-report-request` | `carddemo-report-request-dlq` | CICS extrapartition TD queue `JOBS` (80-byte JCL card images written by `CORPT00C`, submitted to JES internal reader) | online-services (`POST /api/v1/reports/transactions`) | report dispatcher → starts Step Functions `carddemo-report` (`batch.md` §3) | §6 |

## 3. VSAM/MQ inquiry sub-app (`app/app-vsam-mq/`)

Legacy request (both programs, `REQUEST-MSG-COPY`, 1000 bytes):
`WS-FUNC X(04)` + `WS-KEY 9(11)` + `WS-FILLER X(985)`.

### 3.1 Account inquiry request — `carddemo-acct-inquiry-request`

```json
{
  "schemaVersion": "1",
  "messageId": "7b0e6a2c-1f0e-4a57-9d5e-2f7c0c7b1a11",
  "function": "INQA",
  "acctId": 11,
  "sentAt": "2026-09-28T17:00:00Z"
}
```

| JSON | Legacy | Rule |
|---|---|---|
| `function` | `WS-FUNC X(04)` | must be `"INQA"` |
| `acctId` | `WS-KEY 9(11)` | integer 1..99999999999 (`WS-KEY > ZEROES`) |

### 3.2 Account inquiry reply — `carddemo-acct-inquiry-reply`

Legacy reply is the fixed-format `WS-ACCT-RESPONSE` text (labels + `ACCOUNT-RECORD` fields read via CICS
`READ FILE('ACCTDAT')`). JSON:

```json
{
  "schemaVersion": "1",
  "messageId": "…", "correlationId": "7b0e6a2c-…",
  "status": "OK",
  "account": {
    "acctId": 11, "activeStatus": "Y",
    "currBal": "1940.00", "creditLimit": "2020.00", "cashCreditLimit": "1020.00",
    "openDate": "2014-11-20", "expirationDate": "2025-05-20", "reissueDate": "2025-05-20",
    "currCycCredit": "0.00", "currCycDebit": "0.00", "groupId": "A000000000"
  }
}
```

`status` values: `OK`; `NOT_FOUND` (`DFHRESP(NOTFND)` on the account read → legacy text
"INVALID REQUEST PARAMETERS ACCT ID : <id>"), `INVALID_REQUEST` (function ≠ `INQA` or key ≤ 0 → same
legacy text); `account` omitted when not `OK`.

### 3.3 Date inquiry request — `carddemo-date-inquiry-request`

Same envelope as §3.1; `function` is carried but not validated by `CODATE01` (any request returns the date);
`acctId` optional/ignored.

### 3.4 Date inquiry reply — `carddemo-date-inquiry-reply`

Legacy: `'SYSTEM DATE : ' MM-DD-YYYY 'SYSTEM TIME : ' HH:MM:SS` from `ASKTIME`/`FORMATTIME`.

```json
{
  "schemaVersion": "1", "messageId": "…", "correlationId": "…",
  "status": "OK",
  "systemDate": "09-28-2026",
  "systemTime": "17:00:00",
  "text": "SYSTEM DATE : 09-28-2026SYSTEM TIME : 17:00:00"
}
```

`systemDate` keeps the legacy `MM-DD-YYYY` format; `text` reproduces the legacy string byte-for-byte
(no separator between the two parts, as in the `STRING` statement).

## 4. Authorization sub-app (`app/app-authorization-ims-db2-mq/`)

### 4.1 Authorization request — `carddemo-pauth-request`

Legacy: comma-delimited text parsed with `UNSTRING … DELIMITED BY ','` into `CCPAURQY` fields (buffer
`W01-GET-BUFFER X(500)`). JSON field order below equals the legacy field order.

| # | JSON property | Legacy field | Type / format |
|---|---|---|---|
| 1 | `authDate` | `PA-RQ-AUTH-DATE X(06)` | string `YYMMDD` |
| 2 | `authTime` | `PA-RQ-AUTH-TIME X(06)` | string `HHMMSS` |
| 3 | `cardNum` | `PA-RQ-CARD-NUM X(16)` | string, 16 digits |
| 4 | `authType` | `PA-RQ-AUTH-TYPE X(04)` | string ≤ 4 |
| 5 | `cardExpiryDate` | `PA-RQ-CARD-EXPIRY-DATE X(04)` | string `MMYY` |
| 6 | `messageType` | `PA-RQ-MESSAGE-TYPE X(06)` | string ≤ 6 |
| 7 | `messageSource` | `PA-RQ-MESSAGE-SOURCE X(06)` | string ≤ 6 |
| 8 | `processingCode` | `PA-RQ-PROCESSING-CODE 9(06)` | integer |
| 9 | `transactionAmt` | `PA-RQ-TRANSACTION-AMT +9(10).99` (parsed from `WS-TRANSACTION-AMT-AN`) | decimal string |
| 10 | `merchantCategoryCode` | `PA-RQ-MERCHANT-CATAGORY-CODE X(04)` | string |
| 11 | `acqrCountryCode` | `PA-RQ-ACQR-COUNTRY-CODE X(03)` | string |
| 12 | `posEntryMode` | `PA-RQ-POS-ENTRY-MODE 9(02)` | integer |
| 13 | `merchantId` | `PA-RQ-MERCHANT-ID X(15)` | string |
| 14 | `merchantName` | `PA-RQ-MERCHANT-NAME X(22)` | string |
| 15 | `merchantCity` | `PA-RQ-MERCHANT-CITY X(13)` | string |
| 16 | `merchantState` | `PA-RQ-MERCHANT-STATE X(02)` | string |
| 17 | `merchantZip` | `PA-RQ-MERCHANT-ZIP X(09)` | string |
| 18 | `transactionId` | `PA-RQ-TRANSACTION-ID X(15)` | string |

Envelope: `schemaVersion`, `messageId`, `sentAt` plus the 18 properties; SQS attribute `replyTo` optional.

### 4.2 Authorization reply — `carddemo-pauth-reply` (or `replyTo`)

Legacy: `STRING PA-RL-CARD-NUM ',' PA-RL-TRANSACTION-ID ',' PA-RL-AUTH-ID-CODE ',' PA-RL-AUTH-RESP-CODE ','
PA-RL-AUTH-RESP-REASON ',' WS-APPROVED-AMT-DIS ','` (`CCPAURLY`), `MQFMT-STRING`, non-persistent,
expiry 50, correlation id = request message id.

```json
{
  "schemaVersion": "1", "messageId": "…", "correlationId": "<request messageId>",
  "expiresAt": "2026-09-28T17:00:05Z",
  "cardNum": "4111111111111111",
  "transactionId": "000000000000123",
  "authIdCode": "170000",
  "authRespCode": "00",
  "authRespReason": "0000",
  "approvedAmt": "125.40"
}
```

### 4.3 Response and reason codes (from `COPAUA0C` paragraph building the reply)

| `authRespCode` | Meaning |
|---|---|
| `00` | approved (`AUTH-RESP-APPROVED`, approved amount = requested amount) |
| `05` | declined (`AUTH-RESP-DECLINED`, approved amount = 0) |

| `authRespReason` | Condition |
|---|---|
| `0000` | approved / default |
| `3100` | card not found in xref, account not found, or customer not found |
| `4100` | insufficient funds (amount > available credit) |
| `4200` | card not active |
| `4300` | account closed |
| `5100` | card fraud |
| `5200` | merchant fraud |
| `9000` | other decline |

Side effects preserved by the consumer (same unit of work): read `card_xref`, `account`, `customer`;
insert/update `pending_auth_summary` (IMS `GU`/`REPL`/`ISRT` of `PAUTSUM0`) and insert `pending_auth_detail`
(`ISRT` of `PAUTDTL1`). Idempotency (SQS is at-least-once): in the same DB transaction as those side effects
the consumer inserts `processed_message(message_id, queue, reply_payload, processed_at)` (`data-model.md` §3.4),
where `reply_payload` holds only the six §4.2 result fields (no envelope). If `message_id` already exists it applies
**no** side effects and sends a new reply built from the stored `reply_payload` with a fresh envelope: new
`messageId`, `sentAt` = now, `expiresAt` = now + 5 s, `correlationId` = request `messageId` (unchanged). The IMS part
is a **replatform candidate** (inventory §9); a replatformed consumer must apply the same `messageId` check. If the online-services
session does not refactor it, the queue contract above still stands and is served by the replatformed
program via an MQ↔SQS bridge.

## 5. Error message — `carddemo-error`

Replaces legacy `ERROR-MSG` text writes to `CARD.DEMO.ERROR` and `CCPAUERY` records written to TD `CSSL`.

```json
{
  "schemaVersion": "1", "messageId": "…", "correlationId": "<offending message id or null>",
  "errDate": "260928", "errTime": "170000",
  "application": "CARDDEMO", "program": "COPAUA0C",
  "location": "1100", "level": "C", "subsystem": "M",
  "code1": "2033", "code2": "", "message": "MQGET FAILED",
  "eventKey": "4111111111111111",
  "sourceQueue": "carddemo-pauth-request"
}
```

Field mapping: `ERR-DATE X(06)`, `ERR-TIME X(06)`, `ERR-APPLICATION X(08)`, `ERR-PROGRAM X(08)`,
`ERR-LOCATION X(04)`, `ERR-LEVEL X(01)` (`L` log, `I` info, `W` warning, `C` critical), `ERR-SUBSYSTEM X(01)`
(`A` app, `C` CICS, `I` IMS, `D` DB2, `M` MQ, `F` file), `ERR-CODE-1/2 X(09)`, `ERR-MESSAGE X(50)`,
`ERR-EVENT-KEY X(20)`. `program` holds the legacy program name for traceability.

## 6. Report request — `carddemo-report-request`

Replaces the JCL deck `CORPT00C` writes to TD queue `JOBS` (`//TRNRPT00 JOB …`, `EXEC PROC=TRANREPT`,
`SYMNAMES` and `DATEPARM` in-stream data with the start/end dates).

```json
{
  "schemaVersion": "1", "messageId": "…",
  "reportType": "MONTHLY",
  "startDate": "2026-09-01",
  "endDate": "2026-09-30",
  "requestedBy": "USER0001",
  "requestedAt": "2026-09-28T17:00:00Z"
}
```

`reportType` ∈ `MONTHLY` (current month), `YEARLY` (current year), `CUSTOM` (user dates) — the three options
on map `CORPT0A`. The dispatcher starts the `carddemo-report` state machine with
`{"startDate","endDate","runId": messageId, "businessDate": endDate}` and execution name = `messageId`
(`batch.md` job `transaction-report`). `messageId` is the `requestId` returned by `POST /api/v1/reports/transactions`;
report status is read from that execution (`api.md`).
