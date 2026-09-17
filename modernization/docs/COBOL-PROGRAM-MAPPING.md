# COBOL program to Java service mapping

Legend for the status column:

- **ported** - the business logic of the COBOL program is implemented and covered by tests.
- **scaffolded** - the endpoint/entity exists and works, but the legacy edit rules are not fully
  reproduced yet.
- **not built** - the contract is described in `API-CONTRACT.md` but no code exists yet.

## Online (CICS) transactions

| TransID | COBOL | BMS map | Java owner | Endpoint / operation | Status |
|---|---|---|---|---|---|
| `CC00` | `COSGN00C` | `COSGN00` | auth-service | `POST /api/v1/auth/signon` | ported |
| `CM00` | `COMEN01C` | `COMEN01` | SPA route | menu is a UI concern; the signon response returns `nextScreen` (`MAIN` or `ADMIN`) so the client picks the menu | ported |
| `CAVW` | `COACTVWC` | `COACTVW` | account-service | `GET /api/v1/accounts/{acctId}` (card to account goes through `GET /api/v1/cards/{cardNum}/xref`) | ported |
| `CAUP` | `COACTUPC` | `COACTUP` | account-service | `PUT /api/v1/accounts/{acctId}` | ported |
| `CCLI` | `COCRDLIC` | `COCRDLI` | card-service | `GET /api/v1/cards?accountId=&customerId=&page=&size=` | ported |
| `CCDL` | `COCRDSLC` | `COCRDSL` | card-service | `GET /api/v1/cards/{cardNum}` | ported |
| `CCUP` | `COCRDUPC` | `COCRDUP` | card-service | `PUT /api/v1/cards/{cardNum}` | ported |
| `CT00` | `COTRN00C` | `COTRN00` | transaction-service | `GET /api/v1/transactions?cardNumber=&page=&size=` | ported |
| `CT01` | `COTRN01C` | `COTRN01` | transaction-service | `GET /api/v1/transactions/{tranId}` | ported |
| `CT02` | `COTRN02C` | `COTRN02` | transaction-service | `POST /api/v1/transactions` | ported |
| `CR00` | `CORPT00C` | `CORPT00` | transaction-service | `POST /api/v1/reports/transactions` (submits the `CBTRN03C` equivalent) | not built |
| `CB00` | `COBIL00C` | `COBIL00` | transaction-service | `POST /api/v1/bill-payments` | ported |
| `CA00` | `COADM01C` | `COADM01` | auth-service | admin routing comes from `nextScreen` = `ADMIN`; the option list is a UI concern | ported |
| `CU00` | `COUSR00C` | `COUSR00` | auth-service | `GET /api/v1/users` | ported |
| `CU01` | `COUSR01C` | `COUSR01` | auth-service | `POST /api/v1/users` | ported |
| `CU02` | `COUSR02C` | `COUSR02` | auth-service | `PUT /api/v1/users/{userId}` | ported |
| `CU03` | `COUSR03C` | `COUSR03` | auth-service | `DELETE /api/v1/users/{userId}` | ported |

Customer maintenance has no dedicated CICS transaction in CardDemo (customer data is reached
through `COACTVWC`/`COACTUPC`); customer-service exposes it directly as
`GET|PUT /api/v1/customers/{custId}` and `GET /api/v1/customers?page=&size=`.

### Pseudo-conversational flow

`COSGN00C` -> `COMEN01C` -> program, with the `COMMAREA` (`COCOM01Y`) carrying the selected
account/card between screens. In the modern stack the COMMAREA disappears: the SPA holds the
selection in client state and every REST call is stateless, with identity carried by the
signon token instead of `CDEMO-USER-ID`/`CDEMO-USER-TYPE`.

## Batch programs

| JCL | COBOL | Java owner | Job | Status |
|---|---|---|---|---|
| `POSTTRAN.jcl` | `CBTRN02C` | transaction-service | Spring Batch `postTransactionsJob` | ported |
| `INTCALC.jcl` | `CBACT04C` | transaction-service | Spring Batch `interestCalculationJob` | ported |
| `TRANREPT.jcl` | `CBTRN03C` | transaction-service | `POST /api/v1/reports/transactions` | not built |
| `CREASTMT.JCL` | `CBSTM03A`/`CBSTM03B` | transaction-service | statement generation | not built |
| `READACCT/READCARD/READCUST/READXREF` | `CBACT01C`, `CBACT02C`, `CBCUS01C`, `CBACT03C` | - | pure file dumps; replaced by `GET` list endpoints | ported |

Both ported jobs are launched over HTTP (`POST /api/v1/batch/post-transactions`,
`POST /api/v1/batch/interest-calculation`) so an external scheduler replaces the JCL submit.

### `CBTRN02C` - transaction posting

Per input record of `DALYTRAN` the COBOL does: validate, then post or reject.

```
1500-A-LOOKUP-XREF   card num not in CARDXREF        -> reject 100 "INVALID CARD NUMBER FOUND"
1500-B-LOOKUP-ACCT   account not in ACCTDATA         -> reject 101 "ACCOUNT RECORD NOT FOUND"
                     curr_cyc_credit - curr_cyc_debit + amount > credit_limit
                                                     -> reject 102 "OVERLIMIT TRANSACTION"
                     account expiration < orig_ts(1:10)
                                                     -> reject 103 "TRANSACTION RECEIVED AFTER ACCT EXPIRATION"
2700-UPDATE-TCATBAL  upsert (acct,type,cat) balance += amount
2800-UPDATE-ACCOUNT  curr_bal += amount; amount >= 0 ? curr_cyc_credit += amount
                                                     : curr_cyc_debit  += amount
2900-WRITE-TRANFILE  copy the daily record into TRANSACT with proc_ts = now
RETURN-CODE 4 when at least one record was rejected
```

The Java port keeps the reason codes and the (faithfully reproduced, sign-quirky) cycle arithmetic
of `2800-UPDATE-ACCOUNT`: a negative amount is added to `curr_cyc_debit`, so the debit bucket goes
negative exactly as on the mainframe. The three updates that CICS/batch performed against three
VSAM files inside one unit of work become one database transaction plus one call to
`POST /api/v1/accounts/{id}/postings`.

### `CBACT04C` - interest calculation

```
read TCATBALF in account order (KSDS sequential)
on account break: curr_bal += total_interest; curr_cyc_credit = 0; curr_cyc_debit = 0
per category row:  rate = DISCGRP(acct_group_id, type, cat)  (fallback group id 'DEFAULT')
                   monthly_interest = (category_balance * rate) / 1200
                   total_interest += monthly_interest
                   write a TRANSACT record: type '01', cat '05', source 'System',
                       desc 'Int. for a/c <acct>', amount monthly_interest,
                       card num from the CARDXREF alternate index on account id,
                       tran id = <parm date><6 digit sequence>
1400-COMPUTE-FEES is empty in the COBOL and is intentionally left empty here
```

## Messaging (`app-vsam-mq`, `app-authorization-ims-db2-mq`)

The authorization modules exchange request/response messages over IBM MQ queues (`CDRD` request,
`CDRA` response). Target design, not implemented in this change:

| Mainframe | Modern |
|---|---|
| MQ request queue `CDRD` | Amazon SQS queue `carddemo-authorization-request` |
| MQ response queue `CDRA` | SQS queue `carddemo-authorization-response`, correlation id = MQ message id |
| MQ pub/sub fan out of posted transactions | SNS topic `carddemo-transaction-posted`, SQS subscribers per consumer |
| IMS DB `PCB` authorization segment reads | account-service / card-service REST calls |

Amazon MQ (managed IBM MQ) is the lift and shift option when the message format must stay
unchanged; SQS/SNS is the target once the payload is JSON. The seam in the code is the
`transaction-service` posting path: it publishes a domain event after the database transaction
commits (outbox pattern), which is where the SNS publish belongs.
