# @carddemo/online-transactions

The CICS online transaction programs of `app/cbl` as typed handlers.

| Program | Map | Handler |
| :------ | :-- | :------ |
| `COTRN00C` | `COTRN0A` | `handleTransactionList` |
| `COTRN01C` | `COTRN1A` | `handleTransactionView` |
| `COTRN02C` | `COTRN2A` | `handleTransactionAdd` |
| `COBIL00C` | `COBIL0A` | `handleBillPayment` |
| `CORPT00C` | `CORPT0A` | `handleReportRequest` |

Every handler is a pure function of a request and the task's resources:

```ts
handler(
  { screen, aid, commarea },   // RECEIVE MAP + EIBAID + DFHCOMMAREA
  context,                     // the FILE(...) datasets, the clock, the report queue
): { screen, header, commarea, nextProgram, transfer, cursor, messageColor };
```

`nextProgram` with `transfer: true` is the `XCTL` the program would issue;
otherwise it is the `RETURN TRANSID` back to the same screen. `commarea`
mirrors `COCOM01Y`, including the `COTRN00C` paging state (first and last
transaction id, page number, next page flag, selection). Those types live in
`commarea.ts` for now and should move to `@carddemo/online-signon` once that
package exists.

`createApp(context)` exposes one POST route per handler and does nothing but
decode the request body, call the handler and serialise the response.

## Internal reader substitution

`CORPT00C` builds the `TRANREPT` JCL in `WS-JOB-LINES` and writes it to the
transient data queue `JOBS`, which is defined against the CICS internal
reader, so JES runs the batch report. There is no internal reader
off-platform, so the port keeps every decision the screen makes — report
type, start and end date, the confirmation — and hands them to a
`ReportRequestQueue` as a typed `ReportRequest` instead of emitting JCL.
Whatever backs that queue (a table, a message queue, a scheduler API) runs
the report. A queue that rejects a request drives the same
`Unable to Write TDQ (JOBS)...` screen the `WRITEQ TD` failure does.

## Data

`TRANSACT` has no sample file in `app/data/ASCII`; `dailytran.txt` carries the
same 350 byte layout, so the tests copy it (and `acctdata.txt`) to a temporary
directory and run the handlers against the real data without writing to the
repository.
