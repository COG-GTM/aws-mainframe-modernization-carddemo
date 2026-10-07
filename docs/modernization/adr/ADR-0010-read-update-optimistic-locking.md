# ADR-0010: `READ UPDATE` / `REWRITE` → `@Transactional` + `@Version` optimistic locking

- Status: Accepted (UNT51-5, 2026-10-07)
- Applies to: `modernization/carddemo-app`

## Context
CICS online update programs (`COACTUPC`, `COCRDUPC`, `COUSR02C`, `COBIL00C`) `READ ... UPDATE` and `REWRITE` the
record in a later pseudo-conversational task than the one that displayed it. `COACTUPC` and `COCRDUPC` compare the
record with the image saved in the COMMAREA and show "Record changed by some one else. Please review" if it differs;
`COUSR02C` and `COBIL00C` rewrite without that check. `COACTUPC` rewrites ACCTDAT and CUSTDAT and
`SYNCPOINT ROLLBACK`s if the second rewrite fails.

## Decision
- Every updatable entity has a `@Version long version` column. The GET response returns it; the update request
  must send it back (replacing the COMMAREA old-image comparison).
- The service method that performs the update is `@Transactional`. After loading each entity and **before** changing
  it, it compares the client-supplied version with the loaded one (`Versions.requireCurrent(...)`). JPA's `@Version`
  check at flush only catches changes that race after the load; a change committed between the GET and the update is
  already in the loaded version, so without the explicit check a stale form would silently overwrite it.
  ```java
  @Transactional
  void update(AccountUpdate req) {
      Account acct = accounts.findById(req.acctId()).orElseThrow(() -> new RecordNotFoundException(...));
      Versions.requireCurrent(Account.class, req.acctId(), req.acctVersion(), acct.getVersion());
      Customer cust = ...;   // same check with req.custVersion()
      acct.apply(req); cust.apply(req);   // flush: @Version guards the race after the load
  }
  ```
  Both paths raise `ObjectOptimisticLockingFailureException` → 409 `cicsResp=CHANGED` with the COBOL message
  (`CicsResponseExceptionHandler`).
- Multi-record rewrites (account + customer) run in one transaction; any failure rolls back both, which is the
  `SYNCPOINT ROLLBACK` behaviour.
- No pessimistic locks (`SELECT ... FOR UPDATE`) in online code. Batch jobs that own a dataset for the step may use
  chunk transactions without `@Version` checks, but must still increment the version when they change a row.
- `DELETE` follows the same rule (version must match).
