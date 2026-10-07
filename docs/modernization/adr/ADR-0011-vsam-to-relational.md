# ADR-0011: VSAM KSDS → table with primary key; AIX → index

- Status: Accepted (UNT51-5, 2026-10-07)
- Applies to: `modernization/carddemo-app`

## Decision
- Each KSDS becomes one table (Flyway migration, snake_case names); its `KEYS(len offset)` field is the primary key
  with the same type/width (ADR-0003/0004). From the IDCAMS JCL:

| Dataset | KSDS key | Table PK |
| --- | --- | --- |
| ACCTDAT | `KEYS(11 0)` account id | `account.acct_id` |
| CARDDAT | `KEYS(16 0)` card number | `card.card_num` |
| CUSTDAT | `KEYS(9 0)` customer id | `customer.cust_id` |
| CCXREF | `KEYS(16 0)` card number | `card_xref.card_num` |
| TRANSACT | `KEYS(16 0)` transaction id | `transaction.tran_id` |
| USRSEC | `KEYS(8,0)` user id | `user_security.usr_id` |

  KSDS keys that concatenate several copybook fields (TCATBALF `KEYS(17 0)` = account + type + category, DISCGRP
  `KEYS(16 0)`, TRANCATG `KEYS(6 0)`) become composite PKs (`@EmbeddedId`), keeping the COBOL field order.
- Each AIX becomes a database index on the alternate key column(s): `CARDAIX` `KEYS(11 16) NONUNIQUEKEY` → non-unique
  index on `card.acct_id`; `CXACAIX` `KEYS(11,25)` → index on `card_xref.acct_id`; TRANSACT AIX `KEYS(26 304)` → index
  on the processed timestamp. `UNIQUEKEY` AIXs become unique indexes. The PATH name is not modelled.
- `STARTBR`/`READNEXT`/`READPREV` browses become keyset-paginated queries ordered by the key, never `OFFSET`, so
  paging matches the COBOL screens. `STARTBR` (default `GTEQ`) includes the start key; continuation cursors exclude
  the boundary record already shown:
  - first page from a start key: `WHERE key >= :startKey ORDER BY key LIMIT n`
  - next page (`READNEXT`, PF8): `WHERE key > :lastKeyShown ORDER BY key LIMIT n`
  - previous page (`READPREV`, PF7): `WHERE key < :firstKeyShown ORDER BY key DESC LIMIT n`, then reverse for display
  Tests cover an exact-key start, a start key that does not exist, and both directions at the file boundaries.
- Record layout fields with no business meaning (`FILLER`) are not stored. Copybook names go in column comments.
- Sequential files (DALYTRAN, reports, exports) are not tables; they stay files read/written by Spring Batch.
