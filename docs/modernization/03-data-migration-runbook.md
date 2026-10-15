# 03 - Data migration design and VSAM-to-RDS runbook

## Mapping rules

| COBOL picture / usage | PostgreSQL | Conversion |
|---|---|---|
| `X(n)` | `VARCHAR(n)` (or `CHAR(n)` for fixed codes and keys: card numbers, type codes, Y/N flags) | IBM037 (EBCDIC) to UTF-8, then trailing spaces trimmed. Empty values become NULL for optional fields |
| `9(n)` (display) | `INTEGER` / `BIGINT` | Zoned decimal. In EBCDIC the digits are `X'F0'-X'F9'` |
| `S9(n)V99` (display, sign trailing overpunch) | `NUMERIC(n+2,2)` | Last byte holds the sign. EBCDIC zone `C`/`F` = +, `D` = -. ASCII overpunch `{A-I` = +0..9, `}J-R` = -0..9 |
| `S9(n)V99 COMP-3` | `NUMERIC` | Packed decimal, two digits per byte, sign in the low nibble (`C`/`F` +, `D` -). `CobolCodec.packed` decodes it and `toPacked` encodes it |
| `X(10)` dates `YYYY-MM-DD` | `DATE` | Parsed. Blank becomes NULL |
| `X(26)` DB2-style timestamps `YYYY-MM-DD-HH.MM.SS.nnnnnn` | `TIMESTAMP` | Parsed |
| `FILLER` | dropped | - |
| KSDS primary key | `PRIMARY KEY` | - |
| AIX (CARDAIX, CXACAIX, TRANSACT proc-ts) | secondary B-tree index (`ix_card_acct_id`, `ix_card_xref_acct_id`, `ix_card_transaction_proc_ts`) | Non-unique, like the `NONUNIQUEKEY` AIX definitions |
| SEC-USR-PWD `X(8)` | `password_hash VARCHAR(100)` | Upper-cased, then BCrypt-hashed at load time |

The full DDL is in `modernization/common/src/main/resources/db/migration/V1__carddemo_schema.sql`. Each table
comment names its source file and copybook. Byte offsets are in `CopybookLayouts.java`.

None of the core copybooks use COMP-3; it appears in the optional DB2/IMS modules. The codec supports it and has
tests, so it is ready for phase 2 and for customer variants of CardDemo that pack amounts.

## Tooling

`modernization/data-migration` builds `carddemo-data-migration-<v>-exec.jar`:

```bash
export CARDDEMO_DB_PASSWORD=...   # from Secrets Manager. Never pass it on the command line
java -jar data-migration/target/carddemo-data-migration-1.0.0-SNAPSHOT-exec.jar \
  --jdbc-url=jdbc:postgresql://<aurora-endpoint>:5432/carddemo --user=carddemo \
  --data-dir=/mnt/unloads --encoding=EBCDIC --truncate
```

- `--encoding=EBCDIC` reads the binary RECFM=FB unloads (`AWS.M2.CARDDEMO.*.PS`, the same names as in `app/data/EBCDIC`).
  `--encoding=ASCII` reads the line-delimited samples in `app/data/ASCII`.
- Checks that every required unload exists before touching the database. Then `--truncate` deletes and the
  whole load runs in **one transaction** in foreign-key order, so a failed reload keeps the previous data.
- TRANSACT history (`AWS.M2.CARDDEMO.TRANSACT.PS` / `transact.txt`, CVTRA05Y) loads into `card_transaction`. It is
  optional only for non-truncating loads. With `--truncate` the loader refuses to run without it, unless you pass
  `--allow-missing-transact`. That flag is for the sample data only (CardDemo ships no TRANSACT unload) and must
  never be used at cutover.
- USRSEC passwords are upper-cased (as COSGN00C compares them) before BCrypt hashing.
- Prints a reconciliation report per table: records read, rows loaded, and control totals (for example the sum of
  `ACCT-CURR-BAL`, the sum of `TRAN-AMT`). Compare it with the mainframe-side counts (see below).
- Moves `tran_id_seq` past the highest numeric TRAN-ID so new online transactions continue the legacy numbering.

## Runbook

**T-14 days: rehearsal**
1. Provision Aurora PostgreSQL 16 (Multi-AZ, encrypted, password in Secrets Manager). Deploy the services with
   `SPRING_FLYWAY_ENABLED=true` on identity-service only.
2. On the mainframe, unload every KSDS (including TRANSACT) with `IDCAMS REPRO` to flat PS files (the shape of `app/data/EBCDIC`) and
   capture the record counts and control totals: `IDCAMS PRINT` counts, plus a sum-of-balance utility run over
   ACCTDAT and TRANSACT.
3. Transfer in binary mode (Transfer Family / SFTP or AWS Mainframe Modernization file transfer) to S3, then run the
   loader from an ECS task.
4. Reconcile: the loader report must match the mainframe counts and totals exactly. Then run the parity suite
   against the rehearsal database by pointing `CARDDEMO_DB_URL` at it.

**T-1 day: freeze.** Announce the change freeze. Run the last POSTTRAN/INTCALC cycle on the mainframe.

**T-0: cutover (target window 2 hours)**
1. Close the CICS files (`CLOSEFIL` JCL) so no more online updates happen. Stop the batch scheduler.
2. Unload, transfer, load with `--truncate`, and reconcile (as in the rehearsal).
3. Smoke test: sign on, view account, list cards, list transactions, bill payment on a test account, and an INTCALC
   dry run on a restored snapshot.
4. Switch DNS / API Gateway to the new services and enable the EventBridge schedules.

**Rollback:** until the first new batch cycle runs, roll back by re-opening the CICS files (`OPENFIL`) and pointing
users back to the 3270 / web-terminal front end. The VSAM files were not changed after the unload. After the first
modern batch cycle, reverse-feed the changes with `CobolCodec.toPacked` and the layout definitions, or accept a
forward fix.

## Validation queries

```sql
select count(*), sum(curr_bal) from account;            -- vs ACCTDAT counts/totals
select count(*), sum(amount) from card_transaction;     -- vs TRANSACT
select acct_id from card_xref except select acct_id from account;  -- must return no rows (orphans)
```
