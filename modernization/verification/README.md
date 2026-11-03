# Verification and handover

The golden-set harness: CardDemo's batch chain under GnuCOBOL against the Java jobs on a freshly loaded
database, compared field for field. CI runs it on every PR (`golden-set` job in
`.github/workflows/modernization-verify.yml`).

## Run it

Needs Docker (Compose v2.24+ for `!reset`), Python 3 and GnuCOBOL 3.1 (`cobc`).

```bash
cd modernization
verification/run.sh /tmp/golden          # work directory, default verification/target
# offline/proxied Maven inside the image builds:
MAVEN_MIRROR_URL=https://maven-central.storage-download.googleapis.com/maven2/ verification/run.sh /tmp/golden
```

Exit code 0 means all four jobs reconcile. Reports: `<work>/reconciliation/<JOB>/reconciliation.md`;
golden set: `<work>/cobol/<JOB>/`; Java outputs and table exports: `<work>/java/`.

| Step | Script | What it does |
|---|---|---|
| 1 | `reference.py` | Compiles CBTRN02C, CBACT04C, CBTRN03C, CBSTM03A/B with `cobc -x -std=ibm -fsign=EBCDIC -I app/cpy` (VSAM → GnuCOBOL indexed files loaded from `app/data/ASCII`; sequential files as they are) and runs, with `COB_CURRENT_DATE='2022/07/18 00:00:00.00'`, the Control-M order of `docs/migration/jcl-mapping.md`: POSTTRAN → INTCALC (PARM `2022071800`) → COMBTRAN → TRANREPT → CREASTMT. Every output file and RETURN-CODE is kept. Reuses the per-job references: `scripts/migration/intcalc-reference/run.py`, `scripts/migration/tranrept/reference.sh`, the CREASTMT part of `scripts/migration/golden/batch-service-reference.sh`. |
| 2 | `java.sh` | `docker compose` project `carddemo-verify` with `compose.verify.yml` (no published ports, so it runs beside a dev stack): `down -v`, `up --build --wait`, data-loader, then the same jobs via the CLI (`--job= --date= --parm= --dd= --output-dir=`). After POSTTRAN and after INTCALC it unloads the five schemas with `data-loader --export DIR` (`TableExport`: every mapped table through the phase-3 `RecordWriter`, except `auth.users` whose passwords are hashed). |
| 3 | `compare.py` | Pairs each COBOL output with its Java counterpart and compares field by field using the layouts in `common/.../record/layout` (parsed from the Java source, so both sides use one definition). Writes `reconciliation.md` per job, with `notes/<JOB>.md` appended; any difference not covered by an accepted deviation fails the run. |

Accepted deviations (each is printed with its reason in the report):

- TRAN-PROC-TS (and TRAN-ORIG-TS on INTCALC's interest rows): COBOL's clock is frozen, the Java jobs stamp
  the wall clock. Compared by format (digits masked), so a wrong layout still fails.
- FILLER: zeros in `app/data`, spaces from `RecordWriter`; not a data field.
- DALYTRAN-PROC-TS: POSTTRAN stamps the staged daily row it processed (idempotent posting); CBTRN02C only
  reads DALYTRAN.

"Matches COBOL, source defect" items (CBACT04C last account, CBTRN03C quirks) are listed in the notes; they
compare equal, so they need no mask.

## Coverage matrix

Every program in `app/cbl` (the extensions under `app/app-*` are out of scope, `docs/migration/scope.md` §3).

| COBOL | Java | Verification |
|---|---|---|
| CBTRN02C | transaction-service `POSTTRAN` | golden set, this harness (TRANSACT, DALYREJS, TCATBALF, ACCTFILE vs `account.accounts`) |
| CBACT04C | transaction-service `INTCALC` | golden set, this harness (SYSTRAN, ACCTFILE, TCATBALF); `IntcalcCobolParityTest` |
| CBTRN03C | batch-service `TRANREPT` | golden set, this harness (TRANREPT, TRANSACT.BKUP/DALY) |
| CBSTM03A, CBSTM03B | batch-service `CREASTMT` | golden set, this harness (TRXFL.SEQ, STMTFILE, HTMLFILE); shims checked (`SHIMS.txt`) |
| CBEXPORT, CBIMPORT | batch-service `CBEXPORT` / `CBIMPORT` | GnuCOBOL golden set in `GoldenSetTest` / `BranchRoundTripTest` (`scripts/migration/golden/batch-service-reference.sh`, CBIMPORT with its CARDOUT DD); not in the four-job chain |
| CBTRN01C | transaction-service `CBTRN01C` | `BatchJobsTest` (SYSOUT as the COBOL DISPLAYs); not in the chain |
| CBACT01C, CBACT02C, CBACT03C, CBCUS01C | batch-service `READACCT`, `READCARD`, `READXREF`, `READCUST` | `GoldenSetTest` |
| CSUTLDTC | `common` `DateValidation` | unit tests |
| COBSWAIT | batch-service `WAITSTEP` | unit tests; the scheduler order replaces the wait |
| COSGN00C, COMEN01C, COADM01C | auth-service signon, `GET /api/v1/menu` | integration tests in the service modules |
| COACTVWC, COACTUPC | account-service view / update | integration tests |
| COCRDLIC, COCRDSLC, COCRDUPC | card-service list / view / update | integration tests |
| COTRN00C, COTRN01C, COTRN02C, COBIL00C | transaction-service list / view / add / bill payment | integration tests |
| CORPT00C | batch-service `POST /api/v1/reports/transactions` → TRANREPT | integration tests |
| COUSR00C–COUSR03C | auth-service user CRUD | integration tests |

Per-program detail and status: `docs/migration/architecture.md` §4.

## Intentionally not migrated

From `docs/migration/scope.md` §3 and `docs/migration/jcl-mapping.md`:

- IMS/DB2/MQ extensions (`app/app-authorization-ims-db2-mq`, `app/app-transaction-type-db2`,
  `app/app-vsam-mq`) and the CSD transaction `CDV1` (no source in the repository).
- CICS file open/close (CLOSEFIL, OPENFIL, CEMT steps): online and batch share one database.
- Assembler `COBDATFT` / `MVSWAIT`: replaced by Java date formatting and `Thread.sleep`.
- Control-M / CA-7: replaced by the explicit job order (above, and `jcl-mapping.md`).
- TXT2PDF1: PDF rendering of the statement; the text and HTML statements are migrated.
- IDCAMS/IEBGENER data-definition jobs: replaced by Flyway and the data-loader.
- Demonstration/template jobs (ESDSRRDS, DEFCUST, FTPJCL, INTRDRJ1/2, CBADMCDJ).
