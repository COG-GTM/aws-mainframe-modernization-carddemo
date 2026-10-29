# Transaction Type (Db2) - business logic specs

> **Provenance:** Authored by **Devin** in the AWS Transform for mainframe *business logic extraction* layout (program overview, business rules, data dictionary, flows). **Not produced by AWS Transform.** The real AWS Transform job (workspace `CardDemo-Transform-Demo`, job `CardDemo-Transaction-Type-Db2`) was started on the same source; see `README.md` for its status.

Source analysed: `app/app-transaction-type-db2/` (CICS `COTRTLIC`, `COTRTUPC`; batch `COBTUPDT` via `MNTTRDB2.jcl`; extract `TRANEXTR.jcl`; DDL, DCLGEN, load control cards, BMS maps).

| File | Content |
|---|---|
| [COTRTLIC.md](COTRTLIC.md) | CICS list / browse / inline update / delete (transaction CTLI) |
| [COTRTUPC.md](COTRTUPC.md) | CICS add / update / delete of one type (transaction CTTU) |
| [COBTUPDT.md](COBTUPDT.md) | Batch maintenance from a 53-byte input file |
| [TRANEXTR.md](TRANEXTR.md) | Db2 -> fixed-width extract for the VSAM-based programs |
| [DATA-MODEL.md](DATA-MODEL.md) | Tables, keys, seed data, data dictionary |
| [BUSINESS-RULES.md](BUSINESS-RULES.md) | Rules catalog with stable IDs `BR-TT-nnn` |
| [GAP-REVIEW.md](GAP-REVIEW.md) | Legacy COBOL vs this spec vs the Java implementation |

Implementation: `modernization/transaction-type-service/` (traceability table in its README).

> **Update:** the real AWS Transform technical documentation for this module is now committed, unmodified, in [`aws-transform-output/`](aws-transform-output/README.md).
