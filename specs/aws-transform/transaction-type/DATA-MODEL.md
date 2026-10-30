# Data model - CARDDEMO transaction types

> **Provenance:** Authored by **Devin** in the AWS Transform for mainframe *business logic extraction* layout (program overview, business rules, data dictionary, flows). **Not produced by AWS Transform.** The real AWS Transform job (workspace `CardDemo-Transform-Demo`, job `CardDemo-Transaction-Type-Db2`) was started on the same source; see `README.md` for its status.

## Tables (from `ddl/TRNTYPE.ddl`, `ddl/TRNTYCAT.ddl`)
| Table | Column | Type | Null | Key | Notes |
|---|---|---|---|---|---|
| CARDDEMO.TRANSACTION_TYPE | TR_TYPE | CHAR(2) | NOT NULL | PK (XTRNTYPE unique index) | two-digit numeric code, 01-99 |
| | TR_DESCRIPTION | VARCHAR(50) | NOT NULL | | letters, digits, spaces (online edit) |
| CARDDEMO.TRANSACTION_TYPE_CATEGORY | TRC_TYPE_CODE | CHAR(2) | NOT NULL | PK part 1, FK -> TRANSACTION_TYPE.TR_TYPE **ON DELETE RESTRICT** | |
| | TRC_TYPE_CATEGORY | CHAR(4) | NOT NULL | PK part 2 (XTRNTYCAT) | |
| | TRC_CAT_DATA | VARCHAR(50) | NOT NULL | | category description |

Host variables (`dcl/DCLTRTYP.dcl`, `dcl/DCLTRCAT.dcl`): `DCL-TR-TYPE PIC X(2)`, `DCL-TR-DESCRIPTION` (49-level LEN + TEXT X(50)); `DCL-TRC-TYPE-CODE X(2)`, `DCL-TRC-TYPE-CATEGORY X(4)`, `DCL-TRC-CAT-DATA` VARCHAR(50).

## Seed data (`ctl/DB2LTTYP.ctl`, `ctl/DB2LTCAT.ctl`)
7 types: 01 PURCHASE, 02 PAYMENT, 03 CREDIT, 04 AUTHORIZATION, 05 REFUND, 06 REVERAL (sic), 07 ADJUSTMENT. 18 category rows (01:5, 02:3, 03:3, 04:3, 05:1, 06:2, 07:1), so every seeded type has children and cannot be deleted (BR-TT-051).

## Record layouts
| Layout | Bytes | Fields |
|---|---|---|
| COBTUPDT input (`INPUT-REC`) | 53 | action X(1), TR_TYPE X(2), description X(50) |
| TRANEXTR TRANTYPE.PS | 60 | TR_TYPE X(2), description X(50), '00000000' |
| TRANEXTR TRANCATG.PS | 60 | type X(2), category X(4), description X(50), '0000' |

## Platform notes
- Index DDL carries z/OS-only clauses `ERASE NO` / `CLOSE NO`; drop them on Db2 LUW. Db2 LUW already creates the PK index, so `XTRNTYPE`/`XTRNTYCAT` return SQL0605W (equivalent index exists).
- `DB2LTTYP.ctl` ends with `COMMIT;` with no blank line between it and the INSERT, so Db2 parses `COMMIT` as a correlation name. The insert still succeeds.
