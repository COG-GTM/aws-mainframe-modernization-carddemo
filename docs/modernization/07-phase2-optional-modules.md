# 07 - Phase 2: optional modules

| Module | Legacy | Proposed target |
|---|---|---|
| `app-authorization-ims-db2-mq` | MQ-triggered authorization (COPAUA0C), IMS DB pending-authorization summary/detail segments, DB2 fraud table, CICS screens COPAUS0C-2C, batch purge CBPAUP0C | New `authorization-service`: an Amazon MQ or SQS consumer replaces the MQ trigger. IMS parent/child segments become tables `pending_auth_summary` (1) and `pending_auth_detail` (many). DB2 `AUTHFRDS` moves to PostgreSQL. The purge job becomes a Spring Batch job |
| `app-transaction-type-db2` | DB2 `TRANSACTION_TYPE` maintenance (COTRTLIC/COTRTUPC), batch COBTUPDT | Write endpoints on transaction-service's `/transaction-types` (ADMIN). The tables already exist in V1 |
| `app-vsam-mq` | MQ request/reply account and date inquiry (COACCT01, CODATE01) | Not needed once the REST APIs exist. If integration partners still use MQ, add a thin Amazon MQ bridge that calls account-service |

These modules are scoped out of this phase as agreed in the plan. The data-migration codec already supports COMP-3,
which the DB2/IMS copybooks use.
