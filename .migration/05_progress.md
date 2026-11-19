# 05 - Progress ledger (append-only)

One row per stream per status change; never edit a past row, append a new one. Statuses: `not started` -> `analyzed` -> `FR done` -> `planned (STOP C approved)` -> `in wave N` -> `signed off` (or `blocked: <reason>`, `deferred: <reason>`).

## Engagement milestones

| Date | Step | Result | Artifact (repo path, branch) | By |
|---|---|---|---|---|
| 2026-11-19 | `!mf_ingest_target_state` | CONFIRMED at STOP A by the user | `functional/CARDDEMO/CardDemo_target_state.md` @ `devin/1795090039-mf-posttran-stream` (`07a8fbb`) | orchestrator devin-4abefb5b7baf4bfab62c956941e00bf7 |
| 2026-11-19 | `!mf_migration_setup` | `.migration/` created; runbook verified (see 07); awaiting STOP A re-confirmation of workspace/topology/runbook | `.migration/**` @ `devin/1795090039-mf-posttran-stream` | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | `!mf_module_inventory_analysis` | 46 programs = 37 + 8 + 1; 38 streams catalogued (22 online, 16 batch) + 32 utility-only jobs; 18 boundaries registered; **awaiting STOP B** (user confirms POSTTRAN type/hard stop) | `functional/CARDDEMO/CardDemo_inventory.md` @ same branch | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | STOP B | User confirmed POSTTRAN (BATCH, CBTRN02C, hard stop GOBACK) and D-007/D-008/D-009/D-010 | `.migration/06_decisions.md` | devin-4abefb5b7baf4bfab62c956941e00bf7 |

## Streams

| Date | Stream | Kind | Status | Note | By |
|---|---|---|---|---|---|
| 2026-11-19 | ONL-00 Sign-on + menus | ONLINE | not started | 3 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | ONL-M01 Account View | ONLINE | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | ONL-M02 Account Update | ONLINE | not started | 2 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | ONL-M03 Credit Card List | ONLINE | not started | 3 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | ONL-M04 Credit Card View | ONLINE | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | ONL-M05 Credit Card Update | ONLINE | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | ONL-M06 Transaction List | ONLINE | not started | 2 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | ONL-M07 Transaction View | ONLINE | not started | 2 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | ONL-M08 Transaction Add | ONLINE | not started | 2 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | ONL-M09 Transaction Reports | ONLINE | not started | 2 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | ONL-M10 Bill Payment | ONLINE | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | ONL-M11 Pending Authorization View | ONLINE | not started | 3 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | ONL-A01 User List | ONLINE | not started | 3 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | ONL-A02 User Add | ONLINE | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | ONL-A03 User Update | ONLINE | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | ONL-A04 User Delete | ONLINE | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | ONL-A05 Transaction Type List/Update (Db2) | ONLINE | not started | 2 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | ONL-A06 Transaction Type Maintenance (Db2) | ONLINE | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | ONL-CP00 Authorization request processor (MQ) | ONLINE (MQ-triggered, no screen) | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | ONL-CDRA Account inquiry via MQ | ONLINE (MQ request/response) | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | ONL-CDRD System date via MQ | ONLINE (MQ request/response) | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | ONL-CDV1 (BLOCKED: COCRDSEC absent) | ONLINE | blocked: COCRDSEC absent | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | BAT-POSTTRAN | BATCH | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | BAT-INTCALC | BATCH | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | BAT-TRANREPT | BATCH | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | BAT-CREASTMT | BATCH | not started | 2 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | BAT-READACCT | BATCH | not started | 2 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | BAT-READCARD | BATCH | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | BAT-READCUST | BATCH | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | BAT-READXREF | BATCH | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | BAT-CBEXPORT | BATCH | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | BAT-CBIMPORT | BATCH | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | BAT-WAITSTEP | BATCH | not started | 2 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | BAT-CBPAUP0J | BATCH | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | BAT-LOADPADB | BATCH | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | BAT-UNLDPADB | BATCH | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | BAT-UNLDGSAM | BATCH | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | BAT-MNTTRDB2 | BATCH | not started | 1 program(s) | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | POSTTRAN | BATCH | not started -> scope confirmed (STOP B) | analysis next | devin-4abefb5b7baf4bfab62c956941e00bf7 |
| 2026-11-19 | POSTTRAN | BATCH | **analyzed** | `functional/CARDDEMO/POSTTRAN_analysis.md`; 1 program, 1 wave (+ Phase 0/1); BND-001..009 contracts completed | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | POSTTRAN | BATCH | **FR generated** | `functional/CARDDEMO/POSTTRAN_functional_requirements.md`; 17 FRs, sign-off set FR-01..FR-17 | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | POSTTRAN | BATCH | **planned, awaiting STOP C** | `functional/CARDDEMO/POSTTRAN_migration_plan.md`; BND-001..009 PROPOSED; D-011..D-014 PROPOSED; LR-1..3 listed, not fired | devin-c4fce2ffab2c4abaac2aa02e217e00e4 |
| 2026-11-19 | POSTTRAN | BATCH | FR done -> planned (STOP C approved) | BND-001..009 DECIDED; D-011..D-014 approved; next: program FR CBTRN02C | devin-4abefb5b7baf4bfab62c956941e00bf7 |
