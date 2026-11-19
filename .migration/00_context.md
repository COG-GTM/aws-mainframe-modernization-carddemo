# 00 - Engagement context (CardDemo)

Created by `!mf_migration_setup` on 2026-11-19 (session devin-c4fce2ffab2c4abaac2aa02e217e00e4, child of orchestrator devin-4abefb5b7baf4bfab62c956941e00bf7 running `!mf_migrate_stream`). Re-run the playbook idempotently at the start of any resumed engagement; it re-verifies paths, branches and the runbook and appends to 04/05/06, never rewrites them.

## Engagement facts

| Field | Value | Source |
|---|---|---|
| Module | **CardDemo** (whole `app/**` tree, including the optional extensions `app/app-authorization-ims-db2-mq`, `app/app-transaction-type-db2`, `app/app-vsam-mq`) | user hand-off 2026-11-19 |
| Stream requested by the user | **POSTTRAN** - BATCH, `app/jcl/POSTTRAN.jcl:23` `STEP15 EXEC PGM=CBTRN02C` | user hand-off 2026-11-19 (process type and hard stop still to be confirmed at STOP B) |
| Target state | [`functional/CARDDEMO/CardDemo_target_state.md`](../functional/CARDDEMO/CardDemo_target_state.md), **CONFIRMED** at STOP A on 2026-11-19 | commit `07a8fbb` |
| Working language | English for all artifacts. Source labels are US-English ASCII (no accented text found, see 02) | 02_conventions.md |
| Mainframe / VSAM / DB2 / IMS / MQ access | **None.** Every fact is source-derived; behavior is proven with GnuCOBOL 3.1.2 on this VM | 07_runbook.md |
| Reference evidence (never a base) | `COG-GTM/aws-mainframe-modernization-carddemo` branch `migration/carddemo-java` @ `c251be5`, cited `REF:` | target state header |
| Look-alike repo to ignore | `COG-GTM/mainframe-modernization-carddemo` (not used) | user hand-off |

## Repository topology (resolved role table)

Single repo: `COG-GTM/aws-mainframe-modernization-carddemo`. Every wave produces one PR.

| Role | Path | Base branch | Status on `main` today |
|---|---|---|---|
| SOURCE (read-only) | `app/**` (`app/cbl`, `app/cpy`, `app/jcl`, `app/proc`, `app/csd`, `app/scheduler`, `app/data`, `app/asm`, `app/app-*/**`) | `main` | present; **never edited** |
| BACKEND | `modernization/` (Maven multi-module: `carddemo-common`, `carddemo-app`) | `main` | **absent** (expected; Phase 0 builds it) |
| FRONTEND | N/A (no UI-bearing stream in scope; POSTTRAN is BATCH) | - | - |
| DOCS | `functional/CARDDEMO/**`, `.migration/**` | `main` (delivered through PRs) | target state on the stream branch |

Working branch for the pre-migration artifacts: `devin/1795090039-mf-posttran-stream` (based on `main`). No PR is opened by this step.

## Autonomy defaults (inherited by every later playbook)

| Stop | Mode |
|---|---|
| STOP A target state + workspace + topology | **blocking** |
| STOP B stream choice + process type + hard stop | **blocking** |
| STOP C plan approval | **blocking** |
| STOP D per-wave review | notify |
| STOP E sign-off + merge | **blocking** |

Children do not wait on a human: when a child reaches a stop it finishes everything before it and returns to the orchestrator, which holds the stop.

## Artifact contract (where every later deliverable is committed)

All paths are in DOCS on `main` (via the stream PR). `<Stream>` is the stream id from the inventory (e.g. `POSTTRAN`). Program names are upper-case as in source.

| Deliverable | Path | Producer |
|---|---|---|
| Target state | `functional/CARDDEMO/CardDemo_target_state.md` | `!mf_ingest_target_state` |
| Module inventory | `functional/CARDDEMO/CardDemo_inventory.md` (+ `functional/CARDDEMO/inventory/` evidence: call graph script, edge list, coverage JSON, flow diagram) | `!mf_module_inventory_analysis` |
| Stream analysis | `functional/CARDDEMO/<Stream>_analysis.md` | `!mf_stream_analysis` |
| Stream FR (business sign-off oracle) | `functional/CARDDEMO/<Stream>_functional_requirements.md` | `!mf_stream_fr_generation` |
| Stream migration plan | `functional/CARDDEMO/<Stream>_migration_plan.md` | `!mf_stream_migration_plan` |
| Per-program FR | `functional/CARDDEMO/programs/<Program>_functional_requirement.md` | `!mf_program_fr_generation` |
| Parity results | `functional/CARDDEMO/<Stream>_parity/wave-<N>.md` (+ golden files under `modernization/carddemo-app/src/test/resources/golden/<Stream>/`) | `!mf_program_parity_test` |
| Verification evidence | `functional/CARDDEMO/<Stream>_evidence/` | `!mf_stream_signoff` |
| Sign-off | `functional/CARDDEMO/<Stream>_signoff.md` | `!mf_stream_signoff` |
| Independent audit | `functional/CARDDEMO/<Stream>_audit.md` (written by a session that did not migrate) | `!mf_stream_signoff` |
| Boundary register | `.migration/04_boundary_register.md` (append-only) | inventory, stream analysis, plan, waves |
| Progress ledger | `.migration/05_progress.md` (append-only) | every playbook |
| Decision log | `.migration/06_decisions.md` (append-only, dated) | every playbook |

Naming rules: Markdown only; one file per artifact; never rename a committed artifact (supersede with a dated note instead); cite source as `<path>:<line>`; cite REF as `REF: <path>`.

## Macro chain (resume from here)

`!mf_ingest_target_state` -> `!mf_migration_setup` -> `!mf_module_inventory_analysis` -> per stream: `!mf_stream_analysis` -> `!mf_stream_fr_generation` -> `!mf_stream_migration_plan` -> `!mf_program_fr_generation` -> `!mf_program_migration` -> `!mf_program_parity_test` -> optional `!mf_online_ui_testing` -> `!mf_stream_signoff`, with `!mf_boundary_resolution` invoked per flagged boundary (register mode in inventory/analysis, decide mode at STOP C, implement mode in waves). `!mf_migrate_stream` is the thin orchestrator that sequences them.

Current position: see `05_progress.md`.

## Existing assets (registered as cross-checks, never as fact over source)

| Asset | Use |
|---|---|
| `README.md` (repo root) | job and transaction catalog cross-check (e.g. POSTTRAN at `README.md:225`, `README.md:316`) |
| `app/app-*/README.md` | extension descriptions (MQ queues, IMS/DB2 setup) |
| `scripts/run_posting.sh`, `scripts/run_full_batch.sh` | operator job order on the original host (FTP to JES); not runnable here (needs the Ensono tunnel) |
| REF branch `migration/carddemo-java` @ `c251be5` | prior five-service port: conventions and golden-set method only; its service split is rejected (target state C-1) |
| `samples/m2/**` | AWS M2 / UniKix runtime samples; not used |
