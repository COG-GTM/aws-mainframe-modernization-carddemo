# 11 — Hand-over for sign-off: CardDemo COBOL/CICS/JCL/VSAM → Java 21

One page for the sign-off gate (`g-signoff`). Everything below is on the PR stack ending in
`devin/unt51-26-hardening`; nothing is on `main` and nothing is merged.

## What was migrated (from [07-traceability.md](07-traceability.md), CI-checked)

| Item | Count | Java 21 target |
| --- | --- | --- |
| COBOL programs | 31/31 | one Spring Boot modular monolith `modernization/carddemo-app` (ADR-0001) |
| Paragraphs | 576: 234 mapped to methods, 169 retired-cics, 66 retired-jcl, 107 folded, **0 GAP** | `{@code NNNN-PARA}` Javadoc tags (ADR-0002) |
| Copybooks | 47/47 (30 data + 17 BMS) | records/entities/tables, React pages |
| JCL members | 38/38 | 11 batch jobs/streams + `initial-load`, `nightly-cycle`, on-demand, retired or out of scope |
| CICS transactions | 17/17 | REST endpoints under `/api/v1` + React 18 UI (`modernization/carddemo-ui`) |
| Scheduler definitions | 45/45 (15 Control-M + 30 CA-7) | in-app `nightly-cycle` (ADR-0016) |
| VSAM files (KSDS, AIX, sequential, GDG) | 11 business tables, see [03-data-model](03-data-model.md) | PostgreSQL 16, Flyway V1–V6; GDGs as dated generations (ADR-0011/0012) |

Tests: 1,127 unit + 131 Testcontainers ITs; merged line coverage 92.8%, every domain package ≥ 80% enforced by
JaCoCo ([coverage](../validation/hardening/coverage.md)).

## How it is verified

- **CI** — `.github/workflows/modernization-ci.yml`, 6 jobs (build + `traceability-check`, compose + Playwright e2e,
  baseline, batch-equivalence, ui, golden-set): [latest runs on this branch](https://github.com/COG-GTM/aws-mainframe-modernization-carddemo/actions/workflows/modernization-ci.yml?query=branch%3Adevin%2Funt51-26-hardening)
  (the run on the PR head is linked from the PR).
- **Golden set** — online scenario + full nightly cycle, Java vs GnuCOBOL, field by field:
  [reconciliation](../validation/golden-set/2026-10-07/reconciliation.md) — PASS, 11 allow-listed differences, all
  justified. Unchanged by the hardening (`make golden-set-check`).
- **Traceability** — [07-traceability.md](07-traceability.md) (`make traceability-check`).
- **Batch equivalence per job** — `make batch-equivalence`; **baseline** — `docs/validation/baseline/`.
- **UI recordings** — React UI: [USER flow](https://app.devin.ai/boards/board-eefbf729df3b4344abdc7cc409e8c8f6?testRecording=c25ed29d-0d29-4a6f-a636-2b187c57fc95),
  [ADMIN flow](https://app.devin.ai/boards/board-eefbf729df3b4344abdc7cc409e8c8f6?testRecording=1326f709-03d8-4341-a2fe-97dd392acbac) (s5.6);
  [sign-on → account view](https://app.devin.ai/boards/board-eefbf729df3b4344abdc7cc409e8c8f6?testRecording=f657d277-7581-4b60-9997-75c7246610a9) (s6.3);
  final end-to-end recording of this branch on the "Hardening and final review" ticket and the PR.
- **Hardening evidence** — [dependency scan](../validation/hardening/dependency-scan.md),
  [volume smoke 100k](../validation/hardening/volume-smoke.md), [sign-on first entry](../validation/hardening/signon-first-entry.md),
  [review findings](../validation/hardening/review-findings.md); ops: [09-configuration](09-configuration.md),
  [10-runbook-nightly-cycle](10-runbook-nightly-cycle.md), [clean-checkout walkthrough](../validation/ops/clean-checkout-walkthrough.md).

## Deliberate deviations (index in [07 §7](07-traceability.md#7-adr-index-and-deviation-index))

| Where | Deviation | Why |
| --- | --- | --- |
| CBSTM03A R-4 | Entry beyond the 51 × 10 statement table abends RC 16 | COBOL result is undefined storage overwrite |
| CBSTM03A D-1 | `carddemo.batch.creastmt.html-escape` (default **off**) escapes names/addresses | stored XSS in STATEMNT.HTML |
| CBTRN02C D-1 | Table-mode POSTTRAN holds the transaction-id lock for the whole step | no id collision with online adds |
| COACTUPC R-39 | Keep-zip: the COBOL overlay of `ACCT-ADDR-ZIP` is not reproduced (`d-zip-overlay`) | data corruption defect in the COBOL |
| COBIL00C R-17/R-18 | Payment rolled back if the account REWRITE fails; stop on CXACAIX error | no payment without balance update |
| COCRDUPC R-30 | Stored account id kept on card update | typed key is protected in the map |
| CORPT00C / ADR-0021 | Start date after end date rejected; reports run async | step definition; no CICS START |
| COTRN02C | TRANTYPE/TRANCATG existence checked on add | migration plan requirement |
| COUSR01C, COUSR03C R-13 | Field length/type edits; no DELETE after a failed read | input safety |
| ADR-0020 | PANs masked in lists/logs, opaque card references | PCI |
| ADR-0023 | BCrypt hash compared at sign-on (plain field kept for export); current admin type re-read per request | credential storage, revocation |

## Accepted review findings

See [review-findings.md](../validation/hardening/review-findings.md): plain-text `SEC-USR-PWD` still stored for VSAM
export parity (F-1); no login rate limit/lockout (F-2); HS256 shared-secret JWT without revocation (F-3); no TLS/CSP
in compose (F-5); stale "POSTTRAN lacks the id lock" sentence in the 2026-10-07 golden-set template — closed by this
PR (F-9); 23 dependency findings with justification (F-11, [dependency-scan.md](../validation/hardening/dependency-scan.md));
in-process report queue (F-12); POSTTRAN 104 s / 100k records (F-13); TRANBKP→COMBTRAN empty-table window (1.5c).

## Known limitations

- [What the golden set does not prove](../validation/golden-set/2026-10-07/what-this-does-not-prove.md): CICS is not
  run, single-user scenario, sample volumes, no 3270 rendering.
- **No Enterprise COBOL / real VSAM proof**: the baseline is GnuCOBOL 3.1.2 on ASCII files; EBCDIC collation is
  emulated with `COLLATE "C"` only where the data needs it. A z/OS run of the same golden set is the remaining proof.
- **Single-instance report queue** (ADR-0021): scale-out needs a shared queue.
- **Extension apps out of scope** (`d-scope`): IMS/Db2/MQ authorization, transaction-type Db2, VSAM-MQ; jobs
  TRANEXTR, MNTTRDB2, CBPAUP0J.
- Batch window: online adds during TRANBKP→COMBTRAN are lost; online adds wait while POSTTRAN posts.

## Recommended next steps

1. **Merge order** (bottom-up, each PR retargets to the merged base): #50 → #53 → #54 → #55 → #56 → #57 → #58 → #59
   → #60 → #61 → #62 → #63 → #64 → #65 → #66 → #67 → #68 → #69 → #70 → #71 → #72 → #73 → #74 → #75 → #77 → this PR.
   CI must be green on each head after retargeting. #76 is a closed failure demo, not part of the stack.
2. **Cutover topics**: data migration rehearsal from the production VSAM unloads (EBCDIC, `initial-load` with
   max-rejects 0) and reconciliation counts; run the golden set against Enterprise COBOL output; enterprise IdP
   instead of HS256 + USRSEC, then retire the plain-text password column (F-1, F-3); secrets in a vault
   (`CARDDEMO_DB_PASSWORD`, `CARDDEMO_JWT_SECRET`), `CARDDEMO_API_DOCS_ENABLED=false`; TLS/WAF/rate limiting at the
   ingress; PostgreSQL backup/PITR aligned to the runbook restore table; nightly-cycle cron and batch window agreed
   with operations; parallel run period comparing TRANREPT/STATEMNT outputs; upgrade to Spring Boot 4 to clear
   the remaining dependency findings.
