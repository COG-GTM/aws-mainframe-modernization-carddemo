# 00 - Engagement context

- **Engagement:** migrate the BATCH stream POSTTRAN end to end (USER, 2026-11-19: "Migra el stream POSTTRAN de principio a fin. Para en STOP E para pedirme el merge.").
- **Source:** `app/` of COG-GTM/aws-mainframe-modernization-carddemo, read-only.
- **Target:** Java 21 + Spring Boot 3.3 + Spring Batch 5 under `modernization/` on `main`: modular monolith `carddemo-app` + `carddemo-common`, PostgreSQL 16, Flyway (USER). Full profile: `functional/CARDDEMO/CardDemo_target_state.md`.
- **Parity:** GnuCOBOL golden master: same DALYREJS, same tables, same RC (USER). Oracle: `.migration/tools/posttran_gnucobol_oracle.sh`.
- **Stream boundary (STOP B):** BATCH POSTTRAN = `app/jcl/POSTTRAN.jcl` STEP15 → `CBTRN02C`, hard stop `GOBACK` (`app/cbl/CBTRN02C.cbl:234`). ONLINE / SUBTRANSACTION: N/A.
- **Evidence only, never a base:** `REF:` `migration/carddemo-java` @ `c251be50`; `PRIOR:` `devin/1795090039-mf-posttran-stream` @ `e0a9ad14` (PRs #210/#211/#212 untouched). `COG-GTM/mainframe-modernization-carddemo` is ignored.
- **Operating rule:** no questions; recommended option applied and logged in `06_decisions.md` as AUTO-APPLIED (revisable). Block only on destructive/irreversible actions, stream widening or credentials. No merges, no Devin Review.
- **Stops:** after inventory (docs PR open), after plan, STOP E (sign-off before merge).

| File | Purpose |
|---|---|
| `01_target_state.md` | Pointer + digest of the confirmed target state |
| `02_conventions.md` | Coding / naming / test conventions for generated code |
| `03_glossary.md` | Mainframe ↔ target vocabulary |
| `04_boundary_register.md` | Boundaries (append-only, dated) |
| `05_progress.md` | Progress log (append-only, dated) |
| `06_decisions.md` | Decisions (append-only, dated) |
| `07_runbook.md` | Every command, executed, with its observed result |
| `tools/` | Oracle and helper scripts (only code allowed in docs PRs) |
