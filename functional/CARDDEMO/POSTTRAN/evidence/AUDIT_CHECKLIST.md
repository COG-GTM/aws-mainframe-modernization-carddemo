# POSTTRAN independent audit – checklist and handoff

For the **fresh audit session** launched by the orchestrator (`mf-stream-signoff` step 5). You did not migrate this stream; do not
trust its reports, including `POSTTRAN_signoff.md`, `CBTRN02C_parity_results.md` and the files in this directory. Disagree freely.
Write your result to `functional/CARDDEMO/POSTTRAN_independent_audit.md` (findings by severity with `file:line` cites, explicit
verdict); the sign-off §10 will reproduce it verbatim.

## Inputs

- Source (read-only): `app/jcl/POSTTRAN.jcl`, `app/cbl/CBTRN02C.cbl`, copybooks in `app/cpy` (CVTRA06Y, CVTRA05Y, CVACT01Y, CVACT03Y, CVTRA01Y), `app/data/ASCII`, `app/data/EBCDIC`.
- Docs: `functional/CARDDEMO/POSTTRAN_functional_requirements.md` (business oracle), `functional/CARDDEMO/programs/CBTRN02C_functional_requirement.md`, `POSTTRAN_migration_plan.md`, `CardDemo_target_state.md`, `.migration/01..07`.
- Code: `modernization/` on the sign-off PR head. Stack: #213 → #214 → #215 → #216 → #217 → sign-off PR (unmerged).
- Tools: GnuCOBOL 3.1 (`cobc`), `.migration/tools/posttran_gnucobol_oracle.sh`, `cbtrn02c_parity_oracle.sh`, `posttran_e2e*.sh`, Docker (Testcontainers `postgres:16`, `docker compose`). Java 21: `JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64` (plain `mvn` picks Java 17).

## Checklist (tick each with your own evidence)

### A. Scope rebuilt independently
- [ ] From `POSTTRAN.jcl` and the COBOL call graph (CALL statements in `CBTRN02C.cbl`), list every in-scope program/routine up to `GOBACK`; diff against `modernization/`. Expected by the migrating side: `CBTRN02C` only, plus `CEE3ABD` as boundary B-07 – verify, do not accept.
- [ ] Every PROCEDURE DIVISION paragraph of `CBTRN02C` (0000-… to 9999-…) maps to behaviour in Java; flag any paragraph with no equivalent.
- [ ] No in-scope class is empty, a stub, `TODO`, `UnsupportedOperationException`, or reachable only from tests/mocks.

### B. Program vs FR docs and target state
- [ ] Each FR-POSTTRAN-01..17, NFR-01..04, PR-CBTRN02C-01..17 against the code (cite file:line) and against the COBOL (cite `CBTRN02C.cbl` line).
- [ ] Target state: Java 21, Boot 3.3, Batch 5, modules `carddemo-app` + `carddemo-common`, PostgreSQL 16 + Flyway, one job per JCL / one step per EXEC, chunk 1 restartable, DALYTRAN / DALYREJS files, KSDS → tables; no Lombok, no H2, no `double` for money; every dependency registered in `06_decisions.md`.
- [ ] Each boundary B-01..B-09 seam against its decided contract (plan §4, `04_boundary_register.md`).

### C. Hands-on re-run (do not read reports instead)
- [ ] `(cd modernization && JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify)` – record test counts and failures.
- [ ] Regenerate at least a sample of oracle outputs yourself from `app/cbl/CBTRN02C.cbl` (e.g. `posttran_e2e_oracle.sh <new dir> <new dir>`) and compare with the committed `oracle/` (only TRAN-PROC-TS cols 325-326 may differ).
- [ ] `posttran_e2e.sh <new dir> [your oracle dir]` on docker compose; check `e2e FAILS=0` and inspect at least L01, L06a/L06b and L07 artefacts.
- [ ] Construct at least one case of your own (not in the 20 parity cases or the 7 golden cases), run it on GnuCOBOL and on Java, compare.

### D. Do the tests assert behaviour?
- [ ] Expected values in parity / golden / e2e come from GnuCOBOL or the FR text, never from a Java run (look for expected files written by Java, or assertions comparing Java with itself).
- [ ] No tautologies (assert result == result, mocks asserting their own stubs), no assertions weakened to `isNotNull` where a value is specified.
- [ ] The comparator normalisation (`posttran_e2e_compare.py`, `ParityHarness`, `Cbtrn02cIndependentParityTest`) masks only FILLER and TRAN-PROC-TS centiseconds; RC mapping only 231 → 16.

### E. Claims to challenge specifically
- [ ] Sign-off §4: 37 PASS / 1 UNTESTED. Check each "PASS" has an executing check; especially FR-POSTTRAN-17 (L04), NFR-02 (static only), NFR-04 (log scan; the only card-bearing log is unreachable).
- [ ] Accepted deviations AD-1 (D-042 atomic vs COBOL half-posted abend), AD-2 (untested open/read/write failures), AD-3 (PR-CBTRN02C-15 close failure): are they honestly scoped?
- [ ] Open human decisions D-015/D-039 (231 ≡ 16), D-024 (new-instance reset / rerun reposting), PR #214 review findings (batch options without `=` silently ignored; PostgreSQL port published with fixed credentials): confirm none is hidden or mis-stated.
- [ ] The e2e seeds masters through `KsdsDataService` (D-053) – confirm it does not bypass anything the job itself must do.
- [ ] CI: `.github/workflows/modernization-verify.yml` jobs `verify` and `posttran-e2e-compose` actually run the above on the PR head and are green.

## Verdict format
`PASS` (no blocker/major), `PASS WITH FINDINGS` (minors only), or `FAIL` (any blocker/major), followed by the findings table:
`| # | Severity (blocker/major/minor/info) | Finding | Evidence (file:line, command output) | Recommendation |`.
