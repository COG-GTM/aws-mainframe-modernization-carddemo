# Strangler cutover: the daily posting step (CBTRN02C → transaction-posting-service)

This is how the Java posting slice replaces one batch step at a time: run it beside the COBOL job, compare the two, cut over that one job, and keep a rollback path. Nothing outside this step changes. It is a plan for this slice. It has not been run against a production scheduler.

## 1. What is replaced and what stays

In the daily cycle (`scripts/run_full_batch.sh`, jobs 12-15) the order is
`POSTTRAN (CBTRN02C)` → `INTCALC (CBACT04C)` → `TRANBKP` → `COMBTRAN` → `TRANIDX`.
Only `POSTTRAN STEP15` moves to Java. Every job before and after it keeps running unchanged and keeps reading the same datasets.

## 2. The interface that stays fixed

The cutover is safe only because the step's contract does not move. The contract is the datasets and the return code in `app/jcl/POSTTRAN.jcl`:

| DD | Direction | Record | Fixed by |
|---|---|---|---|
| `DALYTRAN` | input, sequential (feeder extract) | 350 bytes, `CVTRA06Y` | copybook |
| `XREFFILE` | input, keyed | 50 bytes, `CVACT03Y` | copybook |
| `ACCTFILE` | update, keyed | 300 bytes, `CVACT01Y` | copybook |
| `TCATBALF` | update, keyed | 50 bytes, `CVTRA01Y` | copybook |
| `TRANFILE` (TRANSACT) | output, keyed | 350 bytes, `CVTRA05Y` | copybook |
| `DALYREJS` | output, new GDG generation | 430 bytes = input record + 4-byte reason + 76-byte text | `CBTRN02C` 176-182, 446-448 |
| return code | to the scheduler | 0 = clean, 4 = rejects written | `CBTRN02C` 229-231 |

Downstream jobs (`INTCALC`, `TRANBKP`/`COMBTRAN`, reporting) and whatever builds the outbound interface file read only these datasets. If the bytes and the return code stay the same, they cannot tell which implementation ran. That is the property `scripts/run-equivalence.sh` checks on the golden sets (`06-golden-set-results.md`). The service reads and writes the same fixed-width layouts (`carddemo-recordio`) for exactly this reason. A move to a relational store (`03-data-model.md`) is a later, separate step that comes after the cutover.

## 3. Phases

| Phase | Production of record | Java runs on | Gate to the next phase |
|---|---|---|---|
| 0. Golden sets | COBOL | the golden inputs | `scripts/run-equivalence.sh` → `OVERALL: PASS` on every set |
| 1. Shadow (dual-run) | COBOL | a copy of each day's real `DALYTRAN` plus the masters as they were before the run, written to a separate location | N consecutive cycles with zero record, control-total or return-code differences. N, and the reviewers, are set by the system owner. |
| 2. Cutover | Java | live datasets | sign-off on the shadow evidence and a tested rollback |
| 3. Retire | Java | live | COBOL step removed from the schedule after an agreed hold period |

**Shadow run, each cycle:**
1. Snapshot `ACCTFILE` and `TCATBALF` before `POSTTRAN` runs. Both implementations update these in place, so the Java run must start from the same before-image.
2. The COBOL job runs as today. Its outputs are the outputs of record.
3. The Java service runs on the snapshot and a copy of `DALYTRAN`. The clock is pinned to the COBOL run's timestamp so `TRAN-PROC-TS` is comparable. It writes to a shadow location only.
4. `tests/golden/compare.py <cobol-outputs> <java-outputs>` reconciles them byte-for-byte, record-by-record and by control totals: records in, accepted, rejected, rejects by reason code, accepted and rejected amounts, closing balance per account and per category, return code.
5. Any difference blocks the next phase. It is triaged as a Java defect, a COBOL behaviour nobody had written down (record it in `open-questions.md`), or an input/environment difference.

Phase 0 has already found one difference in this slice. The Java process returned 0 where `CBTRN02C` returns 4 when it writes rejects. It was fixed before any shadow run (`06-golden-set-results.md` §6.4). The comparison exists to catch exactly this kind of difference.

## 4. Cutover, one job at a time

- The scheduler entry for `POSTTRAN` changes from `EXEC PGM=CBTRN02C` to a call of the service (`java -jar transaction-posting-service.jar` with the same dataset locations as properties). The return-code mapping stays 0/4. A failure is non-zero, so any condition-code tests the scheduler applies to later steps see the same values they see today.
- Other jobs move later, each with its own shadow phase: `INTCALC` (`interest-calculation-service`), then the report (`transaction-report-service`). Two jobs never cut over in the same cycle, so any difference can be traced to one change.
- During the hold period the COBOL step can stay in shadow, running after the Java step against the snapshot. That swaps the dual-run roles and keeps the comparison going.

## 5. Rollback

- **Trigger:** a non-zero, non-4 return code, a control-total break against the feeder's own counts, or a reconciliation difference found after cutover.
- **Action:** restore `ACCTFILE` and `TCATBALF` from the pre-run snapshot, delete that cycle's `TRANSACT` additions and the new `DALYREJS` generation, point the scheduler entry back at `CBTRN02C`, and rerun the cycle. The datasets are identical in both implementations, so no conversion is needed in either direction.
- **Known limit:** the service writes its four outputs one after another, not atomically (OQ-07). The pre-run snapshot is the recovery point until the persistence decision in `03-data-model.md` is made.

## 6. Not covered here

- This plan does not assume a mainframe run of the comparison. The golden-set reference is a GnuCOBOL run of the same source. Shadow runs against the real job are what establish equivalence with Enterprise COBOL on z/OS.
- Access control, service identity and audit for the new step are out of scope for this document.
- Production volumes and run-time windows are not measured yet. `FixedWidthFile` holds whole datasets in memory; sizing is listed under "Performance / volume" in `05-equivalence-evidence.md` §5.3.
