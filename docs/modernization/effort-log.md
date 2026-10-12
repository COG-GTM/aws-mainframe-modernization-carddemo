# Effort log

Single agent session, 2026-09-03 (UTC). Times are wall-clock from shell/file timestamps
(`git reflog`, file mtimes, Maven output), not estimates. Phases overlapped: the reading of one
program continued while the code for the previous one was being tested, so the per-phase
numbers below sum to more than the elapsed time.

| Phase | Start | End | Wall-clock | Notes |
|---|---|---|---|---|
| 0. Repo clone, branch, environment probe (Java 21 / Maven 3.6.3 / GnuCOBOL 3.1.2, sample data sizes, PR #5/#6 read) | 11:58 | 12:03 | 5 min | `JAVA_HOME` defaulted to Java 11; had to export Java 21 for every Maven call. |
| 1. Inventory — read CBTRN02C, CBACT04C, CBTRN03C, 8 copybooks, 3 JCL, PROC/CTL, IDCAMS DEFINEs, CA-7/Control-M exports; write `01-inventory.md`, `call-graph.mmd`, render SVG | 12:03 | 12:16 | ~13 min | Scheduler exports contradicted the brief (INTCALC monthly, no TRANREPT) → recorded as OQ-01 rather than resolved. |
| 2. Module docs (three files, line-cited) | 12:12 | 12:36 | ~20 min (interleaved) | CBTRN03C's `NEXT SENTENCE` and EOF re-add cost the most time: re-read the loop three times before accepting the source really does that (OQ-17/18/19). |
| 3. Data model DDL + normalization notes | 12:33 | 12:38 | 5 min | Fast because no REDEFINES/OCCURS/COMP-3 exist in these copybooks — verified by grep, stated in §3.0. |
| 4. Implementation: recordio (codec, 7 layouts, keyed store), posting, interest, reporting services and jobs | 12:03 | 12:27 | ~24 min (interleaved) | See failed iterations. |
| 5. Tests (15 classes, 81 tests) incl. shipped-data round trips and malformed input; `mvn test`; GnuCOBOL syntax checks | 12:09 | 12:32 | ~20 min (interleaved) | Six red→green cycles listed below. |
| 6. Equivalence evidence, open questions, effort log, implementation notes | 12:36 | 12:45 | ~9 min | |
| 7. Diff review, commit, PR | 12:45 | — | | |

Elapsed at PR creation: ~50 minutes. A human team's comparable deliverable (per the brief,
"days") would additionally have included the level-C mainframe comparison that this session
could not perform at all — that gap is not time, it is access (see `05-equivalence-evidence.md` §5.3).

## Points that required a human decision or input (none were available; each is recorded, not decided)

| When | Decision needed | What was done instead |
|---|---|---|
| Phase 1 | Which scheduler cadence is real (brief says daily cycle; Control-M says INTCALC monthly; TRANREPT absent) | Documented both sources; three independent jobs. OQ-01 |
| Phase 2 | Whether `NEXT SENTENCE` in CBTRN03C is a bug | Reproduced it as default, exposed a policy switch. OQ-17 |
| Phase 2 | Whether the debit sign convention is intentional | Reproduced. OQ-04 |
| Phase 3 | Normalize (merchant, card/account, dates as typed columns, FKs) or stay copybook-faithful | Delivered faithful DDL; listed 10 normalization decisions D-1..D-10 for the customer |
| Phase 4 | What to do on empty TCATBALF (COBOL rewrites uninitialised storage) | Only deliberate deviation: no-op. OQ-12 |
| Phase 4 | Whether to keep an in-memory KSDS simulation vs. wire the DDL to a database now | Kept files; the data-store choice depends on §3.2 answers |
| Phase 5 | Ground truth for level-C comparison | None exists in repo; listed exact artefacts needed (Zowe profile, compile-and-run unit, output datasets) |
| Phase 6 | Lint tooling | None configured in repo; not added unilaterally |

## Failed iterations (chronological)

1. `mvn -o compile` failed: Spring Batch artifacts not in the local repo. Re-ran online once; all
   later runs offline. (~1 min)
2. Java 11 picked up by default → `release 21 not supported`. Fixed by `JAVA_HOME` export. (<1 min)
3. `CobolNumericTest`: expected `99999999.99` for a 9-digit-integer picture; the correct maximum
   is `999999999.99` — test expectation was wrong, implementation right. (2 min)
4. `ShippedDatasetRoundTripTest`: asserted account 1 balance `19400.00` from eyeballing a hex dump;
   decoded value is `194.00` (the `V99` implies two decimals I had skipped). Same for credit limit.
   Corrected the expectation from the copybook. (3 min)
5. Posting fixture helper called with (type, category, amount, date) in the wrong order — compile
   error. (1 min)
6. Spring context failed: `PostingRunState` was `final`, `@StepScope` needs a CGLIB subclass.
   Removed `final`. (2 min)
7. `InterestPostingRunTest`: compared `"System"` to the fixed-width `"System    "`. Trimmed in the
   assertion, not in the record (the record must stay 10 wide). (1 min)
8. `TransactionReportWriterTest`: used `endsWith` on total lines, but report records are padded to
   133 — assertions rewritten to check position, and detail spacing corrected against CVTRA07Y
   (one extra space after `TYPE-DESC` in the first attempt). (4 min)
9. First draft of the edited-picture zero case was written as `12 spaces + ".00"`; the standard rule
   for an all-`Z` picture is all spaces. Doc and test aligned. (2 min)

No iteration required rolling back a design decision; all were expectation or wiring errors.

## Post-PR review fixes (Devin Review on PR #9, ~25 min)

Three behavioural findings were valid and fixed; three were analysis notes answered on the PR.

10. Posting outputs were persisted in a `StepExecutionListener.afterStep`; a write failure there is
    logged by Spring Batch but does not fail the step, so the job could report success with partial
    datasets. Moved persistence into its own tasklet step (`closeDatasets`), added
    `PostingJobOutputFailureTest`. This *was* a design error, not a wiring error.
11. Rejected records were rebuilt from the decoded `Transaction`, so the 20-byte filler of the input
    image was replaced by spaces. The reader now carries the raw `FixedWidthRecord` alongside the
    decoded value (`DailyTransaction`) and the reject path copies it byte for byte; test added.
12. An empty DATEPARM threw and failed the report job, whereas CBTRN03C ends normally with an empty
    report (lines 235-236). Now writes an empty TRANREPT and completes; test added.

## Not done

* No pre-commit hooks installed — none are defined in the repository.
* No UI/browser testing — not applicable, no UI in scope.
* Nothing outside the CBTRN02C → CBACT04C / CBTRN03C call graph was read in depth or modified.
