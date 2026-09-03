# Sliver Modernization Playbook

How to take one COBOL batch program off the mainframe and into idiomatic Java, **from a static code
export only**, with a parity harness that proves the result. This is the codified version of the
CBACT04C engagement (`modernization/CBACT04C-logic-map.md`) and then **re-run, unchanged in
shape, on CBTRN02C** (`modernization/CBTRN02C-logic-map.md`). Everything marked *(sliver 2)* below
is a generalisation the second run forced; the Effort delta at the end records what that run cost.

## Parameters

Fill these in before you start. The two completed runs:

| Parameter | CBACT04C (done) | CBTRN02C (done) |
| --- | --- | --- |
| `PROGRAM` | `CBACT04C` | `CBTRN02C` |
| `DOMAIN` | monthly interest accrual | daily transaction posting |
| `JOB` | `app/jcl/INTCALC.jcl` | `app/jcl/POSTTRAN.jcl` |
| `INPUT_DDS` | `TCATBALF`, `XREFFILE`/`XREFFIL1`, `ACCTFILE`, `DISCGRP` | `DALYTRAN`, `TRANSACT`, `XREFFILE`, `ACCTFILE`, `TCATBALF` |
| `OUTPUT_DDS` | `ACCTFILE` (rewrite), `TRANSACT` (new) | `TRANSACT`, `ACCTFILE`, `TCATBALF`, `DALYREJS` |
| `COPYBOOKS` | `CVTRA01Y`, `CVACT03Y`, `CVTRA02Y`, `CVACT01Y`, `CVTRA05Y` | `CVTRA06Y`, `CVTRA05Y`, `CVACT03Y`, `CVACT01Y`, `CVTRA01Y` |
| `MODULE` | `modernization/interest-service` | `modernization/transaction-posting-service` |
| `PACKAGE` | `com.carddemo.interest` | `com.carddemo.posting` |
| `DATA_DIR` | `app/data/EBCDIC` | `app/data/EBCDIC` |
| `SHARED_IO` | written inside `interest-service` | extracted to `modernization/carddemo-mainframe-io`, depended on by both |

Everything below is written against those names.

---

## Phase 0 — Frame the engagement (30 minutes)

- [ ] Confirm the constraint out loud: **no mainframe connectivity**. The static export is the only
      source of truth, and the deliverables must be defensible from it alone.
- [ ] Confirm the acceptance criterion up front: 100% record-level parity on every output dataset,
      committed as a report.
- [ ] Confirm you are modernising a *sliver*: one program, its data, its job. Resist the pull to
      port neighbouring programs.

**Exit:** the sliver's inputs, outputs and job are named.

---

## Phase 1 — Discovery, before any code (half a day)

- [ ] Read `PROGRAM` end to end once without taking notes. Then again, listing its paragraphs
      (`grep -nE "^ {7}[0-9A-Z-]+\." app/cbl/PROGRAM.cbl`).
- [ ] Classify every paragraph as **business rule**, **file plumbing**, or **abend/diagnostics**.
      Only the first class survives into Java; the other two become repositories and exceptions.
- [ ] Read the `SELECT` clauses: organisation, access mode, record key, **alternate keys**. An
      alternate index is a business relationship (here: account → card) and a design decision in
      Java.
- [ ] Read `JOB`: the `PARM`, the DD-name → dataset mapping, `RECFM`/`LRECL` on outputs, and where
      the job sits in the scheduler (`app/scheduler/CardDemo.controlm`).
- [ ] Expand every copybook: field, PIC, `COMP-3` or `DISPLAY`, offsets, total record length. Check
      the length against the actual dump size (`stat -c%s` divided by the record length must be a
      whole number). This is your first parity test and it is free.
- [ ] Grep the whole of `app/cbl` for `CALL` to find Assembler and sub-program dependencies, and
      **write down the ones that turn out not to exist**. Proving an absence is a deliverable.
- [ ] *(sliver 2)* Note which DDs are **read and written**. In-place updates change three things:
      the run stops being idempotent, an abend leaves partial state, and the harness must compare
      the updated datasets as output streams in key order, not just the newly written files.
- [ ] *(sliver 2)* Diff the input and output copybooks. If they are the same layout (`CVTRA06Y` vs
      `CVTRA05Y`) the "transformation" is a copy and the only interesting field is the one the
      program overwrites. Establishing that early collapses a day of field mapping into a sentence.
- [ ] Inspect the shipped data before trusting it: decode a few records
      (`open(f,'rb').read().decode('cp037')`) and check the distribution of the fields the rules key
      on. On CBACT04C this revealed every balance is zero and every pricing group is blank — which
      changed the whole harness design (see Phase 4).

**Exit:** you can explain the program to a business person without opening the source again.

---

## Phase 2 — The logic map (half a day)

Write `modernization/PROGRAM-logic-map.md` **for a business SME, not an engineer**. Reuse the
CBACT04C structure:

1. One-paragraph summary of what the program is for.
2. JCL job context: job, step, `PARM`, DD → dataset table, schedule position.
3. Data structures: one table per copybook — field, PIC, business meaning, Java type — plus a short
   explanation of zoned vs packed decimal and of truncation-not-rounding. *(sliver 2)* If the
   sliver contains **no** `COMP-3`, write that down and say how you checked: on a disconnected
   engagement an absence has to be proved, and it is the first thing a reviewer asks about.
4. Numbered business rules `BR-1…BR-n`. Each one: what the business would say, then the
   `file:line` citation. One rule per paragraph of behaviour, not per COBOL paragraph.
5. *(sliver 2)* **Validation ordering is a rule.** Where several checks each overwrite one reason
   code, the *last* failure wins and the earlier ones are invisible downstream. Number that as its
   own rule with its own defect sub-rule; an SME can act on it immediately.
6. **Defects and dead code as first-class rules.** If the COBOL does something wrong or does
   nothing (CBACT04C: the unposted final account, the empty fee routine), say so, cite it, and
   state what the Java does about it.
7. Assembler / sub-program dependency table, including "not reachable".
8. Rule → COBOL → Java index table (fill the Java column in Phase 3).
9. **Questions the static export cannot answer.** Every disconnected engagement has them; listing
   them is what makes the rest credible.

**Exit:** every business rule has a number, a citation, and a sentence a non-programmer understands.

---

## Phase 3 — The idiomatic Java module (one to two days)

Create `MODULE` as a Java 21 Maven module, package `PACKAGE`. Layer it like this — the CBACT04C
module is the template, and the `io` layer is **reusable as-is**:

| Package | Contents | Reuse for the next sliver |
| --- | --- | --- |
| `carddemo-mainframe-io` (its own module) | `EbcdicText`, `ZonedDecimalCodec`, `PackedDecimalCodec`, `CobolFixedPoint`, `RecordLayout`, `FixedLengthRecordReader`, `Db2TimestampFormatter` | **depend on it, never copy it.** Sliver 2 promoted this out of `interest-service` under a reactor `pom.xml`. Promote on the *second* sliver, not the first: by then you know what is genuinely sliver-independent |
| `io.layout` | one `RecordLayout` constant per copybook, length-checked at construction | lives in the shared module; add the new copybooks |
| `io.codec` | copybook ↔ domain object, both directions | new |
| `domain` | immutable records with business meaning; value objects for keys | new |
| `repository` | interfaces plus in-memory implementations over the dataset images | new, same shape |
| `rules` | one class per business rule or cohesive rule family | new |
| `service` | the cycle/flow that orders the rules | new |
| `batch` | dataset loading and the job entry point | new, same shape |
| `exception` | one typed exception per failure mode | new, same shape |

Rules of construction, each of which prevents a specific failure mode:

- [ ] **No God class.** If a class has a method named after a COBOL paragraph, delete the class and
      start again. The flow class orders rules; it does not contain them.
- [ ] **`BigDecimal` with explicit scales everywhere.** `double` is a defect. Encode COBOL semantics
      once in a `CobolFixedPoint`-style helper: truncate toward zero, and drop excess high-order
      digits the way an unguarded `MOVE`/`COMPUTE` does.
- [ ] **Immutable domain objects.** A posting returns a new object (`withInterestPosted`), it does
      not mutate a record area.
- [ ] **Exceptions, not file-status codes.** One base exception per sliver, one subclass per failure
      mode. `9999-ABEND-PROGRAM` has no Java equivalent beyond "throw".
- [ ] **Inject the clock and every other ambient input.** `FUNCTION CURRENT-DATE` becomes a
      `java.time.Clock` parameter, or the harness cannot be deterministic.
- [ ] **JavaDoc every business-rule method with its COBOL citation** (`app/cbl/PROGRAM.cbl:462-468`)
      and its `BR-n` number. This is the audit trail that makes the port reviewable by someone who
      only trusts the COBOL.
- [ ] **Name known defects.** Make each one an explicit enum/flag with both behaviours implemented
      and the mainframe-compatible one as the default (`FinalAccountPolicy`). Never silently "fix"
      and never silently copy.
- [ ] **Do not implement what the COBOL does not.** An empty paragraph stays empty in Java.
- [ ] *(sliver 2)* **Keep the raw record image for any dataset you rewrite in place.** A COBOL
      `REWRITE` emits the whole record area, so every byte the program never touched — `FILLER`,
      fields it does not read — must come back out unchanged. Decode to a domain object for the
      rules, then re-encode *into a clone of the bytes you read*. Re-encoding from scratch passes
      every unit test and fails parity on filler.
- [ ] *(sliver 2)* **A rejected record is a business outcome, not an exception.** Model validation
      as a sealed result type (`Accepted`/`Rejected`, the reason an enum carrying the COBOL code
      and its exact description text) and keep exceptions for what the COBOL abends on. Getting
      this backwards turns a rejects file into a stack trace.
- [ ] *(sliver 2)* **Preserve the order in which outputs are written.** Category balance, then
      account, then transaction master: each write can abend leaving the earlier ones applied.
      Reproduce the order even where it looks arbitrary, and record the absence of a commit scope
      as a risk in the logic map.

**Exit:** `mvn compile` is clean and no class knows both how to decode a record and how to price it.

---

## Phase 4 — The black-box parity harness (half a day)

The oracle problem: with no mainframe there is no captured output to compare against. Solve it with
**two independent readings of the same COBOL**.

- [ ] Write a deliberately literal transliteration of `PROGRAM` in the test sources
      (`Cbact04cReferenceModel` is the template): one class, paragraph-named methods, offsets, a
      mutable record area. This is the only place the "JOBOL" style is allowed.
- [ ] Give the oracle **its own** EBCDIC and decimal handling. If it calls the production record
      reader, a reader bug cancels itself out and the harness proves nothing.
- [ ] Compare **encoded output records byte for byte**, not Java objects: that puts field
      formatting, padding, sign overpunches and truncation in scope.
- [ ] Freeze every non-deterministic input identically on both sides (clock, run-date `PARM`).
- [ ] **Scenario 1 — shipped population.** Run over the whole of `DATA_DIR`, unmodified.
- [ ] **Scenario 2 — derived population.** The shipped data usually cannot reach the arithmetic
      (CBACT04C: every balance zero, every pricing group blank. CBTRN02C: one reject reason out of
      four, not one negative amount). *(sliver 2)* Prefer **patching the real datasets** — flip a
      sign, point a cross-reference at a missing key, expire an account — over synthesising
      records: patches are a dozen lines, stay realistic, and cannot invent a layout that never
      occurs. Cover every branch and reason code, both signs and zero, the create path and the
      update path, and a value truncation and rounding disagree about.
- [ ] *(sliver 2)* **Scenario 3 — capacity overflow.** Drive one key hard enough that a receiving
      field overflows (300 × 999,999,999.99 into `PIC S9(09)V99`). Unguarded COBOL drops the
      high-order digits silently, so this is the scenario most likely to catch a naive port — and
      assert the *expected truncated value*, not merely that the two engines agree, or a scenario
      that quietly never overflowed will look like it passed.
- [ ] *(sliver 2)* Compare **every** output stream, including the datasets updated in place, in the
      key order an unload would produce. Compare the run's counters too — records read, records
      created, step return code — because they catch whole-run drift that record comparison misses.
- [ ] *(sliver 2)* Keep the oracle **structurally** different from production, not just separate:
      scaled `long` cents against `BigDecimal`, its own sign-overpunch handling, its own offsets.
      Two implementations of one idea agreeing proves much less than two different ideas agreeing.
- [ ] Assert `mismatched == 0` and `matched == compared` per output stream, and print any mismatching
      pair decoded to text so a failure is diagnosable in one run.
- [ ] Add unit tests for the record reader (round-trip every shipped record byte for byte) and one
      per business rule.
- [ ] Have the harness **generate** `modernization/PARITY-REPORT-PROGRAM.md` — scenario, records processed,
      matched, mismatched, match rate — so the report can never drift from the code.

**Exit:** `mvn test` green, report at 100%.

---

## Phase 5 — Hand over (an hour)

- [ ] One PR containing logic map, module, harness, report and this playbook.
- [ ] PR description carries the parity table inline; a reviewer should not have to run anything.
- [ ] Call out, at the top, every place the Java deliberately differs from or preserves a mainframe
      defect.
- [ ] Add the open questions from the logic map to the PR as the business follow-ups they are.

---

## Effort delta: sliver 1 (CBACT04C) → sliver 2 (CBTRN02C)

The repeatability claim, concretely. CBTRN02C is the *larger* program — six datasets against five,
four of them written, two of those updated in place, four rejection paths — and it still cost
materially less, because the expensive parts of sliver 1 turned out to be the reusable ones.

| Area | Reused from sliver 1 | Net-new for sliver 2 |
| --- | --- | --- |
| EBCDIC / zoned / packed decimal / `CobolFixedPoint` / `RecordLayout` / `FixedLengthRecordReader` | **100% reused, no behaviour changes.** Promoted into `carddemo-mainframe-io` and depended on | the promotion itself: a reactor `pom.xml`, a module `pom.xml`, package renames in sliver 1. One-off, now amortised |
| `RecordLayout` capability | field kinds, offsets, length validation at construction | one addition: `RAW` fields with `raw`/`putRaw`, needed to carry a rejected record through verbatim (~20 lines, now shared) |
| Copybook layouts | `CVACT01Y`, `CVACT03Y`, `CVTRA01Y`, `CVTRA05Y` already declared and proven | `CVTRA06Y` and the 430-byte reject record: ~25 declarative lines |
| `Db2TimestampFormatter` | the identical paragraph appears in both programs — reused verbatim | none |
| Logic map | structure, section order, citation style, the "questions we cannot answer" section | the content: 13 rules and 3 newly found defects |
| Java module shape | `domain`/`repository`/`rules`/`service`/`batch`/`exception` layering, immutability, injected clock, JavaDoc-with-citation convention | the business logic itself: ~35 classes, none of them a paragraph translation |
| Parity harness | the whole pattern — transliterated oracle, byte-level comparison of encoded records, frozen clock, generated report | the oracle (~250 lines) and the patched populations (~130 lines) |
| Report format | scenario / compared / matched / mismatched table, generated by the test run | extended to per-stream rows, because this sliver has four output streams |
| Playbook | followed step for step; no phase skipped or reordered | the *(sliver 2)* items above, all of which come from in-place updates and multi-stream output |

**What changed in the procedure itself:** nothing structural. The five phases held. Every addition
above is a new checklist item inside an existing phase — which is the actual repeatability result:
the second sliver refined the playbook instead of rewriting it.

**Where the time went on sliver 2:** discovery and the logic map (unavoidable, and the part the SME
actually reads), then the domain model. The mechanical work that dominates a *first* mainframe
port — character sets, sign overpunches, packed decimal, fixed-point truncation, harness
architecture, report generation — was a dependency line in a `pom.xml`.

**Extrapolation for the workshop:** the cost of sliver *n* is the logic map plus the oracle, both
proportional to the program's rule count rather than to its I/O complexity, because the shared
module absorbs the I/O complexity. A third sliver reuses `carddemo-mainframe-io` untouched again,
and the first sliver that meets `COMP-3` on disk will find the codec already written and tested.

---

## What made the CBACT04C sliver cheap, and made CBTRN02C cheaper

1. **The `io` layer is sliver-independent.** EBCDIC, zoned and packed decimal, COBOL truncation,
   fixed-length reading and `RecordLayout` are already written and tested. New copybooks are a
   dozen declarative lines each.
2. **The oracle pattern generalises.** Transliterate, compare encoded bytes, freeze the clock.
3. **The derived-population trick generalises.** Shipped demo data is almost always degenerate;
   synthesising the driving file while keeping the real reference files gives real coverage without
   inventing an entire universe.
4. **Naming defects rather than fixing them** keeps parity at 100% and turns each finding into a
   business conversation instead of a silent behaviour change. Sliver 2 found three more this way
   (BR-4a, BR-6a, BR-10a) without a single parity failure.
5. *(sliver 2)* **One reactor, one `mvn test`.** Both slivers build and prove themselves with a
   single command, so a reviewer runs one thing and sliver 1 cannot silently regress while sliver 2
   is being written.

## Anti-patterns, each of which cost time somewhere

- Translating paragraph by paragraph into methods ("JOBOL") — you inherit the defects *and* the
  structure, and you cannot review the result against the business.
- `double` for money, or `RoundingMode.HALF_UP` because it "looks right".
- Comparing decoded Java objects instead of encoded records — hides every formatting bug.
- Trusting the shipped dump to exercise the rules.
- Porting Assembler before checking whether anything calls it.
- Fixing a mainframe defect inside the port, so parity fails and nobody can tell whether the port
  or the fix is at fault.
- *(sliver 2)* Re-encoding a rewritten record from its decoded fields, losing the filler bytes the
  mainframe would have written straight back out.
- *(sliver 2)* Writing a stress scenario without asserting the value it is meant to produce: a
  scenario that never actually overflowed passes silently and proves nothing.
- *(sliver 2)* Copying the shared I/O layer into the new module "just for now". It never gets
  merged back, and the copies diverge on the first bug fix.
