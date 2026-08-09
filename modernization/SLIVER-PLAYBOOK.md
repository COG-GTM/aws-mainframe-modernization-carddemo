# Sliver Modernization Playbook

How to take one COBOL batch program off the mainframe and into idiomatic Java, **from a static code
export only**, with a parity harness that proves the result. This is the codified version of the
CBACT04C engagement in `modernization/CBACT04C-logic-map.md`; the next sliver should follow it
step for step and cost materially less.

## Parameters

Fill these in before you start. The CBACT04C run and a worked example for the next sliver:

| Parameter | CBACT04C (done) | `CBTRN02C` (next) |
| --- | --- | --- |
| `PROGRAM` | `CBACT04C` | `CBTRN02C` |
| `DOMAIN` | monthly interest accrual | daily transaction posting |
| `JOB` | `app/jcl/INTCALC.jcl` | `app/jcl/POSTTRAN.jcl` |
| `INPUT_DDS` | `TCATBALF`, `XREFFILE`/`XREFFIL1`, `ACCTFILE`, `DISCGRP` | `DALYTRAN`, `TRANSACT`, `XREFFILE`, `ACCTFILE`, `TCATBALF` |
| `OUTPUT_DDS` | `ACCTFILE` (rewrite), `TRANSACT` (new) | `TRANSACT`, `ACCTFILE`, `TCATBALF`, `DALYREJS` |
| `COPYBOOKS` | `CVTRA01Y`, `CVACT03Y`, `CVTRA02Y`, `CVACT01Y`, `CVTRA05Y` | `CVTRA06Y`, `CVTRA05Y`, `CVACT03Y`, `CVACT01Y`, `CVTRA01Y` |
| `MODULE` | `modernization/interest-service` | `modernization/posting-service` |
| `PACKAGE` | `com.carddemo.interest` | `com.carddemo.posting` |
| `DATA_DIR` | `app/data/EBCDIC` | `app/data/EBCDIC` |

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
   explanation of zoned vs packed decimal and of truncation-not-rounding.
4. Numbered business rules `BR-1…BR-n`. Each one: what the business would say, then the
   `file:line` citation. One rule per paragraph of behaviour, not per COBOL paragraph.
5. **Defects and dead code as first-class rules.** If the COBOL does something wrong or does
   nothing (CBACT04C: the unposted final account, the empty fee routine), say so, cite it, and
   state what the Java does about it.
6. Assembler / sub-program dependency table, including "not reachable".
7. Rule → COBOL → Java index table (fill the Java column in Phase 3).
8. **Questions the static export cannot answer.** Every disconnected engagement has them; listing
   them is what makes the rest credible.

**Exit:** every business rule has a number, a citation, and a sentence a non-programmer understands.

---

## Phase 3 — The idiomatic Java module (one to two days)

Create `MODULE` as a Java 21 Maven module, package `PACKAGE`. Layer it like this — the CBACT04C
module is the template, and the `io` layer is **reusable as-is**:

| Package | Contents | Reuse for the next sliver |
| --- | --- | --- |
| `io` | `EbcdicText`, `ZonedDecimalCodec`, `PackedDecimalCodec`, `CobolFixedPoint`, `RecordLayout`, `FixedLengthRecordReader` | copy or depend on it unchanged |
| `io.layout` | one `RecordLayout` constant per copybook, length-checked at construction | add the new copybooks |
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
      (CBACT04C: all balances zero, all pricing groups blank). Keep the real datasets and synthesise
      the driving one, covering: positive, negative and zero amounts; the exact-key path and the
      fallback path; the suppression path (zero rate); a value that truncation and rounding disagree
      about (79.60 at 15% → 0.995 → **0.99**); and a capacity-stressing value.
- [ ] Assert `mismatched == 0` and `matched == compared` per output stream, and print any mismatching
      pair decoded to text so a failure is diagnosable in one run.
- [ ] Add unit tests for the record reader (round-trip every shipped record byte for byte) and one
      per business rule.
- [ ] Have the harness **generate** `modernization/PARITY-REPORT.md` — scenario, records processed,
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

## What made the CBACT04C sliver cheap, and will again

1. **The `io` layer is sliver-independent.** EBCDIC, zoned and packed decimal, COBOL truncation,
   fixed-length reading and `RecordLayout` are already written and tested. New copybooks are a
   dozen declarative lines each.
2. **The oracle pattern generalises.** Transliterate, compare encoded bytes, freeze the clock.
3. **The derived-population trick generalises.** Shipped demo data is almost always degenerate;
   synthesising the driving file while keeping the real reference files gives real coverage without
   inventing an entire universe.
4. **Naming defects rather than fixing them** keeps parity at 100% and turns each finding into a
   business conversation instead of a silent behaviour change.

## Anti-patterns, each of which cost time somewhere

- Translating paragraph by paragraph into methods ("JOBOL") — you inherit the defects *and* the
  structure, and you cannot review the result against the business.
- `double` for money, or `RoundingMode.HALF_UP` because it "looks right".
- Comparing decoded Java objects instead of encoded records — hides every formatting bug.
- Trusting the shipped dump to exercise the rules.
- Porting Assembler before checking whether anything calls it.
- Fixing a mainframe defect inside the port, so parity fails and nobody can tell whether the port
  or the fix is at fault.
