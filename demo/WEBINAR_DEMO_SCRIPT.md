# CardDemo — Webinar Demo Runbook (COBOL → Python, AskDevin/Devin)

> Presenter-facing teleprompter script for a 60-minute webinar.
> Target program: `app/cbl/CBACT04C.cbl` (the `INTCALC` interest calculator).
> Target language: **Python**. Flow: ask-only AskDevin → analysis → fixtures → harness → playbook → live migration.
> Paste the fenced blocks verbatim into AskDevin/Devin. Keep your own talk to the one-line cues.

---

## Timing (60 min, ~25 min live migration)

| # | Segment | Min | What you show |
|---|---------|-----|---------------|
| 1 | Connect repo + DeepWiki (ask-only) | 6 | DeepWiki wiki + auto system map of CICS/batch/VSAM/BMS |
| 2 | Select & analyze CBACT04C | 5 | Devin confirms scope + I/O contract of the program |
| 3 | COBOL semantics to preserve | 6 | Devin lists decimal/precision/layout rules that must survive |
| 4 | Golden-file fixtures | 6 | `test-data/` input + derived expected, edge cases, masked timestamps |
| 5 | Parity/characterization harness | 6 | pytest harness + green CI giving clean pass/fail |
| 6 | Reusable Playbook | 3 | `PLAYBOOK.md` Analyze/Propose/Convert/Validate + Risks |
| 7 | Snapshot + backup PR | 3 | Clean machine, harness green, backup branch ready |
| 8 | **LIVE modernization ask** | 25 | One prompt → Devin migrates CBACT04C → Python, gated on harness |

---

## SEGMENT 1 — Connect repo + DeepWiki (ask-only AskDevin) · ~6 min

**1.1 Point DeepWiki/Devin at the repo and build the wiki**
```
Index the repository COG-GTM/aws-mainframe-modernization-carddemo with DeepWiki and
generate the wiki. This is the AWS CardDemo mainframe credit-card system (COBOL/CICS/JCL/VSAM).
Give me the high-level architecture: online vs batch, the data stores, and how a transaction
flows from a 3270 screen through to the account master.
```
- Cue: "No setup — we just point Devin at the codebase and let it read."
- Expect: DeepWiki wiki + a plain-language architecture summary of CardDemo.

**1.2 Ask for a concrete system map referencing real artifacts**
```
Produce a system map of CardDemo with four tables, using the actual artifacts in the repo:
1. Online CICS transactions → programs (e.g. CC00 → COSGN00C signon, CT02 → COTRN02C
   transaction add) from the README transaction table.
2. Batch jobs → programs (e.g. INTCALC → CBACT04C interest calc, POSTTRAN → CBTRN02C
   transaction posting) from app/jcl/.
3. VSAM/sequential files and the copybooks that define them
   (CVACT01Y account, CVACT02Y card, CVCUS01Y customer, CVACT03Y card-xref,
   CVTRA05Y transaction, CVTRA02Y disclosure-group, CVTRA01Y tran-cat-balance).
4. BMS screen maps in app/bms/ and the transactions they back.
Cite the file paths for each row.
```
- Cue: "Watch it cross-reference JCL, copybooks, and BMS maps on its own."
- Expect: four tables with real names + `app/...` paths, no hand-waving.

---

## SEGMENT 2 — Select & analyze the target program · ~5 min

**2.1 Confirm CBACT04C is a good live-migration target**
```
Read app/cbl/CBACT04C.cbl. Confirm it is self-contained (no CALLs to other business
programs except the CEE3ABD abend service) and small enough for a 20–30 minute live
migration. Tell me its line count, the paragraphs it contains, and why it is or isn't
a clean standalone unit.
```
- Cue: "We pick the interest calculator — one program, clear math, real files."
- Expect: ~650 lines, paragraphs `0000`–`9999` + `Z-GET-DB2-FORMAT-TIMESTAMP`; confirmed self-contained.

**2.2 Summarize the I/O contract**
```
Summarize CBACT04C's inputs and outputs precisely:
- Reads TCATBALF (tran-category-balance, copybook CVTRA01Y) sequentially as the driver file.
- Reads XREFFILE (card xref, CVACT03Y), DISCGRP (disclosure group, CVTRA02Y),
  and ACCTFILE (account master, CVACT01Y) — ACCTFILE is opened I-O.
- Writes TRANSACT (transaction file, CVTRA05Y) and REWRITEs the account record.
For each file give the copybook, key, access mode, and what fields the program reads or writes.
```
- Cue: "One driver file in, interest transactions out, account balance updated in place."
- Expect: table of 5 files with copybook/key/mode matching the FILE-CONTROL section.

---

## SEGMENT 3 — COBOL semantics to preserve · ~6 min

**3.1 Enumerate the must-preserve behaviors (human-language verification)**
```
List every COBOL semantic in CBACT04C that the Python port MUST preserve exactly. Cover:
- Decimal precision: all PIC S9(n)V99 / packed fields become Python decimal.Decimal,
  never float (TRAN-CAT-BAL S9(09)V99, DIS-INT-RATE S9(04)V99, ACCT-CURR-BAL S9(10)V99).
- The interest formula COMPUTE WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200
  has NO ROUNDED clause → truncate (not round) to 2 decimals.
- Signed fields keep their sign (S9(09)V99, S9(04)V99, S9(10)V99).
- REDEFINES: the DB2-FORMAT-TS timestamp work area built from COBOL-TS via redefinition.
- The TRAN-RECORD is fixed-width 350 bytes — preserve every field offset and width.
- Sequential read order of TCATBALF and the account-break logic (WS-LAST-ACCT-NUM):
  interest accumulates per account, account is updated when the account id changes.
- 88-levels APPL-AOK (0) and APPL-EOF (16), and the file-status handling around them.
- WORKING-STORAGE defaults, especially WS-FIRST-TIME VALUE 'Y' (first-account guard).
- Disclosure-group fallback: file status '23' (not found) → set group id to 'DEFAULT'
  and re-read (paragraph 1200-A-GET-DEFAULT-INT-RATE).
For each item, state the failure mode if a naive port gets it wrong.
```
- Cue: "This is where migrations usually break — Devin states the invariants before touching code."
- Expect: itemized list mapping each COBOL construct to a Python rule + the risk if violated.

---

## SEGMENT 4 — Golden-file fixtures · ~6 min

**4.1 Create the fixture tree from the shipped ASCII data**
```
Create a test-data/ directory for CBACT04C parity testing, using app/data/ASCII/ as the
fixture base (tcatbal.txt, discgrp.txt, cardxref.txt, acctdata.txt). Build:
- test-data/input/  : tcatbalf, discgrp, xref, acct  (trimmed to a small, readable set of accounts)
- test-data/expected/: transact, acct.out  (the program's outputs for that input)
Keep the exact fixed-width record layouts from the copybooks (CVTRA01Y 50, CVTRA02Y 50,
CVACT03Y 50, CVACT01Y 300, CVTRA05Y 350).
```
- Cue: "We seed it with the project's own ASCII data — real records, not invented ones."
- Expect: `test-data/input/` + `test-data/expected/` created with correctly-sized records.

**4.2 Add the edge-case fixtures (plain language)**
```
Add fixture cases that exercise these edge conditions, each as its own small input set:
- zero balance account
- negative balance account
- max precision balance 9999999999.99
- one account with multiple tran-category rows (forces the account-break + update path)
- empty input file (no records at all)
- zero interest rate (the IF DIS-INT-RATE NOT = 0 skip path — no transaction written)
- missing XREF key (the 'ACCOUNT NOT FOUND' path)
- missing disclosure group (file status '23' → DEFAULT fallback)
Document what each case proves in a short README in test-data/.
```
- Cue: "The edge cases are the demo — zero, negative, max precision, account-break, fallbacks."
- Expect: one fixture per case + a `test-data/README` describing intent.

**4.3 Label derived outputs and mask non-deterministic timestamps**
```
The expected outputs are derived by hand from CBACT04C's logic, not run on z/OS. At the top
of every file in test-data/expected/ add the marker:
  # DERIVED FROM CBACT04C LOGIC — NOT EXECUTED ON Z/OS
TRAN-RECORD has two X(26) timestamp fields (TRAN-ORIG-TS at offset 304, TRAN-PROC-TS at
offset 330) set from the current timestamp — they are non-deterministic. Define a masking
rule that blanks those 26-byte windows in both actual and expected before byte comparison.
```
- Cue: "We're honest about provenance and we mask the only fields that can't be deterministic."
- Expect: provenance marker present + a documented timestamp-mask rule for the two X(26) fields.

---

## SEGMENT 5 — Parity / characterization harness · ~6 min

**5.1 Write the parity harness**
```
Write a pytest parity harness for the migrated CBACT04C that, for each fixture in test-data/:
- runs the Python program against test-data/input/
- compares numeric outputs as decimal.Decimal at scale 2, sign-aware (value-identical, not
  string-identical) — interest amounts and the updated account balance
- compares the TRANSACT and account outputs byte-for-byte against test-data/expected/,
  except the two masked X(26) timestamp windows
- fails with a readable diff (record number + field) on any mismatch
- exits non-zero if any fixture fails
Make the harness data-driven so new fixtures are picked up automatically.
```
- Cue: "This harness is Devin's own grader — value-identical decimals, byte-identical records."
- Expect: pytest suite, one parametrized case per fixture, non-zero exit on mismatch.

**5.2 Wire it into CI**
```
Add a GitHub Actions workflow that installs Python deps and runs the CBACT04C parity harness
on push and pull_request, producing a clean pass/fail check. The goal is a green/red signal
Devin can self-verify against during the live migration.
```
- Cue: "Green check = parity. That's the gate for everything that follows."
- Expect: `.github/workflows/...` running the harness with a visible pass/fail.

---

## SEGMENT 6 — Reusable Playbook · ~3 min

**6.1 Author PLAYBOOK.md**
```
Write a PLAYBOOK.md at the repo root for migrating a CardDemo batch COBOL program to Python.
Phases, each ending in a "Validation:" line stating how to prove that phase is done:
1. Analyze — read the program + copybooks, capture the I/O contract and semantics.
2. Propose structures — map each copybook to a Python fixed-width record (field, PIC,
   offset, width, decimal scale).
3. Convert — port the procedure logic, decimals as decimal.Decimal, truncation not rounding.
4. Validate — run the Segment 5 parity harness; all fixtures green.
End with a "Risks" section: decimal precision, record-layout fidelity (350-byte TRAN-RECORD),
and file semantics (sequential read order, account-break, I-O rewrite, '23'→DEFAULT fallback).
```
- Cue: "We capture the method as a playbook so the next 50 programs follow the same path."
- Expect: `PLAYBOOK.md` with 4 phases (each with `Validation:`) + a `Risks` section.

---

## SEGMENT 7 — Snapshot + known-good backup PR · ~3 min

**7.1 Prepare a clean, ready machine**
```
Prepare a clean Devin machine snapshot for this demo: repo cloned, Python toolchain + deps
installed, test-data/ fixtures present, and the CBACT04C parity harness running green.
Confirm the harness passes from a fresh checkout.
```
- Cue: "Everything pre-warmed so the live run starts from green, not from setup."
- Expect: fresh-checkout harness run is green.

**7.2 Do a full dry run and save a backup branch**
```
Do one full end-to-end dry run: migrate CBACT04C.cbl to Python following PLAYBOOK.md until
the parity harness is fully green, then open a PR and also push it as a backup branch named
backup/cbact04c-python so we have a known-good result to fall back to live.
```
- Cue: "We rehearse the exact live ask once and keep the result in our back pocket."
- Expect: a green PR + `backup/cbact04c-python` branch ready as the fallback.

---

## SEGMENT 8 — THE LIVE MODERNIZATION ASK · ~25 min

**8.1 The single prompt you type on stage**
```
Modernize app/cbl/CBACT04C.cbl into Python.

Follow PLAYBOOK.md exactly (Analyze → Propose structures → Convert → Validate).

Preserve all COBOL semantics:
- all PIC S9(n)V99 / packed fields → decimal.Decimal, never float
- interest = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200, truncated (no rounding) to 2 decimals
- signed fields keep sign; sequential read order + account-break logic (WS-LAST-ACCT-NUM,
  WS-FIRST-TIME='Y') preserved
- TRAN-RECORD stays fixed-width 350 bytes with exact field offsets
- disclosure-group file status '23' → 'DEFAULT' fallback; zero-rate accounts write no transaction
- 88-levels APPL-AOK / APPL-EOF behavior preserved

Completion is gated on the test-data/ golden files passing the Segment 5 parity harness:
value-identical decimals (scale 2, sign-aware) and byte-identical 350-byte records except the
two masked X(26) timestamps. Do not report done until the harness and CI are green. Open a PR.
```
- Cue: "One prompt. It follows our playbook, preserves the invariants, and won't call itself done until parity is green."
- Expect: Devin migrates → harness + CI green → PR opened, live.

**8.2 Fallback (say it only if needed)**
```
If the live run stalls, stop and switch to the backup branch backup/cbact04c-python — it is the
known-good migration that already passes the parity harness. Show that PR instead.
```
- Cue: "If anything snags live, we cut to the rehearsed backup PR and keep moving."
- Expect: backup PR shown, parity already green.
