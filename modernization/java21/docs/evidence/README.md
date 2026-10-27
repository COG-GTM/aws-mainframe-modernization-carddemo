# Regenerating the classified evidence

These are differential results on synthetic fixtures, not an SME approval. `mixed`
is the **named** set; `volume` is the seeded volume set.

From a clean checkout with the prerequisite branches present:

```sh
python3 modernization/java21/docs/evidence/generate.py
```

Prerequisites are the [golden harness prerequisites](../../golden/README.md)
(GnuCOBOL 3.1 with BDB, GCC, Bash, Python 3 and `faketime`), JDK 21 and Maven.
Maven resolves the pinned dependencies in the parent POM. If Central returns
HTTP 429, use the existing plan's Central mirror in local Maven settings; do
not commit machine-specific settings. There are no new Python dependencies.

The command runs its own comparator regression tests, regenerates the COBOL
golden set in `golden/.build/generated`, checks every generated golden file
against the committed acceptance manifest, runs the complete Maven reactor
with `clean verify` (including interest/posting differentials and packaged-jar
integration tests), then independently exports both Java services and runs the
packaged Spring Batch jar on fresh copies of both sets. The jar clock and PARM
come from the freshly generated metadata. It never updates the accepted golden
set, modifies the original COBOL, or edits the SME checklist.

## Artifacts

- [REPORT.md](REPORT.md): generated human-readable counts, individual rejects,
  differences, disabled/non-passing tests, provenance, trace links and gaps,
  and current SME checklist status.
- [report.json](report.json): the same evidence plus full test identities,
  source commit/tree and generator hashes, file hashes and every difference.
- `comparisons.csv.gz`: deterministic gzip (no filename or timestamp), one row
  for every record/line and every copybook field compared, including fillers.
  Reject payloads are expanded into their individual daily-transaction fields.
  The CSV carries the complete expected and actual raw bytes as hex, even for
  matches. Use `gzip -dc modernization/java21/docs/evidence/comparisons.csv.gz`
  to inspect it. Its SHA-256 is in both reports.

Record numbers are one-based positions in the exact fixed-length output order;
primary keys are retained in the field bytes. Missing and extra records are
compared over the union of positions, never the shorter list. Partial trailing
records, final newlines, high bytes, signed zero and filler differences are not
normalized. A record row is classified independently of its individual fields.
The totals therefore count record/line **and** field rows, not unique business
transactions. Batch master rows use the final CBACT04C snapshot names, including
masters that interest only reads; standalone posting covers its intermediate
account and category states.

Every equal byte slice is `match`; every difference is `unresolved`, with both
values and a reason retained without truncation. There are **no intended
changes** approved by this script and no mismatch suppression list. Introducing
one requires an explicit catalog rule ID, written reason and regression tests;
do not turn a mismatch into an unreviewed baseline update. The gate also fails
on non-passing tests, disabled annotations, partial records or wrong jar exits.
The five highlighted semantic decisions remain SME-pending even when all bytes
match; trace links are not a branch-coverage measurement or approval.

## Reproducibility

Run twice into separate destinations and byte-compare all generated artifacts:

```sh
python3 modernization/java21/docs/evidence/generate.py --output /tmp/evidence-a
python3 modernization/java21/docs/evidence/generate.py --output /tmp/evidence-b
diff -r /tmp/evidence-a /tmp/evidence-b
```

For the same committed sources, generator and toolchain, all three artifacts
are byte-identical. No wall-clock timestamp or elapsed duration is emitted.
Provenance records last-touch commit SHAs and tree/blob IDs for every input
subtree, and hashes of the generator sources, instead of a self-referential
report commit SHA. Inputs outside this evidence directory must be committed
and clean. Different source/tool versions intentionally change provenance.

Use `--work /absolute/new-directory` to retain raw Maven/golden/jar logs and
all exported datasets outside the checkout; the directory must not exist.
Otherwise scratch outputs are automatically removed. Command failures stop
generation; compare the exit code before trusting earlier committed reports.
Completed comparisons with mismatches are published with a failing gate and
nonzero exit code. Missing build artifacts or output files fail loudly rather
than being omitted from a purported passing report.

Run only the generator regressions:

```sh
python3 -m unittest discover -s modernization/java21/docs/evidence -p 'test_*.py' -v
```
