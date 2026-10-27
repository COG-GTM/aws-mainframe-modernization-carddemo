# Business rules: CBACT04C and CBTRN02C

This catalog lists the business rules of the two batch programs ported in this slice. The Java 21 ports under
`modernization/java21/` cite these rule ids.

| File | Program | Job | Rules |
|------|---------|-----|-------|
| [CBACT04C.md](CBACT04C.md) | `app/cbl/CBACT04C.cbl`: interest calculation | INTCALC | INT-01 to INT-20 |
| [CBTRN02C.md](CBTRN02C.md) | `app/cbl/CBTRN02C.cbl`: daily transaction posting | POSTTRAN | POST-01 to POST-19 |
| [SME-CHECKLIST.md](SME-CHECKLIST.md) | approval checklist, one row per rule | | 39 rows |

## Conventions

- **Id.** `INT-nn` or `POST-nn`. An id is stable once published. A dropped rule keeps its number and is marked
  withdrawn instead of being renumbered.
- **Class.** One of calculation, validation, file I/O, control flow or error handling.
- **Source.** Each cite has the form `path:line` or `path:start-end` and points at `main`. Every rule cites
  `app/cbl/` or `app/cpy/`. Some rules also cite JCL.
- **SME question.** The rule describes behaviour that looks unintended. The port reproduces it until the SME
  checklist says otherwise.
- **Coverage.** Each program file has a paragraph map, which lists every paragraph of the PROCEDURE DIVISION, and a
  branch map, which places every IF / ELSE / INVALID KEY / PERFORM UNTIL in a row. Each row maps to rule ids or
  says "no business logic".

The catalog was written from the source. The migration-docs code scan findings for these two jobs were used only as
hypotheses, and each one was checked against the lines cited here.

## Check

```sh
python3 modernization/java21/docs/business-rules/check_catalog.py --ref origin/main
```

The script checks that ids are unique and numbered without gaps, that every cite resolves against `origin/main`,
that every paragraph and decision line is mapped, that the checklist lists every rule id exactly once, and that
none of the files contains a banned claim word. Each branch-map row must sit inside the paragraph it names, start on a
decision line and not overlap another row, so one catch-all range cannot stand in for the real mapping. Before the
real check, a self-test confirms that tampered copies are rejected: a catch-all range, a cite past the end of the
file, and an unknown rule id. Add `--show` to print the source lines behind each rule's cites
for review.
