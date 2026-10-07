# ADR-0002: COBOL paragraph → camelCase Java method, original name in Javadoc

- Status: Accepted (UNT51-5, 2026-10-07)
- Applies to: `modernization/carddemo-app`

## Decision
- Each COBOL paragraph (or `SECTION`) that carries logic becomes a method named by lowerCamelCase of its name,
  dropping the numeric prefix and the `-EXIT` paragraph: `1200-EDIT-MAP-INPUTS` → `editMapInputs()`,
  `9000-READ-ACCT` → `readAcct()`. Keep COBOL abbreviations (`acct`, `xref`, `tran`) so names grep across languages.
- The method Javadoc starts with the original name and program: `/** COACTUPC 1200-EDIT-MAP-INPUTS. ... */`.
- `PERFORM X THRU X-EXIT` is one call. `PERFORM UNTIL` becomes a loop in the caller. `GO TO` inside a paragraph
  becomes an early `return`; `GO TO` between paragraphs is restructured, never emulated with flags or labels.
- Paragraphs that only move fields between the map and working storage may be folded into a mapper method; keep
  the paragraph names in its Javadoc.
- Name clashes after dropping the prefix (e.g. two `editAccount`) keep the number: `editAccount1210()`.

## Why
Reviewers and the parity tests trace a Java method back to the COBOL paragraph listed in
`docs/modernization/rules/<PGM>.md` without a lookup table.
