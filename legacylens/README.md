# LegacyLens

A web app that shows what a Devin scan and a source parse reveal about the CardDemo mainframe estate, and tracks its migration.

- **Estate map**: every program, job, transaction, screen, copybook and data store, with the references between them (COPY, CALL, EXEC CICS, EXEC SQL, JCL DD, Control-M). Click a component to trace upstream and downstream.
- **Source**: all tracked files, with the lines that carry a parsed connection marked.
- **Decomposition**: per program, the scan's logic and flow document, the business-rules catalog, and its connections.
- **Scan findings**: the 211 findings of migration-docs scan `scan-dcf9da1645664298850b3547a8b50fca`.
- **Approach**: a proposed route per component (refactor, rewrite, replace with COTS/GOTS, retire), where to start, and areas to focus on.
- **Tracker**: stage per program, derived from merged pull requests and editable; a ready-to-paste next prompt for Devin.
- **Compliance**: NIST SP 800-53 Rev 5 control-gap findings from a compliance scan of a second COBOL repository.
- **Evidence**: which claims are repository-derived, scan-derived, proposed, or not demonstrated.

## Run

```
cd legacylens
npm ci
npm start            # http://localhost:4310 (PORT to change)
```

Needs Node 22.13 or later (uses the built-in `node:sqlite`). Tracker edits are stored in `~/legacylens-data/tracker.db` (`LEGACYLENS_DATA` to change).

## Rebuild the data

`data/*.json` is generated; do not edit it by hand.

```
gh pr list --state all --limit 400 --json number,title,state,headRefName,baseRefName,url,createdAt,mergedAt,additions,deletions > raw/prs.json
python3 tools/build_data.py
```

Inputs: the repository source, `raw/findings.txt` and `raw/compliance.txt` (scan findings exported from Devin), the rules catalog on branch `devin/cd-107-business-rules-catalog` (fetch it first), and `raw/prs.json`.

The estate map comes from static parsing. Program names or file names assembled at run time can be missed.
