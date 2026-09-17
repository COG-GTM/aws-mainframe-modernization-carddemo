# Estate discovery dossier

**Headline: the generator resolved 547 distinct dependency edges and could not resolve 126** (categories: program→copybook, program→dataset, JCL step→program, transaction→program, program→program; distinct per category/from/to). Every number in this folder is computed by `build_discovery.py` from the source under `app/`; none is hand-typed.

## What is in this folder

| File | Contents | Produced by |
| --- | --- | --- |
| `build_discovery.py` | Standard-library Python 3 generator: parses the source trees, links dependencies, writes every file below and refreshes the marked blocks in the two authored files | — |
| `inventory.json` | Machine-readable source of truth: artifacts, edges, datasets, CSD entries, scheduler jobs, construct occurrences, summary | generator |
| `01-inventory.md` | Counts by artifact type, the full inventory table, and what could not be resolved | generator |
| `02-dependency-map.md` | Batch and online dependency chains, Mermaid graph plus plain tables, reverse dataset view, orphans | generator |
| `03-conversion-construct-register.md` | Every occurrence of conversion-relevant constructs with `path:line`, counted per construct and per program | generator |
| `04-field-lineage.md` | Two worked field lineages (money and date), hop by hop, each hop Confirmed or Inferred | authored; hop counts refreshed by generator |
| `05-government-decisions.md` | Numbered decisions only the system owner can make, with evidence and impact if undecided | authored; count refreshed by generator |
| `../../tests/test_discovery.py` | Tests: JSON↔Markdown count agreement both ways, control totals, stale-file detection, real dependency edges | — |

## How to regenerate and check

```bash
python3 docs/discovery/build_discovery.py          # rewrite every generated file
python3 docs/discovery/build_discovery.py --check  # exit 1 if any generated file is stale
python3 -m pytest tests/test_discovery.py
```

## Numbers

| Measure | Value |
| --- | --- |
| Dependency edges resolved (headline) | 547 |
| Dependency edges unresolved (headline) | 126 |
|   program->copybook | 253 resolved / 64 unresolved |
|   program->dataset | 77 resolved / 7 unresolved |
|   jclstep->program | 126 resolved / 0 unresolved |
|   transaction->program | 26 resolved / 7 unresolved |
|   program->program | 65 resolved / 48 unresolved |
| Artifacts (total) | 237 |
|   COBOL program | 44 |
|   Copybook (COBOL) | 41 |
|   Copybook (BMS symbolic map) | 21 |
|   Copybook (SQL DCLGEN) | 3 |
|   BMS map source | 21 |
|   JCL job | 46 |
|   JCL procedure | 2 |
|   CICS CSD definition file | 4 |
|   Assembler program | 2 |
|   Assembler macro | 2 |
|   Control card / parameter member | 8 |
|   SQL DDL member | 6 |
|   IMS DBD/PSB definition | 8 |
|   Sample data file | 23 |
|   Catalog listing | 1 |
|   Scheduler definition | 2 |
|   Module documentation | 3 |
|   COBOL programs classified batch | 17 |
|   COBOL programs classified online | 25 |
|   COBOL programs classified utility | 2 |
| Construct-register occurrences | 2,658 |
| Lineage hops | 37 (30 Confirmed / 7 Inferred) |
|   Money field: TRAN-AMT | 18 (15 Confirmed / 3 Inferred) |
|   Date field: TRAN-PROC-TS | 19 (15 Confirmed / 4 Inferred) |
| Government decisions | 17 |
| Orphans | 185 (1 programs, 1 copybooks, 183 datasets) |
| Distinct datasets seen | 281 |
| CSD DEFINE statements | 134 |
| Scheduler job definitions | 32 |

## Confirmed / Inferred convention

- **Confirmed** — the statement is visible in source at the cited `path:line`; a reader can open the file and see it.
- **Inferred** — derived from source but not directly visible (for example, run-time behaviour of a utility, the value a variable holds when a statement executes, or whether an orphan is dead). Inferred items need a runtime trace or a subject-matter expert before they are treated as fact.
- Generated tables carry the citation of the statement they were derived from; `unresolved` in a generated table means the generator found the reference but could not bind it to a source artifact.

## Limits of this analysis

- **Static analysis only.** No program was executed, no runtime trace was taken, no CICS region or batch scheduler was queried. Dynamic `CALL`/`XCTL` targets are resolved only when a literal can be followed through the `MOVE`/`VALUE` definitions that reach the statement (same paragraph, or its `PERFORM`/`GO TO`/fall-through entry points) or through the menu copybooks; a value-dependent branch is not evaluated, so a resolved dynamic target is a static over-approximation.
- **No production data.** Only the sample data files committed in `app/data` were inventoried, by name; their contents were not read.
- **Sample repository, not a customer estate.** The source is a public sample application. Dataset qualifiers, transaction ids and program names are reproduced verbatim as identifiers; they are evidence, not endorsements.
- **Heuristic parsing.** The COBOL, JCL, CSD, BMS and scheduler readers are purpose-built and lexical, not full compilers. Utility control statements (IDCAMS, SORT) are read only for dataset names. Where a rule was applied (for example, the `online`/`batch`/`utility` classification) it is stated next to the table it produced.
- **Decision-neutral.** The dossier records what the code does and what only the system owner can decide. It does not recommend a target of any kind.
