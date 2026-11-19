# 04 - Boundary register (module-wide, append-only)

Owned by `!mf_migration_setup`; appended by `!mf_module_inventory_analysis` (register mode, first pass), `!mf_stream_analysis` (register mode, full contract), `!mf_stream_migration_plan` (decide mode) and `!mf_program_migration` (implement mode). **Never rewrite or delete an entry**: append a dated update row in the entry's history instead. Taxonomy and procedure: `!mf_boundary_resolution`.

## Schema

Every entry is a level-3 section `### BND-<NNN> <short title>` with this table, then a `History` list (append-only, dated).

| Field | Content |
|---|---|
| Class | exactly one of B1 intra-stream call, B2 shared module, B3 foreign subroutine, B4 data-access leaf, B5 cross-transaction switch, B6 queue/MQ/event, B7 dataset hand-off, B8 online-to-batch, B9 scheduler dependency, B10 shared data contract, B11 external system |
| Why this class | one line |
| Streams affected | stream ids from the inventory |
| Caller / producer + cite | `<path>:<line>` |
| Callee / consumer / resource | name; present in repo? (Y/N) |
| Interface | copybook / record layout / DD / queue / table; fields in/out with PIC; return-code / status protocol incl. every value the caller branches on |
| Read-only or state-changing | which datasets/tables/segments |
| Transactional coupling | inside caller's unit of work? commits? partial-failure behavior |
| Volume / frequency / latency | |
| Error, timeout, retry semantics | incl. abend behavior |
| Idempotency | is retry safe, and why |
| Security context | user id / terminal / region assumed |
| Owner today / who to ask | |
| Strangler direction + facade | outbound / inbound / both; named routing point |
| Required action type | e.g. decide-at-plan, port-once, keep-file-contract, coexistence-decision, lead-time-request |
| Contract complete? | YES / NO (+ what is unresolved: a NO is a STOP C blocker) |
| Status | UNDECIDED -> DECIDED -> IMPLEMENTED (or DEFERRED with dependency, impact, re-entry condition) |
| Decision / owner / request status / seam / verification | filled in decide mode |
| PR / tests / residual risk | filled in implement mode |

## Counts by class (update when appending)

| Class | Count |
|---|---|
| (none yet) | 0 |

## Entries

(none yet - first pass is appended by `!mf_module_inventory_analysis`)
