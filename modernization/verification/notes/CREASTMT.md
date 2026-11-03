## Compile shims

CBSTM03A does not compile/run under GnuCOBOL as it is: `CUSTREC.cpy` is tab-indented and the TIOT walk
dereferences z/OS control blocks. `reference.py` writes `SHIMS.txt` next to the outputs and fails unless
CUSTREC.cpy differs from `app/cpy` only in white space and the CBSTM03A copy is a pure deletion starting at
the TIOT walk (27 lines: SET ADDRESS / DISPLAY of the job, step and DD names; no file or data effect).

## Interest timestamp provenance (PR #171 caveat)

The table keeps no provenance, so the TRANSACT unloads (CREASTMT: `SourceTables`, TRANREPT:
`ServiceTransactionSource`) recognise INTCALC rows by the record CBACT04C writes (type 01, category 05,
source `System`, description `Int. for a/c `). Chained after real POSTTRAN and INTCALC runs the inference
holds: every TRAN-ORIG-TS / TRAN-PROC-TS of TRXFL.SEQ has the COBOL format. A user-entered transaction with
that exact shape would still be unloaded with DB2 timestamps; reported, not patched.
