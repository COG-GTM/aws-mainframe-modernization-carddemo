# WAITSTEP

JCL: `app/jcl/WAITSTEP.jcl (EXEC PGM=COBSWAIT, SYSIN DD * -> 00003600)`
Program: `COBSWAIT`
- CALL 'MVSWAIT' is satisfied by `scripts/baseline/stubs/MVSWAIT.cbl` (sleeps 36 s; `--fast` skips the sleep without changing sysout).

| DD | file |
|---|---|

## Outputs
