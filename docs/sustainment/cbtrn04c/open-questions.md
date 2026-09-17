# Open questions (CBTRN04C)

The repository has no requirements document for the daily feed, so the points
below were inferred from copybooks, `CBTRN02C`, the JCL and the sample data. Each
is implemented conservatively and marked **Inferred — needs owner confirmation**
in `change-request.md` where it corresponds to a rule.

| # | Inferred | Basis in the repository | What was implemented |
|---|----------|-------------------------|----------------------|
| 1 | `DALYTRAN-ID` must be populated (R01) | `CVTRA06Y.cpy:5` gives `PIC X(16)`; nothing in the repository checks or generates it, and `CBTRN02C` copies it into the transaction master key without a check | Reject when all spaces or all LOW-VALUES; any other content accepted (no format rule) |
| 2 | Transaction type must exist in `TRANTYPE` (R02) | The file and its layout exist (`CVTRA03Y.cpy:5`) but `CBTRN02C` never reads it | Reject on `INVALID KEY` (status 23) |
| 3 | Category is validated as type + category, not alone (R03) | `TRANCATG` is keyed on `TRAN-CAT-KEY` = type + category (`CVTRA04Y.cpy:5`-`7`); there is no category-only key or file, and posting keys `TCATBALF` on the same pair (`CBTRN02C.cbl:506`) | One keyed read of `TRANCATG` on the pair; a category valid under another type is a reject |
| 4 | Sign convention of the amount field | `PIC S9(09)V99` zoned decimal (`CVTRA06Y.cpy:10`); the ASCII sample data carries EBCDIC-style overpunched signs (`{`, `A`-`I`, `}`, `J`-`R`) | NUMERIC class test; GnuCOBOL tests compile with `-fsign=EBCDIC` so the sample data reads as it would on the mainframe |
| 5 | Amount range is checked against the projected category balance (R06) | Narrowest downstream field is `S9(09)V99` (`CVTRA01Y.cpy:9`, `CVTRA05Y.cpy:10`); `CBTRN02C` adds without `SIZE ERROR` (`CBTRN02C.cbl:508`, `:527`, `:547`); `ACCT-CURR-BAL` is wider (`CVACT01Y.cpy:7`) | Read `TCATBALF` by account + type + category, add the amount in `COMP-3`, reject when outside ±999,999,999.99. Missing balance record = 0 (as `CBTRN02C` creates it). The account balance is not projected: it is `S9(10)V99` and would need every prior transaction of the run to be summed |
| 6 | Timestamp text format is `YYYY-MM-DD hh:mm:ss.ffffff` (R07, R08) | `CBTRN02C.cbl:414` compares `DALYTRAN-ORIG-TS (1:10)` with `ACCT-EXPIRAION-DATE PIC X(10)`; `Z-GET-DB2-FORMAT-TIMESTAMP` builds the same shape | Only bytes 1-10 are validated; the time portion is not checked |
| 7 | Whether a blank processing timestamp is valid (R08, R09) | All 300 sample records have a blank `DALYTRAN-PROC-TS`; `CBTRN02C.cbl:438` overwrites it at posting | Blank accepted; R08/R09/R10-processing checks apply only when present |
| 8 | Run-date `PARM` format is `YYYYMMDD` and dates after it are rejected (R10) | `INTCALC.jcl:22` passes `PARM='2022071800'` to `CBACT04C`, whose `PARM-DATE` is `PIC X(10)` (`CBACT04C.cbl:178`); the tasking asks for `YYYYMMDD` | Exactly 8 numeric bytes, validated as a Gregorian date, else RC 8 before any file is opened; "after" means calendar date only |
| 9 | How the `PARM` reaches the program | `CBACT04C` receives it through `PROCEDURE DIVISION USING` linkage, which GnuCOBOL does not allow in a main program built with `-x` | `CALL 'CEE3PRM'` (Language Environment) with `ON EXCEPTION`; `tests/cbtrn04c/stubs/CEE3PRM.cbl` supplies it under GnuCOBOL from `CBTRN04C_PARM` |
| 10 | Reason-code numbering for the new rules | `CBTRN02C` uses 100-103 (`CBTRN02C.cbl:385`, `:397`, `:410`, `:417`) | `0100` reused for the card check; `0201`-`0209` for new rules |
| 11 | Return code for file errors | `CBTRN02C` abends via `CEE3ABD` (`CBTRN02C.cbl:707`) rather than setting a return code | RC 12, so the `COND` on `POSTTRN2.jcl` can test it; an abend would also skip the posting step |
| 12 | Rejected amounts that are not numeric cannot be totalled | A non-numeric zoned field has no defined value | Counted on a separate report line and excluded from the accepted, rejected and total amounts so the reconciliation is exact |
| 13 | The Julian range on the report covers origination dates only | The tasking asks for "the Julian range of accepted transactions"; processing timestamps may be blank | Min/max of accepted origination dates in `YYYYDDD`; `NONE` when nothing was accepted |
| 14 | Data set names and high-level qualifier in the new JCL | The new jobs cannot be executed in this environment | `SET HLQ=SITE.HLQ` placeholder; set to the qualifier used by `POSTTRAN.jcl` at implementation |
| 15 | Report destination | `CBTRN02C` writes only `DISPLAY` lines to `SYSOUT` | Separate `VALDRPT` DD, `RECFM=F LRECL=133`, routed to `SYSOUT=*` in the JCL; `DISPLAY` totals also go to `SYSOUT` as in `CBTRN02C` |
