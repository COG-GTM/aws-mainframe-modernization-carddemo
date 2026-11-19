# 03 - Glossary

| Mainframe term | Meaning in POSTTRAN | Target |
|---|---|---|
| JCL job `POSTTRAN` | Batch job running `CBTRN02C` (`app/jcl/POSTTRAN.jcl:1,23`) | Spring Batch `Job` "POSTTRAN" |
| `EXEC` step `STEP15` | Only step | Spring Batch `Step` "STEP15" |
| DD name | Logical file name bound to a dataset | `--dd=<NAME>=<path>` job parameter or table |
| DALYTRAN | Daily transactions, PS, 350-byte records (`CVTRA06Y`) | Input file |
| DALYREJS | Rejects, GDG `(+1)`, LRECL 430 = 350 record + 80 trailer | Output file |
| TRANSACT / TRANFILE | Posted transactions KSDS, key TRAN-ID (`CVTRA05Y`) | Table `transaction.transactions` |
| CARDXREF / XREFFILE | Card → account/customer xref KSDS (`CVACT03Y`) | Table `card.card_xref` |
| ACCTDATA / ACCTFILE | Account master KSDS (`CVACT01Y`) | Table `account.accounts` |
| TCATBALF | Balance per account+type+category KSDS (`CVTRA01Y`) | Table `reference.tran_cat_balances` |
| KSDS | VSAM keyed file | PostgreSQL table with PK |
| GDG `(+1)` | New generation of a dataset per run | New file per run, never overwritten |
| FILE STATUS | 2-char I/O result (`00`, `10` EOF, `23` not found, `22` dup) | Typed outcome / exception |
| `CEE3ABD` | LE abend service, U999 here | Exception → step FAILED, exit 16 |
| RETURN-CODE / RC / CC | Job condition code (0 / 4) | Process exit code |
| GnuCOBOL oracle | Original COBOL run on Linux as golden master | `.migration/tools/posttran_gnucobol_oracle.sh` |
| CA-7 | Scheduler chaining CLOSEFIL → CBPAUP0J → POSTTRAN → WAITSTEP | External scheduler on exit code |
| Overpunch | Zoned decimal sign in last digit (`{`=+0, `}`=-0, ...) | Codec in `carddemo-common` |
