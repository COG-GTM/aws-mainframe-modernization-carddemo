# 02 - Conventions (probed from source, 2026-11-19)

Every fact below was checked on this VM against `app/**` at commit `07a8fbb`. Re-verify on resume.

## Encoding (headline finding)

- **Source is 7-bit ASCII, not LATIN-1/EBCDIC.** `file` on `app/cbl/*`, `app/csd/*`, `app/jcl/POSTTRAN.jcl`: 32 "ASCII text", 1 "ASCII text, with CRLF line terminators". `grep -P '[\x80-\xff]'` over every `*.cbl *.CBL *.cpy *.jcl *.bms` under `app/` returns **no file**, so there are no accented labels and no mojibake risk in FR text or screen specs. `iconv -f latin1 -t utf-8` is therefore a no-op for source.
- **Data comes in two encodings:** `app/data/EBCDIC/AWS.M2.CARDDEMO.*.PS` (CP037, fixed records, no newline: e.g. `DALYTRAN.PS` = 105000 bytes = 300 x 350) and `app/data/ASCII/*.txt` (one record per line).
- **ASCII sample quirks** (affect every loader and golden run):
  - Signed zoned fields keep **EBCDIC-style overpunch** in ASCII (`{`, `A`-`I` positive, `}`, `J`-`R` negative), e.g. DALYTRAN-AMT `0000005047G`. Compile/decode with GnuCOBOL `-fsign=EBCDIC`.
  - `tcatbal.txt` has CRLF on 49 of 50 lines; `cardxref.txt` lines are 36 bytes (trailing FILLER trimmed, LRECL 50); `acctdata.txt` 300, `dailytran.txt` 350. Strip `\r` and right-pad to LRECL.
  - Sample ACCTDATA carries the disclosure-group value in ACCT-ADDR-ZIP and a blank ACCT-GROUP-ID; sample TCATBALF balances are all 0; `dailytran.txt` has empty DALYTRAN-PROC-TS and all ORIG dates 2022-06-10.

## Program-name prefixes (what each denotes)

| Prefix | Meaning | Examples |
|---|---|---|
| `CO*` + `C` suffix | CICS online program (BMS screen) | `COSGN00C`, `COMEN01C`, `COACTVWC`, `COTRN02C` |
| `CO*` without screen | CICS programs triggered by MQ (`COPAUA0C`, `COACCT01`, `CODATE01`), CICS LINK target (`COPAUS2C`), DB2 batch run by TSO `RUN PROGRAM` (`COBTUPDT`), batch wait (`COBSWAIT`) | |
| `CB*` | batch COBOL run by JCL `EXEC PGM=` (or IMS BMP `CBPAUP0C`) | `CBTRN02C`, `CBACT04C` |
| `CS*` | shared utility subroutine | `CSUTLDTC` (date validation via LE `CEEDAYS`) |
| `PA*`, `DB*` | IMS load/unload utilities (DFSRRC00 DLI/BMP) | `PAUDBLOD`, `PAUDBUNL`, `DBUNLDGS` |
| Assembler | `app/asm/COBDATFT.asm` (date format, called by `CBACT01C`), `app/asm/MVSWAIT.asm` (timer wait, called by `COBSWAIT`); macros in `app/maclib` | |
| Copybooks `CV*` | record layouts (VSAM/sequential): `CVTRA06Y` DALYTRAN, `CVTRA05Y` TRANSACT, `CVACT01Y` ACCOUNT, `CVACT03Y` XREF, `CVTRA01Y` TCATBAL, ... | `app/cpy` |
| Copybooks `CO*`/`CS*` | online commarea (`COCOM01Y`), menu tables (`COMEN02Y`, `COADM02Y`), titles/messages, shared procedure code (`CSUTLDPY` contains `CALL 'CSUTLDTC'`) | `app/cpy` |
| BMS | maps `app/bms/*.bms`, symbolic maps `app/cpy-bms/*.CPY` (one per `CO*` screen) | |

Program-name suffix digits: `00` list, `01` view/add, `02` update/add, `03` delete (online), e.g. `COUSR00C..03C`.

## Where things live

| Artifact | Location |
|---|---|
| COBOL | `app/cbl` (31) + `app/app-authorization-ims-db2-mq/cbl` (8) + `app/app-transaction-type-db2/cbl` (3) + `app/app-vsam-mq/cbl` (2) = 44 |
| Assembler | `app/asm` (2) |
| Copybooks | `app/cpy` (30), `app/cpy-bms` (17), extension `cpy`/`cpy-bms` dirs |
| DB2 DCLGEN / DDL | `app/app-transaction-type-db2/dcl`, `ddl`; `app/app-authorization-ims-db2-mq/dcl`, `ddl` |
| IMS DBD/PSB | `app/app-authorization-ims-db2-mq/ims` |
| JCL | `app/jcl` (38), extension `jcl` dirs; PROCs `app/proc` (`REPROC.prc`, `TRANREPT.prc`); IDCAMS control cards `app/ctl` |
| CICS definitions (CSD) | `app/csd/CARDDEMO.CSD`, `app/app-*/csd/*.csd`; plus `app/jcl/CBADMCDJ.jcl` (DFHCSDUP job with additional, partly stale definitions) |
| Scheduler | `app/scheduler/CardDemo.ca7` (CA-7 job listing with triggers), `app/scheduler/CardDemo.controlm` (Control-M XML folders) |
| Catalog | `app/catlg/LISTCAT.txt` |

## How to read things

- **Fixed-format COBOL:** columns 1-6 sequence, column 7 indicator (`*` or `/` = comment, skip), code in 8-72, 73-80 ident. Some extension sources carry numeric sequence numbers in 1-6 and 73-80.
- **Dispatch idioms used:** table-driven XCTL from menu option tables (`COMEN01C` + `COMEN02Y`, `COADM01C` + `COADM02Y`); `EVALUATE EIBAID` PF-key dispatch; `CDEMO-TO-PROGRAM` / `CDEMO-FROM-PROGRAM` in `COCOM01Y` for forward/return navigation; `CCARD-NEXT-PROG` in the card programs; pseudo-conversational `RETURN TRANSID(<own trancode>)`.
- **Batch error protocol:** `APPL-RESULT` (`0` ok, `16` EOF, `12` error) + `FILE STATUS` two-byte fields; any non-ok I/O -> `9910-DISPLAY-IO-STATUS` + `9999-ABEND-PROGRAM` -> `CALL 'CEE3ABD'` with ABCODE 999 (e.g. `app/cbl/CBTRN02C.cbl:707-711`). Business rejects use `RETURN-CODE 4` (`app/cbl/CBTRN02C.cbl:229-231`).
- **Data access:** VSAM only for the base app (batch `SELECT ... ASSIGN TO <DD>`, online `EXEC CICS READ/WRITE/REWRITE/DELETE/STARTBR DATASET(<FCT name>)`). No logical-DB access modules exist in the base app. Extensions add DB2 (`EXEC SQL`, `DSNTIAC` via `CSDB2RPY.cpy`), IMS (`EXEC DLI`, `CALL 'CBLTDLI'`) and MQ (`CALL 'MQOPEN'/'MQGET'/'MQPUT'/'MQPUT1'/'MQCLOSE'`).
- **Message/label tables:** in source (`CSMSG01Y`, `CSMSG02Y`, `COTTL01Y`, menu copybooks); no external message library.
- **Stored procedures:** none in the repo. No SP family is in use.
- **Return codes on Linux (GnuCOBOL):** a `CEE3ABD` abend 999 surfaces as process exit **231** (999 mod 256) with the stub in `.migration/tools/posttran_gnucobol_oracle.sh`; the target state maps it to RC 16.
