# 03 - Glossary (CardDemo engagement)

## Engagement vocabulary (how the playbooks use these words here)

| Term | Meaning in this engagement |
|---|---|
| Module | The whole CardDemo application: everything under `app/**`, including the three optional extensions |
| Stream | One selectable, end-to-end unit that can be migrated alone: one CICS menu route / trancode, one MQ-triggered transaction, or one JCL job (with its programs). Catalogued in `functional/CARDDEMO/CardDemo_inventory.md` |
| Entry point | A runtime declaration that starts execution: CSD `DEFINE TRANSACTION`, JCL `EXEC PGM=` / `RUN PROGRAM` / DFSRRC00 `PARM`, scheduler job |
| Hard stop | The last program/dataset a stream owns; anything past it is a boundary |
| Boundary | Any point where execution or data leaves the stream (B1-B11, see `!mf_boundary_resolution`) |
| Wave | A set of programs migrated together by one child session in Phase 2 |
| Sub-transaction | A called sub-flow with its own contract. None in POSTTRAN |
| Golden master / oracle | Outputs + RC of the unmodified COBOL compiled with GnuCOBOL on the same inputs |

## Mainframe / shop terms

| Term | Meaning |
|---|---|
| CICS | IBM online transaction monitor; programs are pseudo-conversational (`RETURN TRANSID`) |
| Trancode / TransID | 4-char CICS transaction id, e.g. `CC00` sign-on, `CM00` main menu, `CA00` admin menu |
| COMMAREA | data passed between CICS programs (`CARDDEMO-COMMAREA`, `app/cpy/COCOM01Y.cpy`) |
| XCTL / LINK | CICS transfer of control without / with return |
| BMS map / mapset | 3270 screen definition (`app/bms`) and its symbolic copybook (`app/cpy-bms`) |
| TDQ `JOBS` | extrapartition transient data queue to the internal reader (`app/csd/CARDDEMO.CSD:499-503`); `CORPT00C` writes JCL to it to submit TRANREPT |
| VSAM KSDS / AIX / PATH | key-sequenced file / alternate index / path relating AIX to base |
| GDG `(+1)` | generation data group: new generation of a dataset per run (DALYREJS, TRANSACT.BKUP, SYSTRAN, ...) |
| PS | physical sequential dataset (e.g. `AWS.M2.CARDDEMO.DALYTRAN.PS`) |
| DD | JCL data definition; COBOL `ASSIGN TO <DD>` name |
| RECFM=F / FB, LRECL | fixed / fixed-blocked records, record length |
| IDCAMS / REPRO | VSAM utility / copy |
| CEE3ABD | LE callable service: abend the program (ABCODE 999 in CardDemo batch) |
| CEEDAYS | LE date service used by `CSUTLDTC` |
| CA-7 / Control-M | job schedulers (`app/scheduler`) |
| CLOSEFIL / OPENFIL | jobs closing/opening CICS files via SDSF `CEMT` so batch can update them |
| BMP / DLI | IMS batch message program / batch DL/I region (`DFSRRC00 PARM='BMP,...'`) |
| PSB / PCB / DBD | IMS program spec block / program communication block / database description |
| DCLGEN | DB2 host-variable declarations (`*.dcl`) |
| Overpunch | sign carried in the last digit of a zoned decimal (`{`=+0, `}`=-0, ...) |

## Business vocabulary

| Term | Meaning | Source |
|---|---|---|
| Daily transaction (DALYTRAN) | card transaction received during the day, to be posted | `app/cpy/CVTRA06Y.cpy` |
| Posting | validating a daily transaction and applying it to the transaction master, account balance and category balance (CBTRN02C) | `app/cbl/CBTRN02C.cbl:424-444` |
| Reject (DALYREJS) | daily record that failed validation, written with an 80-byte trailer (4-digit reason + description) | `app/cbl/CBTRN02C.cbl:176-182`, `:446-465` |
| Reject reasons | 100 invalid card number (no XREF), 101 account not found, 102 overlimit, 103 transaction after account expiration; 109 set on account REWRITE invalid key (not written) | `app/cbl/CBTRN02C.cbl:385,397,410,417,556` |
| Card cross-reference (XREF) | card number -> customer id + account id | `app/cpy/CVACT03Y.cpy` |
| Transaction category balance (TCATBAL) | balance per account + type code + category code | `app/cpy/CVTRA01Y.cpy` |
| Cycle credit / cycle debit | ACCT-CURR-CYC-CREDIT / -DEBIT: current-cycle sums of positive / negative amounts | `app/cpy/CVACT01Y.cpy`, `app/cbl/CBTRN02C.cbl:545-553` |
| Disclosure group | interest-rate group used by INTCALC | `app/cpy/CVTRA02Y.cpy` |
| User type `A` / `U` | admin / regular user (`CDEMO-USRTYP-ADMIN` / `-USER`) | `app/cpy/COCOM01Y.cpy:27-28` |
| Pending authorization | card authorization held in IMS (extension) | `app/app-authorization-ims-db2-mq` |
