# CBACT01C → C# .NET 8

Port of the CardDemo batch program [`app/cbl/CBACT01C.cbl`](../../app/cbl/CBACT01C.cbl) to a
.NET 8 console app, with xUnit tests proving **byte-for-byte parity** with the original COBOL
compiled and run under GnuCOBOL.

## What CBACT01C does

A batch "read and reformat" step (JCL `app/jcl/READACCT.jcl`). It reads every record of the
**account master file** (VSAM KSDS, in account-ID order) and for each account:

1. Prints the account's fields to SYSOUT, then the raw 300-byte record.
2. Writes a fixed 107-byte record to **OUTFILE**, re-formatting the reissue date from
   `YYYY-MM-DD` to `YYYYMMDD` via the assembler routine `COBDATFT`, and storing the cycle debit
   as packed decimal (COMP-3) — **set to 2525.00 only when the input debit is zero**.
3. Writes a 110-byte record to **ARRYFILE** containing a 5-slot balance array
   (slots 1–2 = current balance with fixed debits 1005.00 / 1525.00, slot 3 = -1025.00 / -2500.00,
   slots 4–5 left at zero).
4. Writes two variable-length records to **VBRCFILE**: a 12-byte one (ID + status) and a
   39-byte one (ID + balance + credit limit + reissue year).

| Item | Kind | Role |
|---|---|---|
| `ACCTFILE` | VSAM KSDS, 300 B, key = `ACCT-ID` | input |
| `OUTFILE` | sequential, fixed 107 B | output |
| `ARRYFILE` | sequential, fixed 110 B | output |
| `VBRCFILE` | sequential, variable 10–80 B | output |
| `CVACT01Y.cpy` | copybook | `ACCOUNT-RECORD` layout |
| `CODATECN.cpy` | copybook | parameter block for `COBDATFT` |
| `COBDATFT` (`app/asm`) | HLASM subprogram | date re-format |
| `CEE3ABD` | LE service | abend (code 999) on any I/O error |

## Field mapping (COBOL → C#)

Money is always `decimal` (never `float`/`double`); the sign of `-0` is preserved, as in COBOL.

### `ACCOUNT-RECORD` (CVACT01Y, 300 bytes) → `AccountRecord`

| Offset | COBOL field | PIC | C# property | C# type |
|---|---|---|---|---|
| 0 | `ACCT-ID` | `9(11)` | `AcctId` | `long` |
| 11 | `ACCT-ACTIVE-STATUS` | `X(01)` | `ActiveStatus` | `string` (1) |
| 12 | `ACCT-CURR-BAL` | `S9(10)V99` | `CurrBal` | `decimal` |
| 24 | `ACCT-CREDIT-LIMIT` | `S9(10)V99` | `CreditLimit` | `decimal` |
| 36 | `ACCT-CASH-CREDIT-LIMIT` | `S9(10)V99` | `CashCreditLimit` | `decimal` |
| 48 | `ACCT-OPEN-DATE` | `X(10)` | `OpenDate` | `string` (10) |
| 58 | `ACCT-EXPIRAION-DATE` *(sic)* | `X(10)` | `ExpirationDate` | `string` (10) |
| 68 | `ACCT-REISSUE-DATE` | `X(10)` | `ReissueDate` | `string` (10) |
| 78 | `ACCT-CURR-CYC-CREDIT` | `S9(10)V99` | `CurrCycCredit` | `decimal` |
| 90 | `ACCT-CURR-CYC-DEBIT` | `S9(10)V99` | `CurrCycDebit` | `decimal` |
| 102 | `ACCT-ADDR-ZIP` | `X(10)` | `AddrZip` | `string` (10) |
| 112 | `ACCT-GROUP-ID` | `X(10)` | `GroupId` | `string` (10) |
| 122 | `FILLER` | `X(178)` | `Filler` | `string` (178) |

Dates stay `string`: the program only moves/slices them and the data is not guaranteed to be a
valid date (e.g. blank), so parsing to `DateOnly` would change behaviour.

### Output records

| COBOL field | PIC / usage | C# property | C# type |
|---|---|---|---|
| `OUT-ACCT-ID` | `9(11)` | `OutAccountRecord.AcctId` | `long` |
| `OUT-ACCT-ACTIVE-STATUS` | `X(01)` | `.ActiveStatus` | `string` |
| `OUT-ACCT-CURR-BAL` / `-CREDIT-LIMIT` / `-CASH-CREDIT-LIMIT` / `-CURR-CYC-CREDIT` | `S9(10)V99` | `.CurrBal` … | `decimal` |
| `OUT-ACCT-OPEN-DATE` / `-EXPIRAION-DATE` / `-REISSUE-DATE` | `X(10)` | `.OpenDate` … | `string` |
| `OUT-ACCT-CURR-CYC-DEBIT` | `S9(10)V99 COMP-3` (7 B) | `.CurrCycDebit` | `decimal?` |
| `OUT-ACCT-GROUP-ID` | `X(10)` | `.GroupId` | `string` |
| `ARR-ACCT-ID` | `9(11)` | `ArrayRecord.AcctId` | `long` |
| `ARR-ACCT-BAL OCCURS 5` | group | `ArrayRecord.Balances` | `ArrayBalance[5]` |
| `ARR-ACCT-CURR-BAL` | `S9(10)V99` | `ArrayBalance.CurrBal` | `decimal?` |
| `ARR-ACCT-CURR-CYC-DEBIT` | `S9(10)V99 COMP-3` | `ArrayBalance.CurrCycDebit` | `decimal` |
| `ARR-FILLER` | `X(04)` | `ArrayRecord.Filler` | `string` |
| `VB1-ACCT-ID` / `VB2-ACCT-ID` | `9(11)` | `VbrcRecord1/2.AcctId` | `long` |
| `VB1-ACCT-ACTIVE-STATUS` | `X(01)` | `VbrcRecord1.ActiveStatus` | `string` |
| `VB2-ACCT-CURR-BAL` / `-CREDIT-LIMIT` | `S9(10)V99` | `VbrcRecord2.CurrBal` / `.CreditLimit` | `decimal` |
| `VB2-ACCT-REISSUE-YYYY` | `X(04)` | `VbrcRecord2.ReissueYyyy` | `string` |
| `CODATECN-TYPE` / `-OUTTYPE` | `X` (88s `'1'`/`'2'`) | `DateConversionRecord.Type` / `.OutType` | `string` |
| `CODATECN-INP-DATE` / `-0UT-DATE` | `X(20)` | `.InpDate` / `.OutDate` | `string` |
| `CODATECN-ERROR-MSG` | `X(38)` | `.ErrorMsg` | `string` |

Storage codecs live in `src/CardDemo.Cbact01c/Cobol`: `ZonedDecimal` (DISPLAY numerics with
EBCDIC overpunch sign, `{`=+0 … `}`=-0) and `PackedDecimal` (COMP-3).

### Legacy behaviour kept on purpose

- `OUT-ACCT-CURR-CYC-DEBIT` is only assigned when the input debit is zero; otherwise the
  previous record's value is written again (low-values before the first assignment) — hence `decimal?`.
- `ARR-ACCT-CURR-BAL` slots that were only `INITIALIZE`d are written as plain `000000000000`
  (no overpunch) — hence `decimal?`.
- The reissue date written to OUTFILE is `YYYYMMDD` + 2 trailing bytes left over from the
  `COBDATFT` work area.
- Output files are never explicitly closed by the COBOL (GnuCOBOL closes them implicitly).

## Run it

```bash
# C#  (args or env vars named like the JCL DDs: ACCTFILE OUTFILE ARRYFILE VBRCFILE)
dotnet run --project src/CardDemo.Cbact01c -- testdata/acctdata.txt out/OUTFILE out/ARRYFILE out/VBRCFILE

# Original COBOL under GnuCOBOL (loads the KSDS, stubs COBDATFT/CEE3ABD)
cobol-harness/run_cobol.sh testdata/acctdata.txt out-cobol/

# Parity + unit tests
dotnet test
```

- `testdata/acctdata.txt` — the 50 CardDemo sample accounts (`app/data/ASCII/acctdata.txt`).
- `testdata/edge_cases.txt` — negatives, `-0`, max values, non-zero debits, blank dates, short record.
- `testdata/duplicate_keys.txt` — a repeated account ID; like REPRO into a KSDS, the duplicate is rejected and the first record kept.
- `golden/` — SYSOUT + the three output files captured from GnuCOBOL 3.1 (`-std=ibm -fsign=EBCDIC`).
- `cobol-harness/` — `COBDATFT.cbl` (COBOL port of the HLASM routine), `CEE3ABD.cbl` stub,
  `LOADACCT.cbl` (IDCAMS-REPRO-style KSDS loader), `run_cobol.sh`.

`ParityTests` compares SYSOUT, OUTFILE, ARRYFILE and VBRCFILE byte-for-byte against both the
committed golden files and a fresh GnuCOBOL run (skipped if `cobc` isn't installed).
