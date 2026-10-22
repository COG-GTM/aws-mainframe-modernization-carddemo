# CBSTM03B — statement file-I/O subroutine

Port of `app/cbl/CBSTM03B.CBL`, CALLed by [CBSTM03A](../cbstm03a/README.md).

```ts
const sub = new Cbstm03b(catalog);           // one loaded copy: file state persists across calls
const area = newM03bArea();                  // LK-M03B-AREA
Object.assign(area, { dd: 'CUSTFILE', oper: M03B_OPER.READ_K, key: '000000001', keyLn: 9 });
sub.call(area);                              // area.rc = FILE STATUS, area.fldt = record (1000 bytes)
```

| `dd`     | Access     | Operations honoured |
|----------|------------|---------------------|
| TRNXFILE | SEQUENTIAL | `O` open input, `R` read next, `C` close |
| XREFFILE | SEQUENTIAL | `O`, `R`, `C` |
| CUSTFILE | RANDOM     | `O`, `K` read by `key(1:keyLn)` (MOVE to X(9)), `C` |
| ACCTFILE | RANDOM     | `O`, `K` read by `key(1:keyLn)` (MOVE to 9(11)), `C` |

Bug-for-bug behaviour kept:
- Any other operation (`W`, `Z`, `K` on a sequential file, ...) falls through to
  the paragraph EXIT and returns the file's *previous* FILE STATUS (spaces before
  the first I/O on that file).
- Unknown DD names (`WHEN OTHER`) return without touching the area.
- `READ ... INTO LK-M03B-FLDT` only moves the record on a successful read; on
  EOF / not-found FLDT keeps whatever the caller left there.

## LIMITATIONS
- Files resolve through a `VsamCatalog` (default DD names, overridable via the
  constructor) instead of JCL DD allocation.
- A `keyLn` outside 1..25 throws `RangeError` (the COBOL reference modification
  would be undefined behaviour); CBSTM03A always passes 9 or 11.
