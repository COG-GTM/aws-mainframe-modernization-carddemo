# CardDemo TypeScript

TypeScript migration of the CardDemo mainframe application. This workspace holds
the runtime foundation the migrated COBOL programs build on; the programs
themselves land in later phases.

## Packages

| Package | Replaces | Purpose |
| :------ | :------- | :------ |
| `@carddemo/copybook` | COBOL `PICTURE` / `USAGE` semantics | Fixed-width record codec: zoned decimal with sign overpunch, packed decimal (`COMP-3`), implied decimal scaling, `OCCURS`, `FILLER` |
| `@carddemo/domain` | copybooks in `app/cpy` | Record layouts and typed records for account, customer, card, xref, transaction, daily transaction, category balance, disclosure group, transaction type/category and user security, plus their VSAM key builders |
| `@carddemo/vsam` | VSAM KSDS / QSAM access | Key-ordered `Ksds` store and `SequentialReader`/`SequentialWriter`, reporting COBOL file status codes (`00`, `10`, `22`, `23`, `42`, …), wired to the sample data in `app/data/ASCII` |

## Usage

```ts
import { openAccountFile, FileStatus } from "@carddemo/vsam";

const accounts = openAccountFile();
accounts.openFile();
const result = accounts.read("00000000001");
if (result.status === FileStatus.ok) {
  console.log(result.record.acctCurrBal);
}
```

## Development

```bash
nvm use 22       # Node >= 20
npm install
npm run build    # tsc project references
npm test         # vitest, including round-trip checks against app/data/ASCII
npm run lint
```

## Conventions for migrated programs

- One package per functional area; batch programs expose a CLI entry point, online programs expose HTTP handlers.
- Record access goes through `@carddemo/vsam`; programs branch on file status codes the same way the COBOL `FILE STATUS` checks do.
- Money uses the copybook scale (`S9(n)V99`) and is rounded on encode, so records round-trip byte-for-byte with the mainframe data files.
