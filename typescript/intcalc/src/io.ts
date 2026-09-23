import { readFileSync, writeFileSync } from 'node:fs';

import { AbendError } from './errors.ts';
import {
  ACCOUNT_RECORD_LENGTH,
  DISCGRP_RECORD_LENGTH,
  TCATBAL_RECORD_LENGTH,
  XREF_RECORD_LENGTH,
  formatAccount,
  padRecord,
  parseAccount,
  parseCardXref,
  parseDisGroup,
  parseTranCatBal,
  unsignedDisplay,
  type AccountRecord,
  type CardXrefRecord,
  type DisGroupRecord,
  type TranCatBalRecord,
} from './records.ts';

export type LineTerminator = '\n' | '\r\n' | '';

/**
 * Splits a fixed-width flat file into record images.
 *
 * The `app/data/ASCII` fixtures are newline separated (some with CRLF, and the
 * cross-reference file omits its trailing FILLER), so records are recovered per
 * line and padded to the copybook length. A file with no line separators at all
 * is split on the record length instead, which is what a true RECFM=F dataset
 * unload looks like.
 */
export function splitFixedWidth(content: string, recordLength: number, what: string): string[] {
  if (!content.includes('\n')) {
    const records: string[] = [];
    for (let offset = 0; offset < content.length; offset += recordLength) {
      records.push(padRecord(content.slice(offset, offset + recordLength), recordLength, what));
    }
    return records;
  }
  return content
    .split('\n')
    .map((line) => (line.endsWith('\r') ? line.slice(0, -1) : line))
    .filter((line) => line.trim().length > 0)
    .map((line) => padRecord(line, recordLength, what));
}

function readRecords(path: string, recordLength: number, what: string): string[] {
  return splitFixedWidth(readFileSync(path, 'latin1'), recordLength, what);
}

/**
 * `TCATBALF` — KSDS read with `ACCESS MODE IS SEQUENTIAL`, i.e. ascending
 * record-key order. The images are sorted on the key so that the account-break
 * logic sees the same sequence a VSAM browse would.
 */
export function readTranCatBalFile(path: string): TranCatBalRecord[] {
  return readRecords(path, TCATBAL_RECORD_LENGTH, 'TCATBALF')
    .map(parseTranCatBal)
    .sort((a, b) =>
      `${a.acctId}${a.tranTypeCd}${a.tranCatCd}`.localeCompare(
        `${b.acctId}${b.tranTypeCd}${b.tranCatCd}`,
      ),
    );
}

/** `XREFFILE` read through the alternate index on `FD-XREF-ACCT-ID`. */
export class XrefStore {
  private readonly byAcctId = new Map<string, CardXrefRecord>();

  constructor(records: readonly CardXrefRecord[]) {
    // An alternate index with duplicates returns the entry whose base-cluster
    // (card number) key is lowest; there is no rule in CBACT04C for choosing
    // among several cards of one account.
    for (const record of [...records].sort((a, b) => a.cardNum.localeCompare(b.cardNum))) {
      const key = unsignedDisplay(record.acctId, 11);
      if (!this.byAcctId.has(key)) {
        this.byAcctId.set(key, record);
      }
    }
  }

  static fromFile(path: string): XrefStore {
    return new XrefStore(readRecords(path, XREF_RECORD_LENGTH, 'XREFFILE').map(parseCardXref));
  }

  /** `1110-GET-XREF-DATA` (`cbl:393-412`) — a miss abends the run. */
  read(acctId: string): CardXrefRecord {
    const record = this.byAcctId.get(unsignedDisplay(acctId, 11));
    if (record === undefined) {
      throw new AbendError(`ERROR READING XREF FILE (ACCOUNT NOT FOUND: ${acctId})`);
    }
    return record;
  }
}

export interface DiscgrpKey {
  acctGroupId: string;
  tranTypeCd: string;
  tranCatCd: string;
}

export function discgrpKeyImage(key: DiscgrpKey): string {
  return `${key.acctGroupId.padEnd(10, ' ').slice(0, 10)}${key.tranTypeCd
    .padEnd(2, ' ')
    .slice(0, 2)}${unsignedDisplay(key.tranCatCd, 4)}`;
}

/** `DISCGRP` — random reads on group + type + category. */
export class DiscgrpStore {
  private readonly byKey = new Map<string, DisGroupRecord>();

  constructor(records: readonly DisGroupRecord[]) {
    for (const record of records) {
      this.byKey.set(discgrpKeyImage(record), record);
    }
  }

  static fromFile(path: string): DiscgrpStore {
    return new DiscgrpStore(readRecords(path, DISCGRP_RECORD_LENGTH, 'DISCGRP').map(parseDisGroup));
  }

  /** Returns `undefined` for VSAM status 23 (record not found). */
  read(key: DiscgrpKey): DisGroupRecord | undefined {
    return this.byKey.get(discgrpKeyImage(key));
  }
}

/**
 * `ACCTFILE` — opened `I-O`: random reads plus in-place `REWRITE`. Records are
 * held in their original file order so the rewritten file keeps byte positions
 * of untouched records.
 */
export class AccountStore {
  private readonly order: string[] = [];
  private readonly byAcctId = new Map<string, AccountRecord>();

  constructor(records: readonly AccountRecord[]) {
    for (const record of records) {
      const key = unsignedDisplay(record.acctId, 11);
      this.order.push(key);
      this.byAcctId.set(key, record);
    }
  }

  static fromFile(path: string): AccountStore {
    return new AccountStore(readRecords(path, ACCOUNT_RECORD_LENGTH, 'ACCTFILE').map(parseAccount));
  }

  /** `1100-GET-ACCT-DATA` (`cbl:372-390`) — a miss abends the run. */
  read(acctId: string): AccountRecord {
    const record = this.byAcctId.get(unsignedDisplay(acctId, 11));
    if (record === undefined) {
      throw new AbendError(`ERROR READING ACCOUNT FILE (ACCOUNT NOT FOUND: ${acctId})`);
    }
    return record;
  }

  /** `REWRITE FD-ACCTFILE-REC` in `1050-UPDATE-ACCOUNT` (`cbl:356`). */
  rewrite(record: AccountRecord): void {
    const key = unsignedDisplay(record.acctId, 11);
    if (!this.byAcctId.has(key)) {
      throw new AbendError(`ERROR RE-WRITING ACCOUNT FILE (ACCOUNT NOT FOUND: ${record.acctId})`);
    }
    this.byAcctId.set(key, record);
  }

  records(): AccountRecord[] {
    return this.order.map((key) => {
      const record = this.byAcctId.get(key);
      if (record === undefined) {
        throw new AbendError(`ERROR RE-WRITING ACCOUNT FILE (ACCOUNT MISSING: ${key})`);
      }
      return record;
    });
  }

  writeToFile(path: string, lineTerminator: LineTerminator): void {
    const images = this.records().map(formatAccount);
    writeFileSync(path, joinRecords(images, lineTerminator), 'latin1');
  }
}

export function joinRecords(images: readonly string[], lineTerminator: LineTerminator): string {
  if (lineTerminator === '') {
    return images.join('');
  }
  return images.length === 0 ? '' : `${images.join(lineTerminator)}${lineTerminator}`;
}

/** `TRANSACT` — `OPEN OUTPUT` on a brand-new `SYSTRAN(+1)` generation. */
export class TransactionWriter {
  private readonly images: string[] = [];

  write(image: string): void {
    this.images.push(image);
  }

  get records(): readonly string[] {
    return this.images;
  }

  writeToFile(path: string, lineTerminator: LineTerminator): void {
    writeFileSync(path, joinRecords(this.images, lineTerminator), 'latin1');
  }
}
