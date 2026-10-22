/**
 * CBSTM03B: file-I/O subroutine CALLed by CBSTM03A (statement generation).
 *
 * The CALL interface is LK-M03B-AREA, mirrored by `M03bArea`; `call(area)`
 * mutates the area in place exactly like `CALL 'CBSTM03B' USING WS-M03B-AREA`.
 * One `Cbstm03b` instance = one loaded copy of the subprogram: its files and
 * FILE STATUS fields keep their state between calls (WORKING-STORAGE).
 *
 * | DD       | File                         | Access     | Operations |
 * |----------|------------------------------|------------|------------|
 * | TRNXFILE | TRNX-FILE (KSDS, key 32)     | SEQUENTIAL | O, R, C    |
 * | XREFFILE | XREF-FILE (KSDS, key 16)     | SEQUENTIAL | O, R, C    |
 * | CUSTFILE | CUST-FILE (KSDS, key 9)      | RANDOM     | O, K, C    |
 * | ACCTFILE | ACCT-FILE (KSDS, key 9(11))  | RANDOM     | O, K, C    |
 */
import type { VsamCatalog } from '../../common/vsam/catalog.js';
import { IndexedFile } from '../../common/vsam/cobol-file.js';

/** LK-M03B-OPER 88-levels. */
export const M03B_OPER = {
  OPEN: 'O',
  CLOSE: 'C',
  READ: 'R',
  READ_K: 'K',
  WRITE: 'W',
  REWRITE: 'Z',
} as const;

/** LK-M03B-FLDT length. */
export const M03B_FLDT_LENGTH = 1000;
/** LK-M03B-KEY length. */
export const M03B_KEY_LENGTH = 25;

/** LK-M03B-AREA. */
export interface M03bArea {
  /** LK-M03B-DD PIC X(08). */
  dd: string;
  /** LK-M03B-OPER PIC X(01): one of `M03B_OPER`. */
  oper: string;
  /** LK-M03B-RC PIC X(02): FILE STATUS of the file after the operation. */
  rc: string;
  /** LK-M03B-KEY PIC X(25). */
  key: string;
  /** LK-M03B-KEY-LN PIC S9(4). */
  keyLn: number;
  /** LK-M03B-FLDT PIC X(1000): record area for READ ... INTO. */
  fldt: Buffer;
}

/** A WS-M03B-AREA as it is before CBSTM03A first sets it (no VALUE clauses: spaces / zero). */
export function newM03bArea(): M03bArea {
  return { dd: ' '.repeat(8), oper: ' ', rc: '  ', key: ' '.repeat(M03B_KEY_LENGTH), keyLn: 0, fldt: Buffer.alloc(M03B_FLDT_LENGTH, 0x20) };
}

/** DD names of the four files, as resolved through the catalog. */
export interface Cbstm03bAssign {
  trnxfile?: string;
  xreffile?: string;
  custfile?: string;
  acctfile?: string;
}

/** MOVE X(n) TO X(len): left-justify, pad with spaces / truncate on the right. */
function moveAlnum(src: string, len: number): string {
  return src.length >= len ? src.slice(0, len) : src.padEnd(len, ' ');
}

/** MOVE X(n) TO 9(len): the sending item is treated as an unsigned integer (right-justified, zero filled). */
function moveAlnumToUnsigned(src: string, len: number): string {
  return src.length >= len ? src.slice(src.length - len) : src.padStart(len, '0');
}

export class Cbstm03b {
  private readonly trnx: IndexedFile;
  private readonly xref: IndexedFile;
  private readonly cust: IndexedFile;
  private readonly acct: IndexedFile;
  /** TRNXFILE-STATUS ... ACCTFILE-STATUS: no VALUE clause; spaces until the first I/O. */
  private readonly status = { trnx: '  ', xref: '  ', cust: '  ', acct: '  ' };

  constructor(catalog: VsamCatalog, assign: Cbstm03bAssign = {}) {
    this.trnx = new IndexedFile(catalog, assign.trnxfile ?? 'TRNXFILE', { access: 'SEQUENTIAL' });
    this.xref = new IndexedFile(catalog, assign.xreffile ?? 'XREFFILE', { access: 'SEQUENTIAL' });
    this.cust = new IndexedFile(catalog, assign.custfile ?? 'CUSTFILE', { access: 'RANDOM' });
    this.acct = new IndexedFile(catalog, assign.acctfile ?? 'ACCTFILE', { access: 'RANDOM' });
  }

  /** CALL 'CBSTM03B' USING area (0000-START). */
  call(area: M03bArea): void {
    switch (area.dd) {
      case 'TRNXFILE':
        area.rc = this.sequentialProc(this.trnx, area, 'trnx');
        break;
      case 'XREFFILE':
        area.rc = this.sequentialProc(this.xref, area, 'xref');
        break;
      case 'CUSTFILE':
        area.rc = this.randomProc(this.cust, area, 'cust', (k) => moveAlnum(k, 9));
        break;
      case 'ACCTFILE':
        area.rc = this.randomProc(this.acct, area, 'acct', (k) => moveAlnumToUnsigned(k, 11));
        break;
      default:
        // WHEN OTHER: GO TO 9999-GOBACK, area untouched.
        break;
    }
  }

  /** 1000-TRNXFILE-PROC / 2000-XREFFILE-PROC. Any other operation falls through to the EXIT and returns the last status. */
  private sequentialProc(file: IndexedFile, area: M03bArea, which: 'trnx' | 'xref'): string {
    if (area.oper === M03B_OPER.OPEN) this.status[which] = file.open('INPUT').status;
    else if (area.oper === M03B_OPER.READ) {
      const res = file.readNext();
      this.status[which] = res.status;
      if (res.record && res.status.startsWith('0')) intoFldt(area, res.record);
    } else if (area.oper === M03B_OPER.CLOSE) this.status[which] = file.close().status;
    return this.status[which];
  }

  /** 3000-CUSTFILE-PROC / 4000-ACCTFILE-PROC: READ-K moves LK-M03B-KEY (1:LK-M03B-KEY-LN) to the record key. */
  private randomProc(file: IndexedFile, area: M03bArea, which: 'cust' | 'acct', toKey: (k: string) => string): string {
    if (area.oper === M03B_OPER.OPEN) this.status[which] = file.open('INPUT').status;
    else if (area.oper === M03B_OPER.READ_K) {
      if (area.keyLn < 1 || area.keyLn > M03B_KEY_LENGTH) {
        throw new RangeError(`CBSTM03B: reference modification LK-M03B-KEY (1:${area.keyLn}) is out of range`);
      }
      const res = file.read(toKey(area.key.padEnd(M03B_KEY_LENGTH, ' ').slice(0, area.keyLn)));
      this.status[which] = res.status;
      if (res.record && res.status.startsWith('0')) intoFldt(area, res.record);
    } else if (area.oper === M03B_OPER.CLOSE) this.status[which] = file.close().status;
    return this.status[which];
  }
}

/** READ ... INTO LK-M03B-FLDT: MOVE record TO FLDT (alphanumeric group move, space padded). */
function intoFldt(area: M03bArea, record: Buffer): void {
  area.fldt.fill(0x20);
  record.copy(area.fldt, 0, 0, Math.min(record.length, area.fldt.length));
}
