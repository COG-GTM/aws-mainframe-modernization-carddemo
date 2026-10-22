/**
 * Shared pieces of the user-administration programs (COUSR00C-03C): the
 * CARDDEMO-COMMAREA extension each program declares after COCOM01Y and the
 * SEC-USER-DATA (CSUSR01Y) working-storage record.
 */
import { SecUserDataLayout } from '../../common/copybooks/index.js';
import { CarddemoCommarea, isSpacesOrLowValues } from '../framework/index.js';

export const WS_USRSEC_FILE = 'USRSEC';

/** CDEMO-CUxx-INFO (identical in COUSR00C/02C/03C), appended to COCOM01Y. */
const INFO = {
  usridFirst: [0, 8],
  usridLast: [8, 8],
  pageNum: [16, 8],
  nextPageFlg: [24, 1],
  usrSelFlg: [25, 1],
  usrSelected: [26, 8],
} as const;
const INFO_LENGTH = 34;
type InfoField = keyof typeof INFO;

/**
 * CARDDEMO-COMMAREA as extended by COUSR00C/02C/03C (COCOM01Y + CDEMO-CUxx-INFO).
 * Raw bytes are kept, so LOW-VALUES and short commareas survive bug-for-bug.
 */
export class UserAdminCommarea {
  base: CarddemoCommarea;
  info: Buffer;

  /** Fresh WORKING-STORAGE: LOW-VALUES except CDEMO-CUxx-NEXT-PAGE-FLG VALUE 'N'. */
  constructor() {
    this.base = new CarddemoCommarea();
    this.info = Buffer.alloc(INFO_LENGTH, 0);
    this.info.write('N', INFO.nextPageFlg[0], 'latin1');
  }

  static readonly LENGTH = CarddemoCommarea.LENGTH + INFO_LENGTH;

  /** MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA (space-padded / truncated). */
  static fromDfhCommarea(commarea: Buffer | undefined): UserAdminCommarea {
    const c = new UserAdminCommarea();
    c.base = CarddemoCommarea.fromDfhCommarea(commarea);
    c.info.fill(0x20);
    if (commarea && commarea.length > CarddemoCommarea.LENGTH) {
      commarea.copy(c.info, 0, CarddemoCommarea.LENGTH, Math.min(commarea.length, UserAdminCommarea.LENGTH));
    }
    return c;
  }

  get(f: InfoField): string {
    const [off, len] = INFO[f];
    return this.info.toString('latin1', off, off + len);
  }

  /** Alphanumeric MOVE (left-justified, space-padded, truncated). */
  set(f: InfoField, v: string): this {
    const [off, len] = INFO[f];
    Buffer.from(v.padEnd(len, ' ').slice(0, len), 'latin1').copy(this.info, off);
    return this;
  }

  isBlank(f: InfoField): boolean {
    return isSpacesOrLowValues(this.get(f));
  }

  /** CDEMO-CUxx-PAGE-NUM PIC 9(08) as a number (non-digits count as 0). */
  get pageNum(): number {
    let n = 0;
    for (const ch of this.get('pageNum')) n = n * 10 + (ch >= '0' && ch <= '9' ? ch.charCodeAt(0) - 48 : 0);
    return n;
  }

  /** MOVE / COMPUTE into PIC 9(08): high-order digits truncated. */
  set pageNum(n: number) {
    this.set('pageNum', String(n % 100_000_000).padStart(8, '0'));
  }

  toBuffer(): Buffer {
    return Buffer.concat([this.base.buffer, this.info]);
  }
}

/** SEC-USER-DATA (CSUSR01Y) working storage: fresh copy is LOW-VALUES. */
export class SecUserData {
  buffer: Buffer = Buffer.alloc(SecUserDataLayout.length, 0);

  private range(name: string): { offset: number; length: number } {
    return SecUserDataLayout.range(name);
  }

  get(name: string): string {
    const { offset, length } = this.range(name);
    return this.buffer.toString('latin1', offset, offset + length);
  }

  /** Alphanumeric MOVE into the field (padding with spaces). */
  set(name: string, v: string): this {
    const { offset, length } = this.range(name);
    Buffer.from(v.padEnd(length, ' ').slice(0, length), 'latin1').copy(this.buffer, offset);
    return this;
  }

  /** READ ... INTO(SEC-USER-DATA). */
  load(record: Buffer): void {
    this.buffer = Buffer.alloc(SecUserDataLayout.length, 0x20);
    record.copy(this.buffer, 0, 0, Math.min(record.length, this.buffer.length));
  }

  get id(): string { return this.get('SEC-USR-ID'); }
  set id(v: string) { this.set('SEC-USR-ID', v); }
  get fname(): string { return this.get('SEC-USR-FNAME'); }
  set fname(v: string) { this.set('SEC-USR-FNAME', v); }
  get lname(): string { return this.get('SEC-USR-LNAME'); }
  set lname(v: string) { this.set('SEC-USR-LNAME', v); }
  get pwd(): string { return this.get('SEC-USR-PWD'); }
  set pwd(v: string) { this.set('SEC-USR-PWD', v); }
  get type(): string { return this.get('SEC-USR-TYPE'); }
  set type(v: string) { this.set('SEC-USR-TYPE', v); }
}
