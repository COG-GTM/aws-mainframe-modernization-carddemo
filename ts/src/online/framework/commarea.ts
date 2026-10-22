import { CarddemoCommareaLayout, CDEMO_USRTYP_ADMIN, CDEMO_USRTYP_USER } from '../../common/copybooks/index.js';

const LEN = CarddemoCommareaLayout.length;

/**
 * Byte view over CARDDEMO-COMMAREA (COCOM01Y). It keeps the raw bytes (so
 * uninitialised LOW-VALUES and garbage survive round trips bug-for-bug) and
 * exposes COBOL-style MOVEs by field name.
 */
export class CarddemoCommarea {
  readonly buffer: Buffer;

  /** Fresh WORKING-STORAGE copy (no VALUE clauses => LOW-VALUES). */
  constructor(buffer?: Buffer) {
    this.buffer = Buffer.alloc(LEN, 0);
    if (buffer) buffer.copy(this.buffer, 0, 0, Math.min(LEN, buffer.length));
  }

  /**
   * MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA: alphanumeric group move,
   * so a shorter commarea is space-padded and a longer one truncated.
   */
  static fromDfhCommarea(commarea: Buffer | undefined): CarddemoCommarea {
    const c = new CarddemoCommarea();
    c.buffer.fill(0x20);
    commarea?.copy(c.buffer, 0, 0, Math.min(LEN, commarea.length));
    return c;
  }

  static readonly LENGTH = LEN;

  /** Field text (latin1, exact length) by COBOL name, e.g. 'CDEMO-USER-ID'. */
  get(cobolName: string): string {
    const { offset, length } = CarddemoCommareaLayout.range(cobolName);
    return this.buffer.toString('latin1', offset, offset + length);
  }

  /** Alphanumeric MOVE (left-justified, space-padded). */
  set(cobolName: string, value: string): this {
    const { offset, length } = CarddemoCommareaLayout.range(cobolName);
    this.buffer.write(value.padEnd(length, ' ').slice(0, length), offset, length, 'latin1');
    return this;
  }

  /** Unsigned numeric DISPLAY MOVE (right-justified, zero-filled, high-order truncation). */
  setNumber(cobolName: string, value: number | bigint | string): this {
    const { offset, length } = CarddemoCommareaLayout.range(cobolName);
    const digits = String(value).replace(/^-/, '').padStart(length, '0').slice(-length);
    this.buffer.write(digits, offset, length, 'latin1');
    return this;
  }

  get fromTranid(): string { return this.get('CDEMO-FROM-TRANID'); }
  set fromTranid(v: string) { this.set('CDEMO-FROM-TRANID', v); }
  get fromProgram(): string { return this.get('CDEMO-FROM-PROGRAM'); }
  set fromProgram(v: string) { this.set('CDEMO-FROM-PROGRAM', v); }
  get toTranid(): string { return this.get('CDEMO-TO-TRANID'); }
  set toTranid(v: string) { this.set('CDEMO-TO-TRANID', v); }
  get toProgram(): string { return this.get('CDEMO-TO-PROGRAM'); }
  set toProgram(v: string) { this.set('CDEMO-TO-PROGRAM', v); }
  get userId(): string { return this.get('CDEMO-USER-ID'); }
  set userId(v: string) { this.set('CDEMO-USER-ID', v); }
  get userType(): string { return this.get('CDEMO-USER-TYPE'); }
  set userType(v: string) { this.set('CDEMO-USER-TYPE', v); }
  get pgmContext(): string { return this.get('CDEMO-PGM-CONTEXT'); }

  /** 88 CDEMO-USRTYP-ADMIN / CDEMO-USRTYP-USER. */
  get isAdmin(): boolean { return this.userType === CDEMO_USRTYP_ADMIN; }
  get isUser(): boolean { return this.userType === CDEMO_USRTYP_USER; }
  /** 88 CDEMO-PGM-ENTER VALUE 0 / CDEMO-PGM-REENTER VALUE 1. */
  get isPgmEnter(): boolean { return this.pgmContext === '0'; }
  get isPgmReenter(): boolean { return this.pgmContext === '1'; }
  /** SET CDEMO-PGM-ENTER TO TRUE / MOVE ZEROS TO CDEMO-PGM-CONTEXT. */
  setPgmEnter(): this { return this.set('CDEMO-PGM-CONTEXT', '0'); }
  /** SET CDEMO-PGM-REENTER TO TRUE. */
  setPgmReenter(): this { return this.set('CDEMO-PGM-CONTEXT', '1'); }

  /** True if the field is all SPACES or all LOW-VALUES (COBOL `= SPACES OR LOW-VALUES`). */
  isBlank(cobolName: string): boolean {
    const v = this.get(cobolName);
    return /^ *$/.test(v) || /^\0*$/.test(v);
  }

  toBuffer(): Buffer {
    return Buffer.from(this.buffer);
  }
}
