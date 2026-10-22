/**
 * Shared pieces of the Session 8 transaction / bill-pay / report programs:
 * the extended CARDDEMO-COMMAREA (COCOM01Y + CDEMO-CTxx-INFO), COBOL MOVE
 * helpers and the "no RESP" CICS command check.
 */
import { CarddemoCommareaLayout } from '../../common/copybooks/index.js';
import { Decimal, fromScaledDigits, toScaledDigits, type NumericInput } from '../../common/codecs/decimal.js';
import { LATIN1_TO_CP037 } from '../../common/codecs/ebcdic.js';
import { CicsCondition, DFHRESP, type CommandResult } from '../framework/index.js';

export const LOW_VALUES_16 = '\0'.repeat(16);
export const HIGH_VALUES_16 = '\xff'.repeat(16);

interface Range { offset: number; length: number }

const BASE_LEN = CarddemoCommareaLayout.length;

/**
 * CDEMO-CT00-INFO / CDEMO-CT01-INFO / CDEMO-CT02-INFO / CDEMO-CB00-INFO: the
 * four programs append the same 58-byte group after COCOM01Y.
 */
const INFO: Readonly<Record<string, Range>> = {
  'TRNID-FIRST': { offset: BASE_LEN, length: 16 },
  'TRNID-LAST': { offset: BASE_LEN + 16, length: 16 },
  'PAGE-NUM': { offset: BASE_LEN + 32, length: 8 },
  'NEXT-PAGE-FLG': { offset: BASE_LEN + 40, length: 1 },
  'TRN-SEL-FLG': { offset: BASE_LEN + 41, length: 1 },
  'TRN-SELECTED': { offset: BASE_LEN + 42, length: 16 },
};

/** LENGTH OF CARDDEMO-COMMAREA in COTRN00C/01C/02C and COBIL00C (160 + 58). */
export const EXT_COMMAREA_LENGTH = BASE_LEN + 58;
/** LENGTH OF CARDDEMO-COMMAREA in CORPT00C (plain COCOM01Y). */
export const BASE_COMMAREA_LENGTH = BASE_LEN;

/**
 * Byte view of a program's CARDDEMO-COMMAREA (COCOM01Y, optionally followed by
 * CDEMO-xxxx-INFO). Raw bytes are kept so LOW-VALUES / garbage round-trip.
 */
export class ProgramCommarea {
  readonly buffer: Buffer;

  private constructor(length: number, fill: number) {
    this.buffer = Buffer.alloc(length, fill);
  }

  /** Fresh WORKING-STORAGE: no VALUE clauses (LOW-VALUES) except NEXT-PAGE-FLG VALUE 'N'. */
  static fresh(length: number): ProgramCommarea {
    const c = new ProgramCommarea(length, 0);
    if (length > BASE_LEN) c.set('NEXT-PAGE-FLG', 'N');
    return c;
  }

  /** MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA (space-padded / truncated group move). */
  static fromDfhCommarea(commarea: Buffer | undefined, length: number): ProgramCommarea {
    const c = new ProgramCommarea(length, 0x20);
    commarea?.copy(c.buffer, 0, 0, Math.min(length, commarea.length));
    return c;
  }

  private range(name: string): Range {
    const info = INFO[name];
    if (info) {
      if (info.offset + info.length > this.buffer.length) throw new Error(`commarea has no ${name}`);
      return info;
    }
    return CarddemoCommareaLayout.range(name);
  }

  /** Field text by COCOM01Y name (e.g. 'CDEMO-TO-PROGRAM') or INFO name (e.g. 'PAGE-NUM'). */
  get(name: string): string {
    const { offset, length } = this.range(name);
    return this.buffer.toString('latin1', offset, offset + length);
  }

  /** Alphanumeric MOVE (left-justified, space-padded, truncated). */
  set(name: string, value: string): this {
    const { offset, length } = this.range(name);
    this.buffer.write(value.padEnd(length, ' ').slice(0, length), offset, length, 'latin1');
    return this;
  }

  /** Value of an unsigned PIC 9(n) DISPLAY item (non-digits count by their CP037 low nibble). */
  getNum(name: string): number {
    return Number(displayDigits(this.get(name)));
  }

  /** MOVE n TO PIC 9(n) (high-order truncation, zero fill). */
  setNum(name: string, value: number): this {
    const { length } = this.range(name);
    return this.set(name, String(Math.abs(Math.trunc(value))).padStart(length, '0').slice(-length));
  }

  get toProgram(): string { return this.get('CDEMO-TO-PROGRAM'); }
  set toProgram(v: string) { this.set('CDEMO-TO-PROGRAM', v); }
  get fromProgram(): string { return this.get('CDEMO-FROM-PROGRAM'); }
  get isPgmReenter(): boolean { return this.get('CDEMO-PGM-CONTEXT') === '1'; }
  setPgmReenter(): this { return this.set('CDEMO-PGM-CONTEXT', '1'); }

  /**
   * RETURN-TO-PREV-SCREEN prologue shared by every program in this area:
   * default target COSGN00C, FROM-TRANID/FROM-PROGRAM, MOVE ZEROS TO CDEMO-PGM-CONTEXT.
   */
  prepareReturn(tranid: string, pgmname: string): string {
    if (isBlankField(this.toProgram)) this.toProgram = 'COSGN00C';
    this.set('CDEMO-FROM-TRANID', tranid);
    this.set('CDEMO-FROM-PROGRAM', pgmname);
    this.set('CDEMO-PGM-CONTEXT', '0');
    return this.toProgram.trim();
  }

  toBuffer(): Buffer {
    return Buffer.from(this.buffer);
  }
}

/** `x = SPACES OR LOW-VALUES` */
export const isBlankField = (s: string): boolean => /^ *$/.test(s) || /^\0*$/.test(s);

/** Digit string of a DISPLAY numeric item; a non-digit byte contributes its CP037 low nibble (mod 10). */
export function displayDigits(s: string): string {
  let out = '';
  for (const ch of s) {
    if (ch >= '0' && ch <= '9') out += ch;
    else out += String(((LATIN1_TO_CP037[ch.charCodeAt(0) & 0xff] ?? 0) & 0x0f) % 10);
  }
  return out;
}

/**
 * MOVE alphanumeric X(n) TO unsigned PIC 9(len) DISPLAY: the sender is treated
 * as an unsigned integer, right-aligned (left-truncated / zero-filled), and the
 * last byte gets the unsigned zone (Enterprise COBOL `OI X'F0'`), so a trailing
 * space becomes '0'. Other non-digit bytes are moved unchanged.
 */
export function moveAlnumToUnsigned(s: string, len: number): string {
  const aligned = s.length >= len ? s.slice(s.length - len) : s.padStart(len, '0');
  const last = aligned.slice(-1);
  return aligned.slice(0, -1) + displayDigits(last);
}

/** Numeric MOVE into a PIC [S]9(i)V9(f) item: truncation on both ends, sign dropped if unsigned. */
export function moveNumeric(value: NumericInput, digits: number, scale: number, signed: boolean): Decimal {
  const { digitString, negative } = toScaledDigits(value, digits, scale, signed);
  return fromScaledDigits(digitString, scale, negative);
}

/** A command coded without RESP: any condition other than NORMAL is raised (abend unless handled). */
export function requireNormal<T extends CommandResult>(command: string, r: T): T {
  if (r.resp !== DFHRESP.NORMAL) throw new CicsCondition(command, r.resp, r.resp2);
  return r;
}

export { Decimal };
