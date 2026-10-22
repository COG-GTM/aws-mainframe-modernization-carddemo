/**
 * Batch runtime pieces shared by the CBTRN01C/02C/03C ports: DISPLAY capture,
 * the 9910-DISPLAY-IO-STATUS / 9999-ABEND-PROGRAM paragraphs, truncating
 * numeric stores and CURRENT-DATE.
 */
import { Decimal, fromScaledDigits, toScaledDigits, type NumericInput } from '../../common/codecs/index.js';

/** CEE3ABD was called: the run ends with the abend code (CardDemo always uses 999). */
export class Abend extends Error {
  constructor(readonly abcode: number) {
    super(`ABEND U${abcode}`);
    this.name = 'Abend';
  }
}

export interface BatchResult {
  /** RETURN-CODE at GOBACK, or the abend code (999) when the program abended. */
  returnCode: number;
  abended: boolean;
  /** DISPLAY output (SYSOUT), one entry per DISPLAY. */
  sysout: string[];
}

export interface BatchOptions {
  /** Called for every DISPLAY line as it happens (sysout is also returned). */
  onDisplay?: (line: string) => void;
}

/** Anything with a COBOL CLOSE (closed implicitly when the run unit ends). */
interface Closable {
  close(): unknown;
}

export class BatchContext {
  readonly sysout: string[] = [];
  private readonly opened: Closable[] = [];

  constructor(private readonly opts: BatchOptions = {}) {}

  display(...parts: string[]): void {
    const line = parts.join('');
    this.sysout.push(line);
    this.opts.onDisplay?.(line);
  }

  /** Registers a file for the implicit CLOSE at end of run unit (STOP RUN / abend). */
  track<T extends Closable>(file: T): T {
    this.opened.push(file);
    return file;
  }

  /** 9910-DISPLAY-IO-STATUS. */
  displayIoStatus(ioStatus: string): void {
    this.display('FILE STATUS IS: NNNN', ioStatus04(ioStatus));
  }

  /** 9999-ABEND-PROGRAM / Z-ABEND-PROGRAM: DISPLAY then CALL 'CEE3ABD' (ABCODE 999). */
  abend(): never {
    this.display('ABENDING PROGRAM');
    throw new Abend(999);
  }

  /** DISPLAY msg, 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM: the common error tail. */
  fail(message: string, ioStatus: string): never {
    this.display(message);
    this.displayIoStatus(ioStatus);
    return this.abend();
  }

  closeAll(): void {
    for (const f of this.opened) f.close();
  }
}

/** Runs a program body; `body` returns RETURN-CODE. Abends map to returnCode 999. */
export function runBatch(opts: BatchOptions, body: (ctx: BatchContext) => number): BatchResult {
  const ctx = new BatchContext(opts);
  try {
    const returnCode = body(ctx);
    return { returnCode, abended: false, sysout: ctx.sysout };
  } catch (e) {
    if (e instanceof Abend) return { returnCode: e.abcode, abended: true, sysout: ctx.sysout };
    throw e;
  } finally {
    ctx.closeAll();
  }
}

/**
 * IO-STATUS-04 as built by 9910-DISPLAY-IO-STATUS: '00' + status when numeric,
 * else status byte 1 followed by the binary value of byte 2 as 999.
 */
export function ioStatus04(ioStatus: string): string {
  const s = ioStatus.padEnd(2).slice(0, 2);
  if (!/^[0-9]{2}$/.test(s) || s[0] === '9') {
    return s[0]! + String(s.charCodeAt(1) & 0xff).padStart(3, '0').slice(-3);
  }
  return '00' + s;
}

/** INVALID KEY condition: any status of class 2. */
export const isInvalidKey = (status: string): boolean => status.startsWith('2');

/** Stores into PIC S9(int)V9(scale) without ON SIZE ERROR: high-order digits and extra decimals are lost. */
export function truncateTo(value: NumericInput, intDigits: number, scale: number, signed = true): Decimal {
  const { digitString, negative } = toScaledDigits(value, intDigits + scale, scale, signed);
  return fromScaledDigits(digitString, scale, negative);
}

/** GnuCOBOL DISPLAY of a signed DISPLAY numeric item: all digits then a trailing '+'/'-'. */
export function displaySigned(value: NumericInput, intDigits: number, scale: number): string {
  const { digitString, negative } = toScaledDigits(value, intDigits + scale, scale, true);
  return digitString + (negative ? '-' : '+');
}

/** Z-GET-DB2-FORMAT-TIMESTAMP: FUNCTION CURRENT-DATE as YYYY-MM-DD-HH.MM.SS.hh0000. */
export function db2FormatTimestamp(now: Date): string {
  const p = (n: number, w = 2) => String(n).padStart(w, '0');
  return (
    `${p(now.getFullYear(), 4)}-${p(now.getMonth() + 1)}-${p(now.getDate())}-` +
    `${p(now.getHours())}.${p(now.getMinutes())}.${p(now.getSeconds())}.${p(Math.floor(now.getMilliseconds() / 10))}0000`
  );
}

/** Raw text of bytes [offset, offset+length) of a record (Latin-1, i.e. byte-exact). */
export const slice = (rec: Uint8Array, offset: number, length: number): string =>
  Buffer.from(rec.subarray(offset, offset + length)).toString('latin1');
