/**
 * Runtime pieces shared by the Session 2 batch ports (CBACT01C-04C, CBCUS01C):
 * DISPLAY capture, 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM (CEE3ABD) and
 * COBOL numeric store semantics for intermediate WORKING-STORAGE items.
 */
import { closeSync, openSync, writeSync } from 'node:fs';
import { asciiToEbcdic } from '../../common/codecs/ebcdic.js';
import { Decimal, fromScaledDigits, toScaledDigits, type NumericInput } from '../../common/codecs/decimal.js';
import { loadSeedCatalog } from '../../common/seed/index.js';
import type { VsamCatalog } from '../../common/vsam/catalog.js';

/** Raised by 9999-ABEND-PROGRAM: CALL 'CEE3ABD' USING ABCODE (999), TIMING (0). */
export class BatchAbend extends Error {
  constructor(readonly abendCode: number) {
    super(`ABEND U${String(abendCode).padStart(4, '0')}`);
    this.name = 'BatchAbend';
  }
}

export interface BatchRunResult {
  /** RETURN-CODE at GOBACK (none of these programs set it, so 0), or undefined after an abend. */
  returnCode: number | undefined;
  /** True when the program ended through CEE3ABD. */
  abended: boolean;
  /** CEE3ABD user abend code (999) when abended. */
  abendCode?: number;
  /** SYSOUT: one entry per DISPLAY statement, exactly as COBOL writes it (no trailing-space trimming). */
  display: string[];
}

export interface BatchRunOptions {
  /** VSAM clusters; defaults to a fresh `loadSeedCatalog()` (app/data seed). */
  catalog?: VsamCatalog;
  /** Also echo each DISPLAY line to this sink (e.g. `console.log`). */
  sysout?: (line: string) => void;
}

/** SYSOUT collector. */
export class Sysout {
  readonly lines: string[] = [];
  constructor(private readonly sink?: (line: string) => void) {}
  display(...items: (string | Uint8Array)[]): void {
    const line = items.map((i) => (typeof i === 'string' ? i : Buffer.from(i).toString('latin1'))).join('');
    this.lines.push(line);
    this.sink?.(line);
  }
}

export function resolveCatalog(opts: BatchRunOptions): VsamCatalog {
  return opts.catalog ?? loadSeedCatalog().catalog;
}

/**
 * Runs a ported PROCEDURE DIVISION: GOBACK -> RC 0, CEE3ABD -> abended.
 * Other exceptions propagate (they are porting bugs, not COBOL behaviour).
 */
export function runProgram<T extends object>(
  opts: BatchRunOptions,
  body: (out: Sysout) => T,
  onAbend: () => T,
): BatchRunResult & T {
  const out = new Sysout(opts.sysout);
  try {
    const extra = body(out);
    return { returnCode: 0, abended: false, display: out.lines, ...extra };
  } catch (e) {
    if (e instanceof BatchAbend) {
      return { returnCode: undefined, abended: true, abendCode: e.abendCode, display: out.lines, ...onAbend() };
    }
    throw e;
  }
}

/** 9999-ABEND-PROGRAM. */
export function abendProgram(out: Sysout): never {
  out.display('ABENDING PROGRAM');
  throw new BatchAbend(999);
}

/**
 * 9910-DISPLAY-IO-STATUS. A numeric status '0035' style; a non-numeric status or
 * one starting with '9' shows STAT1 followed by the binary value of the STAT2
 * byte as 3 digits. That byte value is the z/OS (EBCDIC) code point, e.g.
 * '9' + '3' -> X'F3' -> '9243'.
 */
export function displayIoStatus(out: Sysout, ioStatus: string): void {
  const s = ioStatus.padEnd(2, ' ').slice(0, 2);
  const stat1 = s[0]!;
  const stat2 = s[1]!;
  if (!/^[0-9]{2}$/.test(s) || stat1 === '9') {
    const code = asciiToEbcdic(Buffer.from(stat2, 'latin1'))[0]!;
    out.display('FILE STATUS IS: NNNN', stat1 + String(code % 1000).padStart(3, '0'));
  } else {
    out.display('FILE STATUS IS: NNNN', '00' + s);
  }
}

/**
 * Stores a value into a numeric item PIC [S]9(digits-scale)V9(scale) the way
 * COMPUTE/ADD/MOVE without ROUNDED and without ON SIZE ERROR do: excess
 * decimals truncated toward zero, excess high-order integer digits dropped,
 * sign dropped for unsigned receivers.
 */
export function storeNumeric(value: NumericInput, digits: number, scale: number, signed = true): Decimal {
  const { digitString, negative } = toScaledDigits(value, digits, scale, signed);
  return fromScaledDigits(digitString, scale, negative);
}

/**
 * ORGANIZATION SEQUENTIAL output file (RECFM F/FB or V/VB). OPEN OUTPUT
 * creates/truncates the path; with no path the records are only kept in memory.
 */
export class OutputFile {
  private fd: number | undefined;
  constructor(
    private readonly path: string | undefined,
    private readonly variable: boolean,
  ) {}
  open(): string {
    if (this.path === undefined) return '00';
    try {
      this.fd = openSync(this.path, 'w');
      return '00';
    } catch {
      return '30';
    }
  }
  write(data: Buffer): string {
    if (this.fd === undefined) return '00';
    if (this.variable) {
      const rdw = Buffer.alloc(4);
      rdw.writeUInt16BE(data.length + 4, 0);
      writeSync(this.fd, rdw);
    }
    writeSync(this.fd, data);
    return '00';
  }
  close(): void {
    if (this.fd !== undefined) closeSync(this.fd);
    this.fd = undefined;
  }
}

export { Decimal };
