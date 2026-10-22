/**
 * Sequential disk files for batch programs: LINE SEQUENTIAL (text lines) and
 * fixed-length RECORD SEQUENTIAL (no delimiters, e.g. EBCDIC dumps), with
 * COBOL OPEN/READ/WRITE/CLOSE semantics and FILE STATUS codes.
 * I/O is synchronous so ported PROCEDURE DIVISION logic keeps its shape.
 */
import { closeSync, existsSync, openSync, readFileSync, writeSync } from 'node:fs';
import { FileStatus, type FileStatusCode } from '../vsam/status.js';

export type SequentialOrganization = 'LINE' | 'FIXED';
export type SequentialOpenMode = 'INPUT' | 'OUTPUT' | 'EXTEND';

export interface SequentialFileOptions {
  organization: SequentialOrganization;
  /** Record length: short LINE records are space padded to it on READ; FIXED records are split by it. */
  recordLength: number;
  /** LINE: strip trailing spaces on WRITE (GnuCOBOL default). Default false (records kept full width). */
  trimTrailing?: boolean;
  /** LINE: line terminator on WRITE. Default '\n'. */
  newline?: string;
  /** Padding byte for short records (0x20 ASCII space, 0x40 EBCDIC space). */
  padByte?: number;
  /** OPTIONAL file: OPEN INPUT of a missing file gives status 05 and immediate EOF. */
  optional?: boolean;
}

export interface SequentialReadResult {
  status: FileStatusCode;
  record?: Buffer;
}

/** Splits a LINE SEQUENTIAL file into records (CRLF/LF), padding each to `recordLength` when given. */
export function readLineRecords(path: string, recordLength?: number, padByte = 0x20): Buffer[] {
  const text = readFileSync(path).toString('latin1');
  if (text.length === 0) return [];
  const lines = text.split(/\r?\n/);
  if (lines.length > 0 && lines[lines.length - 1] === '') lines.pop();
  return lines.map((l) => pad(Buffer.from(l, 'latin1'), recordLength, padByte));
}

/** Splits a fixed-length file into records. A trailing partial record is padded. */
export function readFixedRecords(path: string, recordLength: number, padByte = 0x20): Buffer[] {
  const data = readFileSync(path);
  const out: Buffer[] = [];
  for (let i = 0; i < data.length; i += recordLength) out.push(pad(data.subarray(i, i + recordLength), recordLength, padByte));
  return out;
}

function pad(buf: Buffer, length: number | undefined, padByte: number): Buffer {
  if (length === undefined || buf.length >= length) return Buffer.from(buf);
  const out = Buffer.alloc(length, padByte);
  buf.copy(out);
  return out;
}

/** COBOL-style sequential file. */
export class SequentialFile {
  status: FileStatusCode = FileStatus.SUCCESS;
  private mode: SequentialOpenMode | undefined;
  private records: Buffer[] = [];
  private pos = 0;
  private eof = false;
  private fd: number | undefined;

  constructor(
    readonly path: string,
    readonly opts: SequentialFileOptions,
  ) {}

  private set(status: FileStatusCode, record?: Buffer): SequentialReadResult {
    this.status = status;
    return record ? { status, record } : { status };
  }

  open(mode: SequentialOpenMode): SequentialReadResult {
    if (this.mode) return this.set(FileStatus.ALREADY_OPEN);
    const padByte = this.opts.padByte ?? 0x20;
    if (mode === 'INPUT') {
      if (!existsSync(this.path)) {
        if (!this.opts.optional) return this.set(FileStatus.FILE_NOT_FOUND);
        this.records = [];
        this.pos = 0;
        this.eof = false;
        this.mode = mode;
        return this.set(FileStatus.OPTIONAL_NOT_PRESENT);
      }
      this.records =
        this.opts.organization === 'LINE'
          ? readLineRecords(this.path, this.opts.recordLength, padByte)
          : readFixedRecords(this.path, this.opts.recordLength, padByte);
      this.pos = 0;
      this.eof = false;
    } else {
      this.fd = openSync(this.path, mode === 'OUTPUT' ? 'w' : 'a');
    }
    this.mode = mode;
    return this.set(FileStatus.SUCCESS);
  }

  read(): SequentialReadResult {
    if (this.mode !== 'INPUT') return this.set(FileStatus.READ_NOT_OPEN_INPUT);
    if (this.eof) return this.set(FileStatus.NO_NEXT_RECORD);
    const rec = this.records[this.pos];
    if (!rec) {
      this.eof = true;
      return this.set(FileStatus.END_OF_FILE);
    }
    this.pos++;
    return this.set(FileStatus.SUCCESS, Buffer.from(rec));
  }

  write(record: Uint8Array | string): SequentialReadResult {
    if (this.mode !== 'OUTPUT' && this.mode !== 'EXTEND') return this.set(FileStatus.WRITE_NOT_OPEN_OUTPUT);
    let buf: Buffer = typeof record === "string" ? Buffer.from(record, "latin1") : Buffer.from(record);
    if (this.opts.organization === 'FIXED') {
      buf = pad(buf.subarray(0, this.opts.recordLength), this.opts.recordLength, this.opts.padByte ?? 0x20);
      writeSync(this.fd!, buf);
    } else {
      let text = buf.toString('latin1');
      if (this.opts.trimTrailing) text = text.replace(/ +$/, '');
      writeSync(this.fd!, Buffer.from(text + (this.opts.newline ?? '\n'), 'latin1'));
    }
    return this.set(FileStatus.SUCCESS);
  }

  close(): SequentialReadResult {
    if (!this.mode) return this.set(FileStatus.NOT_OPEN);
    if (this.fd !== undefined) closeSync(this.fd);
    this.fd = undefined;
    this.mode = undefined;
    this.records = [];
    return this.set(FileStatus.SUCCESS);
  }
}

/** Writes records as a LINE SEQUENTIAL file. */
export function writeLineRecords(path: string, records: Iterable<Uint8Array | string>, opts: { trimTrailing?: boolean; newline?: string } = {}): void {
  const f = new SequentialFile(path, { organization: 'LINE', recordLength: 0, ...opts });
  f.open('OUTPUT');
  for (const r of records) f.write(r);
  f.close();
}

/** Writes records as a fixed-length RECORD SEQUENTIAL file. */
export function writeFixedRecords(path: string, records: Iterable<Uint8Array>, recordLength: number, padByte = 0x20): void {
  const f = new SequentialFile(path, { organization: 'FIXED', recordLength, padByte });
  f.open('OUTPUT');
  for (const r of records) f.write(r);
  f.close();
}
