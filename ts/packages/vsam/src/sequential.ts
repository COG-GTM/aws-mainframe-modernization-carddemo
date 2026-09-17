/**
 * Sequential (QSAM / ESDS) file equivalents for the batch programs: a reader
 * that walks records in file order and a writer that appends fixed-width lines.
 */

import { appendFileSync, readFileSync, writeFileSync } from "node:fs";

import type { RecordCodec } from "@carddemo/domain";

import { FileStatus, failure, ok, type FileStatusCode, type IoResult } from "./status.js";

export interface SequentialOptions<T> {
  readonly name: string;
  readonly path: string;
  readonly codec: RecordCodec<T>;
}

export class SequentialReader<T> {
  private lines: string[] = [];
  private cursor = 0;
  private open = false;

  constructor(private readonly options: SequentialOptions<T>) {}

  get name(): string {
    return this.options.name;
  }

  openFile(): FileStatusCode {
    let content: string;
    try {
      content = readFileSync(this.options.path, "latin1");
    } catch {
      return FileStatus.openFailed;
    }
    this.lines = content.split("\n").filter((line) => line.trim().length > 0);
    this.cursor = 0;
    this.open = true;
    return FileStatus.ok;
  }

  readNext(): IoResult<T> {
    if (!this.open) {
      return failure(FileStatus.fileNotOpen);
    }
    if (this.cursor >= this.lines.length) {
      return failure(FileStatus.endOfFile);
    }
    const line = this.lines[this.cursor] as string;
    this.cursor += 1;
    return ok(this.options.codec.decode(line));
  }

  close(): FileStatusCode {
    this.open = false;
    return FileStatus.ok;
  }

  toArray(): T[] {
    return this.lines.map((line) => this.options.codec.decode(line));
  }
}

export class SequentialWriter<T> {
  private open = false;

  constructor(private readonly options: SequentialOptions<T>) {}

  get name(): string {
    return this.options.name;
  }

  /** `OPEN OUTPUT`: truncates any existing data set. */
  openFile(): FileStatusCode {
    writeFileSync(this.options.path, "", "latin1");
    this.open = true;
    return FileStatus.ok;
  }

  write(record: T): FileStatusCode {
    if (!this.open) {
      return FileStatus.fileNotOpen;
    }
    appendFileSync(this.options.path, `${this.options.codec.encode(record)}\n`, "latin1");
    return FileStatus.ok;
  }

  writeLine(line: string): FileStatusCode {
    if (!this.open) {
      return FileStatus.fileNotOpen;
    }
    appendFileSync(this.options.path, `${line}\n`, "latin1");
    return FileStatus.ok;
  }

  close(): FileStatusCode {
    this.open = false;
    return FileStatus.ok;
  }
}
