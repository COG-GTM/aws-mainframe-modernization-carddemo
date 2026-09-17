/**
 * Key-sequenced data set (KSDS) equivalent: an in-memory, key-ordered record
 * store loaded from and persisted to a fixed-width flat file.
 *
 * The API mirrors the COBOL verbs the batch and CICS programs use — `read`,
 * `startBrowse`/`readNext`, `write`, `rewrite`, `delete` — and reports outcomes
 * as file status codes rather than throwing.
 */

import { readFileSync, writeFileSync } from "node:fs";

import type { RecordCodec } from "@carddemo/domain";

import { FileStatus, failure, ok, type FileStatusCode, type IoResult } from "./status.js";

export interface KsdsOptions<T> {
  readonly name: string;
  readonly path: string;
  readonly codec: RecordCodec<T>;
  readonly keyOf: (record: T) => string;
}

export class Ksds<T> {
  private readonly records = new Map<string, T>();
  private orderedKeys: string[] = [];
  private cursor = 0;
  private open = false;

  constructor(private readonly options: KsdsOptions<T>) {}

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

    this.records.clear();
    for (const line of content.split("\n")) {
      if (line.trim().length === 0) {
        continue;
      }
      const record = this.options.codec.decode(line);
      this.records.set(this.options.keyOf(record), record);
    }
    this.reindex();
    this.cursor = 0;
    this.open = true;
    return FileStatus.ok;
  }

  close(): FileStatusCode {
    this.open = false;
    return FileStatus.ok;
  }

  read(key: string): IoResult<T> {
    if (!this.open) {
      return failure(FileStatus.fileNotOpen);
    }
    const record = this.records.get(key);
    return record === undefined ? failure(FileStatus.notFound) : ok(record);
  }

  /** `START ... KEY NOT LESS THAN`; positions the browse cursor. */
  startBrowse(key?: string): FileStatusCode {
    if (!this.open) {
      return FileStatus.fileNotOpen;
    }
    if (key === undefined) {
      this.cursor = 0;
      return FileStatus.ok;
    }
    const index = this.orderedKeys.findIndex((candidate) => candidate >= key);
    if (index < 0) {
      this.cursor = this.orderedKeys.length;
      return FileStatus.notFound;
    }
    this.cursor = index;
    return FileStatus.ok;
  }

  readNext(): IoResult<T> {
    if (!this.open) {
      return failure(FileStatus.fileNotOpen);
    }
    if (this.cursor >= this.orderedKeys.length) {
      return failure(FileStatus.endOfFile);
    }
    const key = this.orderedKeys[this.cursor] as string;
    this.cursor += 1;
    return ok(this.records.get(key) as T);
  }

  write(record: T): FileStatusCode {
    if (!this.open) {
      return FileStatus.fileNotOpen;
    }
    const key = this.options.keyOf(record);
    if (this.records.has(key)) {
      return FileStatus.duplicateKey;
    }
    this.records.set(key, record);
    this.reindex();
    return FileStatus.ok;
  }

  rewrite(record: T): FileStatusCode {
    if (!this.open) {
      return FileStatus.fileNotOpen;
    }
    const key = this.options.keyOf(record);
    if (!this.records.has(key)) {
      return FileStatus.notFound;
    }
    this.records.set(key, record);
    return FileStatus.ok;
  }

  delete(key: string): FileStatusCode {
    if (!this.open) {
      return FileStatus.fileNotOpen;
    }
    if (!this.records.delete(key)) {
      return FileStatus.notFound;
    }
    this.reindex();
    return FileStatus.ok;
  }

  /** Every record in key order, without moving the browse cursor. */
  toArray(): T[] {
    return this.orderedKeys.map((key) => this.records.get(key) as T);
  }

  save(path = this.options.path): void {
    const lines = this.toArray().map((record) => this.options.codec.encode(record));
    writeFileSync(path, `${lines.join("\n")}\n`, "latin1");
  }

  private reindex(): void {
    this.orderedKeys = [...this.records.keys()].sort();
  }
}
