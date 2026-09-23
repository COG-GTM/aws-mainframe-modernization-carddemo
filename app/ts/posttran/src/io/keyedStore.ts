import { FILE_STATUS, FileAbendError } from './errors.ts';

/** Random read by key — the VSAM KSDS access used for `XREFFILE`. */
export interface KeyedReader<TRecord> {
  /** `READ ... INVALID KEY`: undefined stands for file status '23'. */
  read(key: string): TRecord | undefined;
}

/** Random read plus update — `OPEN I-O` on `ACCTFILE` and `TCATBALF`. */
export interface KeyedUpdater<TRecord> extends KeyedReader<TRecord> {
  write(record: TRecord): void;
  rewrite(record: TRecord): void;
}

/** Sequential write — `OPEN OUTPUT` on `DALYREJS`. */
export interface SequentialWriter {
  write(image: string): void;
}

export interface KeyedStoreOptions<TRecord> {
  readonly ddName: string;
  readonly keyOf: (record: TRecord) => string;
}

/**
 * In-memory VSAM KSDS equivalent: records indexed by their fixed-width key and
 * iterated in ascending key order, as a KSDS would be read sequentially.
 */
export class KeyedStore<TRecord> implements KeyedUpdater<TRecord> {
  readonly ddName: string;
  private readonly keyOf: (record: TRecord) => string;
  private readonly records = new Map<string, TRecord>();

  constructor(options: KeyedStoreOptions<TRecord>, initial: Iterable<TRecord> = []) {
    this.ddName = options.ddName;
    this.keyOf = options.keyOf;
    for (const record of initial) {
      this.records.set(this.keyOf(record), record);
    }
  }

  read(key: string): TRecord | undefined {
    return this.records.get(key);
  }

  /** `WRITE`: a duplicate key is file status '22' and abends the job. */
  write(record: TRecord): void {
    const key = this.keyOf(record);
    if (this.records.has(key)) {
      throw new FileAbendError(
        `DUPLICATE KEY ${key} ON WRITE`,
        this.ddName,
        FILE_STATUS.duplicateKey,
      );
    }
    this.records.set(key, record);
  }

  /** `REWRITE`: an unknown key is `INVALID KEY` / file status '23'. */
  rewrite(record: TRecord): void {
    const key = this.keyOf(record);
    if (!this.records.has(key)) {
      throw new FileAbendError(
        `RECORD NOT FOUND ${key} ON REWRITE`,
        this.ddName,
        FILE_STATUS.recordNotFound,
      );
    }
    this.records.set(key, record);
  }

  /** Records in ascending key order, i.e. KSDS sequential read order. */
  all(): TRecord[] {
    return [...this.records.keys()].sort().map((key) => {
      const record = this.records.get(key);
      if (record === undefined) {
        throw new Error(`missing record for key ${key}`);
      }
      return record;
    });
  }

  get size(): number {
    return this.records.size;
  }
}

/**
 * Sequential load of a KSDS, i.e. `OPEN OUTPUT` on `TRANFILE`: the dataset is
 * replaced, and records must arrive in ascending key order or the WRITE fails
 * (CBTRN02C l.564-578 abends on any status other than '00').
 */
export class KeyedLoader<TRecord> {
  readonly ddName: string;
  private readonly keyOf: (record: TRecord) => string;
  private readonly loaded: TRecord[] = [];
  private lastKey: string | undefined;

  constructor(options: KeyedStoreOptions<TRecord>) {
    this.ddName = options.ddName;
    this.keyOf = options.keyOf;
  }

  write(record: TRecord): void {
    const key = this.keyOf(record);
    if (this.lastKey !== undefined && key <= this.lastKey) {
      const status = key === this.lastKey ? FILE_STATUS.duplicateKey : FILE_STATUS.recordNotFound;
      throw new FileAbendError(
        `KEY ${key} OUT OF SEQUENCE AFTER ${this.lastKey} ON SEQUENTIAL LOAD`,
        this.ddName,
        status,
      );
    }
    this.lastKey = key;
    this.loaded.push(record);
  }

  all(): readonly TRecord[] {
    return this.loaded;
  }

  get size(): number {
    return this.loaded.length;
  }
}
