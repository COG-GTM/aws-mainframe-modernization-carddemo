/**
 * In-memory VSAM cluster: KSDS (prime key + alternate indexes) or ESDS.
 *
 * This is the storage engine only; it has no notion of OPEN modes or status
 * codes. Programs use it through `IndexedFile` (COBOL file I/O, FILE STATUS)
 * or `CicsFileControl` (EXEC CICS file commands, RESP). Records are fixed
 * length Buffers; keys are byte strings (Latin-1 JS strings, one char per byte).
 */
import { asciiToEbcdic } from '../codecs/ebcdic.js';

export interface KeySpec {
  readonly offset: number;
  readonly length: number;
}

export interface AlternateIndexDef extends KeySpec {
  /** AIX / PATH name, e.g. 'CXACAIX'. */
  readonly name: string;
  /** UNIQUEKEY (true) or NONUNIQUEKEY (false). */
  readonly unique: boolean;
}

export interface DatasetDef {
  readonly name: string;
  readonly organization: 'KSDS' | 'ESDS';
  readonly recordLength: number;
  /** Prime key (KSDS only). */
  readonly key?: KeySpec;
  readonly alternateIndexes?: readonly AlternateIndexDef[];
  /**
   * Key collating sequence. Data is held in ASCII, but a mainframe KSDS orders
   * keys by their EBCDIC bytes (letters before digits). Default 'ebcdic' so
   * browse order matches z/OS; use 'ascii' to match GnuCOBOL indexed files.
   */
  readonly collation?: 'ascii' | 'ebcdic';
}

export type Key = string;

export function keyOf(record: Uint8Array, spec: KeySpec): Key {
  return Buffer.from(record.buffer, record.byteOffset, record.byteLength).toString('latin1', spec.offset, spec.offset + spec.length);
}

export function toKey(key: string | Uint8Array): Key {
  return typeof key === 'string' ? key : Buffer.from(key.buffer, key.byteOffset, key.byteLength).toString('latin1');
}

export interface IndexEntry {
  /** Collation form of the index key. */
  readonly key: string;
  /** Collation form of the prime key of the record. */
  readonly primary: string;
}

function lowerBound(arr: readonly string[], value: string): number {
  let lo = 0;
  let hi = arr.length;
  while (lo < hi) {
    const mid = (lo + hi) >>> 1;
    if (arr[mid]! < value) lo = mid + 1;
    else hi = mid;
  }
  return lo;
}

/**
 * An ordered index (prime or alternate). For a non-unique AIX the prime keys of
 * records sharing an alternate key are kept in arrival order, as VSAM does for
 * upgrade-set maintenance; `sortDuplicates()` reproduces BLDINDEX (prime-key order).
 */
export class OrderedIndex {
  private readonly keys: string[] = [];
  private readonly dups = new Map<string, string[]>();
  private cache: IndexEntry[] | undefined;

  constructor(
    readonly name: string,
    readonly spec: KeySpec,
    readonly unique: boolean,
  ) {}

  get size(): number {
    return this.entries().length;
  }

  primariesFor(key: string): readonly string[] {
    return this.dups.get(key) ?? [];
  }

  add(key: string, primary: string): void {
    const list = this.dups.get(key);
    if (list) list.push(primary);
    else {
      this.dups.set(key, [primary]);
      this.keys.splice(lowerBound(this.keys, key), 0, key);
    }
    this.cache = undefined;
  }

  remove(key: string, primary: string): void {
    const list = this.dups.get(key);
    if (!list) return;
    const i = list.indexOf(primary);
    if (i >= 0) list.splice(i, 1);
    if (list.length === 0) {
      this.dups.delete(key);
      this.keys.splice(lowerBound(this.keys, key), 1);
    }
    this.cache = undefined;
  }

  clear(): void {
    this.keys.length = 0;
    this.dups.clear();
    this.cache = undefined;
  }

  sortDuplicates(): void {
    for (const list of this.dups.values()) list.sort();
    this.cache = undefined;
  }

  /** All entries in index order (duplicates in their stored order). */
  entries(): readonly IndexEntry[] {
    if (!this.cache) {
      const out: IndexEntry[] = [];
      for (const key of this.keys) for (const primary of this.dups.get(key)!) out.push({ key, primary });
      this.cache = out;
    }
    return this.cache;
  }

  /** Position of the first entry whose key is >= `key` (or > when `strict`). */
  seek(key: string, strict = false): number {
    const entries = this.entries();
    let lo = 0;
    let hi = entries.length;
    while (lo < hi) {
      const mid = (lo + hi) >>> 1;
      const k = entries[mid]!.key;
      if (strict ? k <= key : k < key) lo = mid + 1;
      else hi = mid;
    }
    return lo;
  }

  /**
   * Resume point after `entry` was removed while a cursor pointed at it: the
   * next surviving entry, skipping same-key duplicates in `preceding` (those
   * that came before it), so the rest of its duplicate group is not skipped.
   */
  nextAfterRemoved(entry: IndexEntry, preceding: readonly string[]): number {
    const entries = this.entries();
    let pos = this.seek(entry.key);
    while (entries[pos]?.key === entry.key && preceding.includes(entries[pos]!.primary)) pos++;
    return pos;
  }

  /** Like `nextAfterRemoved`, backwards: the last surviving entry before the removed one, or -1. */
  prevBeforeRemoved(entry: IndexEntry, preceding: readonly string[]): number {
    const entries = this.entries();
    const start = this.seek(entry.key);
    let pos = start - 1;
    for (let i = start; entries[i]?.key === entry.key; i++) if (preceding.includes(entries[i]!.primary)) pos = i;
    return pos;
  }

  /** Current position of a specific entry, or -1. */
  positionOf(entry: IndexEntry): number {
    const entries = this.entries();
    for (let i = this.seek(entry.key); i < entries.length && entries[i]!.key === entry.key; i++) {
      if (entries[i]!.primary === entry.primary) return i;
    }
    return -1;
  }
}

/** `duplicateIndexes`: names of the non-unique AIXs in which the record now shares its key with another record. */
export type InsertResult = { ok: true; duplicateAltKey: boolean; duplicateIndexes: string[] } | { ok: false; reason: 'DUPREC' | 'DUPALT'; index?: string };
export type UpdateResult = { ok: true; duplicateAltKey: boolean; duplicateIndexes: string[] } | { ok: false; reason: 'NOTFND' | 'DUPALT'; index?: string };

export class VsamDataset {
  readonly primaryIndex: OrderedIndex | undefined;
  readonly alternateIndexes = new Map<string, OrderedIndex>();
  private readonly data = new Map<string, Buffer>();
  private readonly esds: Buffer[] = [];
  private readonly ebcdic: boolean;

  constructor(readonly def: DatasetDef) {
    this.ebcdic = (def.collation ?? 'ebcdic') === 'ebcdic';
    if (def.organization === 'KSDS') {
      if (!def.key) throw new Error(`${def.name}: KSDS requires a prime key`);
      this.primaryIndex = new OrderedIndex(def.name, def.key, true);
      for (const a of def.alternateIndexes ?? []) this.alternateIndexes.set(a.name, new OrderedIndex(a.name, a, a.unique));
    }
  }

  get name(): string {
    return this.def.name;
  }

  get recordLength(): number {
    return this.def.recordLength;
  }

  get size(): number {
    return this.def.organization === 'KSDS' ? this.data.size : this.esds.length;
  }

  /** Raw key -> collation form (order-preserving per byte, so generic prefixes still work). */
  collate(key: Key): string {
    return this.ebcdic ? asciiToEbcdic(Buffer.from(key, 'latin1')).toString('latin1') : key;
  }

  /** Index by name; undefined/'' or the dataset name selects the prime index. */
  index(name?: string): OrderedIndex {
    const idx = !name || name === this.def.name ? this.primaryIndex : this.alternateIndexes.get(name);
    if (!idx) throw new Error(`${this.def.name}: no index '${name}'`);
    return idx;
  }

  private checkLength(record: Uint8Array): void {
    if (record.length !== this.def.recordLength) {
      throw new RangeError(`${this.def.name}: record length ${record.length}, expected ${this.def.recordLength}`);
    }
  }

  private ksds(): OrderedIndex {
    if (!this.primaryIndex) throw new Error(`${this.def.name}: not a KSDS`);
    return this.primaryIndex;
  }

  /** Reads a record by raw prime key (a copy), or undefined. */
  read(key: Key): Buffer | undefined {
    const rec = this.data.get(this.collate(key));
    return rec ? Buffer.from(rec) : undefined;
  }

  /** Record by collation prime key (internal; returns a copy). */
  recordAt(primary: string): Buffer | undefined {
    const rec = this.data.get(primary);
    return rec ? Buffer.from(rec) : undefined;
  }

  /** Raw key of an index entry, read back from the record. */
  rawKey(entry: IndexEntry, index: OrderedIndex): Key {
    const rec = this.data.get(entry.primary);
    return rec ? keyOf(rec, index.spec) : '';
  }

  private altConflicts(record: Uint8Array, selfPrimary?: string): { conflict?: string; duplicates: string[] } {
    const duplicates: string[] = [];
    for (const idx of this.alternateIndexes.values()) {
      const others = idx.primariesFor(this.collate(keyOf(record, idx.spec))).filter((p) => p !== selfPrimary);
      if (others.length > 0) {
        if (idx.unique) return { conflict: idx.name, duplicates };
        duplicates.push(idx.name);
      }
    }
    return { duplicates };
  }

  insert(record: Uint8Array): InsertResult {
    this.checkLength(record);
    const prime = this.ksds();
    const pk = this.collate(keyOf(record, prime.spec));
    if (this.data.has(pk)) return { ok: false, reason: 'DUPREC' };
    const alt = this.altConflicts(record);
    if (alt.conflict) return { ok: false, reason: 'DUPALT', index: alt.conflict };
    const copy = Buffer.from(record);
    this.data.set(pk, copy);
    prime.add(pk, pk);
    for (const idx of this.alternateIndexes.values()) idx.add(this.collate(keyOf(copy, idx.spec)), pk);
    return { ok: true, duplicateAltKey: alt.duplicates.length > 0, duplicateIndexes: alt.duplicates };
  }

  /** Replaces the record with the same prime key. */
  update(record: Uint8Array): UpdateResult {
    this.checkLength(record);
    const prime = this.ksds();
    const pk = this.collate(keyOf(record, prime.spec));
    const old = this.data.get(pk);
    if (!old) return { ok: false, reason: 'NOTFND' };
    const alt = this.altConflicts(record, pk);
    if (alt.conflict) return { ok: false, reason: 'DUPALT', index: alt.conflict };
    const copy = Buffer.from(record);
    for (const idx of this.alternateIndexes.values()) {
      const oldKey = this.collate(keyOf(old, idx.spec));
      const newKey = this.collate(keyOf(copy, idx.spec));
      if (oldKey !== newKey) {
        idx.remove(oldKey, pk);
        idx.add(newKey, pk);
      }
    }
    this.data.set(pk, copy);
    return { ok: true, duplicateAltKey: alt.duplicates.length > 0, duplicateIndexes: alt.duplicates };
  }

  /** Deletes by raw prime key; false when not found. */
  delete(key: Key): boolean {
    const prime = this.ksds();
    const pk = this.collate(key);
    const old = this.data.get(pk);
    if (!old) return false;
    this.data.delete(pk);
    prime.remove(pk, pk);
    for (const idx of this.alternateIndexes.values()) idx.remove(this.collate(keyOf(old, idx.spec)), pk);
    return true;
  }

  /** Empties the cluster (OPEN OUTPUT / IDCAMS DELETE+DEFINE). */
  clear(): void {
    this.data.clear();
    this.esds.length = 0;
    this.primaryIndex?.clear();
    for (const idx of this.alternateIndexes.values()) idx.clear();
  }

  /** IDCAMS BLDINDEX: re-orders non-unique AIX duplicates by prime key. */
  buildIndexes(): void {
    for (const idx of this.alternateIndexes.values()) idx.sortDuplicates();
  }

  /** ESDS: appends a record, returning its RBA. */
  append(record: Uint8Array): number {
    this.checkLength(record);
    if (this.def.organization !== 'ESDS') throw new Error(`${this.def.name}: not an ESDS`);
    const rba = this.esds.length * this.def.recordLength;
    this.esds.push(Buffer.from(record));
    return rba;
  }

  /** ESDS: record at relative position (0-based), copy. */
  esdsAt(position: number): Buffer | undefined {
    const rec = this.esds[position];
    return rec ? Buffer.from(rec) : undefined;
  }

  /** ESDS: replaces record at position (REWRITE; length cannot change). */
  esdsReplace(position: number, record: Uint8Array): void {
    this.checkLength(record);
    if (position < 0 || position >= this.esds.length) throw new RangeError(`${this.def.name}: no ESDS record ${position}`);
    this.esds[position] = Buffer.from(record);
  }

  /** All records in prime-key (KSDS) or entry (ESDS) order, as copies. */
  records(): Buffer[] {
    if (this.def.organization === 'ESDS') return this.esds.map((r) => Buffer.from(r));
    return this.ksds()
      .entries()
      .map((e) => Buffer.from(this.data.get(e.primary)!));
  }

  /** Bulk load (REPRO): inserts records in order then runs BLDINDEX. Returns rejected records. */
  load(records: Iterable<Uint8Array>): { loaded: number; rejected: { record: Buffer; reason: string }[] } {
    let loaded = 0;
    const rejected: { record: Buffer; reason: string }[] = [];
    for (const rec of records) {
      if (this.def.organization === 'ESDS') {
        this.append(rec);
        loaded++;
        continue;
      }
      const res = this.insert(rec);
      if (res.ok) loaded++;
      else rejected.push({ record: Buffer.from(rec), reason: res.reason });
    }
    this.buildIndexes();
    return { loaded, rejected };
  }
}
