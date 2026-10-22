/**
 * COBOL file I/O over a VSAM cluster (ORGANIZATION INDEXED or SEQUENTIAL/ESDS),
 * returning and recording FILE STATUS codes like IBM Enterprise COBOL.
 *
 * ```ts
 * const acct = new IndexedFile(catalog, 'ACCTFILE', { access: 'DYNAMIC' });
 * acct.open('I-O');                       // '00' (or '35' if not defined)
 * const { status, record } = acct.read(key); // '00' / '23'
 * acct.rewrite(updatedBuffer);            // '00'
 * acct.close();
 * ```
 */
import { type Key, type IndexEntry, keyOf, type OrderedIndex, toKey, type VsamDataset } from './dataset.js';
import type { VsamCatalog } from './catalog.js';
import { FileStatus, type FileStatusCode } from './status.js';

export type OpenMode = 'INPUT' | 'OUTPUT' | 'I-O' | 'EXTEND';
export type AccessMode = 'SEQUENTIAL' | 'RANDOM' | 'DYNAMIC';
export type StartCondition = '=' | '>' | '>=' | '<' | '<=';

export interface IoResult {
  status: FileStatusCode;
  /** Record read (a copy), when the operation reads one. */
  record?: Buffer;
}

abstract class CobolFileBase {
  /** Last FILE STATUS. */
  status: FileStatusCode = FileStatus.SUCCESS;
  protected mode: OpenMode | undefined;
  protected ds: VsamDataset | undefined;

  constructor(
    protected readonly catalog: VsamCatalog,
    /** DD name / dataset name resolved through the catalog at OPEN time. */
    readonly assign: string,
  ) {}

  get isOpen(): boolean {
    return this.mode !== undefined;
  }

  get openMode(): OpenMode | undefined {
    return this.mode;
  }

  protected set(status: FileStatusCode, record?: Buffer): IoResult {
    this.status = status;
    return record ? { status, record } : { status };
  }

  protected openDataset(mode: OpenMode, organization: 'KSDS' | 'ESDS'): FileStatusCode | VsamDataset {
    if (this.mode) return FileStatus.ALREADY_OPEN;
    const ds = this.catalog.get(this.assign);
    if (!ds) return FileStatus.FILE_NOT_FOUND;
    // Reject a wrong organization before OPEN OUTPUT empties the cluster.
    if (ds.def.organization !== organization) return FileStatus.OPEN_MODE_NOT_ALLOWED;
    if (mode === 'OUTPUT') ds.clear();
    this.ds = ds;
    this.mode = mode;
    return ds;
  }

  close(): IoResult {
    if (!this.mode) return this.set(FileStatus.NOT_OPEN);
    this.mode = undefined;
    this.ds = undefined;
    this.onClose();
    return this.set(FileStatus.SUCCESS);
  }

  protected abstract onClose(): void;

  protected canRead(): boolean {
    return this.mode === 'INPUT' || this.mode === 'I-O';
  }

  protected canWrite(): boolean {
    return this.mode === 'OUTPUT' || this.mode === 'I-O' || this.mode === 'EXTEND';
  }
}

/** Sequential position: last entry returned (or positioned at by START, not yet consumed). */
interface Cursor {
  entry: IndexEntry;
  consumed: boolean;
  /** Prime keys of same-key duplicates before `entry` (to resume correctly if it is deleted). */
  preceding: readonly string[];
}

export interface IndexedFileOptions {
  access?: AccessMode;
}

/** ORGANIZATION IS INDEXED file (VSAM KSDS, optionally via ALTERNATE RECORD KEYs). */
export class IndexedFile extends CobolFileBase {
  readonly access: AccessMode;
  /** Key of reference: undefined = prime key, else AIX name. */
  private keyName: string | undefined;
  /** Next sequential position: the last entry returned plus whether it is still to be returned. */
  private cursor: Cursor | 'start' | 'eof' | 'invalid' = 'invalid';
  /** Prime key of the last successful READ (for sequential REWRITE/DELETE). */
  private lastRead: Key | undefined;
  private lastWrittenKey: string | undefined;

  constructor(catalog: VsamCatalog, assign: string, opts: IndexedFileOptions = {}) {
    super(catalog, assign);
    this.access = opts.access ?? 'SEQUENTIAL';
  }

  protected onClose(): void {
    this.cursor = 'invalid';
    this.lastRead = undefined;
    this.lastWrittenKey = undefined;
    this.keyName = undefined;
  }

  open(mode: OpenMode): IoResult {
    const res = this.openDataset(mode, 'KSDS');
    if (typeof res === 'string') return this.set(res);
    this.cursor = 'start';
    this.keyName = undefined;
    return this.set(FileStatus.SUCCESS);
  }

  private at(idx: OrderedIndex, entry: IndexEntry, consumed: boolean): Cursor {
    const dups = idx.primariesFor(entry.key);
    return { entry, consumed, preceding: dups.slice(0, Math.max(0, dups.indexOf(entry.primary))) };
  }

  private dataset(): VsamDataset {
    return this.ds!;
  }

  /** True when other records share the alternate key of `entry` after it (status 02). */
  private dupFollows(entry: IndexEntry): boolean {
    if (!this.keyName) return false;
    const idx = this.dataset().index(this.keyName);
    const list = idx.primariesFor(entry.key);
    return list.indexOf(entry.primary) < list.length - 1;
  }

  /**
   * Random READ (ACCESS RANDOM/DYNAMIC): READ file [KEY IS keyName] with the
   * key value given explicitly (in COBOL it is taken from the record area).
   * Establishes the key of reference and position for READ NEXT.
   */
  read(keyValue: string | Uint8Array, keyName?: string): IoResult {
    if (!this.mode) return this.set(FileStatus.READ_NOT_OPEN_INPUT);
    if (!this.canRead()) return this.set(FileStatus.READ_NOT_OPEN_INPUT);
    if (this.access === 'SEQUENTIAL') throw new Error(`${this.assign}: random READ needs ACCESS RANDOM or DYNAMIC`);
    const ds = this.dataset();
    const idx = ds.index(keyName);
    const ck = ds.collate(toKey(keyValue));
    const primaries = idx.primariesFor(ck);
    const first = primaries[0];
    if (first === undefined) {
      this.cursor = 'invalid';
      this.lastRead = undefined;
      return this.set(FileStatus.NOT_FOUND);
    }
    this.keyName = keyName && keyName !== ds.name ? keyName : undefined;
    const entry = { key: ck, primary: first };
    this.cursor = this.at(idx, entry, true);
    const record = ds.recordAt(first)!;
    this.lastRead = keyOf(record, ds.index().spec);
    return this.set(this.dupFollows(entry) ? FileStatus.DUPLICATE_ALT_KEY : FileStatus.SUCCESS, record);
  }

  /** Sequential READ / READ NEXT. */
  readNext(): IoResult {
    if (!this.mode || !this.canRead()) return this.set(FileStatus.READ_NOT_OPEN_INPUT);
    if (this.access === 'RANDOM') throw new Error(`${this.assign}: READ NEXT needs ACCESS SEQUENTIAL or DYNAMIC`);
    if (this.cursor === 'eof' || this.cursor === 'invalid') {
      this.lastRead = undefined;
      return this.set(FileStatus.NO_NEXT_RECORD);
    }
    const ds = this.dataset();
    const idx = ds.index(this.keyName);
    const entries = idx.entries();
    let pos: number;
    if (this.cursor === 'start') pos = 0;
    else {
      const at = idx.positionOf(this.cursor.entry);
      if (at >= 0) pos = this.cursor.consumed ? at + 1 : at;
      else pos = idx.nextAfterRemoved(this.cursor.entry, this.cursor.preceding);
    }
    const entry = entries[pos];
    if (!entry) {
      this.cursor = 'eof';
      this.lastRead = undefined;
      return this.set(FileStatus.END_OF_FILE);
    }
    this.cursor = this.at(idx, entry, true);
    const record = ds.recordAt(entry.primary)!;
    this.lastRead = keyOf(record, ds.index().spec);
    return this.set(this.dupFollows(entry) ? FileStatus.DUPLICATE_ALT_KEY : FileStatus.SUCCESS, record);
  }

  /** Sequential READ (same as READ NEXT for ACCESS SEQUENTIAL). */
  readSequential(): IoResult {
    return this.readNext();
  }

  /** READ PREVIOUS (IBM extension / GnuCOBOL). */
  readPrevious(): IoResult {
    if (!this.mode || !this.canRead()) return this.set(FileStatus.READ_NOT_OPEN_INPUT);
    if (this.cursor === 'invalid' || this.cursor === 'start') {
      this.lastRead = undefined;
      return this.set(FileStatus.NO_NEXT_RECORD);
    }
    const ds = this.dataset();
    const idx = ds.index(this.keyName);
    const entries = idx.entries();
    let pos: number;
    if (this.cursor === 'eof') pos = entries.length - 1;
    else {
      const at = idx.positionOf(this.cursor.entry);
      if (at >= 0) pos = this.cursor.consumed ? at - 1 : at;
      else pos = idx.prevBeforeRemoved(this.cursor.entry, this.cursor.preceding);
    }
    const entry = pos >= 0 ? entries[pos] : undefined;
    if (!entry) {
      this.cursor = 'invalid';
      this.lastRead = undefined;
      return this.set(FileStatus.END_OF_FILE);
    }
    this.cursor = this.at(idx, entry, true);
    const record = ds.recordAt(entry.primary)!;
    this.lastRead = keyOf(record, ds.index().spec);
    return this.set(FileStatus.SUCCESS, record);
  }

  /** START file KEY <cond> keyValue [KEY IS keyName]. Generic (shorter) keys compare on the prefix. */
  start(condition: StartCondition, keyValue: string | Uint8Array, keyName?: string): IoResult {
    if (!this.mode || !this.canRead()) return this.set(FileStatus.READ_NOT_OPEN_INPUT);
    if (this.access === 'RANDOM') throw new Error(`${this.assign}: START needs ACCESS SEQUENTIAL or DYNAMIC`);
    const ds = this.dataset();
    const idx = ds.index(keyName);
    const entries = idx.entries();
    const ck = ds.collate(toKey(keyValue));
    const prefix = (e: IndexEntry) => e.key.slice(0, ck.length);
    let pos = -1;
    switch (condition) {
      case '=': {
        const p = idx.seek(ck);
        if (entries[p] && prefix(entries[p]!) === ck) pos = p;
        break;
      }
      case '>=':
        pos = idx.seek(ck);
        break;
      case '>': {
        let p = idx.seek(ck);
        while (entries[p] && prefix(entries[p]!) <= ck) p++;
        pos = p;
        break;
      }
      case '<=': {
        let p = entries.length - 1;
        while (p >= 0 && prefix(entries[p]!) > ck) p--;
        pos = p;
        break;
      }
      case '<': {
        let p = entries.length - 1;
        while (p >= 0 && prefix(entries[p]!) >= ck) p--;
        pos = p;
        break;
      }
    }
    this.lastRead = undefined;
    const entry = pos >= 0 ? entries[pos] : undefined;
    if (!entry) {
      this.cursor = 'invalid';
      return this.set(FileStatus.NOT_FOUND);
    }
    this.keyName = keyName && keyName !== ds.name ? keyName : undefined;
    this.cursor = this.at(idx, entry, false);
    return this.set(FileStatus.SUCCESS);
  }

  write(record: Uint8Array): IoResult {
    if (!this.mode || !this.canWrite()) return this.set(FileStatus.WRITE_NOT_OPEN_OUTPUT);
    const ds = this.dataset();
    const pk = ds.collate(keyOf(record, ds.index().spec));
    if (this.access === 'SEQUENTIAL' && this.mode !== 'I-O') {
      if (this.lastWrittenKey !== undefined && pk <= this.lastWrittenKey) {
        return this.set(ds.read(keyOf(record, ds.index().spec)) ? FileStatus.DUPLICATE_KEY : FileStatus.SEQUENCE_ERROR);
      }
      if (this.mode === 'EXTEND') {
        const last = ds.index().entries().at(-1);
        if (last && pk <= last.key) return this.set(FileStatus.SEQUENCE_ERROR);
      }
    }
    const res = ds.insert(record);
    if (!res.ok) return this.set(FileStatus.DUPLICATE_KEY);
    this.lastWrittenKey = pk;
    return this.set(res.duplicateAltKey ? FileStatus.DUPLICATE_ALT_KEY : FileStatus.SUCCESS);
  }

  rewrite(record: Uint8Array): IoResult {
    if (!this.mode || this.mode !== 'I-O') return this.set(FileStatus.REWRITE_NOT_OPEN_IO);
    const ds = this.dataset();
    const key = keyOf(record, ds.index().spec);
    if (this.access === 'SEQUENTIAL') {
      if (this.lastRead === undefined) return this.set(FileStatus.NO_PRIOR_READ);
      if (key !== this.lastRead) return this.set(FileStatus.SEQUENCE_ERROR);
    }
    const res = ds.update(record);
    this.lastRead = undefined;
    if (!res.ok) return this.set(res.reason === 'NOTFND' ? FileStatus.NOT_FOUND : FileStatus.DUPLICATE_KEY);
    return this.set(res.duplicateAltKey ? FileStatus.DUPLICATE_ALT_KEY : FileStatus.SUCCESS);
  }

  /** DELETE: sequential access deletes the last record read; otherwise the given prime key. */
  delete(keyValue?: string | Uint8Array): IoResult {
    if (!this.mode || this.mode !== 'I-O') return this.set(FileStatus.REWRITE_NOT_OPEN_IO);
    const ds = this.dataset();
    let key: Key;
    if (this.access === 'SEQUENTIAL') {
      if (this.lastRead === undefined) return this.set(FileStatus.NO_PRIOR_READ);
      key = this.lastRead;
    } else {
      if (keyValue === undefined) throw new Error(`${this.assign}: DELETE needs the prime key in RANDOM/DYNAMIC access`);
      key = toKey(keyValue);
    }
    this.lastRead = undefined;
    return this.set(ds.delete(key) ? FileStatus.SUCCESS : FileStatus.NOT_FOUND);
  }
}

/** ORGANIZATION IS SEQUENTIAL over a VSAM ESDS (access is always sequential). */
export class EsdsFile extends CobolFileBase {
  private position = 0;
  private lastRead = -1;
  private atEnd = false;

  protected onClose(): void {
    this.position = 0;
    this.lastRead = -1;
    this.atEnd = false;
  }

  open(mode: OpenMode): IoResult {
    const res = this.openDataset(mode, 'ESDS');
    if (typeof res === 'string') return this.set(res);
    return this.set(FileStatus.SUCCESS);
  }

  read(): IoResult {
    if (!this.mode || !this.canRead()) return this.set(FileStatus.READ_NOT_OPEN_INPUT);
    if (this.atEnd) return this.set(FileStatus.NO_NEXT_RECORD);
    const record = this.ds!.esdsAt(this.position);
    if (!record) {
      this.atEnd = true;
      this.lastRead = -1;
      return this.set(FileStatus.END_OF_FILE);
    }
    this.lastRead = this.position++;
    return this.set(FileStatus.SUCCESS, record);
  }

  write(record: Uint8Array): IoResult {
    if (!this.mode || (this.mode !== 'OUTPUT' && this.mode !== 'EXTEND')) return this.set(FileStatus.WRITE_NOT_OPEN_OUTPUT);
    this.ds!.append(record);
    return this.set(FileStatus.SUCCESS);
  }

  rewrite(record: Uint8Array): IoResult {
    if (this.mode !== 'I-O') return this.set(FileStatus.REWRITE_NOT_OPEN_IO);
    if (this.lastRead < 0) return this.set(FileStatus.NO_PRIOR_READ);
    this.ds!.esdsReplace(this.lastRead, record);
    this.lastRead = -1;
    return this.set(FileStatus.SUCCESS);
  }
}
