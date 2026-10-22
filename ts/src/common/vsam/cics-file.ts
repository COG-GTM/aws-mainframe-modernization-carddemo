/**
 * EXEC CICS file control (READ/WRITE/REWRITE/DELETE/UNLOCK and browse
 * STARTBR/READNEXT/READPREV/RESETBR/ENDBR) over VSAM clusters, returning RESP
 * conditions. One `CicsFileControl` instance represents one CICS task (UPDATE
 * locks and browses are task-scoped; `syncpoint()` ends them).
 *
 * File names are CSD FILE definitions; a file can be a base cluster or a PATH
 * over an alternate index (e.g. CXACAIX = CCXREF by account id, NONUNIQUE).
 */
import { type IndexEntry, keyOf, type OrderedIndex, toKey, type VsamDataset } from './dataset.js';
import type { VsamCatalog } from './catalog.js';
import { Resp, type RespCode } from './status.js';

export interface CicsFileDef {
  /** Catalog name of the base cluster. */
  dataset: string;
  /** AIX name when this file is a PATH. */
  path?: string;
}

export interface CicsResult {
  resp: RespCode;
  /** RESP2 (0 unless documented below). */
  resp2: number;
  /** INTO area (copy of the record). */
  record?: Buffer;
  /** Updated RIDFLD (full key of the record returned). */
  ridfld?: string;
}

export interface KeyedOptions {
  file: string;
  ridfld: string | Uint8Array;
  /** KEYLENGTH; with `generic` only the first KEYLENGTH bytes are compared. */
  keylength?: number;
  generic?: boolean;
}

export interface ReadOptions extends KeyedOptions {
  /** GTEQ (default EQUAL). */
  gteq?: boolean;
  update?: boolean;
}

export interface BrowseOptions {
  file: string;
  reqid?: number;
}

interface Browse {
  file: string;
  index: OrderedIndex;
  ds: VsamDataset;
  last: IndexEntry | undefined;
  lastDir: 'next' | 'prev' | undefined;
  /** Position established by STARTBR/RESETBR, before the first READNEXT/READPREV. */
  start: { pos: number; exact: boolean; highValues: boolean } | undefined;
}

const ok = (extra: Partial<CicsResult> = {}): CicsResult => ({ resp: Resp.NORMAL, resp2: 0, ...extra });
const fail = (resp: RespCode, resp2 = 0): CicsResult => ({ resp, resp2 });

export class CicsFileControl {
  private readonly files = new Map<string, CicsFileDef>();
  private readonly closed = new Set<string>();
  private readonly disabled = new Set<string>();
  private readonly locks = new Map<string, string>();
  private readonly browses = new Map<string, Browse>();

  constructor(
    private readonly catalog: VsamCatalog,
    files: Record<string, CicsFileDef>,
  ) {
    for (const [name, def] of Object.entries(files)) this.files.set(name, def);
  }

  /** CEMT SET FILE(x) CLOSED/OPEN/DISABLED/ENABLED equivalents. */
  setFileState(file: string, state: 'OPEN' | 'CLOSED' | 'ENABLED' | 'DISABLED'): void {
    if (state === 'CLOSED') this.closed.add(file);
    if (state === 'OPEN') this.closed.delete(file);
    if (state === 'DISABLED') this.disabled.add(file);
    if (state === 'ENABLED') this.disabled.delete(file);
  }

  private resolve(file: string): { ds: VsamDataset; index: OrderedIndex; path: boolean } | RespCode {
    const def = this.files.get(file);
    if (!def) return Resp.FILENOTFOUND;
    if (this.disabled.has(file)) return Resp.DISABLED;
    if (this.closed.has(file)) return Resp.NOTOPEN;
    const ds = this.catalog.get(def.dataset);
    if (!ds) return Resp.NOTOPEN;
    return { ds, index: ds.index(def.path), path: def.path !== undefined };
  }

  private searchKey(ds: VsamDataset, opts: KeyedOptions): string {
    let key = toKey(opts.ridfld);
    if (opts.generic && opts.keylength !== undefined) key = key.slice(0, opts.keylength);
    return ds.collate(key);
  }

  /** Locates the first entry matching (EQUAL, generic-aware) or >= (GTEQ) the key. */
  private locate(index: OrderedIndex, ck: string, generic: boolean, gteq: boolean): number {
    const entries = index.entries();
    const pos = index.seek(ck);
    const e = entries[pos];
    if (!e) return -1;
    if (gteq) return pos;
    const match = generic ? e.key.startsWith(ck) : e.key === ck;
    return match ? pos : -1;
  }

  private dupkeyAfter(index: OrderedIndex, entry: IndexEntry): boolean {
    if (index.unique) return false;
    const list = index.primariesFor(entry.key);
    return list.indexOf(entry.primary) < list.length - 1;
  }

  read(opts: ReadOptions): CicsResult {
    const r = this.resolve(opts.file);
    if (typeof r === 'number') return fail(r);
    const ck = this.searchKey(r.ds, opts);
    const pos = this.locate(r.index, ck, opts.generic ?? false, opts.gteq ?? false);
    if (pos < 0) return fail(Resp.NOTFND, 80);
    const entry = r.index.entries()[pos]!;
    const record = r.ds.recordAt(entry.primary)!;
    if (opts.update) this.locks.set(opts.file, entry.primary);
    const ridfld = keyOf(record, r.index.spec);
    // Via a non-unique path, DUPKEY reports that more records share this key.
    if (r.path && r.index.primariesFor(entry.key).length > 1) return { resp: Resp.DUPKEY, resp2: 0, record, ridfld };
    return ok({ record, ridfld });
  }

  write(opts: { file: string; ridfld: string | Uint8Array; from: Uint8Array }): CicsResult {
    const r = this.resolve(opts.file);
    if (typeof r === 'number') return fail(r);
    if (opts.from.length !== r.ds.recordLength) return fail(Resp.LENGERR);
    if (toKey(opts.ridfld) !== keyOf(opts.from, r.index.spec)) return fail(Resp.INVREQ);
    const res = r.ds.insert(opts.from);
    if (!res.ok) return fail(Resp.DUPREC, res.reason === 'DUPALT' ? 0 : 150);
    return ok();
  }

  rewrite(opts: { file: string; from: Uint8Array }): CicsResult {
    const r = this.resolve(opts.file);
    if (typeof r === 'number') return fail(r);
    const locked = this.locks.get(opts.file);
    if (locked === undefined) return fail(Resp.INVREQ, 30);
    if (opts.from.length !== r.ds.recordLength) return fail(Resp.LENGERR);
    const primeSpec = r.ds.index().spec;
    if (r.ds.collate(keyOf(opts.from, primeSpec)) !== locked) return fail(Resp.INVREQ);
    const res = r.ds.update(opts.from);
    this.locks.delete(opts.file);
    if (!res.ok) return fail(res.reason === 'NOTFND' ? Resp.NOTFND : Resp.DUPREC);
    return ok();
  }

  /** DELETE with RIDFLD (prime key, or alt key for a path), or after READ UPDATE when no RIDFLD. */
  delete(opts: { file: string; ridfld?: string | Uint8Array; keylength?: number; generic?: boolean }): CicsResult {
    const r = this.resolve(opts.file);
    if (typeof r === 'number') return fail(r);
    const primeSpec = r.ds.index().spec;
    if (opts.ridfld === undefined) {
      const locked = this.locks.get(opts.file);
      if (locked === undefined) return fail(Resp.INVREQ);
      this.locks.delete(opts.file);
      const rec = r.ds.recordAt(locked);
      if (!rec || !r.ds.delete(keyOf(rec, primeSpec))) return fail(Resp.NOTFND, 80);
      return ok();
    }
    const ck = this.searchKey(r.ds, { file: opts.file, ridfld: opts.ridfld, keylength: opts.keylength, generic: opts.generic });
    const pos = this.locate(r.index, ck, opts.generic ?? false, false);
    if (pos < 0) return fail(Resp.NOTFND, 80);
    const entry = r.index.entries()[pos]!;
    const rec = r.ds.recordAt(entry.primary)!;
    r.ds.delete(keyOf(rec, primeSpec));
    return ok();
  }

  unlock(opts: { file: string }): CicsResult {
    this.locks.delete(opts.file);
    return ok();
  }

  /** End of task / SYNCPOINT: releases UPDATE locks and browses. */
  syncpoint(): void {
    this.locks.clear();
    this.browses.clear();
  }

  private browseKey(file: string, reqid = 0): string {
    return `${file}\u0000${reqid}`;
  }

  /** STARTBR (GTEQ unless `equal`). RIDFLD of all X'FF' positions at end of file for READPREV. */
  startbr(opts: KeyedOptions & { reqid?: number; equal?: boolean; gteq?: boolean }): CicsResult {
    const r = this.resolve(opts.file);
    if (typeof r === 'number') return fail(r);
    const bk = this.browseKey(opts.file, opts.reqid);
    if (this.browses.has(bk)) return fail(Resp.INVREQ, 33);
    const res = this.position(r.ds, r.index, opts, opts.equal === true && opts.gteq !== true);
    if (!res) return fail(Resp.NOTFND, 80);
    this.browses.set(bk, { file: opts.file, index: r.index, ds: r.ds, last: undefined, lastDir: undefined, start: res });
    return ok();
  }

  /** RESETBR: repositions an active browse. */
  resetbr(opts: KeyedOptions & { reqid?: number; equal?: boolean; gteq?: boolean }): CicsResult {
    const b = this.browses.get(this.browseKey(opts.file, opts.reqid));
    if (!b) return fail(Resp.INVREQ, 35);
    const res = this.position(b.ds, b.index, opts, opts.equal === true && opts.gteq !== true);
    if (!res) return fail(Resp.NOTFND, 80);
    b.start = res;
    b.last = undefined;
    b.lastDir = undefined;
    return ok();
  }

  private position(ds: VsamDataset, index: OrderedIndex, opts: KeyedOptions, equal: boolean): Browse['start'] {
    const raw = toKey(opts.ridfld);
    const highValues = raw.length > 0 && /^\xff+$/.test(raw);
    const entries = index.entries();
    if (highValues && !equal) return { pos: entries.length, exact: false, highValues: true };
    const ck = this.searchKey(ds, opts);
    const generic = opts.generic ?? false;
    const pos = this.locate(index, ck, generic, !equal);
    if (pos < 0) return undefined;
    const e = entries[pos]!;
    return { pos, exact: generic ? e.key.startsWith(ck) : e.key === ck, highValues: false };
  }

  private deliver(b: Browse, entry: IndexEntry, dir: 'next' | 'prev'): CicsResult {
    b.last = entry;
    b.lastDir = dir;
    b.start = undefined;
    const record = b.ds.recordAt(entry.primary)!;
    const ridfld = keyOf(record, b.index.spec);
    if (this.dupkeyAfter(b.index, entry)) return { resp: Resp.DUPKEY, resp2: 0, record, ridfld };
    return ok({ record, ridfld });
  }

  /**
   * READNEXT. Passing a RIDFLD different from the key last returned repositions
   * (skip-sequential, GTEQ). After READPREV, the first READNEXT returns the same
   * record again (CICS direction-change behaviour).
   */
  readnext(opts: BrowseOptions & { ridfld?: string | Uint8Array }): CicsResult {
    const b = this.browses.get(this.browseKey(opts.file, opts.reqid));
    if (!b) return fail(Resp.INVREQ, 34);
    const entries = b.index.entries();
    let pos: number;
    if (opts.ridfld !== undefined && b.last && toKey(opts.ridfld) !== b.ds.rawKey(b.last, b.index)) {
      pos = b.index.seek(b.ds.collate(toKey(opts.ridfld)));
    } else if (b.start) {
      pos = b.start.pos;
    } else if (b.last) {
      const at = b.index.positionOf(b.last);
      if (at < 0) pos = b.index.seek(b.last.key, true);
      else pos = b.lastDir === 'prev' ? at : at + 1;
    } else pos = entries.length;
    const entry = entries[pos];
    if (!entry) return fail(Resp.ENDFILE, 90);
    return this.deliver(b, entry, 'next');
  }

  /**
   * READPREV. Immediately after STARTBR/RESETBR the starting key must exist
   * (else NOTFND), unless RIDFLD was high-values (last record). After READNEXT,
   * the first READPREV returns the same record again.
   */
  readprev(opts: BrowseOptions): CicsResult {
    const b = this.browses.get(this.browseKey(opts.file, opts.reqid));
    if (!b) return fail(Resp.INVREQ, 34);
    const entries = b.index.entries();
    let pos: number;
    if (b.start) {
      if (b.start.highValues) pos = entries.length - 1;
      else if (!b.start.exact) return fail(Resp.NOTFND, 80);
      else pos = b.start.pos;
    } else if (b.last) {
      const at = b.index.positionOf(b.last);
      if (at < 0) pos = b.index.seek(b.last.key) - 1;
      else pos = b.lastDir === 'next' ? at : at - 1;
    } else pos = -1;
    const entry = pos >= 0 ? entries[pos] : undefined;
    if (!entry) return fail(Resp.ENDFILE, 90);
    return this.deliver(b, entry, 'prev');
  }

  endbr(opts: BrowseOptions): CicsResult {
    const bk = this.browseKey(opts.file, opts.reqid);
    if (!this.browses.delete(bk)) return fail(Resp.INVREQ, 35);
    return ok();
  }
}
