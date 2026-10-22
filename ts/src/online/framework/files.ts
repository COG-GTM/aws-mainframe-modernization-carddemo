import { CicsFileControl, type CicsFileDef, type CicsResult, type VsamCatalog, type VsamDataset } from '../../common/vsam/index.js';

type WriteOpts = Parameters<CicsFileControl['write']>[0];
type RewriteOpts = Parameters<CicsFileControl['rewrite']>[0];
type DeleteOpts = Parameters<CicsFileControl['delete']>[0];

/**
 * Task-scoped CICS file control (EXEC CICS READ/WRITE/REWRITE/DELETE/STARTBR/
 * READNEXT/READPREV/RESETBR/ENDBR/UNLOCK + SYNCPOINT [ROLLBACK]) over the
 * Session 1 `CicsFileControl`. Every method returns the CICS RESP/RESP2.
 *
 * Recovery follows the CSD: every CardDemo FILE is RECOVERY(NONE), so by default
 * SYNCPOINT ROLLBACK (and abends) do not back out file updates, exactly as in
 * CICS. Files named in `recoverable` are backed out to their state at the last
 * syncpoint (before-image snapshot of the underlying dataset).
 */
export class TaskFiles {
  readonly control: CicsFileControl;
  private readonly before = new Map<VsamDataset, { records: Buffer[]; aix: Map<string, { key: string; primary: string }[]> }>();

  constructor(
    private readonly catalog: VsamCatalog,
    private readonly defs: Readonly<Record<string, CicsFileDef>>,
    private readonly recoverable: ReadonlySet<string> = new Set(),
  ) {
    this.control = new CicsFileControl(catalog, { ...defs });
  }

  private journal(file: string): void {
    const name = file.trim();
    if (!this.recoverable.has(name)) return;
    const def = this.defs[name];
    const ds = def && this.catalog.get(def.dataset);
    if (!ds || this.before.has(ds)) return;
    // Keep each AIX's entry order: non-unique duplicates stay in arrival order, which load() would re-sort.
    const aix = new Map<string, { key: string; primary: string }[]>();
    for (const [n, idx] of ds.alternateIndexes) aix.set(n, idx.entries().map((e) => ({ key: e.key, primary: e.primary })));
    this.before.set(ds, { records: ds.records(), aix });
  }

  read(opts: Parameters<CicsFileControl['read']>[0]): CicsResult {
    return this.control.read(opts);
  }
  write(opts: WriteOpts): CicsResult {
    this.journal(opts.file);
    return this.control.write(opts);
  }
  rewrite(opts: RewriteOpts): CicsResult {
    this.journal(opts.file);
    return this.control.rewrite(opts);
  }
  delete(opts: DeleteOpts): CicsResult {
    this.journal(opts.file);
    return this.control.delete(opts);
  }
  unlock(opts: { file: string }): CicsResult {
    return this.control.unlock(opts);
  }
  startbr(opts: Parameters<CicsFileControl['startbr']>[0]): CicsResult {
    return this.control.startbr(opts);
  }
  resetbr(opts: Parameters<CicsFileControl['resetbr']>[0]): CicsResult {
    return this.control.resetbr(opts);
  }
  readnext(opts: Parameters<CicsFileControl['readnext']>[0]): CicsResult {
    return this.control.readnext(opts);
  }
  readprev(opts: Parameters<CicsFileControl['readprev']>[0]): CicsResult {
    return this.control.readprev(opts);
  }
  endbr(opts: Parameters<CicsFileControl['endbr']>[0]): CicsResult {
    return this.control.endbr(opts);
  }

  /** EXEC CICS SYNCPOINT: commits, releases locks and browses. */
  syncpoint(): void {
    this.before.clear();
    this.control.syncpoint();
  }

  /** EXEC CICS SYNCPOINT ROLLBACK: backs out recoverable files, releases locks and browses. */
  rollback(): void {
    for (const [ds, image] of this.before) {
      ds.clear();
      ds.load(image.records);
      for (const [n, entries] of image.aix) {
        const idx = ds.alternateIndexes.get(n);
        if (!idx) continue;
        idx.clear();
        for (const e of entries) idx.add(e.key, e.primary);
      }
    }
    this.before.clear();
    this.control.syncpoint();
  }
}
