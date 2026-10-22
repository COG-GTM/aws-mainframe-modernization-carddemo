/** A catalog of VSAM clusters, addressable by cluster name and aliases (DD / CICS file / DSN). */
import { type DatasetDef, VsamDataset } from './dataset.js';

export class VsamCatalog {
  private readonly byName = new Map<string, VsamDataset>();

  /** Defines (IDCAMS DEFINE CLUSTER) a new empty cluster. */
  define(def: DatasetDef, aliases: readonly string[] = []): VsamDataset {
    const ds = new VsamDataset(def);
    for (const name of [def.name, ...aliases]) {
      if (this.byName.has(name)) throw new Error(`catalog: '${name}' already defined`);
      this.byName.set(name, ds);
    }
    return ds;
  }

  /** Adds an alias for an existing cluster. */
  alias(name: string, alias: string): void {
    this.byName.set(alias, this.require(name));
  }

  get(name: string): VsamDataset | undefined {
    return this.byName.get(name);
  }

  require(name: string): VsamDataset {
    const ds = this.byName.get(name);
    if (!ds) throw new Error(`catalog: no dataset '${name}'`);
    return ds;
  }

  has(name: string): boolean {
    return this.byName.has(name);
  }

  /** IDCAMS DELETE: removes the cluster and all its aliases. */
  delete(name: string): void {
    const ds = this.byName.get(name);
    if (!ds) return;
    for (const [k, v] of [...this.byName]) if (v === ds) this.byName.delete(k);
  }

  /** Distinct clusters. */
  datasets(): VsamDataset[] {
    return [...new Set(this.byName.values())];
  }
}
