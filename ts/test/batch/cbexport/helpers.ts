import { mkdtempSync, readFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { defineExportCluster } from '../../../src/batch/cbexport/index.js';
import { loadSeedCatalog, readSeedRecords } from '../../../src/common/seed/index.js';
import type { VsamCatalog } from '../../../src/common/vsam/catalog.js';
import { IndexedFile } from '../../../src/common/vsam/cobol-file.js';

/** ASCII seed catalog with TRANSACT from dailytran.txt and an empty export cluster. */
export function asciiCatalog(): VsamCatalog {
  const { catalog } = loadSeedCatalog({ transactFromDailytran: true });
  defineExportCluster(catalog);
  return catalog;
}

/** The same clusters loaded from the CP037 seeds in app/data/EBCDIC (DALYTRAN as TRANSACT). */
export function ebcdicCatalog(): VsamCatalog {
  const inputs = ['CUSTFILE', 'ACCTFILE', 'XREFFILE', 'TRANSACT', 'CARDFILE'];
  const { catalog } = loadSeedCatalog({ emptyClusters: inputs });
  const seeds: Record<string, [string, number]> = {
    CUSTFILE: ['CUSTDATA', 500],
    ACCTFILE: ['ACCTDATA', 300],
    XREFFILE: ['CARDXREF', 50],
    TRANSACT: ['DALYTRAN', 350],
    CARDFILE: ['CARDDATA', 150],
  };
  for (const dd of inputs) {
    const [name, len] = seeds[dd]!;
    catalog.require(dd).load(readSeedRecords({ file: `EBCDIC/AWS.M2.CARDDEMO.${name}.PS`, encoding: 'ebcdic' }, len));
  }
  defineExportCluster(catalog);
  return catalog;
}

/** Reads a KSDS sequentially (key order) through the COBOL file API. */
export function readAll(catalog: VsamCatalog, dd: string): Buffer[] {
  const f = new IndexedFile(catalog, dd, { access: 'SEQUENTIAL' });
  f.open('INPUT');
  const out: Buffer[] = [];
  for (let r = f.readNext(); r.status === '00'; r = f.readNext()) out.push(r.record!);
  f.close();
  return out;
}

export function split(buf: Buffer, len: number): Buffer[] {
  const out: Buffer[] = [];
  for (let i = 0; i < buf.length; i += len) out.push(buf.subarray(i, i + len));
  return out;
}

export const goldenDir = join(import.meta.dirname, 'golden');
export const golden = (f: string) => readFileSync(join(goldenDir, f));
export const tempDir = () => mkdtempSync(join(tmpdir(), 'cbimport-'));

/** DISPLAY lines with the run date/time masked. */
export const maskClock = (lines: readonly string[]) =>
  lines.map((l) => l.replace(/(Date: )\d{4}-\d\d-\d\d$/, '$1YYYY-MM-DD').replace(/(Time: )\d\d:\d\d:\d\d$/, '$1HH:MM:SS'));
