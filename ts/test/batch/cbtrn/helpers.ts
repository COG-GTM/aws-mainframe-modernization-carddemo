import { mkdtempSync, readFileSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { readLineRecords } from '../../../src/common/io/sequential.js';
import { defaultDataDir, loadSeedCatalog, readDailyTransactions } from '../../../src/common/seed/index.js';
import { IndexedFile, type VsamCatalog } from '../../../src/common/vsam/index.js';

export const GOLDEN = join(import.meta.dirname, 'golden');

/** Golden file lines (one record per line, newline-terminated). */
export function golden(dir: string, file: string): string[] {
  const text = readFileSync(join(GOLDEN, dir, file), 'latin1');
  return text === '' ? [] : text.replace(/\n$/, '').split('\n');
}

/** Golden SYSOUT without the GnuCOBOL-only lines (CEE3ABD stub, libcob implicit-CLOSE warnings). */
export const goldenSysout = (dir: string): string[] =>
  golden(dir, 'sysout.txt').filter((l) => !l.startsWith('libcob:') && !l.startsWith('CEE3ABD ABCODE='));

/** GnuCOBOL exit status: STOP RUN RETURNING 999 (CEE3ABD stub) is reported modulo 256. */
export const goldenRc = (dir: string): number => Number(golden(dir, 'return-code.txt')[0]);

export const rtrim = (lines: readonly string[]): string[] => lines.map((l) => l.replace(/\s+$/, ''));

export function tempDir(): string {
  return mkdtempSync(join(tmpdir(), 'cbtrn-'));
}

export function writeLines(path: string, records: readonly (Uint8Array | string)[]): string {
  writeFileSync(path, records.map((r) => (typeof r === 'string' ? r : Buffer.from(r).toString('latin1')) + '\n').join(''), 'latin1');
  return path;
}

export const readLines = (path: string): string[] => readLineRecords(path).map((b) => b.toString('latin1'));

/** All records of a KSDS in key order (IDCAMS REPRO unload). */
export function unload(catalog: VsamCatalog, dd: string): string[] {
  const f = new IndexedFile(catalog, dd, { access: 'SEQUENTIAL' });
  f.open('INPUT');
  const out: string[] = [];
  for (let r = f.readNext(); r.status.startsWith('0'); r = f.readNext()) out.push(r.record!.toString('latin1'));
  f.close();
  return out;
}

export const maskProcTs = (rec: string): string => rec.slice(0, 304) + '#'.repeat(26) + rec.slice(330);

const put = (rec: Buffer, off: number, text: string): void => {
  rec.write(text, off, 'latin1');
};

/** Same crafted inputs as tools/cbtrn/seqdata.py `prep`. */
export function edgeDalytran(): Buffer[] {
  const edge = readDailyTransactions().slice(0, 8);
  put(edge[0]!, 262, '9999999999999999');
  put(edge[2]!, 278, '2099-01-01 00:00:00.000000');
  put(edge[3]!, 132, '9999999999I');
  put(edge[3]!, 278, '2099-01-01 00:00:00.000000');
  put(edge[4]!, 132, '0000001234J');
  put(edge[5]!, 16, '099999');
  return edge;
}

export function dupDalytran(): Buffer[] {
  const dup = readDailyTransactions().slice(0, 3);
  dup[0]!.copy(dup[2]!, 0, 0, 16);
  return dup;
}

/** Seed catalog; optionally without some account ids. */
export function seedCatalog(dropAccounts: readonly string[] = []): VsamCatalog {
  const { catalog } = loadSeedCatalog();
  if (dropAccounts.length) {
    const f = new IndexedFile(catalog, 'ACCTFILE', { access: 'RANDOM' });
    f.open('I-O');
    for (const id of dropAccounts) f.delete(id);
    f.close();
  }
  return catalog;
}

export const dailytranPath = (): string => join(defaultDataDir(), 'ASCII', 'dailytran.txt');
