/**
 * Seed data loading: defines every CardDemo VSAM cluster (keys and AIXs from
 * the app/jcl DEFINE CLUSTER / ALTERNATEINDEX steps) in a `VsamCatalog` and
 * loads it from app/data (ASCII text files, and the CP037 USRSEC file).
 */
import { existsSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { ebcdicToAscii } from '../codecs/ebcdic.js';
import { readFixedRecords, readLineRecords } from '../io/sequential.js';
import { VsamCatalog } from '../vsam/catalog.js';
import type { CicsFileDef } from '../vsam/cics-file.js';
import type { DatasetDef } from '../vsam/dataset.js';

export interface ClusterSeed {
  def: DatasetDef;
  /** Aliases: batch DD names, CICS file name, DSN. */
  aliases: string[];
  /** Seed file relative to app/data, and its encoding. */
  seed?: { file: string; encoding: 'ascii' | 'ebcdic' };
}

const ascii = (file: string) => ({ file: `ASCII/${file}`, encoding: 'ascii' as const });

/** The CardDemo clusters (core application; add-on IMS/DB2/MQ datasets excluded). */
export const CARDDEMO_CLUSTERS: readonly ClusterSeed[] = [
  {
    def: { name: 'AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS', organization: 'KSDS', recordLength: 300, key: { offset: 0, length: 11 } },
    aliases: ['ACCTDAT', 'ACCTFILE'],
    seed: ascii('acctdata.txt'),
  },
  {
    def: {
      name: 'AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS',
      organization: 'KSDS',
      recordLength: 150,
      key: { offset: 0, length: 16 },
      alternateIndexes: [{ name: 'CARDAIX', offset: 16, length: 11, unique: false }],
    },
    aliases: ['CARDDAT', 'CARDFILE'],
    seed: ascii('carddata.txt'),
  },
  {
    def: { name: 'AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS', organization: 'KSDS', recordLength: 500, key: { offset: 0, length: 9 } },
    aliases: ['CUSTDAT', 'CUSTFILE'],
    seed: ascii('custdata.txt'),
  },
  {
    def: {
      name: 'AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS',
      organization: 'KSDS',
      recordLength: 50,
      key: { offset: 0, length: 16 },
      alternateIndexes: [{ name: 'CXACAIX', offset: 25, length: 11, unique: false }],
    },
    aliases: ['CCXREF', 'XREFFILE', 'CARDXREF'],
    seed: ascii('cardxref.txt'),
  },
  {
    def: {
      name: 'AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS',
      organization: 'KSDS',
      recordLength: 350,
      key: { offset: 0, length: 16 },
      alternateIndexes: [{ name: 'TRANAIX', offset: 304, length: 26, unique: false }],
    },
    aliases: ['TRANSACT', 'TRANFILE'],
  },
  {
    def: { name: 'AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS', organization: 'KSDS', recordLength: 50, key: { offset: 0, length: 17 } },
    aliases: ['TCATBALF'],
    seed: ascii('tcatbal.txt'),
  },
  {
    def: { name: 'AWS.M2.CARDDEMO.DISCGRP.VSAM.KSDS', organization: 'KSDS', recordLength: 50, key: { offset: 0, length: 16 } },
    aliases: ['DISCGRP'],
    seed: ascii('discgrp.txt'),
  },
  {
    def: { name: 'AWS.M2.CARDDEMO.TRANCATG.VSAM.KSDS', organization: 'KSDS', recordLength: 60, key: { offset: 0, length: 6 } },
    aliases: ['TRANCATG'],
    seed: ascii('trancatg.txt'),
  },
  {
    def: { name: 'AWS.M2.CARDDEMO.TRANTYPE.VSAM.KSDS', organization: 'KSDS', recordLength: 60, key: { offset: 0, length: 2 } },
    aliases: ['TRANTYPE'],
    seed: ascii('trantype.txt'),
  },
  {
    def: { name: 'AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS', organization: 'KSDS', recordLength: 80, key: { offset: 0, length: 8 } },
    aliases: ['USRSEC'],
    seed: { file: 'EBCDIC/AWS.M2.CARDDEMO.USRSEC.PS', encoding: 'ebcdic' },
  },
];

/** CICS FILE definitions from app/csd/CARDDEMO.CSD. */
export const CARDDEMO_CICS_FILES: Readonly<Record<string, CicsFileDef>> = {
  ACCTDAT: { dataset: 'ACCTDAT' },
  CARDDAT: { dataset: 'CARDDAT' },
  CARDAIX: { dataset: 'CARDDAT', path: 'CARDAIX' },
  CCXREF: { dataset: 'CCXREF' },
  CXACAIX: { dataset: 'CCXREF', path: 'CXACAIX' },
  CUSTDAT: { dataset: 'CUSTDAT' },
  TRANSACT: { dataset: 'TRANSACT' },
  USRSEC: { dataset: 'USRSEC' },
};

/** Locates app/data by walking up from this module (works from src/ and dist/). */
export function defaultDataDir(): string {
  let dir = dirname(fileURLToPath(import.meta.url));
  for (let i = 0; i < 8; i++) {
    const candidate = join(dir, 'app', 'data');
    if (existsSync(join(candidate, 'ASCII'))) return candidate;
    dir = dirname(dir);
  }
  throw new Error('app/data not found; pass dataDir explicitly');
}

/** Reads the records of a seed file; EBCDIC records are converted to ASCII (display-only layouts). */
export function readSeedRecords(seed: NonNullable<ClusterSeed['seed']>, recordLength: number, dataDir = defaultDataDir()): Buffer[] {
  const path = resolve(dataDir, seed.file);
  if (seed.encoding === 'ebcdic') return readFixedRecords(path, recordLength, 0x40).map((r) => ebcdicToAscii(r));
  return readLineRecords(path, recordLength);
}

export interface SeedOptions {
  dataDir?: string;
  /** Load TRANSACT from DALYTRAN (dailytran.txt); there is no TRANSACT seed. Default false (empty). */
  transactFromDailytran?: boolean;
  /** Only define (do not load) these clusters. */
  emptyClusters?: readonly string[];
}

export interface SeedReport {
  name: string;
  loaded: number;
  rejected: number;
}

/** Defines all CardDemo clusters in a fresh catalog and loads the seed data. */
export function loadSeedCatalog(opts: SeedOptions = {}): { catalog: VsamCatalog; report: SeedReport[] } {
  const dataDir = opts.dataDir ?? defaultDataDir();
  const catalog = new VsamCatalog();
  const report: SeedReport[] = [];
  for (const c of CARDDEMO_CLUSTERS) {
    const ds = catalog.define(c.def, c.aliases);
    const empty = opts.emptyClusters?.some((n) => n === c.def.name || c.aliases.includes(n));
    let seed = c.seed;
    if (!seed && c.aliases.includes('TRANSACT') && opts.transactFromDailytran) seed = ascii('dailytran.txt');
    if (!seed || empty) {
      report.push({ name: c.def.name, loaded: 0, rejected: 0 });
      continue;
    }
    const res = ds.load(readSeedRecords(seed, c.def.recordLength, dataDir));
    report.push({ name: c.def.name, loaded: res.loaded, rejected: res.rejected.length });
  }
  return { catalog, report };
}

/** DALYTRAN input records (sequential, 350 bytes). */
export function readDailyTransactions(dataDir = defaultDataDir()): Buffer[] {
  return readLineRecords(join(dataDir, 'ASCII', 'dailytran.txt'), 350);
}

/** The CBEXPORT/CBIMPORT export file (500-byte EBCDIC records; parse with charset 'ebcdic'). */
export function readExportFile(dataDir = defaultDataDir()): Buffer[] {
  return readFixedRecords(join(dataDir, 'EBCDIC', 'AWS.M2.CARDDEMO.EXPORT.DATA.PS'), 500, 0x40);
}
