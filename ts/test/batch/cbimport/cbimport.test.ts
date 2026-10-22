import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { describe, expect, it } from 'vitest';
import { defineExportCluster, runCbexport } from '../../../src/batch/cbexport/index.js';
import { CBIMPORT_OUTPUTS, runCbimport } from '../../../src/batch/cbimport/index.js';
import { CustomerRecordLayout } from '../../../src/common/copybooks/customer.js';
import { ExportRecordLayout } from '../../../src/common/copybooks/export.js';
import { readLineRecords } from '../../../src/common/io/sequential.js';
import { defaultDataDir, readExportFile, readSeedRecords } from '../../../src/common/seed/index.js';
import { VsamCatalog } from '../../../src/common/vsam/catalog.js';
import { asciiCatalog, split, tempDir } from '../cbexport/helpers.js';

const now = new Date(2025, 8, 28, 22, 53, 40);
const ddPaths = (dir: string) => Object.fromEntries(CBIMPORT_OUTPUTS.map((o) => [o.dd, join(dir, `${o.dd}.dat`)]));
/** FILLER is LOW-VALUES in the import output (INITIALIZE skips FILLER); compare the rest. */
const blankFiller = (rec: Buffer, from: number) => Buffer.concat([rec.subarray(0, from), Buffer.alloc(rec.length - from, 0x20)]);
const seeds: [dd: string, file: string, len: number, fillerAt: number][] = [
  ['CUSTOUT', 'custdata.txt', 500, 332],
  ['ACCTOUT', 'acctdata.txt', 300, 122],
  ['XREFOUT', 'cardxref.txt', 50, 36],
  ['TRNXOUT', 'dailytran.txt', 350, 330],
  ['CARDOUT', 'carddata.txt', 150, 91],
];

describe('CBIMPORT', () => {
  it('round trip: ASCII seeds -> CBEXPORT -> CBIMPORT gives back the master records', () => {
    const catalog = asciiCatalog();
    runCbexport({ catalog, now });
    const dir = tempDir();
    const dd = ddPaths(dir);
    const r = runCbimport({ catalog, dd, now });
    expect(r.abended).toBe(false);
    expect(r.stats).toEqual({ read: 500, customers: 50, accounts: 50, xrefs: 50, transactions: 300, cards: 50, errors: 0, unknown: 0 });
    for (const [name, file, len, fillerAt] of seeds) {
      const out = split(readFileSync(dd[name]!), len);
      const expected = readLineRecords(join(defaultDataDir(), 'ASCII', file), len).sort(Buffer.compare);
      expect(out.map((b) => blankFiller(b, fillerAt)), name).toEqual(expected.map((b) => blankFiller(b, fillerAt)));
      expect(out.every((b) => b.subarray(fillerAt).every((x) => x === 0)), `${name} FILLER`).toBe(true);
    }
    expect(readFileSync(dd.ERROUT!)).toHaveLength(0);
    expect(r.displays).toContain('CBIMPORT: No validation errors detected');
  });

  it('imports the reference EBCDIC export into CP037 output files', () => {
    const catalog = new VsamCatalog();
    defineExportCluster(catalog).load(readExportFile());
    const dir = tempDir();
    const dd = ddPaths(dir);
    const r = runCbimport({ catalog, dd, now, charset: 'ebcdic' });
    expect(r.stats).toMatchObject({ read: 500, customers: 50, accounts: 50, xrefs: 50, transactions: 300, cards: 50, unknown: 0 });
    const lengths = Object.fromEntries(CBIMPORT_OUTPUTS.map((o) => [o.dd, readFileSync(dd[o.dd]!).length]));
    expect(lengths).toEqual({ CUSTOUT: 25000, ACCTOUT: 15000, XREFOUT: 2500, TRNXOUT: 105000, CARDOUT: 7500, ERROUT: 0 });
    // Customers 2..49 were not edited after the seed was taken: identical to CUSTDATA.PS except FILLER.
    const out = split(readFileSync(dd.CUSTOUT!), 500);
    const seed = readSeedRecords({ file: 'EBCDIC/AWS.M2.CARDDEMO.CUSTDATA.PS', encoding: 'ebcdic' }, 500);
    for (let i = 1; i < 49; i++) {
      const c = CustomerRecordLayout.parse(out[i]!, { charset: 'ebcdic' });
      const s = CustomerRecordLayout.parse(seed[i]!);
      expect({ ...c, filler: '' }).toEqual({ ...s, filler: '' });
    }
    // zoned display output (CUST-ID 9(9) DISPLAY) in CP037
    expect(out[0]!.subarray(0, 9).toString('hex')).toBe('f0f0f0f0f0f0f0f0f1');
  });

  it('writes unknown record types to ERROUT and keeps going', () => {
    const catalog = new VsamCatalog();
    const mk = (type: string, seq: number) =>
      ExportRecordLayout.serialize({ exportRecType: type, exportTimestamp: 'x', exportSequenceNum: seq, exportBranchId: '0001', exportRegionCode: 'NORTH' });
    defineExportCluster(catalog).load([mk('Z', 7), mk(' ', 8), mk('c', 12345678)]);
    const dd = ddPaths(tempDir());
    const r = runCbimport({ catalog, dd, now: new Date(Date.UTC(2025, 0, 2, 3, 4, 5, 670)) });
    expect(r.abended).toBe(false);
    expect(r.stats).toMatchObject({ read: 3, errors: 3, unknown: 3, customers: 0 });
    const errs = split(readFileSync(dd.ERROUT!), 132).map((b) => b.toString('latin1'));
    expect(errs.map((e) => e.slice(26, 40))).toEqual(['|Z|0000007|Unk', '| |0000008|Unk', '|c|2345678|Unk']);
    expect(errs[0]).toMatch(/^\d{16}[+-]\d{4} {5}\|Z\|0000007\|Unknown record type encountered {64}$/);
    expect(r.displays.slice(-3)).toEqual(['CBIMPORT: Cards Imported: 000000000', 'CBIMPORT: Errors Written: 000000003', 'CBIMPORT: Unknown Record Types: 000000003']);
  });

  it('abends when an output DD is missing (CBIMPORT.jcl has no CARDOUT DD)', () => {
    const catalog = new VsamCatalog();
    defineExportCluster(catalog);
    const dd = ddPaths(tempDir());
    delete dd.CARDOUT;
    const r = runCbimport({ catalog, dd, now });
    expect(r.abended).toBe(true);
    expect(r.displays.slice(-2)).toEqual(['ERROR: Cannot open CARD-OUTPUT, Status: 35', 'CBIMPORT: ABENDING PROGRAM']);
  });

  it('abends with status 30 when an output path cannot be created, closing what was opened', () => {
    const catalog = new VsamCatalog();
    defineExportCluster(catalog);
    const dir = tempDir();
    const dd = ddPaths(dir);
    dd.ACCTOUT = join(dir, 'missing', 'acct.dat');
    const r = runCbimport({ catalog, dd, now });
    expect(r.abended).toBe(true);
    expect(r.displays.slice(-2)).toEqual(['ERROR: Cannot open ACCOUNT-OUTPUT, Status: 30', 'CBIMPORT: ABENDING PROGRAM']);
    // CUSTOUT was opened and closed again: a second run can reopen it
    expect(runCbimport({ catalog, dd: ddPaths(dir), now }).abended).toBe(false);
  });

  it('abends when the export file does not exist', () => {
    const r = runCbimport({ catalog: new VsamCatalog(), dd: ddPaths(tempDir()), now });
    expect(r.abended).toBe(true);
    expect(r.displays).toEqual(['CBIMPORT: Starting Customer Data Import', 'ERROR: Cannot open EXPORT-INPUT, Status: 35', 'CBIMPORT: ABENDING PROGRAM']);
  });

  it('formats the import date/time from CURRENT-DATE', () => {
    const catalog = new VsamCatalog();
    defineExportCluster(catalog);
    const r = runCbimport({ catalog, dd: ddPaths(tempDir()), now });
    expect(r.displays.slice(1, 3)).toEqual(['CBIMPORT: Import Date: 2025-09-28', 'CBIMPORT: Import Time: 22:53:40']);
  });
});
