/**
 * Parity with the real programs: golden/*.dat and *.txt were produced by
 * golden/gen-golden.sh (CBEXPORT then CBIMPORT under GnuCOBOL 3.1, indexed
 * files standing in for VSAM). Run dates/times are masked.
 */
import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { describe, expect, it } from 'vitest';
import { defineExportCluster, runCbexport } from '../../../src/batch/cbexport/index.js';
import { CBIMPORT_OUTPUTS, runCbimport } from '../../../src/batch/cbimport/index.js';
import { ExportRecordLayout } from '../../../src/common/copybooks/export.js';
import { VsamCatalog } from '../../../src/common/vsam/catalog.js';
import { asciiCatalog, golden, maskClock, readAll, split, tempDir } from './helpers.js';

const lines = (f: string) => golden(f).toString('latin1').split('\n').filter((l) => l.length > 0);
const unknown = (type: string, seq: number) =>
  ExportRecordLayout.serialize({ exportRecType: type, exportTimestamp: 'UNKNOWN', exportSequenceNum: seq, exportBranchId: '0001', exportRegionCode: 'NORTH' });

describe('GnuCOBOL golden: CBEXPORT', () => {
  const catalog = asciiCatalog();
  const result = runCbexport({ catalog });
  const ours = readAll(catalog, 'EXPFILE');
  const theirs = split(golden('export.dat'), 500);

  it('writes the same DISPLAY lines', () => {
    expect(result.abended).toBe(false);
    expect(maskClock(result.displays)).toEqual(maskClock(lines('cbexport.txt')));
  });

  it('writes byte-identical export records (EXPORT-TIMESTAMP masked)', () => {
    expect(ours).toHaveLength(theirs.length);
    const mask = (b: Buffer) => Buffer.concat([b.subarray(0, 1), b.subarray(27)]);
    ours.forEach((rec, i) => expect(mask(rec).equals(mask(theirs[i]!)), `record ${i + 1}`).toBe(true));
  });
});

describe('GnuCOBOL golden: CBIMPORT (export + unknown record types)', () => {
  const catalog = new VsamCatalog();
  defineExportCluster(catalog).load([unknown('Z', 0), ...split(golden('export.dat'), 500), unknown('c', 12345678)]);
  const dir = tempDir();
  const dd = Object.fromEntries(CBIMPORT_OUTPUTS.map((o) => [o.dd, join(dir, `${o.dd.toLowerCase()}.dat`)]));
  const result = runCbimport({ catalog, dd });

  it('writes the same DISPLAY lines', () => {
    expect(result.abended).toBe(false);
    expect(maskClock(result.displays)).toEqual(maskClock(lines('cbimport.txt')));
  });

  it.each(['CUSTOUT', 'ACCTOUT', 'XREFOUT', 'TRNXOUT', 'CARDOUT'])('%s is byte-identical', (name) => {
    const ours = readFileSync(dd[name]!);
    expect(ours.equals(golden(`${name.toLowerCase()}.dat`))).toBe(true);
  });

  it('ERROUT matches except the CURRENT-DATE value', () => {
    const ours = split(readFileSync(dd.ERROUT!), 132);
    const theirs = split(golden('errout.dat'), 132);
    expect(ours).toHaveLength(2);
    ours.forEach((rec, i) => {
      expect(rec.toString('latin1', 0, 26)).toMatch(/^\d{16}[+-]\d{4} {5}$/);
      expect(rec.subarray(26).equals(theirs[i]!.subarray(26))).toBe(true);
    });
  });
});
