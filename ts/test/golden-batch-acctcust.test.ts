/**
 * Parity of the Session 2 batch ports with the reference COBOL run under
 * GnuCOBOL 3.1 (tools/gen-golden-batch.sh -> test/golden/batch/out).
 * Timestamps (FUNCTION CURRENT-DATE) are masked; SYSOUT lines are compared
 * right-trimmed.
 */
import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { describe, expect, it } from 'vitest';
import { TranRecordLayout } from '../src/common/copybooks/transaction.js';
import { readLineRecords } from '../src/common/io/sequential.js';
import { loadSeedCatalog } from '../src/common/seed/index.js';
import { runCbact01c } from '../src/batch/cbact01c/index.js';
import { runCbact02c } from '../src/batch/cbact02c/index.js';
import { runCbact03c } from '../src/batch/cbact03c/index.js';
import { runCbact04c } from '../src/batch/cbact04c/index.js';
import { runCbcus01c } from '../src/batch/cbcus01c/index.js';

const dir = join(import.meta.dirname, 'golden', 'batch');
const out = (f: string) => readFileSync(join(dir, 'out', f));
const lines = (f: string) => trimLines(out(f).toString('latin1').split('\n').slice(0, -1));
const trimLines = (ls: string[]) => ls.map((l) => l.replace(/ +$/, ''));
const fixed = (b: Buffer, n: number) => Array.from({ length: b.length / n }, (_, i) => b.subarray(i * n, (i + 1) * n));
const hex = (bs: Buffer[]) => bs.map((b) => b.toString('hex'));

/** GnuCOBOL variable sequential records: 2-byte big-endian data length + 2 reserved bytes. */
function gnuVarRecords(b: Buffer): Buffer[] {
  const recs: Buffer[] = [];
  for (let p = 0; p < b.length; ) {
    const len = b.readUInt16BE(p);
    recs.push(b.subarray(p + 4, p + 4 + len));
    p += 4 + len;
  }
  return recs;
}

const OVERPUNCH_POS = '{ABCDEFGHI';
const OVERPUNCH_NEG = '}JKLMNOPQR';
/**
 * GnuCOBOL DISPLAYs a signed zoned item edited ('+0000000194.00'); Enterprise
 * COBOL (and this port) shows the raw bytes with the overpunched sign ('00000001940{').
 */
function ibmDisplay(line: string): string {
  return line.replace(/^(.*:)([+-])(\d+)\.(\d+)$/, (_m, label: string, sign: string, int: string, dec: string) => {
    const digits = int + dec;
    const last = Number(digits.at(-1));
    return label + digits.slice(0, -1) + (sign === '-' ? OVERPUNCH_NEG : OVERPUNCH_POS)[last];
  });
}

/**
 * INITIALIZE of an unused ARR-ACCT-CURR-BAL (S9(10)V99 DISPLAY) occurrence:
 * GnuCOBOL stores an unsigned '0' last byte, Enterprise COBOL the C-signed
 * zero X'C0' ('{' in ASCII), which the port keeps.
 */
function ibmInitialize(rec: Buffer): string {
  const r = Buffer.from(rec);
  for (const occ of [3, 4]) {
    const last = 11 + occ * 19 + 11;
    if (r[last] === 0x30) r[last] = 0x7b;
  }
  return r.toString('hex');
}

function maskTs(rec: Buffer): string {
  const r = Buffer.from(rec);
  for (const n of ['TRAN-ORIG-TS', 'TRAN-PROC-TS']) {
    const { offset, length } = TranRecordLayout.range(n);
    r.fill('X', offset, offset + length);
  }
  return r.toString('latin1');
}

function interestCatalog() {
  const { catalog } = loadSeedCatalog({ emptyClusters: ['ACCTFILE', 'TCATBALF', 'DISCGRP'] });
  catalog.require('ACCTFILE').load(readLineRecords(join(dir, 'fixtures', 'acctdata-interest.txt'), 300));
  catalog.require('TCATBALF').load(readLineRecords(join(dir, 'fixtures', 'tcatbal-interest.txt'), 50));
  catalog.require('DISCGRP').load(readLineRecords(join(dir, 'fixtures', 'discgrp-interest.txt'), 50));
  return catalog;
}

function accountsAfter(catalog: ReturnType<typeof interestCatalog>): string[] {
  return trimLines(catalog.require('ACCTFILE').records().map((r: Buffer) => r.toString('latin1')));
}

describe('GnuCOBOL golden: CBACT01C', () => {
  const r = runCbact01c();
  it('SYSOUT', () => expect(trimLines(r.display)).toEqual(lines('cbact01c.sysout.txt').map(ibmDisplay)));
  it('OUTFILE (incl. COMP-3 CYC-DEBIT)', () => expect(hex(r.outRecords)).toEqual(hex(fixed(out('cbact01c.outfile.bin'), 107))));
  it('ARRYFILE', () => expect(hex(r.arryRecords)).toEqual(fixed(out('cbact01c.arryfile.bin'), 110).map(ibmInitialize)));
  it('VBRCFILE record data', () => expect(hex(r.vbrcRecords)).toEqual(hex(gnuVarRecords(out('cbact01c.vbrcfile.bin')))));
});

describe('GnuCOBOL golden: sequential KSDS readers', () => {
  it('CBACT02C', () => expect(trimLines(runCbact02c().display)).toEqual(lines('cbact02c.sysout.txt')));
  it('CBACT03C', () => expect(trimLines(runCbact03c().display)).toEqual(lines('cbact03c.sysout.txt')));
  it('CBCUS01C', () => expect(trimLines(runCbcus01c().display)).toEqual(lines('cbcus01c.sysout.txt')));
});

describe.each([
  ['seed', () => loadSeedCatalog().catalog],
  ['interest', interestCatalog],
] as const)('GnuCOBOL golden: CBACT04C (%s data)', (name, mkCatalog) => {
  const catalog = mkCatalog();
  const r = runCbact04c({ catalog, parmDate: '2022071800' });
  it('SYSOUT and RC', () => {
    expect(r.abended).toBe(false);
    expect(trimLines(r.display)).toEqual(lines(`cbact04c-${name}.sysout.txt`));
  });
  it('TRANSACT records (timestamps masked)', () => {
    expect(r.transactRecords.map(maskTs)).toEqual(fixed(out(`cbact04c-${name}.transact.bin`), 350).map(maskTs));
  });
  it('rewritten ACCTFILE', () => expect(accountsAfter(catalog)).toEqual(lines(`cbact04c-${name}.acctdata.txt`)));
});
