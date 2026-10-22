import { mkdtempSync, readFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { describe, expect, it } from 'vitest';
import { AccountRecordLayout } from '../../common/copybooks/account.js';
import { loadSeedCatalog } from '../../common/seed/index.js';
import { ArrArrayRecLayout, cobdatft, OutAcctRecLayout, runCbact01c } from './index.js';

function codatecn(type: string, input: string, outType: string): Buffer {
  const b = Buffer.alloc(80, 0x20);
  b.write(type, 0, 'latin1');
  b.write(input, 1, 'latin1');
  b.write(outType, 21, 'latin1');
  cobdatft(b);
  return b;
}
const outDate = (b: Buffer) => b.toString('latin1', 22, 42).trimEnd();
const errMsg = (b: Buffer) => b.toString('latin1', 42, 80).trimEnd();

describe('COBDATFT', () => {
  it("type '2' -> '2': YYYY-MM-DD to YYYYMMDD (as CBACT01C calls it)", () => {
    const b = codatecn('2', '2025-05-20', '2');
    expect(outDate(b)).toBe('20250520');
    expect(errMsg(b)).toBe('');
  });
  it("type '1' -> '1': YYYYMMDD to YYYY-MM-DD", () => expect(outDate(codatecn('1', '20250520', '1'))).toBe('2025-05-20'));
  it.each([
    ['1', '2025-05-20', '1'],
    ['1', '20250520', '2'],
    ['2', '2025-05-20', '1'],
    ['3', '20250520', '2'],
  ])('type %s input %s outtype %s -> INVALID INPUT, output untouched', (t, i, o) => {
    const b = codatecn(t, i, o);
    expect(errMsg(b)).toBe('INVALID INPUT');
    expect(outDate(b)).toBe('');
  });
});

describe('CBACT01C', () => {
  it('reads all 50 seed accounts and writes 50/50/100 records', () => {
    const dir = mkdtempSync(join(tmpdir(), 'cbact01c-'));
    const r = runCbact01c({ outFile: join(dir, 'out'), arryFile: join(dir, 'arr'), vbrcFile: join(dir, 'vb') });
    expect(r.returnCode).toBe(0);
    expect(r.display[0]).toBe('START OF EXECUTION OF PROGRAM CBACT01C');
    expect(r.display.at(-1)).toBe('END OF EXECUTION OF PROGRAM CBACT01C');
    expect([r.outRecords.length, r.arryRecords.length, r.vbrcRecords.length]).toEqual([50, 50, 100]);
    expect(readFileSync(join(dir, 'out')).length).toBe(50 * 107);
    expect(readFileSync(join(dir, 'arr')).length).toBe(50 * 110);
    const vb = readFileSync(join(dir, 'vb'));
    expect([vb.readUInt16BE(0), vb.readUInt16BE(16)]).toEqual([16, 43]);
    expect(vb.length).toBe(50 * (16 + 43));
  });

  it('maps fields: reissue date via COBDATFT, CYC-DEBIT 2525.00 when zero, fixed array constants', () => {
    const r = runCbact01c();
    const o = OutAcctRecLayout.parse(r.outRecords[0]!);
    expect(o.outAcctReissueDate.trimEnd()).toBe('20250520');
    expect(o.outAcctCurrCycDebit.toFixed(2)).toBe('2525.00');
    expect(o.outAcctCurrBal.toFixed(2)).toBe('194.00');
    const a = ArrArrayRecLayout.parse(r.arryRecords[0]!);
    expect(a.arrAcctBal.map((x) => [x.arrAcctCurrBal.toFixed(2), x.arrAcctCurrCycDebit.toFixed(2)])).toEqual([
      ['194.00', '1005.00'],
      ['194.00', '1525.00'],
      ['-1025.00', '-2500.00'],
      ['0.00', '0.00'],
      ['0.00', '0.00'],
    ]);
  });

  it('a non-zero ACCT-CURR-CYC-DEBIT leaves the previous record value in OUT-ACCT-CURR-CYC-DEBIT', () => {
    const { catalog } = loadSeedCatalog();
    const ds = catalog.require('ACCTFILE');
    const rec = AccountRecordLayout.parse(ds.read('00000000002')!);
    ds.update(AccountRecordLayout.serialize({ ...rec, acctCurrCycDebit: '12.34' }));
    const r = runCbact01c({ catalog });
    expect(OutAcctRecLayout.parse(r.outRecords[1]!).outAcctCurrCycDebit.toFixed(2)).toBe('2525.00');
  });

  it.runIf(process.platform === 'linux')('out of space on OUTFILE: write status 34, abend 999', () => {
    const r = runCbact01c({ outFile: '/dev/full' });
    expect(r.abended).toBe(true);
    expect(r.abendCode).toBe(999);
    expect(r.display.slice(-3)).toEqual(['ACCOUNT FILE WRITE STATUS IS:34', 'FILE STATUS IS: NNNN0034', 'ABENDING PROGRAM']);
  });

  it('abends with 999 when ACCTFILE cannot be opened', () => {
    const { catalog } = loadSeedCatalog();
    catalog.delete('ACCTFILE');
    const r = runCbact01c({ catalog });
    expect(r.abended).toBe(true);
    expect(r.abendCode).toBe(999);
    expect(r.display.slice(1)).toEqual(['ERROR OPENING ACCTFILE', 'FILE STATUS IS: NNNN0035', 'ABENDING PROGRAM']);
  });
});
