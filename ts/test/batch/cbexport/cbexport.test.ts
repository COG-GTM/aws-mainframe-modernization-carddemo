import { describe, expect, it } from 'vitest';
import { EXPORT_CLUSTER_DEF, exportTimestamp, runCbexport } from '../../../src/batch/cbexport/index.js';
import { ExportRecordLayout } from '../../../src/common/copybooks/export.js';
import { Decimal } from '../../../src/common/record/layout.js';
import { TranRecordLayout } from '../../../src/common/copybooks/transaction.js';
import { readDailyTransactions, readExportFile } from '../../../src/common/seed/index.js';
import { asciiCatalog, ebcdicCatalog, readAll } from './helpers.js';

const fixtureTime = new Date(2025, 8, 28, 22, 53, 40, 120);
const view = { C: 'exportCustomerData', A: 'exportAccountData', X: 'exportCardXrefData', T: 'exportTransactionData', D: 'exportCardData' } as const;

describe('CBEXPORT', () => {
  const catalog = asciiCatalog();
  const result = runCbexport({ catalog, now: fixtureTime });
  const recs = readAll(catalog, 'EXPFILE');
  const parsed = recs.map((r) => ExportRecordLayout.parse(r, { views: true }));

  it('exports customers, accounts, xrefs, transactions, cards in that order, numbered 1..n', () => {
    expect(result.abended).toBe(false);
    expect(result.stats).toEqual({ customers: 50, accounts: 50, xrefs: 50, transactions: 300, cards: 50, total: 500 });
    expect(result.sequence).toBe(500);
    expect(parsed.map((r) => r.exportRecType).join('')).toBe('C'.repeat(50) + 'A'.repeat(50) + 'X'.repeat(50) + 'T'.repeat(300) + 'D'.repeat(50));
    expect(parsed.map((r) => r.exportSequenceNum.toNumber())).toEqual(Array.from({ length: 500 }, (_, i) => i + 1));
  });

  it('fills the common header', () => {
    for (const r of parsed) {
      expect(r.exportTimestamp).toBe('2025-09-28 22:53:40.00    ');
      expect(r.exportBranchId).toBe('0001');
      expect(r.exportRegionCode).toBe('NORTH');
    }
    expect(exportTimestamp(fixtureTime)).toEqual({ date: '2025-09-28', time: '22:53:40', formatted: '2025-09-28 22:53:40.00    ' });
  });

  it('packs COMP / COMP-3 fields', () => {
    const c = recs[0]!;
    expect(c.subarray(27, 31).toString('hex')).toBe('00000001'); // EXPORT-SEQUENCE-NUM COMP
    expect(c.subarray(40, 44).toString('hex')).toBe('00000001'); // EXP-CUST-ID COMP
    const fico = ExportRecordLayout.range('EXP-CUST-FICO-CREDIT-SCORE');
    expect(fico.length).toBe(2);
    expect(c.subarray(fico.offset, fico.offset + 2).toString('hex')).toMatch(/^\d{3}f$/); // unsigned COMP-3
    const a = parsed[50]!.exportAccountData!;
    expect(a.expAcctId.toNumber()).toBe(1);
    expect(a.expAcctCurrBal).toBeInstanceOf(Decimal);
    const bal = ExportRecordLayout.range('EXP-ACCT-CURR-BAL');
    expect(recs[50]!.subarray(bal.offset, bal.offset + bal.length).toString('hex')).toBe('0000000019400c');
    const debit = ExportRecordLayout.range('EXP-ACCT-CURR-CYC-DEBIT');
    expect(debit.length).toBe(8);
    const t = parsed[150]!.exportTransactionData!;
    const seedTran = TranRecordLayout.parse(readDailyTransactions().sort(Buffer.compare)[0]!);
    expect(t.expTranId).toBe(seedTran.tranId);
    expect(t.expTranAmt.equals(seedTran.tranAmt)).toBe(true);
    expect(t.expTranMerchantId.equals(seedTran.tranMerchantId)).toBe(true);
  });

  it('leaves the unused part of the 460-byte data area as spaces (INITIALIZE)', () => {
    const x = parsed[100]!.exportCardXrefData!;
    expect(x.filler).toBe(' '.repeat(427));
  });

  it('matches the reference EBCDIC export except the documented data/version differences', () => {
    const cat = ebcdicCatalog();
    runCbexport({ catalog: cat, charset: 'ebcdic', now: fixtureTime });
    const ours = readAll(cat, 'EXPFILE');
    const fixture = readExportFile();
    expect(ours).toHaveLength(fixture.length);
    const diffs: string[] = [];
    ours.forEach((rec, i) => {
      const type = ExportRecordLayout.parse(rec, { charset: 'ebcdic' }).exportRecType as keyof typeof view;
      for (const f of ExportRecordLayout.fields) {
        if (f.path === 'exportTimestamp' || f.path === 'exportSequenceNum' || f.path === 'exportRecordData' || (f.inRedefines && !f.path.startsWith(view[type]))) continue;
        const a = rec.subarray(f.offset, f.offset + f.length);
        if (!a.equals(fixture[i]!.subarray(f.offset, f.offset + f.length))) diffs.push(`${i + 1}`);
      }
    });
    // Records 1/50 (customers) and 51/100 (accounts) were edited online before the
    // reference file was taken; every other record matches field for field.
    expect([...new Set(diffs)]).toEqual(['1', '50', '51', '100']);
    // The reference file skips sequence numbers 451-459 (cards are numbered 460-509).
    const seq = (b: Buffer) => b.readUInt32BE(27);
    expect(ours.slice(450).map(seq)).toEqual(fixture.slice(450).map((b) => seq(b) - 9));
    // The reference timestamps have six fractional digits; CBEXPORT.cbl writes '.00'.
    expect(ExportRecordLayout.parse(fixture[0]!, { charset: 'ebcdic' }).exportTimestamp).toBe('2025-09-28 22:53:40.000000');
  });

  it('abends when an input cannot be opened', () => {
    const cat = asciiCatalog();
    cat.delete('TRANSACT');
    const r = runCbexport({ catalog: cat, now: fixtureTime });
    expect(r.abended).toBe(true);
    expect(r.displays.slice(-2)).toEqual(['ERROR: Cannot open TRANSACTION-INPUT, Status: 35', 'CBEXPORT: ABENDING PROGRAM']);
    expect(readAll(cat, 'EXPFILE')).toHaveLength(0);
  });

  it('exports nothing from empty inputs', () => {
    const cat = asciiCatalog();
    for (const dd of ['CUSTFILE', 'ACCTFILE', 'XREFFILE', 'TRANSACT', 'CARDFILE']) {
      const ds = cat.require(dd);
      cat.delete(ds.def.name);
      cat.define(ds.def, [dd]);
    }
    const r = runCbexport({ catalog: cat, now: fixtureTime });
    expect(r.abended).toBe(false);
    expect(r.displays).toContain('CBEXPORT: Total Records Exported: 000000000');
  });

  it('uses the JCL cluster definition KEYS(4 28)', () => {
    expect(EXPORT_CLUSTER_DEF.key).toEqual({ offset: 28, length: 4 });
    expect(EXPORT_CLUSTER_DEF.recordLength).toBe(500);
  });
});
