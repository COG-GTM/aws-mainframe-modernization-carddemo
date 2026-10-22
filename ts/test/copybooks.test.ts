import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { describe, expect, it } from 'vitest';
import * as C from '../src/common/copybooks/index.js';
import { defaultDataDir, readExportFile } from '../src/common/seed/index.js';
import type { AnyRecordLayout } from '../src/common/record/layout.js';

const dataDir = defaultDataDir();
const lines = (f: string) => readFileSync(join(dataDir, 'ASCII', f), 'latin1').split(/\r?\n/).filter((l) => l.length > 0);

const ASCII_FILES: [string, AnyRecordLayout, number][] = [
  ['acctdata.txt', C.AccountRecordLayout, 300],
  ['carddata.txt', C.CardRecordLayout, 150],
  ['cardxref.txt', C.CardXrefRecordLayout, 50],
  ['custdata.txt', C.CustomerRecordLayout, 500],
  ['custdata.txt', C.CustrecLayout, 500],
  ['dailytran.txt', C.DalytranRecordLayout, 350],
  ['dailytran.txt', C.TranRecordLayout, 350],
  ['discgrp.txt', C.DisGroupRecordLayout, 50],
  ['tcatbal.txt', C.TranCatBalRecordLayout, 50],
  ['trancatg.txt', C.TranCatRecordLayout, 60],
  ['trantype.txt', C.TranTypeRecordLayout, 60],
];

describe('copybook round-trip against every seed record', () => {
  it.each(ASCII_FILES)('%s via %s', (file, layout, length) => {
    expect(layout.length).toBe(length);
    for (const line of lines(file)) {
      const padded = line.padEnd(length, ' ');
      expect(layout.serialize(layout.parse(line)).toString('latin1')).toBe(padded);
    }
  });

  it('decodes account amounts as zoned decimals', () => {
    const acct = C.AccountRecordLayout.parse(lines('acctdata.txt')[0]!);
    expect(acct.acctId.toString()).toBe('1');
    expect(acct.acctCurrBal.toFixed(2)).toBe('194.00');
    expect(acct.acctCreditLimit.toFixed(2)).toBe('2020.00');
    expect(acct.acctOpenDate).toBe('2014-11-20');
  });

  it('exposes field metadata (offsets/lengths)', () => {
    expect(C.CardXrefRecordLayout.range('XREF-ACCT-ID')).toEqual({ offset: 25, length: 11 });
    expect(C.CardRecordLayout.range('cardAcctId')).toEqual({ offset: 16, length: 11 });
    expect(C.TranRecordLayout.range('TRAN-PROC-TS')).toEqual({ offset: 304, length: 26 });
    expect(C.TranCatBalRecordLayout.range('TRAN-CAT-KEY')).toEqual({ offset: 0, length: 17 });
    expect(C.TrnxRecordLayout.range('trnxKey')).toEqual({ offset: 0, length: 32 });
    const amt = C.AccountRecordLayout.item('ACCT-CURR-BAL');
    expect([amt.offset, amt.length]).toEqual([12, 12]);
  });

  it('serializes a new record from a partial object with INITIALIZE defaults', () => {
    const rec = C.TranCatBalRecordLayout.serializeText({ tranCatKey: { trancatAcctId: 42, trancatTypeCd: '01', trancatCd: 5 }, tranCatBal: '-12.34' });
    expect(rec).toBe('00000000042010005' + '0000000123M' + ' '.repeat(22));
  });

  it('COSTM01 round-trips a transaction rebuilt from DALYTRAN', () => {
    const d = C.DalytranRecordLayout.parse(lines('dailytran.txt')[0]!);
    const t = C.TrnxRecordLayout.serialize({
      trnxKey: { trnxCardNum: d.dalytranCardNum, trnxId: d.dalytranId },
      trnxRest: { trnxTypeCd: d.dalytranTypeCd, trnxCatCd: d.dalytranCatCd, trnxAmt: d.dalytranAmt, trnxDesc: d.dalytranDesc },
    });
    expect(C.TrnxRecordLayout.serialize(C.TrnxRecordLayout.parse(t)).equals(t)).toBe(true);
    expect(C.TrnxRecordLayout.parse(t).trnxRest.trnxAmt.equals(d.dalytranAmt)).toBe(true);
  });
});

describe('EBCDIC seeds', () => {
  it('USRSEC (CSUSR01Y) round-trips in CP037', () => {
    const data = readFileSync(join(dataDir, 'EBCDIC', 'AWS.M2.CARDDEMO.USRSEC.PS'));
    expect(data.length).toBe(800);
    const users = [];
    for (let i = 0; i < data.length; i += 80) {
      const raw = data.subarray(i, i + 80);
      const u = C.SecUserDataLayout.parse(raw, { charset: 'ebcdic' });
      expect(C.SecUserDataLayout.serialize(u, { charset: 'ebcdic' }).equals(raw)).toBe(true);
      users.push(u);
    }
    expect(users[0]).toMatchObject({ secUsrId: 'ADMIN001', secUsrPwd: 'PASSWORD', secUsrType: 'A' });
    expect(users.filter((u) => u.secUsrType === 'U').length).toBeGreaterThan(0);
  });

  it('CVEXPORT round-trips all 500 records and decodes COMP / COMP-3 views', () => {
    const recs = readExportFile(dataDir);
    expect(recs.length).toBe(500);
    const counts: Record<string, number> = {};
    for (const raw of recs) {
      const type = C.ExportRecordLayout.parse(raw, { charset: 'ebcdic' }).exportRecType;
      counts[type] = (counts[type] ?? 0) + 1;
      const view = C.EXPORT_REC_TYPE_VIEW[type as keyof typeof C.EXPORT_REC_TYPE_VIEW];
      const full = C.ExportRecordLayout.parse(raw, { charset: 'ebcdic', views: [view] });
      expect(C.ExportRecordLayout.serialize(full, { charset: 'ebcdic' }).equals(raw)).toBe(true);
    }
    expect(counts).toEqual({ C: 50, A: 50, X: 50, T: 300, D: 50 });
    const tran = C.ExportRecordLayout.parse(recs.find((r) => r[0] === 0xe3)!, { charset: 'ebcdic', views: true });
    expect(tran.exportTransactionData!.expTranAmt.toFixed(2)).toMatch(/^-?\d+\.\d\d$/);
    const xref = C.ExportRecordLayout.parse(recs.find((r) => r[0] === 0xe7)!, { charset: 'ebcdic', views: ['exportCardXrefData'] });
    expect(xref.exportCardXrefData!.expXrefAcctId.gt(0)).toBe(true);
  });
});

describe('working-storage copybooks', () => {
  const layouts: [string, AnyRecordLayout, number][] = [
    ['COCOM01Y', C.CarddemoCommareaLayout, 160],
    ['CSUSR01Y', C.SecUserDataLayout, 80],
    ['CSMSG01Y', C.CcdaCommonMessagesLayout, 100],
    ['CSMSG02Y', C.AbendDataLayout, 134],
    ['CODATECN', C.CodatecnRecLayout, 80],
    ['CVCRD01Y', C.CcWorkAreasLayout, 213],
    ['CSDAT01Y', C.WsDateTimeLayout, 58],
  ];
  it.each(layouts)('%s has the copybook length and round-trips its initial value', (_n, layout, length) => {
    expect(layout.length).toBe(length);
    const init = layout.serialize({});
    expect(layout.serialize(layout.parse(init)).equals(init)).toBe(true);
  });

  it('REDEFINES views share storage', () => {
    const buf = C.CcWorkAreasLayout.serialize({ ccWorkArea: { ccAcctId: '00000000042' } });
    const v = C.CcWorkAreasLayout.parse(buf, { views: true });
    expect(v.ccWorkArea.ccAcctIdN!.toNumber()).toBe(42);
    const dt = C.WsDateTimeLayout.parse(C.WsDateTimeLayout.serialize({ wsCurdateData: { wsCurdateN: 20240229 } }), { views: true });
    expect(dt.wsCurdateData.wsCurdate.wsCurdateMonth.toNumber()).toBe(2);
  });

  it('CVTRA07Y report lines carry VALUE clauses', () => {
    expect(C.TransactionHeader2Layout.serializeText({})).toBe('-'.repeat(133));
    const d = C.TransactionDetailReportLayout.serializeText({ tranReportTransId: 'T1', tranReportAmt: '-12.5' });
    expect(d.endsWith('-         12.50  ')).toBe(true);
    expect(C.ReportGrandTotalsLayout.serializeText({ reptGrandTotal: '1000' })).toMatch(/^Grand Total\.+\+ {6}1,000\.00$/);
  });

  it('CSLKPCDY lookups', () => {
    expect(C.isValidUsStateCode('NY')).toBe(true);
    expect(C.isValidUsStateCode('XX')).toBe(false);
    expect(C.isValidPhoneAreaCode('212')).toBe(true);
    expect(C.isValidUsStateZipCombo('NY10')).toBe(true);
    expect(C.VALID_US_STATE_CODE.size).toBe(56);
  });
});
