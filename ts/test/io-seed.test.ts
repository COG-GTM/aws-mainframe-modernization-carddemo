import { mkdtempSync, readFileSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { describe, expect, it } from 'vitest';
import { SequentialFile, readLineRecords, writeFixedRecords, writeLineRecords } from '../src/common/io/sequential.js';
import { CARDDEMO_CICS_FILES, loadSeedCatalog, readDailyTransactions } from '../src/common/seed/index.js';
import { CicsFileControl } from '../src/common/vsam/cics-file.js';
import { IndexedFile } from '../src/common/vsam/cobol-file.js';
import { Resp } from '../src/common/vsam/status.js';
import { AccountRecordLayout, CardXrefRecordLayout, SecUserDataLayout } from '../src/common/copybooks/index.js';

const dir = mkdtempSync(join(tmpdir(), 'carddemo-ts-'));

describe('sequential files', () => {
  it('LINE: pads short lines, statuses 00/10/46, 35 when missing, 05 when optional', () => {
    const p = join(dir, 'in.txt');
    writeFileSync(p, 'AB\r\nCDEF\n');
    const f = new SequentialFile(p, { organization: 'LINE', recordLength: 4 });
    expect(f.open('INPUT').status).toBe('00');
    expect(f.read().record!.toString()).toBe('AB  ');
    expect(f.read().record!.toString()).toBe('CDEF');
    expect(f.read().status).toBe('10');
    expect(f.read().status).toBe('46');
    expect(f.write('x').status).toBe('48');
    f.close();
    expect(new SequentialFile(join(dir, 'none'), { organization: 'LINE', recordLength: 4 }).open('INPUT').status).toBe('35');
    const opt = new SequentialFile(join(dir, 'none'), { organization: 'LINE', recordLength: 4, optional: true });
    expect(opt.open('INPUT').status).toBe('05');
    expect(opt.read().status).toBe('10');
  });
  it('writes LINE (optionally trimmed) and FIXED files', () => {
    const p = join(dir, 'out.txt');
    writeLineRecords(p, ['A  ', 'B'], { trimTrailing: true });
    expect(readFileSync(p, 'latin1')).toBe('A\nB\n');
    const f = new SequentialFile(p, { organization: 'LINE', recordLength: 3 });
    f.open('EXTEND');
    f.write('C  ');
    f.close();
    expect(readLineRecords(p, 3).map(String)).toEqual(['A  ', 'B  ', 'C  ']);
    const q = join(dir, 'out.bin');
    writeFixedRecords(q, [Buffer.from('ab'), Buffer.from('cdef')], 3, 0x40);
    expect(readFileSync(q).toString('hex')).toBe('616240636465');
  });
});

describe('seed catalog', () => {
  const { catalog, report } = loadSeedCatalog();
  it('loads every seed without rejects', () => {
    const counts = Object.fromEntries(report.map((r) => [r.name.split('.')[3], r.loaded]));
    expect(counts).toEqual({ ACCTDATA: 50, CARDDATA: 50, CUSTDATA: 50, CARDXREF: 50, TRANSACT: 0, TCATBALF: 50, DISCGRP: 51, TRANCATG: 18, TRANTYPE: 7, USRSEC: 10 });
    expect(report.every((r) => r.rejected === 0)).toBe(true);
  });
  it('resolves DD names, CICS names and DSNs to the same cluster', () => {
    expect(catalog.require('ACCTFILE')).toBe(catalog.require('ACCTDAT'));
    expect(catalog.require('XREFFILE')).toBe(catalog.require('AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS'));
  });
  it('USRSEC is loaded from the EBCDIC seed (converted to ASCII)', () => {
    const c = new CicsFileControl(catalog, CARDDEMO_CICS_FILES);
    const r = c.read({ file: 'USRSEC', ridfld: 'ADMIN001' });
    expect(r.resp).toBe(Resp.NORMAL);
    expect(SecUserDataLayout.parse(r.record!).secUsrFname.trim()).toBe('MARGARET');
  });
  it('CXACAIX: card xref by account id (NONUNIQUE path)', () => {
    const c = new CicsFileControl(catalog, CARDDEMO_CICS_FILES);
    const r = c.read({ file: 'CXACAIX', ridfld: '00000000001' });
    expect(r.resp === Resp.NORMAL || r.resp === Resp.DUPKEY).toBe(true);
    expect(CardXrefRecordLayout.parse(r.record!).xrefAcctId.toNumber()).toBe(1);
    expect(c.read({ file: 'CARDAIX', ridfld: '00000000050' }).resp).not.toBe(Resp.NOTFND);
  });
  it('batch programs read the account master via the COBOL API, preserving the ASCII seed bytes as-is (account 49)', () => {
    const f = new IndexedFile(catalog, 'ACCTFILE', { access: 'RANDOM' });
    f.open('INPUT');
    const r = f.read('00000000049');
    expect(r.status).toBe('00');
    expect(AccountRecordLayout.parse(r.record!).acctAddrZip).toBe('A000000000');
  });
  it('can seed TRANSACT from DALYTRAN', () => {
    const { catalog: c2 } = loadSeedCatalog({ transactFromDailytran: true });
    expect(c2.require('TRANSACT').size).toBe(readDailyTransactions().length);
    expect(c2.require('TRANSACT').index('TRANAIX').size).toBe(300);
  });
});
