import { describe, expect, it } from 'vitest';
import { VsamCatalog } from '../src/common/vsam/catalog.js';
import { EsdsFile, IndexedFile } from '../src/common/vsam/cobol-file.js';
import { CicsFileControl } from '../src/common/vsam/cics-file.js';
import { Resp } from '../src/common/vsam/status.js';
import { loadSeedCatalog } from '../src/common/seed/index.js';

// 10-byte records: key(4) alt(3) data(3); AIX on alt is NONUNIQUE, UAIX on data is UNIQUE.
const rec = (k: string, alt: string, data: string) => Buffer.from(k + alt + data, 'latin1');
function catalog(collation: 'ascii' | 'ebcdic' = 'ascii') {
  const cat = new VsamCatalog();
  const ds = cat.define(
    {
      name: 'TEST.KSDS',
      organization: 'KSDS',
      recordLength: 10,
      key: { offset: 0, length: 4 },
      alternateIndexes: [
        { name: 'AIX', offset: 4, length: 3, unique: false },
        { name: 'UAIX', offset: 7, length: 3, unique: true },
      ],
      collation,
    },
    ['TESTFILE'],
  );
  ds.load([rec('0001', 'AAA', 'd01'), rec('0003', 'BBB', 'd03'), rec('0002', 'AAA', 'd02'), rec('0005', 'CCC', 'd05')]);
  return cat;
}
const keys = (bufs: (Buffer | undefined)[]) => bufs.map((b) => b?.toString('latin1', 0, 4));

describe('VsamDataset', () => {
  it('orders by prime key, BLDINDEX orders AIX duplicates by prime key', () => {
    const ds = catalog().require('TESTFILE');
    expect(keys(ds.records())).toEqual(['0001', '0002', '0003', '0005']);
    expect(ds.index('AIX').primariesFor('AAA')).toEqual(['0001', '0002']);
  });
  it('EBCDIC collation puts letters before digits', () => {
    const cat = new VsamCatalog();
    const ds = cat.define({ name: 'E', organization: 'KSDS', recordLength: 4, key: { offset: 0, length: 4 } });
    ds.load([Buffer.from('1AAA'), Buffer.from('AAAA'), Buffer.from('a000')]);
    expect(ds.records().map((r) => r.toString())).toEqual(['a000', 'AAAA', '1AAA']);
  });
});

describe('IndexedFile (COBOL FILE STATUS)', () => {
  it('OPEN errors: 35 / 41 / 42', () => {
    const cat = catalog();
    expect(new IndexedFile(cat, 'MISSING').open('INPUT').status).toBe('35');
    const f = new IndexedFile(cat, 'TESTFILE');
    expect(f.close().status).toBe('42');
    expect(f.open('INPUT').status).toBe('00');
    expect(f.open('INPUT').status).toBe('41');
  });

  it('sequential read to EOF: 00 ... 10 then 46', () => {
    const f = new IndexedFile(catalog(), 'TESTFILE');
    f.open('INPUT');
    const got: string[] = [];
    for (let r = f.readNext(); r.status === '00'; r = f.readNext()) got.push(r.record!.toString('latin1', 0, 4));
    expect(got).toEqual(['0001', '0002', '0003', '0005']);
    expect(f.status).toBe('10');
    expect(f.readNext().status).toBe('46');
  });

  it('random READ: 00 / 23, and 02 for a duplicate alternate key', () => {
    const f = new IndexedFile(catalog(), 'TESTFILE', { access: 'DYNAMIC' });
    f.open('INPUT');
    expect(f.read('0003').status).toBe('00');
    expect(f.read('0004').status).toBe('23');
    const r = f.read('AAA', 'AIX');
    expect([r.status, r.record!.toString('latin1', 0, 4)]).toEqual(['02', '0001']);
    const n = f.readNext();
    expect([n.status, n.record!.toString('latin1', 0, 4)]).toEqual(['00', '0002']);
    expect(f.readNext().record!.toString('latin1', 0, 4)).toBe('0003');
  });

  it('START conditions and READ PREVIOUS', () => {
    const f = new IndexedFile(catalog(), 'TESTFILE', { access: 'DYNAMIC' });
    f.open('INPUT');
    expect(f.start('>=', '0004').status).toBe('00');
    expect(f.readNext().record!.toString('latin1', 0, 4)).toBe('0005');
    expect(f.start('=', '0004').status).toBe('23');
    expect(f.readNext().status).toBe('46');
    expect(f.start('>', '0002').status).toBe('00');
    expect(f.readNext().record!.toString('latin1', 0, 4)).toBe('0003');
    expect(f.readPrevious().record!.toString('latin1', 0, 4)).toBe('0002');
    expect(f.start('<=', '0004').status).toBe('00');
    expect(f.readNext().record!.toString('latin1', 0, 4)).toBe('0003');
    expect(f.start('=', '000').status).toBe('00'); // generic
    expect(f.start('>', '9').status).toBe('23');
  });

  it('WRITE: 22 duplicate prime, 22 unique AIX, 02 non-unique AIX dup, 48 not open output', () => {
    const cat = catalog();
    const f = new IndexedFile(cat, 'TESTFILE', { access: 'RANDOM' });
    f.open('INPUT');
    expect(f.write(rec('0009', 'ZZZ', 'd09')).status).toBe('48');
    f.close();
    f.open('I-O');
    expect(f.write(rec('0001', 'ZZZ', 'x01')).status).toBe('22');
    expect(f.write(rec('0009', 'ZZZ', 'd01')).status).toBe('22');
    expect(f.write(rec('0009', 'AAA', 'd09')).status).toBe('02');
    expect(f.write(rec('0010', 'NEW', 'd10')).status).toBe('00');
    expect(cat.require('TESTFILE').index('AIX').primariesFor('AAA')).toEqual(['0001', '0002', '0009']);
  });

  it('sequential WRITE after OPEN OUTPUT empties the file and enforces ascending keys (21)', () => {
    const cat = catalog();
    const f = new IndexedFile(cat, 'TESTFILE');
    f.open('OUTPUT');
    expect(cat.require('TESTFILE').size).toBe(0);
    expect(f.write(rec('0002', 'AAA', 'd02')).status).toBe('00');
    expect(f.write(rec('0001', 'AAA', 'd01')).status).toBe('21');
    expect(f.write(rec('0002', 'AAA', 'd09')).status).toBe('22');
  });

  it('REWRITE / DELETE: 49, 43, 21, 23', () => {
    const cat = catalog();
    const seq = new IndexedFile(cat, 'TESTFILE');
    seq.open('INPUT');
    seq.readNext();
    expect(seq.rewrite(rec('0001', 'AAA', 'd01')).status).toBe('49');
    seq.close();
    seq.open('I-O');
    expect(seq.rewrite(rec('0001', 'AAA', 'd01')).status).toBe('43');
    seq.readNext();
    expect(seq.rewrite(rec('0002', 'AAA', 'd01')).status).toBe('21');
    seq.readNext();
    expect(seq.rewrite(rec('0002', 'QQQ', 'd02')).status).toBe('00');
    seq.readNext();
    expect(seq.delete().status).toBe('00');
    expect(seq.delete().status).toBe('43');
    seq.close();
    const rnd = new IndexedFile(cat, 'TESTFILE', { access: 'RANDOM' });
    rnd.open('I-O');
    expect(rnd.rewrite(rec('0004', 'AAA', 'd04')).status).toBe('23');
    expect(rnd.delete('0003').status).toBe('23');
    expect(rnd.delete('0005').status).toBe('00');
    expect(keys(cat.require('TESTFILE').records())).toEqual(['0001', '0002']);
    expect(cat.require('TESTFILE').index('AIX').primariesFor('QQQ')).toEqual(['0002']);
  });
});

describe('EsdsFile', () => {
  it('appends and reads sequentially', () => {
    const cat = new VsamCatalog();
    cat.define({ name: 'ES', organization: 'ESDS', recordLength: 3 });
    const f = new EsdsFile(cat, 'ES');
    f.open('OUTPUT');
    f.write(Buffer.from('abc'));
    f.write(Buffer.from('def'));
    f.close();
    f.open('I-O');
    expect(f.read().record!.toString()).toBe('abc');
    expect(f.rewrite(Buffer.from('xyz')).status).toBe('00');
    expect(f.read().record!.toString()).toBe('def');
    expect(f.read().status).toBe('10');
    expect(f.read().status).toBe('46');
    f.close();
    expect(cat.require('ES').records().map((r) => r.toString())).toEqual(['xyz', 'def']);
  });
});

describe('CicsFileControl (RESP)', () => {
  const setup = () => {
    const cat = catalog();
    return new CicsFileControl(cat, { TESTFILE: { dataset: 'TESTFILE' }, TESTAIX: { dataset: 'TESTFILE', path: 'AIX' } });
  };

  it('READ: NORMAL, NOTFND, GTEQ, GENERIC, DUPKEY via path, FILENOTFOUND/NOTOPEN/DISABLED', () => {
    const c = setup();
    expect(c.read({ file: 'TESTFILE', ridfld: '0002' }).resp).toBe(Resp.NORMAL);
    expect(c.read({ file: 'TESTFILE', ridfld: '0004' }).resp).toBe(Resp.NOTFND);
    expect(c.read({ file: 'TESTFILE', ridfld: '0004', gteq: true }).ridfld).toBe('0005');
    expect(c.read({ file: 'TESTFILE', ridfld: '0009', gteq: true }).resp).toBe(Resp.NOTFND);
    expect(c.read({ file: 'TESTFILE', ridfld: '00', keylength: 2, generic: true }).ridfld).toBe('0001');
    const d = c.read({ file: 'TESTAIX', ridfld: 'AAA' });
    expect([d.resp, d.record!.toString('latin1', 0, 4)]).toEqual([Resp.DUPKEY, '0001']);
    expect(c.read({ file: 'TESTAIX', ridfld: 'BBB' }).resp).toBe(Resp.NORMAL);
    expect(c.read({ file: 'NOPE', ridfld: 'x' }).resp).toBe(Resp.FILENOTFOUND);
    c.setFileState('TESTFILE', 'CLOSED');
    expect(c.read({ file: 'TESTFILE', ridfld: '0002' }).resp).toBe(Resp.NOTOPEN);
    c.setFileState('TESTFILE', 'OPEN');
    c.setFileState('TESTFILE', 'DISABLED');
    expect(c.read({ file: 'TESTFILE', ridfld: '0002' }).resp).toBe(Resp.DISABLED);
  });

  it('WRITE DUPREC, REWRITE needs READ UPDATE (INVREQ), DELETE NOTFND', () => {
    const c = setup();
    expect(c.write({ file: 'TESTFILE', ridfld: '0001', from: rec('0001', 'XXX', 'x01') }).resp).toBe(Resp.DUPREC);
    expect(c.write({ file: 'TESTFILE', ridfld: '0007', from: rec('0007', 'XXX', 'd07') }).resp).toBe(Resp.NORMAL);
    expect(c.write({ file: 'TESTFILE', ridfld: '0008', from: Buffer.from('short') }).resp).toBe(Resp.LENGERR);
    expect(c.rewrite({ file: 'TESTFILE', from: rec('0007', 'YYY', 'd07') }).resp).toBe(Resp.INVREQ);
    c.read({ file: 'TESTFILE', ridfld: '0007', update: true });
    expect(c.rewrite({ file: 'TESTFILE', from: rec('0007', 'YYY', 'd07') }).resp).toBe(Resp.NORMAL);
    expect(c.read({ file: 'TESTAIX', ridfld: 'YYY' }).ridfld).toBe('YYY');
    expect(c.delete({ file: 'TESTFILE', ridfld: '0004' }).resp).toBe(Resp.NOTFND);
    expect(c.delete({ file: 'TESTFILE', ridfld: '0007' }).resp).toBe(Resp.NORMAL);
    c.read({ file: 'TESTFILE', ridfld: '0001', update: true });
    expect(c.delete({ file: 'TESTFILE' }).resp).toBe(Resp.NORMAL);
    expect(c.read({ file: 'TESTFILE', ridfld: '0001' }).resp).toBe(Resp.NOTFND);
  });

  it('browse forward/backward with CICS direction-change semantics', () => {
    const c = setup();
    expect(c.startbr({ file: 'TESTFILE', ridfld: '0002' }).resp).toBe(Resp.NORMAL);
    expect(c.startbr({ file: 'TESTFILE', ridfld: '0002' }).resp).toBe(Resp.INVREQ);
    expect(c.readnext({ file: 'TESTFILE' }).ridfld).toBe('0002');
    expect(c.readnext({ file: 'TESTFILE' }).ridfld).toBe('0003');
    expect(c.readprev({ file: 'TESTFILE' }).ridfld).toBe('0003'); // same record on direction change
    expect(c.readprev({ file: 'TESTFILE' }).ridfld).toBe('0002');
    expect(c.readprev({ file: 'TESTFILE' }).ridfld).toBe('0001');
    expect(c.readprev({ file: 'TESTFILE' }).resp).toBe(Resp.ENDFILE);
    expect(c.endbr({ file: 'TESTFILE' }).resp).toBe(Resp.NORMAL);
    expect(c.endbr({ file: 'TESTFILE' }).resp).toBe(Resp.INVREQ);
    expect(c.readnext({ file: 'TESTFILE' }).resp).toBe(Resp.INVREQ);
  });

  it('STARTBR edge cases: NOTFND, READPREV after inexact GTEQ, high-values, ENDFILE, skip-sequential', () => {
    const c = setup();
    expect(c.startbr({ file: 'TESTFILE', ridfld: '0009' }).resp).toBe(Resp.NOTFND);
    expect(c.startbr({ file: 'TESTFILE', ridfld: '0004', equal: true }).resp).toBe(Resp.NOTFND);
    c.startbr({ file: 'TESTFILE', ridfld: '0004' });
    expect(c.readprev({ file: 'TESTFILE' }).resp).toBe(Resp.NOTFND);
    expect(c.readnext({ file: 'TESTFILE' }).ridfld).toBe('0005');
    expect(c.readnext({ file: 'TESTFILE' }).resp).toBe(Resp.ENDFILE);
    c.resetbr({ file: 'TESTFILE', ridfld: '\xff\xff\xff\xff' });
    expect(c.readprev({ file: 'TESTFILE' }).ridfld).toBe('0005');
    c.resetbr({ file: 'TESTFILE', ridfld: '0001' });
    expect(c.readnext({ file: 'TESTFILE' }).ridfld).toBe('0001');
    expect(c.readnext({ file: 'TESTFILE', ridfld: '0003' }).ridfld).toBe('0003');
    c.endbr({ file: 'TESTFILE' });
  });

  it('browse via non-unique path reports DUPKEY until the last duplicate', () => {
    const c = setup();
    c.startbr({ file: 'TESTAIX', ridfld: 'AAA', reqid: 1 });
    const a = c.readnext({ file: 'TESTAIX', reqid: 1 });
    const b = c.readnext({ file: 'TESTAIX', reqid: 1 });
    const d = c.readnext({ file: 'TESTAIX', reqid: 1 });
    expect([a.resp, a.record!.toString('latin1', 0, 4)]).toEqual([Resp.DUPKEY, '0001']);
    expect([b.resp, b.record!.toString('latin1', 0, 4)]).toEqual([Resp.NORMAL, '0002']);
    expect([d.resp, d.ridfld]).toEqual([Resp.NORMAL, 'BBB']);
    c.syncpoint();
    expect(c.readnext({ file: 'TESTAIX', reqid: 1 }).resp).toBe(Resp.INVREQ);
  });
});

describe('review fixes', () => {
  it('rejected OPEN (wrong organization) does not empty the cluster', () => {
    const cat = catalog();
    cat.define({ name: 'ES', organization: 'ESDS', recordLength: 10 });
    cat.require('ES').append(rec('0001', 'AAA', 'd01'));
    expect(new EsdsFile(cat, 'TESTFILE').open('OUTPUT').status).toBe('37');
    expect(cat.require('TESTFILE').size).toBe(4);
    expect(new IndexedFile(cat, 'ES').open('OUTPUT').status).toBe('37');
    expect(cat.require('ES').size).toBe(1);
  });

  it('IndexedFile: deleting a browsed duplicate-AIX record resumes within the duplicate group', () => {
    const cat = catalog();
    const f = new IndexedFile(cat, 'TESTFILE', { access: 'DYNAMIC' });
    f.open('I-O');
    expect(f.read('AAA', 'AIX').record!.toString('latin1', 0, 4)).toBe('0001');
    expect(f.delete('0001').status).toBe('00');
    expect(f.readNext().record!.toString('latin1', 0, 4)).toBe('0002');
    expect(f.delete('0002').status).toBe('00');
    expect(f.readPrevious().status).toBe('10');
    expect(f.start('>=', 'BBB', 'AIX').status).toBe('00');
    expect(f.readNext().record!.toString('latin1', 0, 4)).toBe('0003');
  });

  it('IndexedFile: READ PREVIOUS after deleting the second duplicate returns the first', () => {
    const cat = catalog();
    const f = new IndexedFile(cat, 'TESTFILE', { access: 'DYNAMIC' });
    f.open('I-O');
    f.read('AAA', 'AIX');
    expect(f.readNext().record!.toString('latin1', 0, 4)).toBe('0002');
    f.delete('0002');
    expect(f.readPrevious().record!.toString('latin1', 0, 4)).toBe('0001');
  });

  it('CICS browse via path resumes within the duplicate group after a DELETE', () => {
    const cat = catalog();
    const c = new CicsFileControl(cat, { TESTFILE: { dataset: 'TESTFILE' }, TESTAIX: { dataset: 'TESTFILE', path: 'AIX' } });
    c.startbr({ file: 'TESTAIX', ridfld: 'AAA' });
    expect(c.readnext({ file: 'TESTAIX' }).record!.toString('latin1', 0, 4)).toBe('0001');
    expect(c.delete({ file: 'TESTFILE', ridfld: '0001' }).resp).toBe(Resp.NORMAL);
    expect(c.readnext({ file: 'TESTAIX' }).record!.toString('latin1', 0, 4)).toBe('0002');
    c.delete({ file: 'TESTFILE', ridfld: '0002' });
    expect(c.readprev({ file: 'TESTAIX' }).resp).toBe(Resp.ENDFILE);
  });
});

describe('IndexedFile alternateKeys (declared ALTERNATE RECORD KEYs)', () => {
  it('WRITE/REWRITE report 02 only for duplicates in declared alternate keys', () => {
    const cat = catalog();
    const undeclared = new IndexedFile(cat, 'TESTFILE', { access: 'RANDOM', alternateKeys: [] });
    undeclared.open('I-O');
    expect(undeclared.write(rec('0009', 'AAA', 'd09')).status).toBe('00');
    expect(undeclared.rewrite(rec('0005', 'AAA', 'd05')).status).toBe('00');
    expect(undeclared.write(rec('0010', 'XXX', 'd01')).status).toBe('22'); // unique AIX still enforced
    undeclared.close();
    expect(cat.require('TESTFILE').index('AIX').primariesFor('AAA')).toEqual(['0001', '0002', '0009', '0005']);

    const declared = new IndexedFile(cat, 'TESTFILE', { access: 'RANDOM', alternateKeys: ['AIX'] });
    declared.open('I-O');
    expect(declared.write(rec('0011', 'AAA', 'd11')).status).toBe('02');
    expect(declared.rewrite(rec('0003', 'AAA', 'd03')).status).toBe('02');
    expect(declared.write(rec('0012', 'NEW', 'd12')).status).toBe('00');
    declared.close();

    const legacy = new IndexedFile(cat, 'TESTFILE', { access: 'RANDOM' });
    legacy.open('I-O');
    expect(legacy.write(rec('0013', 'AAA', 'd13')).status).toBe('02');
  });

  it('TRANSACT: duplicate TRAN-PROC-TS (TRANAIX) gives 00 when the SELECT declares no alternate key', () => {
    const { catalog: cat } = loadSeedCatalog({ transactFromDailytran: true });
    const ds = cat.require('TRANFILE');
    const existing = ds.records()[0]!;
    const dupTs = Buffer.from(existing);
    dupTs.write('9999999999999999', 0, 'latin1');
    const f = new IndexedFile(cat, 'TRANFILE', { access: 'RANDOM', alternateKeys: [] });
    f.open('I-O');
    expect(f.write(dupTs).status).toBe('00');
    const again = Buffer.from(existing);
    again.write('9999999999999998', 0, 'latin1');
    const legacy = new IndexedFile(cat, 'TRANFILE', { access: 'RANDOM' });
    f.close();
    legacy.open('I-O');
    expect(legacy.write(again).status).toBe('02');
  });
});
