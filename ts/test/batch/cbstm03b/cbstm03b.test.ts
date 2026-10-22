import { describe, expect, it } from 'vitest';
import { Cbstm03b, type M03bArea, newM03bArea } from '../../../src/batch/cbstm03b/index.js';
import { buildTrnxFile } from '../../../src/batch/cbstm03a/index.js';
import { loadSeedCatalog } from '../../../src/common/seed/index.js';

function setup() {
  const { catalog } = loadSeedCatalog({ transactFromDailytran: true });
  buildTrnxFile(catalog);
  const sub = new Cbstm03b(catalog);
  const area = newM03bArea();
  const call = (dd: string, oper: string, key = '', keyLn = key.length): M03bArea => {
    Object.assign(area, { dd, oper, rc: '00', key: key.padEnd(25, ' '), keyLn });
    area.fldt.fill(0x20);
    sub.call(area);
    return area;
  };
  return { sub, area, call };
}

describe('CBSTM03B', () => {
  it('starts with an all-spaces linkage area', () => {
    const a = newM03bArea();
    expect([a.dd, a.oper, a.rc, a.key.trim(), a.keyLn, a.fldt.length]).toEqual(['        ', ' ', '  ', '', 0, 1000]);
  });

  it('reads TRNXFILE sequentially in key order and returns 10 at end', () => {
    const { call } = setup();
    expect(call('TRNXFILE', 'O').rc).toBe('00');
    const keys: string[] = [];
    for (let a = call('TRNXFILE', 'R'); a.rc === '00'; a = call('TRNXFILE', 'R')) keys.push(a.fldt.toString('latin1', 0, 32));
    expect(keys).toHaveLength(300);
    expect([...keys].sort()).toEqual(keys);
    expect(call('TRNXFILE', 'R').rc).toBe('46');
    expect(call('TRNXFILE', 'C').rc).toBe('00');
  });

  it('end of file on XREFFILE is status 10 and leaves FLDT as it was set by the caller', () => {
    const { call } = setup();
    call('XREFFILE', 'O');
    for (let i = 0; i < 50; i++) expect(call('XREFFILE', 'R').rc).toBe('00');
    const a = call('XREFFILE', 'R');
    expect(a.rc).toBe('10');
    expect(a.fldt.toString('latin1').trim()).toBe('');
  });

  it('reads CUSTFILE / ACCTFILE by key using LK-M03B-KEY (1:KEY-LN)', () => {
    const { call } = setup();
    call('CUSTFILE', 'O');
    call('ACCTFILE', 'O');
    const c = call('CUSTFILE', 'K', '000000001');
    expect(c.rc).toBe('00');
    expect(c.fldt.toString('latin1', 0, 9)).toBe('000000001');
    expect(c.fldt.toString('latin1', 500).trim()).toBe('');
    // key longer than KEY-LN: only the first KEY-LN bytes are used
    expect(call('CUSTFILE', 'K', '000000001XYZ', 9).rc).toBe('00');
    expect(call('ACCTFILE', 'K', '00000000001').fldt.toString('latin1', 0, 11)).toBe('00000000001');
    // short ACCT key: MOVE X(n) TO 9(11) right-justifies with zeros
    expect(call('ACCTFILE', 'K', '1').fldt.toString('latin1', 0, 11)).toBe('00000000001');
    expect(call('CUSTFILE', 'K', '999999999').rc).toBe('23');
    expect(call('ACCTFILE', 'K', '99999999999').rc).toBe('23');
  });

  it('returns COBOL FILE STATUS for open/close/read misuse', () => {
    const { call } = setup();
    expect(call('XREFFILE', 'R').rc).toBe('47');
    expect(call('CUSTFILE', 'K', '000000001').rc).toBe('47');
    expect(call('XREFFILE', 'C').rc).toBe('42');
    expect(call('XREFFILE', 'O').rc).toBe('00');
    expect(call('XREFFILE', 'O').rc).toBe('41');
  });

  it('unhandled operations return the last FILE STATUS of that file', () => {
    const { call } = setup();
    expect(call('XREFFILE', 'W').rc).toBe('  '); // status never set yet: spaces
    call('XREFFILE', 'O');
    expect(call('XREFFILE', 'K', 'x').rc).toBe('00');
    expect(call('CUSTFILE', 'R').rc).toBe('  ');
    expect(call('CUSTFILE', 'O').rc).toBe('00');
    expect(call('CUSTFILE', 'O').rc).toBe('41');
    expect(call('CUSTFILE', 'Z').rc).toBe('41');
  });

  it('unknown DD names leave the area untouched', () => {
    const { call } = setup();
    expect(call('STMTFILE', 'O').rc).toBe('00');
    expect(call('trnxfile', 'O').rc).toBe('00');
  });

  it('missing TRNXFILE cluster opens with status 35', () => {
    const { catalog } = loadSeedCatalog();
    const sub = new Cbstm03b(catalog);
    const a = newM03bArea();
    Object.assign(a, { dd: 'TRNXFILE', oper: 'O' });
    sub.call(a);
    expect(a.rc).toBe('35');
  });
});
