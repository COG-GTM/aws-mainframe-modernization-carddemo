import { describe, expect, it } from 'vitest';
import { Cbstm03b, type M03bArea } from '../../../src/batch/cbstm03b/index.js';
import {
  buildTrnxFile,
  Cbstm03aAbend,
  cobolString,
  CREASTMT_STEP040,
  readTransact,
  runCbstm03a,
  sortTransactToTrnx,
  StorageOverlayError,
} from '../../../src/batch/cbstm03a/index.js';
import { loadSeedCatalog } from '../../../src/common/seed/index.js';

/** dailytran-shaped record: id(16) type(2) cat(4) src(10) desc(100) amt(11) mid(9) mname(50) mcity(50) mzip(10) card(16) ts(26) ts(26) filler(20). */
function dalytran(id: string, card: string, desc: string, amt: string): Buffer {
  const s = id.padEnd(16) + '01' + '0001' + 'POS TERM  ' + desc.padEnd(100) + amt + '000000001' + ''.padEnd(110) + card.padEnd(16) + ''.padEnd(72);
  return Buffer.from(s, 'latin1');
}

function seeded(transact: Buffer[]) {
  const { catalog } = loadSeedCatalog();
  buildTrnxFile(catalog, transact);
  return catalog;
}

const CARD_ACCT1 = '0500024453765740'; // cardxref.txt: account 00000000050, customer 000000050

describe('TRNXFILE build (CREASTMT STEP010/020)', () => {
  it('sorts on card + tran id and moves the card number to the front', () => {
    const recs = sortTransactToTrnx([dalytran('B', '2222', 'b', '00000000100'), dalytran('A', '2222', 'a', '00000000100'), dalytran('C', '1111', 'c', '00000000100')]);
    expect(recs.map((r) => r.toString('latin1', 0, 17))).toEqual(['1111            C', '2222            A', '2222            B']);
    expect(recs[0]!.length).toBe(350);
    expect(recs[0]!.toString('latin1', 328).trim()).toBe('');
  });

  it('sorts in EBCDIC order (digits after letters)', () => {
    const recs = sortTransactToTrnx([dalytran('1', 'X', '', '00000000100'), dalytran('A', 'X', '', '00000000100')]);
    expect(recs.map((r) => r.toString('latin1', 16, 17))).toEqual(['A', '1']);
  });
});

describe('CBSTM03A', () => {
  it('DISPLAYs the job, step and TIOT DD names (last entry printed after the loop)', () => {
    const catalog = seeded([dalytran('T1', CARD_ACCT1, 'x', '00000000100')]);
    const { display } = runCbstm03a({ catalog });
    expect(display).toEqual([
      'Running JCL : CREASTMT Step STEP040 ',
      'DD Names from TIOT: ',
      ...CREASTMT_STEP040.dds.slice(0, -1).map((d) => `: ${d.ddName.padEnd(8)} -- valid UCB`),
      ': HTMLFILE -- valid UCB',
    ]);
    const nullUcb = runCbstm03a({ catalog, job: { jobName: 'J', stepName: 'S', dds: [{ ddName: 'A', ucb: false }, { ddName: 'B', ucb: false }] } });
    expect(nullUcb.display.slice(2)).toEqual([': A        --  null UCB', ': B        -- null  UCB']);
  });

  it('opens the files through the ALTERed GO TO chain, then reads, then closes', () => {
    const catalog = seeded([dalytran('T1', CARD_ACCT1, 'x', '00000000100'), dalytran('T2', CARD_ACCT1, 'y', '00000000100')]);
    const inner = new Cbstm03b(catalog);
    const calls: string[] = [];
    const spy = { call: (a: M03bArea) => { inner.call(a); calls.push(`${a.dd.trim()}:${a.oper}:${a.rc}`); } };
    runCbstm03a({ catalog, job: null, subprogram: spy });
    expect(calls.slice(0, 7)).toEqual(['TRNXFILE:O:00', 'TRNXFILE:R:00', 'TRNXFILE:R:00', 'TRNXFILE:R:10', 'XREFFILE:O:00', 'CUSTFILE:O:00', 'ACCTFILE:O:00']);
    expect(calls.slice(7, 10)).toEqual(['XREFFILE:R:00', 'CUSTFILE:K:00', 'ACCTFILE:K:00']);
    expect(calls.slice(-5)).toEqual(['XREFFILE:R:10', 'TRNXFILE:C:00', 'XREFFILE:C:00', 'CUSTFILE:C:00', 'ACCTFILE:C:00']);
    expect(calls.filter((c) => c === 'XREFFILE:R:00')).toHaveLength(50);
  });

  it('prints every XREF card; cards without transactions get an empty summary and a zero total', () => {
    const catalog = seeded([dalytran('T1', CARD_ACCT1, 'x', '00000000100')]);
    const { stmtRecords } = runCbstm03a({ catalog, job: null });
    expect(stmtRecords.filter((l) => l.startsWith('*****') && l.includes('START'))).toHaveLength(50);
    const totals = stmtRecords.filter((l) => l.startsWith('Total EXP:')).map((l) => l.slice(66).trim());
    expect(totals.filter((t) => t === '$         .00')).toHaveLength(49);
  });

  it('formats rows, negative amounts and the S9(9)V99 total', () => {
    const catalog = seeded([
      dalytran('0000000000000001', CARD_ACCT1, 'Purchase at A', '00000012345'),
      dalytran('0000000000000002', CARD_ACCT1, 'Return at B', '0000000050J'), // -5.01
    ]);
    const { stmtRecords, htmlRecords } = runCbstm03a({ catalog, job: null });
    const start = stmtRecords.findIndex((l) => l.startsWith('Account ID         :00000000050'));
    const rows = stmtRecords.slice(start, start + 15).filter((l) => l.startsWith('000000000000000'));
    expect(rows).toEqual([
      '0000000000000001 Purchase at A                                    $      123.45 ',
      '0000000000000002 Return at B                                      $        5.01-',
    ]);
    expect(stmtRecords[start + 11]).toBe('Total EXP:                                                        $      118.44 ');
    expect(htmlRecords.map((l) => l.trimEnd())).toContain('<p>        5.01-</p>');
  });

  it('accumulates the total with high-order truncation like ADD without ON SIZE ERROR', () => {
    const catalog = seeded([dalytran('T1', CARD_ACCT1, 'a', '99999999999'), dalytran('T2', CARD_ACCT1, 'b', '00000000002')]);
    const { stmtRecords } = runCbstm03a({ catalog, job: null });
    expect(stmtRecords.find((l) => l.startsWith('Total EXP:') && !l.endsWith('.00 '))).toBe(
      'Total EXP:                                                        $         .01 ',
    );
  });

  it('STRING ... DELIMITED BY SPACE keeps only the first word of each name part', () => {
    expect(cobolString('    ', [{ value: 'Mary Ann', delimitedBy: ' ' }, { value: '!', delimitedBy: { size: true } }]).result).toBe('Mary');
    expect(cobolString('......', [{ value: 'ab', delimitedBy: ' ' }])).toEqual({ result: 'ab....', overflow: false });
  });

  it('abends when TRNXFILE is empty (first READ returns 10)', () => {
    const catalog = seeded([]);
    const err = (() => { try { runCbstm03a({ catalog, job: null }); } catch (e) { return e; } })();
    expect(err).toBeInstanceOf(Cbstm03aAbend);
    expect((err as Cbstm03aAbend).display).toEqual(['ERROR READING TRNXFILE', 'RETURN CODE: 10', 'ABENDING PROGRAM']);
  });

  it('abends when TRNXFILE was never built (OPEN status 35)', () => {
    const { catalog } = loadSeedCatalog();
    expect(() => runCbstm03a({ catalog, job: null })).toThrow('ERROR OPENING TRNXFILE; RETURN CODE: 35');
  });

  it('abends on a missing customer, after the statements already produced', () => {
    const catalog = seeded([dalytran('T1', CARD_ACCT1, 'x', '00000000100')]);
    catalog.require('CUSTFILE').delete(readTransact(catalog, 'XREFFILE')[1]!.toString('latin1', 16, 25));
    try {
      runCbstm03a({ catalog, job: null });
      expect.unreachable();
    } catch (e) {
      const ab = e as Cbstm03aAbend;
      expect(ab.display).toEqual(['ERROR READING CUSTFILE', 'RETURN CODE: 23', 'ABENDING PROGRAM']);
      expect(ab.stmtRecords.filter((l) => l.startsWith('Total EXP:'))).toHaveLength(1);
    }
  });

  it('a 52nd card overruns WS-TRNX-TABLE (OCCURS 51, no subscript checking)', () => {
    const recs = Array.from({ length: 52 }, (_, i) => dalytran(`T${i}`, String(i).padStart(16, '0'), 'x', '00000000100'));
    expect(() => runCbstm03a({ catalog: seeded(recs), job: null })).toThrow(StorageOverlayError);
  });

  it('an 11th transaction on a card overlays the next card slot, as the COBOL storage would', () => {
    const recs = Array.from({ length: 11 }, (_, i) => dalytran(`T${String(i).padStart(2, '0')}`, CARD_ACCT1, `d${i}`, '00000000100'));
    const { stmtRecords } = runCbstm03a({ catalog: seeded(recs), job: null });
    const rows = stmtRecords.filter((l) => l.endsWith('$        1.00 '));
    expect(rows).toHaveLength(11);
    expect(rows[10]!.startsWith('T10')).toBe(true);
  });
});

describe('CBSTM03A HTML escaping', () => {
  const recs = () => [dalytran('0000000000000001', CARD_ACCT1, '<script>alert("x")</script> & co', '00000000100')];

  it('by default writes data verbatim into HTMLFILE, like the COBOL', () => {
    const { htmlRecords } = runCbstm03a({ catalog: seeded(recs()), job: null });
    expect(htmlRecords.map((l) => l.trimEnd())).toContain('<p><script>alert("x")</script> & co                 </p>');
  });

  it('escapeHtml: true escapes every data value but leaves the fixed markup and STMTFILE alone', () => {
    const plain = runCbstm03a({ catalog: seeded(recs()), job: null });
    const { htmlRecords, stmtRecords } = runCbstm03a({ catalog: seeded(recs()), job: null, escapeHtml: true });
    expect(htmlRecords.map((l) => l.trimEnd())).toContain('<p>&lt;script&gt;alert(&quot;x&quot;)&lt;/script&gt; &amp; co                 </p>');
    expect(htmlRecords.some((l) => l.includes('<script>'))).toBe(false);
    expect(htmlRecords[0]!.trimEnd()).toBe('<!DOCTYPE html>');
    expect(stmtRecords).toEqual(plain.stmtRecords);
    expect(htmlRecords.length).toBe(plain.htmlRecords.length);
  });

  it('escapeHtml never leaves a character reference cut off at the 100-byte record end', () => {
    // '<p>' + 49-byte ST-TRAN-DT: n x's then '<' * (49 - n) escapes past the 100-byte record, cutting at every offset of '&lt;'.
    for (const n of [20, 21, 22, 23]) {
      const desc = 'x'.repeat(n) + '<'.repeat(49 - n);
      const { htmlRecords } = runCbstm03a({ catalog: seeded([dalytran('0000000000000001', CARD_ACCT1, desc, '00000000100')]), job: null, escapeHtml: true });
      const row = htmlRecords.find((l) => l.startsWith('<p>xxxx'))!;
      expect(row).toHaveLength(100);
      expect(row.trimEnd()).toMatch(/^<p>x+(&lt;)+$/);
      expect(row.trimEnd().length).toBe(100 - ((97 - n) % 4));
    }
  });

  it('on the seed data escapeHtml only changes apostrophes (e.g. merchant names) to &#39;', () => {
    const { catalog } = loadSeedCatalog({ transactFromDailytran: true });
    buildTrnxFile(catalog);
    const plain = runCbstm03a({ catalog, job: null }).htmlRecords.map((l) => l.trimEnd());
    const escaped = runCbstm03a({ catalog, job: null, escapeHtml: true }).htmlRecords.map((l) => l.trimEnd());
    expect(plain.filter((l) => l.includes("'")).length).toBeGreaterThan(0);
    expect(escaped).toEqual(plain.map((l) => l.replaceAll("'", '&#39;')));
  });
});
