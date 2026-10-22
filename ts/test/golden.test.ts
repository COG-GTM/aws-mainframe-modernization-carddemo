/**
 * Parity with GnuCOBOL: test/golden/*.txt were produced by running the COBOL in
 * test/golden/cobol under GnuCOBOL 3.1 (tools/gen-golden.sh).
 */
import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { describe, expect, it } from 'vitest';
import { encodeZoned, formatEdited, parsePic } from '../src/common/codecs/index.js';
import { VsamCatalog } from '../src/common/vsam/catalog.js';
import { IndexedFile } from '../src/common/vsam/cobol-file.js';

const golden = (f: string) => readFileSync(join(import.meta.dirname, 'golden', f), 'latin1').split('\n').filter((l) => l.length > 0);

describe('GnuCOBOL golden: numeric MOVE / editing', () => {
  it.each(golden('editgold.txt').map((l) => [l]))('%s', (line) => {
    const [num, e1, e2, z, z2] = line.split('|') as [string, string, string, string, string];
    const value = (num.endsWith('-') ? '-' : '') + num.slice(0, -1);
    expect(formatEdited('-ZZZ,ZZZ,ZZZ.ZZ', value)).toBe(e1);
    expect(formatEdited('+ZZZ,ZZZ,ZZZ.ZZ', value)).toBe(e2);
    expect(encodeZoned(value, parsePic('S9(10)V99'), 'ascii').toString('latin1')).toBe(z);
    expect(encodeZoned(value, parsePic('S9(09)V99'), 'ascii').toString('latin1')).toBe(z2);
  });
});

describe('GnuCOBOL golden: indexed FILE STATUS sequence (FSGOLD.cbl)', () => {
  it('matches step by step (except IBM-only status 02 on READ via duplicate alternate key)', () => {
    const cat = new VsamCatalog();
    cat.define({
      name: 'fsgold.dat',
      organization: 'KSDS',
      recordLength: 10,
      key: { offset: 0, length: 4 },
      alternateIndexes: [{ name: 'T-ALT', offset: 4, length: 3, unique: false }],
      collation: 'ascii',
    });
    const t = new IndexedFile(cat, 'fsgold.dat', { access: 'DYNAMIC' });
    const m = new IndexedFile(cat, 'fsmissing.dat', { access: 'DYNAMIC' });
    const r = (s: string) => Buffer.from(s, 'latin1');
    const steps: string[] = [
      m.open('INPUT').status,
      t.open('OUTPUT').status,
      t.write(r('0001AAAd01')).status,
      t.write(r('0002AAAd02')).status,
      t.write(r('0003BBBd03')).status,
      t.write(r('0001ZZZx01')).status,
      t.close().status,
      t.open('I-O').status,
      t.read('0004').status,
      t.read('0002').status,
      t.read('AAA', 'T-ALT').status,
      t.readNext().status,
      t.readNext().status,
      t.readNext().status,
      t.readNext().status,
      t.start('>=', '0004').status,
      t.start('>=', '0002').status,
      t.readNext().status,
      t.rewrite(r('0002QQQd02')).status,
      t.delete('0009').status,
      t.delete('0003').status,
      t.start('>=', '0001').status,
      t.readNext().status,
      t.readNext().status,
      t.readNext().status,
      t.open('I-O').status,
      t.close().status,
      t.close().status,
      t.open('INPUT').status,
      t.write(r('0007NEWd07')).status,
      t.rewrite(r('0001AAAd01')).status,
      t.close().status,
    ];
    const expected = golden('fsgold.txt').map((l) => l.split(' ')[1]);
    // Step 11: IBM Enterprise COBOL returns '02' (more records with this alternate key
    // follow); GnuCOBOL's BDB handler returns '00'. We follow IBM - see README LIMITATIONS.
    expect(expected[10]).toBe('00');
    expected[10] = '02';
    expect(steps).toEqual(expected);
  });
});
