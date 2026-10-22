import { describe, expect, it } from 'vitest';
import { runtime, signOn } from '../helpers.js';

const TITLE01 = '      AWS Mainframe Modernization       ';
const TITLE02 = '              CardDemo                  ';

describe('S7 screen header lines', () => {
  it.each([
    ['1', 'CACTVWA', 'CAVW', 'COACTVWC'],
    ['2', 'CACTUPA', 'CAUP', 'COACTUPC'],
    ['3', 'CCRDLIA', 'CCLI', 'COCRDLIC'],
    ['4', 'CCRDSLA', 'CCDL', 'COCRDSLC'],
    ['5', 'CCRDUPA', 'CCUP', 'COCRDUPC'],
  ])('option %s (%s) renders Tran:/Prog: header', (opt, map, tran, pgm) => {
    const rt = runtime();
    signOn(rt, 'USER0001', 'PASSWORD');
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { OPTION: opt } });
    expect(r.screen.map).toBe(map);
    const lines = r.screen.lines;
    expect(lines[0]).toBe(`Tran: ${tran}${' '.repeat(10)}${TITLE01}    Date: 10/22/26  `);
    expect(lines[1]).toBe(`Prog: ${pgm}${' '.repeat(6)}${TITLE02}    Time: 14:05:09  `);
  });
});
