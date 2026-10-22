import { describe, expect, it } from 'vitest';
import { loadSeedCatalog } from '../../common/seed/index.js';
import { runCbact02c } from './index.js';
import { runCbact03c } from '../cbact03c/index.js';
import { runCbcus01c } from '../cbcus01c/index.js';

describe.each([
  ['CBACT02C', runCbact02c, 'CARDFILE', 1, 'ERROR OPENING CARDFILE'],
  ['CBACT03C', runCbact03c, 'XREFFILE', 2, 'ERROR OPENING XREFFILE'],
  ['CBCUS01C', runCbcus01c, 'CUSTFILE', 2, 'ERROR OPENING CUSTFILE'],
] as const)('%s', (name, run, dd, perRecord, openError) => {
  it('reads every seed record in key order and DISPLAYs it', () => {
    const r = run();
    expect(r.returnCode).toBe(0);
    expect(r.records).toHaveLength(50);
    const keys = r.records.map((b) => b.toString('latin1', 0, 9));
    expect([...keys].sort()).toEqual(keys);
    expect(r.display).toHaveLength(2 + 50 * perRecord);
    expect(r.display[0]).toBe(`START OF EXECUTION OF PROGRAM ${name}`);
    expect(r.display[1]).toBe(r.records[0]!.toString('latin1'));
    expect(r.display.at(-1)).toBe(`END OF EXECUTION OF PROGRAM ${name}`);
  });

  it('empty file: EOF on first read, no record lines', () => {
    const { catalog } = loadSeedCatalog({ emptyClusters: [dd] });
    expect(run({ catalog }).display).toEqual([`START OF EXECUTION OF PROGRAM ${name}`, `END OF EXECUTION OF PROGRAM ${name}`]);
  });

  it('missing file: open error, status 35, abend 999', () => {
    const { catalog } = loadSeedCatalog();
    catalog.delete(dd);
    const r = run({ catalog });
    expect(r.abended).toBe(true);
    expect(r.display.slice(1)).toEqual([openError, 'FILE STATUS IS: NNNN0035', 'ABENDING PROGRAM']);
  });
});
