import { describe, expect, it } from 'vitest';
import { ProgramCommarea } from '../../../src/online/transaction/index.js';
import { errmsg, field, menuOption, tranIds, trnRuntime, value } from './helpers.js';

const sorted = (ids: string[]) => [...ids].sort();

describe('COTRN00C transaction list', () => {
  it('menu option 6 shows page 1 with the first ten TRANSACT records', () => {
    const { rt, catalog } = trnRuntime();
    const ids = sorted(tranIds(catalog));
    const r = menuOption(rt, '6');
    expect(r.abend).toBeNull();
    expect(r.programs).toEqual(['COMEN01C', 'COTRN00C']);
    expect(r.screen.map).toBe('COTRN0A');
    expect(r.nextTransid).toBe('CT00');
    expect(value(r, 'PAGENUM')).toBe('00000001');
    expect(value(r, 'TRNID01')).toBe(ids[0]);
    expect(value(r, 'TRNID01')).toBe('0000000000683580');
    expect(value(r, 'TDATE01')).toBe('06/10/22');
    expect(value(r, 'TDESC01')).toBe('Purchase at Abshire-Lowe');
    expect(value(r, 'TAMT001')).toBe('+00000504.77');
    expect(value(r, 'TRNID10')).toBe(ids[9]);
    expect(value(r, 'TRNIDIN')).toBe('');
    expect(errmsg(r)).toBe('');
    expect(r.screen.cursor).toEqual({ row: field(r, 'TRNIDIN').row, col: field(r, 'TRNIDIN').col });
    expect(r.commarea).toHaveLength(218);
    const ca = ProgramCommarea.fromDfhCommarea(Buffer.from(r.commarea!, 'latin1'), 218);
    expect(ca.get('TRNID-FIRST')).toBe(ids[0]);
    expect(ca.get('TRNID-LAST')).toBe(ids[9]);
    expect(ca.get('PAGE-NUM')).toBe('00000001');
    expect(ca.get('NEXT-PAGE-FLG')).toBe('Y');
    expect(ca.isPgmReenter).toBe(true);
  });

  it('PF8 pages forward and PF7 back (page numbers, first/last keys)', () => {
    const { rt, catalog } = trnRuntime();
    const ids = sorted(tranIds(catalog));
    menuOption(rt, '6');
    const p2 = rt.sendInput('T1', { aid: 'PF8' });
    expect(value(p2, 'PAGENUM')).toBe('00000002');
    expect(value(p2, 'TRNID01')).toBe(ids[10]);
    expect(value(p2, 'TRNID10')).toBe(ids[19]);
    const p3 = rt.sendInput('T1', { aid: 'PF8' });
    expect(value(p3, 'PAGENUM')).toBe('00000003');
    expect(value(p3, 'TRNID01')).toBe(ids[20]);
    const back = rt.sendInput('T1', { aid: 'PF7' });
    expect(value(back, 'PAGENUM')).toBe('00000002');
    expect(value(back, 'TRNID01')).toBe(ids[10]);
    expect(value(back, 'TRNID10')).toBe(ids[19]);
    const top = rt.sendInput('T1', { aid: 'PF7' });
    expect(value(top, 'PAGENUM')).toBe('00000001');
    expect(value(top, 'TRNID01')).toBe(ids[0]);
    expect(errmsg(top)).toBe('You have reached the top of the page...');
  });

  it('PF7 on page 1 -> already at the top (screen kept)', () => {
    const { rt, catalog } = trnRuntime();
    const ids = sorted(tranIds(catalog));
    menuOption(rt, '6');
    const r = rt.sendInput('T1', { aid: 'PF7' });
    expect(errmsg(r)).toBe('You are already at the top of the page...');
    expect(value(r, 'TRNID01')).toBe(ids[0]);
    expect(value(r, 'PAGENUM')).toBe('00000001');
  });

  it('pages to the end: page 30 reports the bottom, further PF8 says already at the bottom', () => {
    const { rt, catalog } = trnRuntime();
    const ids = sorted(tranIds(catalog));
    menuOption(rt, '6');
    let r = rt.sendInput('T1', { aid: 'PF8' });
    for (let i = 3; i <= 30; i++) r = rt.sendInput('T1', { aid: 'PF8' });
    expect(value(r, 'PAGENUM')).toBe('00000030');
    expect(value(r, 'TRNID10')).toBe(ids[299]);
    expect(errmsg(r)).toBe('You have reached the bottom of the page...');
    const again = rt.sendInput('T1', { aid: 'PF8' });
    expect(errmsg(again)).toBe('You are already at the bottom of the page...');
    expect(value(again, 'TRNID10')).toBe(ids[299]);
  });

  it('a partial last page clears the unused rows', () => {
    const { rt, catalog } = trnRuntime();
    const ids = sorted(tranIds(catalog));
    menuOption(rt, '6');
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { TRNIDIN: ids[295]! } });
    expect(value(r, 'TRNID01')).toBe(ids[295]);
    expect(value(r, 'TRNID05')).toBe(ids[299]);
    expect(value(r, 'TRNID06')).toBe('');
    expect(value(r, 'PAGENUM')).toBe('00000001');
    expect(errmsg(r)).toBe('You have reached the bottom of the page...');
  });

  it('typed Tran ID starts the list at that key (GTEQ); non-numeric is rejected', () => {
    const { rt, catalog } = trnRuntime();
    const ids = sorted(tranIds(catalog));
    menuOption(rt, '6');
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { TRNIDIN: '0000000025430000' } });
    expect(value(r, 'TRNID01')).toBe(ids[10]);
    expect(value(r, 'TRNIDIN')).toBe('');
    const bad = rt.sendInput('T1', { aid: 'ENTER', fields: { TRNIDIN: '12AB' } });
    expect(errmsg(bad)).toBe('Tran ID must be Numeric ...');
    expect(value(bad, 'TRNIDIN')).toBe('12AB');
  });

  it("'S' on a row XCTLs to COTRN01C with the selected id; other values are rejected", () => {
    const { rt, catalog } = trnRuntime();
    const ids = sorted(tranIds(catalog));
    menuOption(rt, '6');
    const bad = rt.sendInput('T1', { aid: 'ENTER', fields: { SEL0002: 'X' } });
    expect(errmsg(bad)).toBe('Invalid selection. Valid value is S');
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { SEL0002: 's' } });
    expect(r.programs).toEqual(['COTRN00C', 'COTRN01C']);
    expect(r.screen.map).toBe('COTRN1A');
    expect(value(r, 'TRNIDIN')).toBe(ids[1]);
    expect(value(r, 'TRNID')).toBe(ids[1]);
    expect(r.nextTransid).toBe('CT01');
  });

  it('PF3 returns to COMEN01C; other keys are invalid', () => {
    const { rt } = trnRuntime();
    menuOption(rt, '6');
    expect(errmsg(rt.sendInput('T1', { aid: 'PF5' }))).toBe('Invalid key pressed. Please see below...');
    const r = rt.sendInput('T1', { aid: 'PF3' });
    expect(r.programs).toEqual(['COTRN00C', 'COMEN01C']);
    expect(r.screen.map).toBe('COMEN1A');
  });

  it('EIBCALEN = 0 (CT00 typed on a fresh terminal) XCTLs to COSGN00C', () => {
    const { rt } = trnRuntime();
    rt.openTerminal('T9');
    const r = rt.startTransaction('T9', 'CT00');
    expect(r.programs).toEqual(['COTRN00C', 'COSGN00C']);
    expect(r.screen.map).toBe('COSGN0A');
  });

  it('empty TRANSACT: STARTBR NOTFND, then the unconditional ENDBR raises INVREQ (abend, as in CICS)', () => {
    const { rt } = trnRuntime({ emptyTransact: true });
    const r = menuOption(rt, '6');
    expect(r.abend).not.toBeNull();
    expect(r.abend!.code).toBe('AEIP');
  });
});
