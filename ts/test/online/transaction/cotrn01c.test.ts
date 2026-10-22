import { describe, expect, it } from 'vitest';
import { errmsg, menuOption, trnRuntime, value } from './helpers.js';

describe('COTRN01C transaction view', () => {
  it('menu option 7 shows the empty view screen', () => {
    const { rt } = trnRuntime();
    const r = menuOption(rt, '7');
    expect(r.programs).toEqual(['COMEN01C', 'COTRN01C']);
    expect(r.screen.map).toBe('COTRN1A');
    expect(r.nextTransid).toBe('CT01');
    expect(value(r, 'TRNID')).toBe('');
    expect(r.commarea).toHaveLength(218);
  });

  it('ENTER with a Tran ID shows every detail field', () => {
    const { rt } = trnRuntime();
    menuOption(rt, '7');
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { TRNIDIN: '0000000000683580' } });
    expect(errmsg(r)).toBe('');
    expect(value(r, 'TRNIDIN')).toBe('0000000000683580');
    expect(value(r, 'TRNID')).toBe('0000000000683580');
    expect(value(r, 'CARDNUM')).toBe('4859452612877065');
    expect(value(r, 'TTYPCD')).toBe('01');
    expect(value(r, 'TCATCD')).toBe('0001');
    expect(value(r, 'TRNSRC')).toBe('POS TERM');
    expect(value(r, 'TRNAMT')).toBe('+00000504.77');
    expect(value(r, 'TDESC')).toBe('Purchase at Abshire-Lowe');
    expect(value(r, 'TORIGDT')).toBe('2022-06-10');
    expect(value(r, 'TPROCDT')).toBe('');
    expect(value(r, 'MID')).toBe('800000000');
    expect(value(r, 'MNAME')).toBe('Abshire-Lowe');
    expect(value(r, 'MCITY')).toBe('North Enoshaven');
    expect(value(r, 'MZIP')).toBe('72112');
  });

  it('blank / unknown / short Tran IDs', () => {
    const { rt } = trnRuntime();
    menuOption(rt, '7');
    expect(errmsg(rt.sendInput('T1', { aid: 'ENTER', fields: { TRNIDIN: '' } }))).toBe('Tran ID can NOT be empty...');
    expect(errmsg(rt.sendInput('T1', { aid: 'ENTER', fields: { TRNIDIN: '9999999999999999' } }))).toBe('Transaction ID NOT found...');
    // MOVE TRNIDINI TO TRAN-ID: no zero fill, so a short id is not found.
    expect(errmsg(rt.sendInput('T1', { aid: 'ENTER', fields: { TRNIDIN: '683580' } }))).toBe('Transaction ID NOT found...');
  });

  it('PF4 clears, PF5 goes to COTRN00C, PF3 returns to the calling program', () => {
    const { rt } = trnRuntime();
    menuOption(rt, '7');
    rt.sendInput('T1', { aid: 'ENTER', fields: { TRNIDIN: '0000000000683580' } });
    const cleared = rt.sendInput('T1', { aid: 'PF4' });
    expect(value(cleared, 'TRNIDIN')).toBe('');
    expect(value(cleared, 'TRNID')).toBe('');
    const list = rt.sendInput('T1', { aid: 'PF5' });
    expect(list.programs).toEqual(['COTRN01C', 'COTRN00C']);
    expect(list.screen.map).toBe('COTRN0A');
    const view = rt.sendInput('T1', { aid: 'ENTER', fields: { SEL0001: 'S' } });
    expect(view.programs).toEqual(['COTRN00C', 'COTRN01C']);
    const back = rt.sendInput('T1', { aid: 'PF3' });
    expect(back.programs).toEqual(['COTRN01C', 'COTRN00C']);
  });

  it('PF3 from a menu-started view returns to COMEN01C; invalid key message', () => {
    const { rt } = trnRuntime();
    menuOption(rt, '7');
    expect(errmsg(rt.sendInput('T1', { aid: 'PF9' }))).toBe('Invalid key pressed. Please see below...');
    const r = rt.sendInput('T1', { aid: 'PF3' });
    expect(r.programs).toEqual(['COTRN01C', 'COMEN01C']);
  });
});
