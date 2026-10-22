import { describe, expect, it } from 'vitest';
import { errmsg, runtime, signOn, value } from '../helpers.js';

const menu = (opt: string) => {
  const rt = runtime();
  signOn(rt, 'USER0001', 'PASSWORD');
  const r = rt.sendInput('T1', { aid: 'ENTER', fields: { OPTION: opt } });
  return { rt, r };
};

describe('COACTVWC account view', () => {
  it('shows account 1 with customer details', () => {
    const { rt, r } = menu('1');
    expect(r.screen.map).toBe('CACTVWA');
    expect(value(r, 'INFOMSG')).toBe('Enter or update id of account to display');
    const r2 = rt.sendInput('T1', { aid: 'ENTER', fields: { ACCTSID: '00000000001' } });
    expect(errmsg(r2)).toBe('');
    expect(value(r2, 'ACSTTUS')).toBe('Y');
    expect(value(r2, 'ACURBAL').trim()).toBe('+        194.00');
    expect(value(r2, 'ACSFNAM')).toBe('Immanuel');
  });
  it('validates the account filter', () => {
    const { rt } = menu('1');
    expect(errmsg(rt.sendInput('T1', { aid: 'ENTER', fields: { ACCTSID: 'abc' } }))).toBe('Account Filter must  be a non-zero 11 digit number');
    expect(errmsg(rt.sendInput('T1', { aid: 'ENTER', fields: { ACCTSID: '00000000000' } }))).toBe('Account Filter must  be a non-zero 11 digit number');
    expect(errmsg(rt.sendInput('T1', { aid: 'ENTER', fields: { ACCTSID: '99999999999' } }))).toMatch(/^Account:99999999999 not found in Cross ref file\. {2}Resp:000000013/);
  });
  it('PF3 returns to the menu', () => {
    const { rt } = menu('1');
    expect(rt.sendInput('T1', { aid: 'PF3' }).screen.map).toBe('COMEN1A');
  });
});

describe('COCRDLIC / COCRDSLC / COCRDUPC card flow', () => {
  it('pages, selects and returns', () => {
    const { rt, r } = menu('3');
    expect(r.screen.map).toBe('CCRDLIA');
    expect(value(r, 'CRDNUM1')).toBe('0500024453765740');
    expect(value(r, 'PAGENO')).toBe('1');
    expect(value(r, 'INFOMSG')).toBe('TYPE S FOR DETAIL, U TO UPDATE ANY RECORD');
    const r2 = rt.sendInput('T1', { aid: 'PF8' });
    expect(value(r2, 'PAGENO')).toBe('2');
    expect(value(r2, 'CRDNUM1')).toBe('1561409106491600');
    const r3 = rt.sendInput('T1', { aid: 'PF7' });
    expect(value(r3, 'PAGENO')).toBe('1');
    expect(errmsg(r3)).toBe('NO PREVIOUS PAGES TO DISPLAY');
    const r4 = rt.sendInput('T1', { aid: 'ENTER', fields: { CRDSEL1: 'S' } });
    expect(r4.screen.map).toBe('CCRDSLA');
    expect(value(r4, 'CRDNAME')).toBe('Aniya Von');
    expect(value(r4, 'INFOMSG')).toBe('   Displaying requested details');
    expect(rt.sendInput('T1', { aid: 'PF3' }).screen.map).toBe('CCRDLIA');
  });
  it('rejects more than one selection and invalid action codes', () => {
    expect(errmsg(menu('3').rt.sendInput('T1', { aid: 'ENTER', fields: { CRDSEL1: 'S', CRDSEL2: 'U' } }))).toBe('PLEASE SELECT ONLY ONE RECORD TO VIEW OR UPDATE');
    expect(errmsg(menu('3').rt.sendInput('T1', { aid: 'ENTER', fields: { CRDSEL3: 'X' } }))).toBe('INVALID ACTION CODE');
    expect(errmsg(menu('3').rt.sendInput('T1', { aid: 'ENTER', fields: { ACCTSID: 'ABC' } }))).toBe('ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER');
  });
  it('updates a card with validation and PF5 confirmation', () => {
    const { rt } = menu('3');
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { CRDSEL2: 'U' } });
    expect(r.screen.map).toBe('CCRDUPA');
    expect(value(r, 'CRDNAME')).toBe('WARD JONES');
    expect(errmsg(rt.sendInput('T1', { aid: 'ENTER', fields: { CRDNAME: 'J0HN' } }))).toBe('Card name can only contain alphabets and spaces');
    expect(errmsg(rt.sendInput('T1', { aid: 'ENTER', fields: { CRDNAME: 'JOHN DOE', EXPMON: '13' } }))).toBe('Card expiry month must be between 1 and 12');
    const ok = rt.sendInput('T1', { aid: 'ENTER', fields: { CRDNAME: 'JOHN DOE', EXPMON: '12' } });
    expect(value(ok, 'INFOMSG')).toBe('Changes validated.Press F5 to save');
    const done = rt.sendInput('T1', { aid: 'PF5' });
    expect(value(done, 'INFOMSG')).toBe('Changes committed to database');
    expect(rt.sendInput('T1', { aid: 'ENTER' }).screen.map).toBe('CCRDLIA');
  });
});

describe('COACTUPC account update', () => {
  it('customer 1 fails the FICO edit (seed trap)', () => {
    const { rt } = menu('2');
    rt.sendInput('T1', { aid: 'ENTER', fields: { ACCTSID: '00000000001' } });
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { ACSFNAM: 'Bob' } });
    expect(errmsg(r)).toBe('FICO Score: should be between 300 and 850');
  });
  it('account 18 state AP fails, CA/90210 passes and commits', () => {
    const { rt } = menu('2');
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { ACCTSID: '00000000018' } });
    expect(value(r, 'ACSSTTE')).toBe('AP');
    expect(value(r, 'INFOMSG')).toBe('Update account details presented above.');
    expect(errmsg(rt.sendInput('T1', { aid: 'ENTER', fields: { ACSFNAM: 'Bob' } }))).toBe('State: is not a valid state code');
    const r3 = rt.sendInput('T1', { aid: 'ENTER', fields: { ACSSTTE: 'CA', ACSZIPC: '90210' } });
    expect(errmsg(r3)).toBe('');
    expect(value(r3, 'INFOMSG')).toBe('Changes validated.Press F5 to save');
    expect(value(rt.sendInput('T1', { aid: 'PF5' }), 'INFOMSG')).toBe('Changes committed to database');
  });
  it('blank and invalid account numbers', () => {
    const { rt } = menu('2');
    expect(errmsg(rt.sendInput('T1', { aid: 'ENTER', fields: { ACCTSID: 'X1' } }))).toBe('Account Number if supplied must be a 11 digit Non-Zero Number');
  });
});
