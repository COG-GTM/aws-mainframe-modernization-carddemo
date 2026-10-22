import { describe, expect, it } from 'vitest';
import { errmsg, field, menuOption, readTran, trnRuntime, value } from './helpers.js';

const GOOD = {
  TTYPCD: '01',
  TCATCD: '0001',
  TRNSRC: 'POS TERM',
  TDESC: 'Coffee',
  TRNAMT: '-00000012.50',
  TORIGDT: '2026-10-01',
  TPROCDT: '2026-10-02',
  MID: '123456789',
  MNAME: 'Cafe',
  MCITY: 'Seattle',
  MZIP: '98101',
};

function addScreen() {
  const t = trnRuntime();
  const r = menuOption(t.rt, '8');
  return { ...t, first: r };
}

describe('COTRN02C transaction add', () => {
  it('menu option 8 shows the add screen with cursor on ACTIDIN', () => {
    const { first } = addScreen();
    expect(first.programs).toEqual(['COMEN01C', 'COTRN02C']);
    expect(first.screen.map).toBe('COTRN2A');
    expect(first.nextTransid).toBe('CT02');
    expect(first.screen.cursor).toEqual({ row: 6, col: 21 });
  });

  it('valid input without confirmation: looks up the card, normalises the amount, asks to confirm', () => {
    const { rt } = addScreen();
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { ACTIDIN: '00000000001', ...GOOD } });
    expect(errmsg(r)).toBe('Confirm to add this transaction...');
    expect(value(r, 'CARDNIN')).toBe('9680294154603697');
    expect(value(r, 'TRNAMT')).toBe('-00000012.50');
    expect(r.screen.cursor).toEqual({ row: field(r, 'CONFIRM').row, col: field(r, 'CONFIRM').col });
  });

  it("CONFIRM 'Y' writes TRAN-ID = last id + 1 and reports it in green", () => {
    const { rt, catalog } = addScreen();
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { ACTIDIN: '00000000001', ...GOOD, TRNAMT: '+00001234.56', CONFIRM: 'Y' } });
    expect(errmsg(r)).toBe('Transaction added successfully.  Your Tran ID is 0000000996722788.');
    expect(field(r, 'ERRMSG').color).toBe('GREEN');
    expect(value(r, 'ACTIDIN')).toBe('');
    const t = readTran(catalog, '0000000996722788')!;
    expect(t.tranTypeCd).toBe('01');
    expect(t.tranCatCd.toString()).toBe('1');
    expect(t.tranAmt.toFixed(2)).toBe('1234.56');
    expect(t.tranCardNum).toBe('9680294154603697');
    expect(t.tranMerchantId.toString()).toBe('123456789');
    expect(t.tranMerchantName.trimEnd()).toBe('Cafe');
    expect(t.tranOrigTs).toBe('2026-10-01'.padEnd(26));
    expect(t.tranProcTs).toBe('2026-10-02'.padEnd(26));
    const again = rt.sendInput('T1', { aid: 'ENTER', fields: { ACTIDIN: '', CARDNIN: '9680294154603697', ...GOOD, CONFIRM: 'y' } });
    expect(errmsg(again)).toBe('Transaction added successfully.  Your Tran ID is 0000000996722789.');
    expect(readTran(catalog, '0000000996722789')!.tranAmt.toFixed(2)).toBe('-12.50');
  });

  it('card number path fills the account id', () => {
    const { rt } = addScreen();
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { CARDNIN: '0500024453765740', ...GOOD } });
    expect(value(r, 'ACTIDIN')).toBe('00000000050');
    expect(errmsg(r)).toBe('Confirm to add this transaction...');
  });

  it('key field validation', () => {
    const { rt } = addScreen();
    const send = (fields: Record<string, string>) => errmsg(rt.sendInput('T1', { aid: 'ENTER', fields: { ...GOOD, ...fields } }));
    expect(send({})).toBe('Account or Card Number must be entered...');
    // ACTIDIN has no JUSTIFY: a short id is space padded and fails IS NUMERIC.
    expect(send({ ACTIDIN: '1' })).toBe('Account ID must be Numeric...');
    expect(send({ ACTIDIN: '99999999999' })).toBe('Account ID NOT found...');
    expect(send({ ACTIDIN: '', CARDNIN: '12X' })).toBe('Card Number must be Numeric...');
    expect(send({ ACTIDIN: '', CARDNIN: '1111111111111111' })).toBe('Card Number NOT found...');
  });

  it('data field validation, in COBOL order', () => {
    const { rt } = addScreen();
    const send = (fields: Record<string, string>) => errmsg(rt.sendInput('T1', { aid: 'ENTER', fields: { ACTIDIN: '00000000001', ...GOOD, ...fields } }));
    expect(send({ TTYPCD: '', TDESC: '' })).toBe('Type CD can NOT be empty...');
    expect(send({ MZIP: '' })).toBe('Merchant Zip can NOT be empty...');
    expect(send({ TTYPCD: '1' })).toBe('Type CD must be Numeric...');
    expect(send({ TCATCD: '1' })).toBe('Category CD must be Numeric...');
    expect(send({ TRNAMT: '12.50' })).toBe('Amount should be in format -99999999.99');
    expect(send({ TRNAMT: '+0000001250' })).toBe('Amount should be in format -99999999.99');
    expect(send({ TORIGDT: '10/01/2026' })).toBe('Orig Date should be in format YYYY-MM-DD');
    expect(send({ TPROCDT: '2026-1-02' })).toBe('Proc Date should be in format YYYY-MM-DD');
    expect(send({ TORIGDT: '2026-02-30' })).toBe('Orig Date - Not a valid date...');
    expect(send({ TPROCDT: '2026-13-01' })).toBe('Proc Date - Not a valid date...');
    expect(send({ MID: '12345' })).toBe('Merchant ID must be Numeric...');
    expect(send({ CONFIRM: 'X' })).toBe('Invalid value. Valid values are (Y/N)...');
    expect(send({ CONFIRM: 'N' })).toBe('Confirm to add this transaction...');
  });

  it('PF5 copies the last transaction (its blank proc date then fails validation)', () => {
    const { rt } = addScreen();
    const r = rt.sendInput('T1', { aid: 'PF5', fields: { ACTIDIN: '00000000001' } });
    expect(value(r, 'TDESC')).toBe('Purchase at Kilback LLC');
    expect(value(r, 'TRNAMT')).toBe('+00000603.22');
    expect(value(r, 'TORIGDT')).toBe('2022-06-10');
    expect(value(r, 'MID')).toBe('800000000');
    expect(errmsg(r)).toBe('Proc Date can NOT be empty...');
  });

  it('PF4 clears, PF3 returns to the menu, other keys are invalid', () => {
    const { rt } = addScreen();
    rt.sendInput('T1', { aid: 'ENTER', fields: { ACTIDIN: '00000000001', ...GOOD } });
    const cleared = rt.sendInput('T1', { aid: 'PF4' });
    expect(value(cleared, 'ACTIDIN')).toBe('');
    expect(value(cleared, 'TDESC')).toBe('');
    expect(errmsg(rt.sendInput('T1', { aid: 'PF7' }))).toBe('Invalid key pressed. Please see below...');
    expect(rt.sendInput('T1', { aid: 'PF3' }).programs).toEqual(['COTRN02C', 'COMEN01C']);
  });
});
