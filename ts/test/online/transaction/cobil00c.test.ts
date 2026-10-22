import { describe, expect, it } from 'vitest';
import { errmsg, field, menuOption, readAccount, readTran, setBalance, trnRuntime, value } from './helpers.js';

function billPay() {
  const t = trnRuntime();
  const first = menuOption(t.rt, '10');
  return { ...t, first };
}

describe('COBIL00C bill payment', () => {
  it('menu option 10 shows the bill-pay screen', () => {
    const { first } = billPay();
    expect(first.programs).toEqual(['COMEN01C', 'COBIL00C']);
    expect(first.screen.map).toBe('COBIL0A');
    expect(first.nextTransid).toBe('CB00');
    expect(first.commarea).toHaveLength(218);
  });

  it('ENTER shows the balance and asks for confirmation; Y pays it in full', () => {
    const { rt, catalog } = billPay();
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { ACTIDIN: '00000000001' } });
    expect(value(r, 'CURBAL')).toBe('+0000000194.00');
    expect(errmsg(r)).toBe('Confirm to make a bill payment...');
    expect(r.screen.cursor).toEqual({ row: field(r, 'CONFIRM').row, col: field(r, 'CONFIRM').col });

    const paid = rt.sendInput('T1', { aid: 'ENTER', fields: { CONFIRM: 'Y' } });
    expect(errmsg(paid)).toBe('Payment successful.  Your Transaction ID is 0000000996722788.');
    expect(field(paid, 'ERRMSG').color).toBe('GREEN');
    expect(value(paid, 'ACTIDIN')).toBe('');
    expect(value(paid, 'CURBAL')).toBe('');
    expect(readAccount(catalog, '00000000001').acctCurrBal.toFixed(2)).toBe('0.00');
    const t = readTran(catalog, '0000000996722788')!;
    expect(t.tranTypeCd).toBe('02');
    expect(t.tranCatCd.toString()).toBe('2');
    expect(t.tranSource).toBe('POS TERM  ');
    expect(t.tranDesc.trimEnd()).toBe('BILL PAYMENT - ONLINE');
    expect(t.tranAmt.toFixed(2)).toBe('194.00');
    expect(t.tranCardNum).toBe('9680294154603697');
    expect(t.tranMerchantId.toString()).toBe('999999999');
    expect(t.tranMerchantName.trimEnd()).toBe('BILL PAYMENT');
    expect(t.tranMerchantCity.trimEnd()).toBe('N/A');
    expect(t.tranMerchantZip.trimEnd()).toBe('N/A');
    expect(t.tranOrigTs).toBe('2026-10-22 14:05:09.000000');
    expect(t.tranProcTs).toBe('2026-10-22 14:05:09.000000');

    const again = rt.sendInput('T1', { aid: 'ENTER', fields: { ACTIDIN: '00000000001' } });
    expect(errmsg(again)).toBe('You have nothing to pay...');
    expect(value(again, 'CURBAL')).toBe('+0000000000.00');
  });

  it('decimal-exact payment of a balance with cents', () => {
    const { rt, catalog } = billPay();
    setBalance(catalog, '00000000002', '12345678.91');
    rt.sendInput('T1', { aid: 'ENTER', fields: { ACTIDIN: '00000000002', CONFIRM: 'y' } });
    expect(readTran(catalog, '0000000996722788')!.tranAmt.toFixed(2)).toBe('12345678.91');
    expect(readAccount(catalog, '00000000002').acctCurrBal.toFixed(2)).toBe('0.00');
  });

  it('balance wider than TRAN-AMT S9(9)V99: the payment is high-order truncated (bug-for-bug)', () => {
    const { rt, catalog } = billPay();
    setBalance(catalog, '00000000003', '9876543210.99');
    rt.sendInput('T1', { aid: 'ENTER', fields: { ACTIDIN: '00000000003', CONFIRM: 'Y' } });
    expect(readTran(catalog, '0000000996722788')!.tranAmt.toFixed(2)).toBe('876543210.99');
    expect(readAccount(catalog, '00000000003').acctCurrBal.toFixed(2)).toBe('9000000000.00');
  });

  it('negative balance -> nothing to pay', () => {
    const { rt, catalog } = billPay();
    setBalance(catalog, '00000000004', '-5.25');
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { ACTIDIN: '00000000004' } });
    expect(errmsg(r)).toBe('You have nothing to pay...');
    expect(value(r, 'CURBAL')).toBe('-0000000005.25');
  });

  it('validation: empty, not found, N clears, invalid confirm', () => {
    const { rt } = billPay();
    expect(errmsg(rt.sendInput('T1', { aid: 'ENTER', fields: { ACTIDIN: '' } }))).toBe('Acct ID can NOT be empty...');
    expect(errmsg(rt.sendInput('T1', { aid: 'ENTER', fields: { ACTIDIN: '99999999999' } }))).toBe('Account ID NOT found...');
    const n = rt.sendInput('T1', { aid: 'ENTER', fields: { ACTIDIN: '00000000001', CONFIRM: 'N' } });
    expect(value(n, 'ACTIDIN')).toBe('');
    expect(errmsg(n)).toBe('');
    expect(errmsg(rt.sendInput('T1', { aid: 'ENTER', fields: { ACTIDIN: '00000000001', CONFIRM: 'Q' } }))).toBe(
      'Invalid value. Valid values are (Y/N)...',
    );
  });

  it('MOVE ACTIDINI (X) TO ACCT-ID (9): a trailing space gets the unsigned zone -> 0', () => {
    const { rt } = billPay();
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { ACTIDIN: '0000000001' } });
    expect(value(r, 'CURBAL')).toBe('+0000000159.00');
  });

  it('PF4 clears, PF3 returns to the menu, other keys are invalid', () => {
    const { rt } = billPay();
    rt.sendInput('T1', { aid: 'ENTER', fields: { ACTIDIN: '00000000001' } });
    expect(value(rt.sendInput('T1', { aid: 'PF4' }), 'CURBAL')).toBe('');
    expect(errmsg(rt.sendInput('T1', { aid: 'PF5' }))).toBe('Invalid key pressed. Please see below...');
    expect(rt.sendInput('T1', { aid: 'PF3' }).programs).toEqual(['COBIL00C', 'COMEN01C']);
  });
});
