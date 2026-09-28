import { describe, expect, it } from 'vitest';

import { Decimal } from '../src/decimal.ts';
import { computeMonthlyInterest } from '../src/interest.ts';

const monthly = (balance: string, rate: string): string =>
  computeMonthlyInterest(new Decimal(balance), new Decimal(rate)).toFixed(2);

describe('computeMonthlyInterest', () => {
  it('matches the worked example from the spec', () => {
    expect(monthly('1000.00', '18.00')).toBe('15.00');
  });

  it('truncates toward zero rather than rounding half up', () => {
    // 100.00 * 1.50 / 1200 = 0.125 -> 0.12 (DOWN), not 0.13 (HALF_UP)
    expect(monthly('100.00', '1.50')).toBe('0.12');
    // 1234.56 * 19.99 / 1200 = 20.5657... -> 20.56, not 20.57
    expect(monthly('1234.56', '19.99')).toBe('20.56');
    // 999.99 * 7.77 / 1200 = 6.47493... -> 6.47
    expect(monthly('999.99', '7.77')).toBe('6.47');
  });

  it('truncates negative balances toward zero as well', () => {
    expect(monthly('-100.00', '1.50')).toBe('-0.12');
    expect(monthly('-1234.56', '19.99')).toBe('-20.56');
  });

  it('handles negative rates', () => {
    expect(monthly('1000.00', '-18.00')).toBe('-15.00');
  });

  it('returns zero for a zero balance', () => {
    expect(monthly('0.00', '24.00')).toBe('0.00');
  });

  it('is exact at the widest field values', () => {
    expect(monthly('9999999999.99', '9999.99')).toBe('83333249999.91');
  });

  it('accumulates the sum of truncated amounts, not the truncation of the sum', () => {
    const balances = ['100.00', '100.00', '100.00'];
    const rate = new Decimal('1.50');
    const sumOfTruncated = balances.reduce(
      (total, balance) => total.plus(computeMonthlyInterest(new Decimal(balance), rate)),
      new Decimal(0),
    );
    const truncationOfSum = new Decimal('300.00')
      .times(rate)
      .div(1200)
      .toDecimalPlaces(2, Decimal.ROUND_DOWN);
    expect(sumOfTruncated.toFixed(2)).toBe('0.36');
    expect(truncationOfSum.toFixed(2)).toBe('0.37');
  });
});
