import { describe, expect, it } from 'vitest';

import { Decimal } from '../src/decimal.ts';
import { ZonedDecimalError, decodeZoned, encodeZoned } from '../src/zoned.ts';

describe('zoned decimal decode', () => {
  it('decodes positive overpunches', () => {
    // The overpunch byte carries the last digit, so '150{' is 1500 unscaled.
    expect(decodeZoned('150{', 2).toFixed(2)).toBe('15.00');
    expect(decodeZoned('015A', 2).toFixed(2)).toBe('1.51');
    expect(decodeZoned('00150{', 2).toFixed(2)).toBe('15.00');
    expect(decodeZoned('00000001940{', 2).toFixed(2)).toBe('194.00');
  });

  it('decodes negative overpunches', () => {
    expect(decodeZoned('150}', 2).toFixed(2)).toBe('-15.00');
    expect(decodeZoned('015J', 2).toFixed(2)).toBe('-1.51');
    expect(decodeZoned('000000012R', 2).toFixed(2)).toBe('-1.29');
  });

  it('decodes zero with either sign', () => {
    expect(decodeZoned('0000000000{', 2).isZero()).toBe(true);
    expect(decodeZoned('0000000000}', 2).isZero()).toBe(true);
  });

  it('decodes unsigned display fields', () => {
    expect(decodeZoned('00000000001', 0).toString()).toBe('1');
    expect(decodeZoned('0005', 0).toString()).toBe('5');
  });

  it('rejects invalid images', () => {
    expect(() => decodeZoned('12*4', 2)).toThrow(ZonedDecimalError);
    expect(() => decodeZoned('', 2)).toThrow(ZonedDecimalError);
  });
});

describe('zoned decimal encode', () => {
  it('round-trips positive, negative and zero values', () => {
    const cases = ['1.50', '-1.50', '0.00', '-0.01', '999999999.99', '-999999999.99'];
    for (const value of cases) {
      const encoded = encodeZoned(new Decimal(value), 11, 2, true);
      expect(encoded).toHaveLength(11);
      expect(decodeZoned(encoded, 2).toFixed(2)).toBe(new Decimal(value).toFixed(2));
    }
  });

  it('matches the fixture images byte for byte', () => {
    expect(encodeZoned(new Decimal('15.00'), 6, 2, true)).toBe('00150{');
    expect(encodeZoned(new Decimal('194.00'), 12, 2, true)).toBe('00000001940{');
    expect(encodeZoned(new Decimal('-194.00'), 12, 2, true)).toBe('00000001940}');
    expect(encodeZoned(new Decimal('0'), 11, 2, true)).toBe('0000000000{');
  });

  it('writes unsigned fields as plain digits', () => {
    expect(encodeZoned(new Decimal('11'), 11, 0, false)).toBe('00000000011');
  });

  it('truncates excess low-order digits toward zero, as a COBOL store does', () => {
    expect(encodeZoned(new Decimal('1.999'), 6, 2, true)).toBe('00019I');
    expect(encodeZoned(new Decimal('-1.999'), 6, 2, true)).toBe('00019R');
  });

  it('truncates excess high-order digits silently (defect D12, no ON SIZE ERROR)', () => {
    // MOVE of 1 000 000 000.00 into TRAN-AMT PIC S9(09)V99 loses the leading 1.
    expect(decodeZoned(encodeZoned(new Decimal('1000000000.00'), 11, 2, true), 2).toString()).toBe(
      '0',
    );
  });
});
