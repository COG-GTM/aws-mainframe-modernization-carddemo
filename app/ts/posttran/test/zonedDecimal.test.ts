import assert from 'node:assert/strict';
import { describe, it } from 'node:test';

import { decodeZonedDecimal, encodeZonedDecimal } from '../src/codec/zonedDecimal.ts';
import { fromDecimalString, toDecimalString } from '../src/codec/money.ts';

const AMOUNT = { length: 11, scale: 2, signed: true } as const;

void describe('zoned decimal codec', () => {
  void it('decodes a positive overpunch (sample data: 0000005047G = 504.77)', () => {
    assert.equal(toDecimalString(decodeZonedDecimal('0000005047G', AMOUNT)), '504.77');
  });

  void it('decodes a negative overpunch (sample data: 0000009190} = -919.00)', () => {
    assert.equal(toDecimalString(decodeZonedDecimal('0000009190}', AMOUNT)), '-919.00');
  });

  void it('decodes every positive and negative overpunch digit', () => {
    const positives = '{ABCDEFGHI';
    const negatives = '}JKLMNOPQR';
    for (let digit = 0; digit < 10; digit += 1) {
      assert.equal(decodeZonedDecimal(`0000000000${positives[digit]}`, AMOUNT), BigInt(digit));
      assert.equal(decodeZonedDecimal(`0000000000${negatives[digit]}`, AMOUNT), BigInt(-digit));
    }
  });

  void it('treats a trailing plain digit as unsigned positive', () => {
    assert.equal(decodeZonedDecimal('00000000123', AMOUNT), 123n);
  });

  void it('round-trips through encode', () => {
    for (const text of ['0.00', '504.77', '-919.00', '-0.01', '9999999.99']) {
      const value = fromDecimalString(text);
      assert.equal(decodeZonedDecimal(encodeZonedDecimal(value, AMOUNT), AMOUNT), value);
    }
  });

  void it('encodes negative zero as the } overpunch', () => {
    assert.equal(encodeZonedDecimal(0n, AMOUNT), '0000000000{');
    assert.equal(encodeZonedDecimal(-1n, AMOUNT), '0000000000J');
  });

  void it('rejects malformed fields', () => {
    assert.throws(() => decodeZonedDecimal('123', AMOUNT), RangeError);
    assert.throws(() => decodeZonedDecimal('0000000 000{', { ...AMOUNT, length: 12 }), RangeError);
    assert.throws(() => decodeZonedDecimal('0000000000?', AMOUNT), RangeError);
  });

  void it('rejects values that do not fit the field', () => {
    assert.throws(() => encodeZonedDecimal(10n ** 12n, AMOUNT), RangeError);
  });
});

void describe('money', () => {
  void it('parses and renders scale-2 decimals exactly', () => {
    assert.equal(fromDecimalString('-1234.56'), -123456n);
    assert.equal(fromDecimalString('504.7'), 50470n);
    assert.equal(toDecimalString(-5n), '-0.05');
  });

  void it('adds without floating point error', () => {
    const total = [0.1, 0.2].map((n) => fromDecimalString(n.toFixed(2))).reduce((a, b) => a + b, 0n);
    assert.equal(toDecimalString(total), '0.30');
  });

  void it('rejects values that are not scale-2 decimals', () => {
    assert.throws(() => fromDecimalString('1.234'), RangeError);
    assert.throws(() => fromDecimalString('abc'), RangeError);
  });
});
