import { describe, expect, it } from 'vitest';

import { Decimal } from '../src/decimal.ts';
import { storeInterest, storePicture } from '../src/picture.ts';

describe('storePicture', () => {
  it('truncates low-order digits toward zero', () => {
    expect(storePicture(new Decimal('1.999'), 9, 2).toFixed(2)).toBe('1.99');
    expect(storePicture(new Decimal('-1.999'), 9, 2).toFixed(2)).toBe('-1.99');
  });

  it('drops high-order digits that do not fit the receiving item', () => {
    // COMPUTE without ON SIZE ERROR keeps only the low-order 9 integer digits.
    expect(storeInterest(new Decimal('83333249999.91')).toFixed(2)).toBe('333249999.91');
    expect(storeInterest(new Decimal('-83333249999.91')).toFixed(2)).toBe('-333249999.91');
  });

  it('leaves values that fit untouched', () => {
    expect(storeInterest(new Decimal('999999999.99')).toFixed(2)).toBe('999999999.99');
    expect(storeInterest(new Decimal('0')).toFixed(2)).toBe('0.00');
  });
});
