import { describe, expect, it } from 'vitest';
import { displayIoStatus, storeNumeric, Sysout } from './support.js';

describe('9910-DISPLAY-IO-STATUS', () => {
  const show = (s: string) => {
    const out = new Sysout();
    displayIoStatus(out, s);
    return out.lines[0];
  };
  it('numeric status', () => expect(show('23')).toBe('FILE STATUS IS: NNNN0023'));
  it("'9x' status shows the EBCDIC code point of STAT2", () => expect(show('93')).toBe('FILE STATUS IS: NNNN9243'));
  it('non-numeric status', () => expect(show('0A')).toBe('FILE STATUS IS: NNNN0193'));
});

describe('storeNumeric (no ROUNDED, no ON SIZE ERROR)', () => {
  it('truncates excess decimals toward zero', () => {
    expect(storeNumeric('15.432', 11, 2).toFixed(2)).toBe('15.43');
    expect(storeNumeric('-10.41687', 11, 2).toFixed(2)).toBe('-10.41');
  });
  it('drops high-order digits', () => expect(storeNumeric('8333324999.91', 11, 2).toFixed(2)).toBe('333324999.91'));
  it('unsigned receiver drops the sign', () => expect(storeNumeric('-5.25', 5, 2, false).toFixed(2)).toBe('5.25'));
});
