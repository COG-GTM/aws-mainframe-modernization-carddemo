import { Decimal as DecimalJs } from 'decimal.js';

/**
 * Project-wide decimal type. A cloned constructor is used so that the precision
 * and rounding settings below never leak into (or get changed by) other users of
 * decimal.js. 40 significant digits covers every PIC in CardDemo (max 18 digits)
 * plus intermediate COMPUTE results.
 */
export const Decimal = DecimalJs.clone({
  precision: 40,
  rounding: DecimalJs.ROUND_HALF_UP,
  toExpNeg: -40,
  toExpPos: 40,
});
export type Decimal = DecimalJs;

/**
 * Anything accepted where a numeric field value is expected. Native `number`s
 * are only accepted when they are safe integers (counters, codes, ids), so
 * binary floating point can never leak into a monetary field.
 */
export type NumericInput = DecimalJs | string | number | bigint;

export function toDecimal(value: NumericInput): Decimal {
  if (typeof value === 'bigint') return new Decimal(value.toString());
  if (typeof value === 'number' && !Number.isSafeInteger(value)) {
    throw new TypeError(`non-integer JS number ${value} used as a numeric field value; pass a Decimal or string`);
  }
  return new Decimal(value);
}

/**
 * Applies COBOL MOVE semantics for a numeric receiving item: excess decimal
 * places are truncated (no rounding) and high-order integer digits beyond
 * `digits - scale` are dropped. Sign is dropped when the receiver is unsigned.
 * Returns the absolute value as a zero-padded digit string of length `digits`
 * plus the resulting sign.
 */
export function toScaledDigits(
  value: NumericInput,
  digits: number,
  scale: number,
  signed: boolean,
): { digitString: string; negative: boolean } {
  const d = toDecimal(value);
  const scaled = d.abs().times(new Decimal(10).pow(scale)).floor();
  let digitString = scaled.toFixed(0);
  if (digitString.length > digits) digitString = digitString.slice(digitString.length - digits);
  digitString = digitString.padStart(digits, '0');
  const isZero = /^0*$/.test(digitString);
  return { digitString, negative: signed && d.isNegative() && !isZero };
}

/** Builds a Decimal from an unsigned digit string, an implied scale and a sign. */
export function fromScaledDigits(digitString: string, scale: number, negative: boolean): Decimal {
  let v = new Decimal(digitString === '' ? '0' : digitString);
  if (scale > 0) v = v.dividedBy(new Decimal(10).pow(scale));
  return negative && !v.isZero() ? v.negated() : v;
}
