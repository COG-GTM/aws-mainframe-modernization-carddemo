import { Decimal } from './decimal.ts';

/**
 * Zoned-decimal (USAGE DISPLAY) codec.
 *
 * Signed numerics carry their sign as an overpunch in the last byte:
 *   positive digits 0-9 -> { A B C D E F G H I
 *   negative digits 0-9 -> } J K L M N O P Q R
 * Unsigned numerics are plain digits.
 *
 * None of the copybooks used by CBACT04C declare COMP-3, so packed decimal is
 * deliberately not supported here.
 */

const POSITIVE_OVERPUNCH = '{ABCDEFGHI';
const NEGATIVE_OVERPUNCH = '}JKLMNOPQR';

export class ZonedDecimalError extends Error {}

/** Decodes a zoned-decimal field into a Decimal. `scale` is the number of implied decimals. */
export function decodeZoned(raw: string, scale: number): Decimal {
  if (raw.length === 0) {
    throw new ZonedDecimalError('empty zoned-decimal field');
  }
  const body = raw.slice(0, -1);
  const last = raw.slice(-1);

  let negative = false;
  let lastDigit: string;

  const positiveIndex = POSITIVE_OVERPUNCH.indexOf(last);
  const negativeIndex = NEGATIVE_OVERPUNCH.indexOf(last);
  if (last >= '0' && last <= '9') {
    lastDigit = last;
  } else if (positiveIndex >= 0) {
    lastDigit = String(positiveIndex);
  } else if (negativeIndex >= 0) {
    negative = true;
    lastDigit = String(negativeIndex);
  } else {
    throw new ZonedDecimalError(`invalid overpunch character '${last}' in field '${raw}'`);
  }

  const digits = `${body.replace(/ /g, '0')}${lastDigit}`;
  if (!/^\d*$/.test(digits)) {
    throw new ZonedDecimalError(`non-numeric zoned-decimal field '${raw}'`);
  }

  const unscaled = new Decimal(digits === '' ? '0' : digits);
  const value = scale === 0 ? unscaled : unscaled.div(new Decimal(10).pow(scale));
  return negative ? value.neg() : value;
}

/**
 * Encodes a Decimal into a zoned-decimal field of `length` digit positions with
 * `scale` implied decimals.
 *
 * Reproduces a COBOL `MOVE` into a smaller receiving item: excess high-order
 * digits are truncated silently (there is no ON SIZE ERROR anywhere in
 * CBACT04C) and excess low-order digits are truncated toward zero.
 */
export function encodeZoned(value: Decimal, length: number, scale: number, signed: boolean): string {
  const truncated = value.toDecimalPlaces(scale, Decimal.ROUND_DOWN);
  const unscaled = truncated.times(new Decimal(10).pow(scale)).toFixed(0);
  const negative = truncated.isNegative() && !truncated.isZero();
  const digits = unscaled.replace('-', '').padStart(length, '0').slice(-length);

  if (!signed) {
    return digits;
  }
  const head = digits.slice(0, -1);
  const lastDigit = Number(digits.slice(-1));
  const overpunch = negative ? NEGATIVE_OVERPUNCH[lastDigit] : POSITIVE_OVERPUNCH[lastDigit];
  if (overpunch === undefined) {
    throw new ZonedDecimalError(`cannot encode value ${value.toString()}`);
  }
  return `${head}${overpunch}`;
}
