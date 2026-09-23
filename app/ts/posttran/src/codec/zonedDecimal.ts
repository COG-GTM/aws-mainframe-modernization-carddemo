import type { Money } from './money.ts';

/**
 * Signed zoned decimal (COBOL DISPLAY `S9(n)V99`) with a trailing sign
 * overpunch, as produced by the CardDemo data files.
 *
 * The final byte carries both the last digit and the sign:
 * `{`/`A`-`I` are +0..+9 and `}`/`J`-`R` are -0..-9. A plain digit in the
 * final position is read as unsigned positive, which is what the ASCII
 * sample data uses for unsigned `9(n)` fields.
 */
const POSITIVE_OVERPUNCH = '{ABCDEFGHI';
const NEGATIVE_OVERPUNCH = '}JKLMNOPQR';

export interface ZonedDecimalFormat {
  /** Total byte length of the field, including the sign-carrying byte. */
  readonly length: number;
  /** Number of implied decimal places (the `V99` part). */
  readonly scale: number;
  /** Whether the field is signed (`S9(n)`) and therefore uses an overpunch. */
  readonly signed: boolean;
}

export function decodeZonedDecimal(field: string, format: ZonedDecimalFormat): Money {
  if (field.length !== format.length) {
    throw new RangeError(`expected ${format.length} bytes, got ${field.length}`);
  }
  if (format.scale !== 2) {
    throw new RangeError(`only scale 2 amounts are supported, got ${format.scale}`);
  }

  const leading = field.slice(0, -1);
  const last = field.slice(-1);
  if (!/^\d*$/.test(leading)) {
    throw new RangeError(`non-numeric zoned decimal: ${JSON.stringify(field)}`);
  }

  let negative = false;
  let lastDigit: string;
  const positiveIndex = POSITIVE_OVERPUNCH.indexOf(last);
  const negativeIndex = NEGATIVE_OVERPUNCH.indexOf(last);
  if (/^\d$/.test(last)) {
    lastDigit = last;
  } else if (format.signed && positiveIndex >= 0) {
    lastDigit = String(positiveIndex);
  } else if (format.signed && negativeIndex >= 0) {
    negative = true;
    lastDigit = String(negativeIndex);
  } else {
    throw new RangeError(`invalid sign overpunch ${JSON.stringify(last)} in ${JSON.stringify(field)}`);
  }

  const digits = BigInt(`${leading}${lastDigit}`);
  return negative ? -digits : digits;
}

export function encodeZonedDecimal(value: Money, format: ZonedDecimalFormat): string {
  if (format.scale !== 2) {
    throw new RangeError(`only scale 2 amounts are supported, got ${format.scale}`);
  }
  const negative = value < 0n;
  if (negative && !format.signed) {
    throw new RangeError('cannot encode a negative amount in an unsigned field');
  }

  const digits = (negative ? -value : value).toString().padStart(format.length, '0');
  if (digits.length > format.length) {
    throw new RangeError(`value ${value} does not fit in ${format.length} bytes`);
  }

  const leading = digits.slice(0, -1);
  const lastDigit = Number(digits.slice(-1));
  if (!format.signed) {
    return digits;
  }
  const overpunch = negative ? NEGATIVE_OVERPUNCH : POSITIVE_OVERPUNCH;
  return `${leading}${overpunch[lastDigit]}`;
}
