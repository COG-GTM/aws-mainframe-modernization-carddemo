/**
 * Exact decimal money with scale 2, stored as bigint minor units (cents).
 *
 * COBOL amounts in CardDemo are zoned decimal `S9(n)V99`; all arithmetic in
 * CBTRN02C is plain ADD/COMPUTE on 2-decimal operands with no ROUND clause,
 * so cent-accurate integer arithmetic reproduces it exactly.
 */
export type Money = bigint;

export const ZERO: Money = 0n;

export function add(a: Money, b: Money): Money {
  return a + b;
}

export function subtract(a: Money, b: Money): Money {
  return a - b;
}

export function isNegative(a: Money): boolean {
  return a < 0n;
}

/** Parses a decimal string such as `-1234.56` or `504.7` into minor units. */
export function fromDecimalString(value: string): Money {
  const match = /^([+-]?)(\d+)(?:\.(\d{0,2}))?$/.exec(value.trim());
  if (match === null) {
    throw new RangeError(`not a scale-2 decimal amount: ${JSON.stringify(value)}`);
  }
  const sign = match[1] ?? '';
  const whole = match[2] ?? '0';
  const fraction = match[3] ?? '';
  const cents = BigInt(whole) * 100n + BigInt(fraction.padEnd(2, '0'));
  return sign === '-' ? -cents : cents;
}

/** Renders minor units as a signed decimal string with exactly 2 decimals. */
export function toDecimalString(value: Money): string {
  const negative = value < 0n;
  const absolute = negative ? -value : value;
  const whole = absolute / 100n;
  const cents = absolute % 100n;
  return `${negative ? '-' : ''}${whole}.${cents.toString().padStart(2, '0')}`;
}
