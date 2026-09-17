/**
 * Money arithmetic with the copybook scale.
 *
 * Amounts are held in COBOL fields such as `PIC S9(09)V99`, so every value has
 * two decimal places. Arithmetic runs on integer cents and, as in COBOL
 * statements without `ROUNDED`, results are truncated toward zero at that
 * scale rather than rounded.
 */

export const MONEY_SCALE = 2;

const FACTOR = 10 ** MONEY_SCALE;

/** Guards against binary floating point error before truncating. */
const EPSILON = 1e-6;

export function toCents(value: number): number {
  const scaled = value * FACTOR;
  return Math.trunc(scaled + (scaled < 0 ? -EPSILON : EPSILON));
}

export function fromCents(cents: number): number {
  return cents / FACTOR;
}

/** Truncates to the copybook scale, as an unrounded `MOVE` to `V99` does. */
export function truncateMoney(value: number): number {
  return fromCents(toCents(value));
}

export function addMoney(left: number, right: number): number {
  return fromCents(toCents(left) + toCents(right));
}

/** `COMPUTE WS-TEMP-BAL = CREDIT - DEBIT + AMOUNT` in a single expression. */
export function cycleBalance(
  credit: number,
  debit: number,
  amount: number,
): number {
  return fromCents(toCents(credit) - toCents(debit) + toCents(amount));
}
