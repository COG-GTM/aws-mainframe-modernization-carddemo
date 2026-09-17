/**
 * Fixed-point money arithmetic at the copybook scale (`S9(n)V99`).
 *
 * Amounts are carried as integer cents (`bigint`) so the accumulation COBOL
 * performs in packed working storage cannot drift through binary floating
 * point, and so the truncation the `COMPUTE` statements imply is exact.
 */

/** Digits after the implied decimal point in the money copybook fields. */
export const MONEY_SCALE = 2;

const MONEY_FACTOR = 10 ** MONEY_SCALE;

/**
 * Divisor turning `balanceCents * rateBasis` into cents of monthly interest:
 * the COBOL divides by 1200 at the copybook scale of both operands
 * (`100 * 1200 / 100`).
 */
const MONTHLY_DIVISOR = 120_000n;

/** Value of a `S9(n)V99` field as integer cents. */
export function toCents(amount: number): bigint {
  const magnitude = Math.round(Math.abs(amount) * MONEY_FACTOR);
  return BigInt(amount < 0 ? -magnitude : magnitude);
}

/** Integer cents back to the number a `S9(n)V99` field decodes to. */
export function fromCents(cents: bigint): number {
  return Number(cents) / MONEY_FACTOR;
}

/**
 * `COMPUTE WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200`.
 *
 * The statement has no `ROUNDED` phrase, so the result is truncated towards
 * zero when it is stored into `WS-MONTHLY-INT PIC S9(09)V99`. The product is
 * formed exactly in `bigint` before the single truncating division, which is
 * what the COBOL intermediate result does.
 *
 * @param balanceCents `TRAN-CAT-BAL` as cents
 * @param rateBasis `DIS-INT-RATE` (an annual percentage) at scale 2
 */
export function monthlyInterestCents(balanceCents: bigint, rateBasis: bigint): bigint {
  return (balanceCents * rateBasis) / MONTHLY_DIVISOR;
}
