import { Decimal } from './decimal.ts';

/**
 * Stores a value into a numeric `PIC S9(int)V9(scale)` working-storage item.
 *
 * A COBOL `COMPUTE`/`ADD` without `ON SIZE ERROR` — and CBACT04C has none —
 * truncates the result to the receiving item: low-order digits are dropped
 * toward zero and high-order digits that do not fit are silently lost. Keeping
 * unbounded precision instead would post balances the mainframe never produces.
 */
export function storePicture(value: Decimal, intDigits: number, scale: number): Decimal {
  const truncated = value.toDecimalPlaces(scale, Decimal.ROUND_DOWN);
  const modulus = new Decimal(10).pow(intDigits);
  const integral = truncated.abs().floor();
  const kept = integral.mod(modulus);
  const magnitude = kept.plus(truncated.abs().minus(integral));
  return truncated.isNegative() ? magnitude.neg() : magnitude;
}

/** `WS-MONTHLY-INT` and `WS-TOTAL-INT` are both `PIC S9(09)V99` (cbl:168-169). */
export function storeInterest(value: Decimal): Decimal {
  return storePicture(value, 9, 2);
}
