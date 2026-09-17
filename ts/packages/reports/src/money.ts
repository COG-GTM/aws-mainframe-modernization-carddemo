/**
 * Money arithmetic at the copybook scale: `S9(n)V99` fields hold exactly two
 * decimal places, so running totals are accumulated in cents and rounded back
 * the way COBOL keeps intermediate results at the declared scale.
 */

const SCALE = 100;

export function toCents(value: number): number {
  return Math.round(value * SCALE);
}

export function fromCents(cents: number): number {
  return cents / SCALE;
}

export function addMoney(left: number, right: number): number {
  return fromCents(toCents(left) + toCents(right));
}
