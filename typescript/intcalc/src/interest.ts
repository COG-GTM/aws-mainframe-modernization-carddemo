import { Decimal } from './decimal.ts';

/**
 * `1300-COMPUTE-INTEREST` (`CBACT04C.cbl:462-467`):
 *
 *     COMPUTE WS-MONTHLY-INT = ( TRAN-CAT-BAL * DIS-INT-RATE) / 1200
 *
 * `rateAnnualPercent` is an annual percentage, so `/1200` converts percent per
 * year to a fraction per month. There is no `ROUNDED` phrase, so the store into
 * `WS-MONTHLY-INT PIC S9(09)V99` truncates toward zero at two decimals — for
 * negative balances as well. Truncation happens per category, before
 * accumulation, never on the account total.
 */
export function computeMonthlyInterest(balance: Decimal, rateAnnualPercent: Decimal): Decimal {
  return balance.times(rateAnnualPercent).div(1200).toDecimalPlaces(2, Decimal.ROUND_DOWN);
}
