import { Decimal } from 'decimal.js';

/**
 * COBOL evaluates `(TRAN-CAT-BAL * DIS-INT-RATE) / 1200` with intermediate
 * precision wide enough to hold the exact product before the final truncating
 * store. 50 significant digits comfortably covers the widest fields in this job
 * (`S9(10)V99` balances against `S9(04)V99` rates).
 */
Decimal.set({ precision: 50, toExpPos: 40, toExpNeg: -40 });

export { Decimal };
export const ZERO = new Decimal(0);
