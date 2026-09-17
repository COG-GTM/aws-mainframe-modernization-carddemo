import { describe, expect, it } from "vitest";

import { fromCents, monthlyInterestCents, toCents } from "./money.js";

describe("monthlyInterestCents", () => {
  it("computes balance * rate / 1200 at the copybook scale", () => {
    expect(fromCents(monthlyInterestCents(toCents(1200), toCents(15)))).toBe(15);
    expect(fromCents(monthlyInterestCents(toCents(194), toCents(15)))).toBe(2.42);
  });

  it("truncates towards zero, as the COMPUTE has no ROUNDED phrase", () => {
    // 6.00 at 15.00% accrues exactly 0.075 a month; ROUNDED would store 0.08.
    expect(fromCents(monthlyInterestCents(toCents(6), toCents(15)))).toBe(0.07);
    expect(fromCents(monthlyInterestCents(toCents(-6), toCents(15)))).toBe(-0.07);
    // 0.79 at 15.00% accrues 0.0098750, below the smallest storable amount.
    expect(fromCents(monthlyInterestCents(toCents(0.79), toCents(15)))).toBe(0);
  });

  it("keeps full precision for balances beyond 2^53 cents-times-rate", () => {
    expect(fromCents(monthlyInterestCents(toCents(999999999.99), toCents(9999.99)))).toBe(
      8333324999.91,
    );
  });
});

describe("toCents", () => {
  it("rounds half away from zero on both signs", () => {
    expect(toCents(0.005)).toBe(1n);
    expect(toCents(-0.005)).toBe(-1n);
    expect(toCents(1234.56)).toBe(123456n);
  });
});
