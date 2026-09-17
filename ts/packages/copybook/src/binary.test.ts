import { describe, expect, it } from "vitest";

import { binaryLength, decodeBinary, encodeBinary } from "./binary.js";

const unsigned = { digits: 9, scale: 0, signed: false };

describe("binaryLength", () => {
  it("follows the COBOL digit to halfword mapping", () => {
    expect(binaryLength(4)).toBe(2);
    expect(binaryLength(9)).toBe(4);
    expect(binaryLength(18)).toBe(8);
    expect(() => binaryLength(19)).toThrow();
  });
});

describe("binary conversions", () => {
  it("stores values big endian", () => {
    expect([...encodeBinary(1, unsigned)]).toEqual([0, 0, 0, 1]);
    expect([...encodeBinary(258, unsigned)]).toEqual([0, 0, 1, 2]);
  });

  it("round-trips signed and scaled values", () => {
    const options = { digits: 12, scale: 2, signed: true };
    for (const value of [0, 1234.56, -1234.56, -0.01, 99999999.99]) {
      expect(decodeBinary(encodeBinary(value, options), options)).toBeCloseTo(value, 2);
    }
  });

  it("rejects values the picture cannot hold", () => {
    expect(() => encodeBinary(-1, unsigned)).toThrow();
    expect(() => encodeBinary(1_000_000_000, unsigned)).toThrow();
  });
});
