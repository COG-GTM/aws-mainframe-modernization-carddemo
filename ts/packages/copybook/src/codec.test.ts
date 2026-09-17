import { describe, expect, it } from "vitest";

import { decodeRecord, encodeRecord } from "./codec.js";
import { defineLayout } from "./layout.js";
import { decodePacked, decodeZoned, encodePacked, encodeZoned } from "./zoned.js";

describe("zoned decimal", () => {
  const money = { digits: 12, scale: 2, signed: true };

  it("decodes positive overpunch digits", () => {
    expect(decodeZoned("00000001940{", money)).toBe(194);
    expect(decodeZoned("0000000194A", { digits: 11, scale: 2, signed: true })).toBe(19.41);
  });

  it("decodes negative overpunch digits", () => {
    expect(decodeZoned("00000001940}", money)).toBe(-194);
    expect(decodeZoned("00000001941J", money)).toBe(-194.11);
  });

  it("round-trips signed values", () => {
    for (const value of [0, 194, -194, 1234567.89, -0.01]) {
      expect(decodeZoned(encodeZoned(value, money), money)).toBe(value);
    }
  });

  it("decodes unsigned display digits", () => {
    expect(decodeZoned("00000000123", { digits: 11, scale: 0, signed: false })).toBe(123);
  });
});

describe("packed decimal", () => {
  const options = { digits: 5, scale: 2, signed: true };

  it("round-trips through COMP-3 nibbles", () => {
    for (const value of [0, 12.34, -12.34, 999.99]) {
      expect(decodePacked(encodePacked(value, options), options)).toBe(value);
    }
  });

  it("uses 0x0d as the negative sign nibble", () => {
    expect(encodePacked(-1.23, options).at(-1)! & 0x0f).toBe(0x0d);
    expect(encodePacked(1.23, options).at(-1)! & 0x0f).toBe(0x0c);
  });
});

describe("record codec", () => {
  const layout = defineLayout("SAMPLE", [
    { name: "id", pic: "9(05)" },
    { name: "name", pic: "X(10)" },
    { name: "balance", pic: "S9(05)V99" },
    { name: "buckets", pic: "S9(03)V99", occurs: 3 },
    { pic: "X(04)" },
  ]);

  it("computes the record length from the field widths", () => {
    expect(layout.recordLength).toBe(5 + 10 + 7 + 15 + 4);
  });

  it("decodes and re-encodes a record unchanged", () => {
    const line = "00042ACME      000123D0010{0020{0030{FILL";
    const record = decodeRecord(layout, line);
    expect(record).toEqual({
      id: 42,
      name: "ACME",
      balance: 12.34,
      buckets: [1, 2, 3],
    });
    expect(encodeRecord(layout, record).slice(0, layout.recordLength - 4)).toBe(
      line.slice(0, layout.recordLength - 4),
    );
  });

  it("pads short records that drop trailing filler", () => {
    const record = decodeRecord(layout, "00042ACME");
    expect(record.id).toBe(42);
    expect(record.balance).toBe(0);
  });
});
