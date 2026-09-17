import { describe, expect, it } from "vitest";

import { alphanumeric, delimitedBy, displayDigits, formatEdited } from "./picture.js";

describe("numeric edited pictures", () => {
  it("suppresses leading zeroes and prints the sign only when negative", () => {
    expect(formatEdited("-ZZZ,ZZZ,ZZZ.ZZ", 183.88)).toBe("         183.88");
    expect(formatEdited("-ZZZ,ZZZ,ZZZ.ZZ", -47.88)).toBe("-         47.88");
    expect(formatEdited("-ZZZ,ZZZ,ZZZ.ZZ", 1234567.89)).toBe("   1,234,567.89");
    // Every digit position is suppressible, so a zero blanks the whole item.
    expect(formatEdited("-ZZZ,ZZZ,ZZZ.ZZ", 0)).toBe(" ".repeat(15));
  });

  it("always prints a sign for +ZZZ,ZZZ,ZZZ.ZZ", () => {
    expect(formatEdited("+ZZZ,ZZZ,ZZZ.ZZ", 5349.85)).toBe("+      5,349.85");
    expect(formatEdited("+ZZZ,ZZZ,ZZZ.ZZ", -5349.85)).toBe("-      5,349.85");
  });

  it("prints a trailing sign for Z(9).99- and 9(9).99-", () => {
    expect(formatEdited("Z(9).99-", 183.88)).toBe("      183.88 ");
    expect(formatEdited("Z(9).99-", -183.88)).toBe("      183.88-");
    expect(formatEdited("9(9).99-", 492)).toBe("000000492.00 ");
    expect(formatEdited("9(9).99-", -492)).toBe("000000492.00-");
  });

  it("truncates towards zero like a COBOL MOVE to a two decimal field", () => {
    expect(formatEdited("Z(9).99-", 1.005)).toBe("        1.00 ");
    expect(formatEdited("Z(9).99-", -1.009)).toBe("        1.00-");
  });
});

describe("alphanumeric moves", () => {
  it("pads and truncates on the right like PIC X(n)", () => {
    expect(alphanumeric("Purchase", 15)).toBe("Purchase       ");
    expect(alphanumeric("Regular Sales Draft", 10)).toBe("Regular Sa");
  });

  it("zero fills display numerics", () => {
    expect(displayDigits(50, 11)).toBe("00000000050");
    expect(displayDigits(1, 4)).toBe("0001");
  });

  it("stops at the delimiter like STRING ... DELIMITED BY", () => {
    expect(delimitedBy("Aniya                    ", " ")).toBe("Aniya");
    expect(delimitedBy("1588 Nienow Cape  ", "  ")).toBe("1588 Nienow Cape");
  });
});
