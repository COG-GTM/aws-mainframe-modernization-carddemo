/**
 * Parsing of COBOL PICTURE clauses into a normalised descriptor.
 *
 * Supported forms: `X(n)`, `XX`, `9(n)`, `S9(n)V99`, `9(n)V9(m)`, `A(n)`.
 */

export type PicCategory = "alphanumeric" | "numeric";

export interface PicClause {
  readonly category: PicCategory;
  /** Total number of character positions occupied by the display form. */
  readonly length: number;
  /** Count of digits before the implied decimal point. */
  readonly integerDigits: number;
  /** Count of digits after the implied decimal point. */
  readonly decimalDigits: number;
  readonly signed: boolean;
}

const ALPHANUMERIC = /^(?:[XA])(?:\((\d+)\))?$/;

function expand(pic: string): string {
  return pic.replace(/([XA9])\((\d+)\)/g, (_match, symbol: string, count: string) =>
    symbol.repeat(Number(count)),
  );
}

export function parsePic(pic: string): PicClause {
  const normalised = pic.trim().toUpperCase().replace(/\s+/g, "");
  if (normalised.length === 0) {
    throw new Error("empty PICTURE clause");
  }

  const signed = normalised.startsWith("S");
  const body = expand(signed ? normalised.slice(1) : normalised);

  if (!signed && ALPHANUMERIC.test(normalised)) {
    return {
      category: "alphanumeric",
      length: body.length,
      integerDigits: 0,
      decimalDigits: 0,
      signed: false,
    };
  }

  if (/^[X A]+$/.test(body)) {
    return {
      category: "alphanumeric",
      length: body.length,
      integerDigits: 0,
      decimalDigits: 0,
      signed: false,
    };
  }

  if (!/^9*V?9*$/.test(body) || !body.includes("9")) {
    throw new Error(`unsupported PICTURE clause: ${pic}`);
  }

  const [integerPart = "", decimalPart = ""] = body.split("V");
  return {
    category: "numeric",
    length: integerPart.length + decimalPart.length,
    integerDigits: integerPart.length,
    decimalDigits: decimalPart.length,
    signed,
  };
}
