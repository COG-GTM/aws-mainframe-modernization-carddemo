/**
 * Zoned decimal (`USAGE DISPLAY`) and packed decimal (`USAGE COMP-3`) conversions.
 *
 * Signed zoned values carry the sign in the last digit as an overpunch:
 * `{ A..I` for +0..+9 and `} J..R` for -0..-9.
 */

const POSITIVE_OVERPUNCH = "{ABCDEFGHI";
const NEGATIVE_OVERPUNCH = "}JKLMNOPQR";

export interface ZonedOptions {
  readonly digits: number;
  readonly scale: number;
  readonly signed: boolean;
}

function scaleFactor(scale: number): number {
  return 10 ** scale;
}

export function decodeZoned(raw: string, options: ZonedOptions): number {
  const text = raw.padEnd(options.digits, "0");
  const lead = text.slice(0, options.digits - 1).replace(/ /g, "0");
  const last = text.charAt(options.digits - 1);

  let negative = false;
  let lastDigit: string;

  const positiveIndex = POSITIVE_OVERPUNCH.indexOf(last);
  const negativeIndex = NEGATIVE_OVERPUNCH.indexOf(last);
  if (positiveIndex >= 0) {
    lastDigit = String(positiveIndex);
  } else if (negativeIndex >= 0) {
    negative = true;
    lastDigit = String(negativeIndex);
  } else if (/[0-9]/.test(last)) {
    lastDigit = last;
  } else if (last === " ") {
    lastDigit = "0";
  } else {
    throw new Error(`invalid zoned decimal digit: ${JSON.stringify(last)}`);
  }

  if (!/^[0-9]*$/.test(lead)) {
    throw new Error(`invalid zoned decimal value: ${JSON.stringify(raw)}`);
  }

  const magnitude = Number(`${lead}${lastDigit}`) / scaleFactor(options.scale);
  return negative ? -magnitude : magnitude;
}

export function encodeZoned(value: number, options: ZonedOptions): string {
  const scaled = Math.round(Math.abs(value) * scaleFactor(options.scale));
  const digits = String(scaled).padStart(options.digits, "0");
  if (digits.length > options.digits) {
    throw new Error(`value ${value} overflows PIC 9(${options.digits})`);
  }

  if (!options.signed) {
    return digits;
  }

  const head = digits.slice(0, options.digits - 1);
  const tail = Number(digits.charAt(options.digits - 1));
  const overpunch = value < 0 ? NEGATIVE_OVERPUNCH : POSITIVE_OVERPUNCH;
  return `${head}${overpunch.charAt(tail)}`;
}

/** Byte length of a `COMP-3` field holding the given number of digits. */
export function packedLength(digits: number): number {
  return Math.floor(digits / 2) + 1;
}

export function decodePacked(bytes: Uint8Array, options: ZonedOptions): number {
  let digits = "";
  for (let index = 0; index < bytes.length; index += 1) {
    const byte = bytes[index] ?? 0;
    const high = (byte >> 4) & 0x0f;
    const low = byte & 0x0f;
    digits += String(high);
    if (index < bytes.length - 1) {
      digits += String(low);
    } else {
      const negative = low === 0x0d || low === 0x0b;
      const magnitude = Number(digits) / scaleFactor(options.scale);
      return negative ? -magnitude : magnitude;
    }
  }
  throw new Error("empty packed decimal field");
}

export function encodePacked(value: number, options: ZonedOptions): Uint8Array {
  const scaled = Math.round(Math.abs(value) * scaleFactor(options.scale));
  const size = packedLength(options.digits);
  const digits = String(scaled).padStart(size * 2 - 1, "0");
  if (digits.length > size * 2 - 1) {
    throw new Error(`value ${value} overflows COMP-3 PIC 9(${options.digits})`);
  }

  const bytes = new Uint8Array(size);
  for (let index = 0; index < size - 1; index += 1) {
    const high = Number(digits.charAt(index * 2));
    const low = Number(digits.charAt(index * 2 + 1));
    bytes[index] = (high << 4) | low;
  }
  const lastDigit = Number(digits.charAt(digits.length - 1));
  const sign = !options.signed ? 0x0f : value < 0 ? 0x0d : 0x0c;
  bytes[size - 1] = (lastDigit << 4) | sign;
  return bytes;
}
