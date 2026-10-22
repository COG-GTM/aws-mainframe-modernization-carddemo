/**
 * Zoned decimal (USAGE DISPLAY numeric) codec with trailing overpunched sign.
 *
 * ASCII convention (as in app/data/ASCII and GnuCOBOL): the last byte carries the
 * sign: `{`,`A`-`I` = +0..+9, `}`,`J`-`R` = -0..-9. On decode `p`-`y` (Micro
 * Focus style negative) is also accepted.
 * EBCDIC convention: zone nibble X'F'/X'C'/X'A'/X'E' positive, X'D'/X'B' negative.
 */
import { type Charset } from './charset.js';
import { type Decimal, fromScaledDigits, type NumericInput, toScaledDigits } from './decimal.js';
import type { Pic } from './pic.js';

export interface DecodedNumber {
  value: Decimal;
  /** False when the bytes would fail a COBOL `IS NUMERIC` class test. */
  valid: boolean;
}

const ASCII_POS_OVERPUNCH = '{ABCDEFGHI';
const ASCII_NEG_OVERPUNCH = '}JKLMNOPQR';

interface DigitInfo {
  digit: number;
  /** 'none' = plain unsigned digit, 'pos'/'neg' = sign-bearing byte, 'bad' = not a digit. */
  sign: 'none' | 'pos' | 'neg' | 'bad';
}

function asciiDigit(byte: number): DigitInfo {
  if (byte >= 0x30 && byte <= 0x39) return { digit: byte - 0x30, sign: 'none' };
  const ch = String.fromCharCode(byte);
  let i = ASCII_POS_OVERPUNCH.indexOf(ch);
  if (i >= 0) return { digit: i, sign: 'pos' };
  i = ASCII_NEG_OVERPUNCH.indexOf(ch);
  if (i >= 0) return { digit: i, sign: 'neg' };
  if (byte >= 0x70 && byte <= 0x79) return { digit: byte - 0x70, sign: 'neg' };
  // Mainframe behaviour for invalid data: the low-order nibble is used.
  return { digit: (byte & 0x0f) % 10, sign: 'bad' };
}

function ebcdicDigit(byte: number): DigitInfo {
  const zone = byte >> 4;
  const low = byte & 0x0f;
  if (low > 9) return { digit: low % 10, sign: 'bad' };
  if (zone === 0xf) return { digit: low, sign: 'none' };
  if (zone === 0xc || zone === 0xa || zone === 0xe) return { digit: low, sign: 'pos' };
  if (zone === 0xd || zone === 0xb) return { digit: low, sign: 'neg' };
  return { digit: low, sign: 'bad' };
}

export function decodeZoned(bytes: Uint8Array, pic: Pic, charset: Charset): DecodedNumber {
  const lookup = charset === 'ebcdic' ? ebcdicDigit : asciiDigit;
  let digits = '';
  let valid = true;
  let negative = false;
  for (let i = 0; i < bytes.length; i++) {
    const info = lookup(bytes[i]!);
    const isLast = i === bytes.length - 1;
    digits += String(info.digit);
    if (info.sign === 'bad') valid = false;
    else if (info.sign !== 'none') {
      if (!isLast) valid = false;
      else {
        if (!pic.signed) valid = false;
        negative = info.sign === 'neg';
      }
    }
  }
  return { value: fromScaledDigits(digits, pic.scale, negative && pic.signed), valid };
}

export function encodeZoned(value: NumericInput, pic: Pic, charset: Charset): Buffer {
  const { digitString, negative } = toScaledDigits(value, pic.digits, pic.scale, pic.signed);
  const out = Buffer.alloc(pic.digits);
  for (let i = 0; i < pic.digits; i++) {
    const d = digitString.charCodeAt(i) - 0x30;
    const isLast = i === pic.digits - 1;
    if (charset === 'ebcdic') {
      const zone = isLast && pic.signed ? (negative ? 0xd0 : 0xc0) : 0xf0;
      out[i] = zone | d;
    } else if (isLast && pic.signed) {
      out[i] = (negative ? ASCII_NEG_OVERPUNCH : ASCII_POS_OVERPUNCH).charCodeAt(d);
    } else {
      out[i] = 0x30 + d;
    }
  }
  return out;
}

/** COBOL `IS NUMERIC` class test for a zoned decimal item. */
export function isZonedNumeric(bytes: Uint8Array, pic: Pic, charset: Charset): boolean {
  return decodeZoned(bytes, pic, charset).valid;
}
