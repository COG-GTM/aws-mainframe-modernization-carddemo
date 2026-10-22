/**
 * Binary (USAGE COMP / BINARY / COMP-5) codec: big-endian, two's complement when
 * signed, 2/4/8 bytes for 1-4/5-9/10-18 digits (IBM and GnuCOBOL defaults).
 *
 * Encoding follows TRUNC(STD): the value is truncated to the PIC digit count.
 * Decoding returns the full binary value (it may exceed the PIC digits, as with
 * TRUNC(BIN) data).
 */
import { Decimal, fromScaledDigits, type NumericInput, toScaledDigits } from './decimal.js';
import { binaryLength, type Pic } from './pic.js';
import type { DecodedNumber } from './zoned.js';

export function decodeBinary(bytes: Uint8Array, pic: Pic): DecodedNumber {
  let v = 0n;
  for (const b of bytes) v = (v << 8n) | BigInt(b);
  const bits = BigInt(bytes.length * 8);
  if (pic.signed && bytes.length > 0 && (bytes[0]! & 0x80) !== 0) v -= 1n << bits;
  const negative = v < 0n;
  const abs = negative ? -v : v;
  return { value: fromScaledDigits(abs.toString(), pic.scale, negative), valid: true };
}

export function encodeBinary(value: NumericInput, pic: Pic): Buffer {
  const len = binaryLength(pic.digits);
  const { digitString, negative } = toScaledDigits(value, pic.digits, pic.scale, pic.signed);
  let v = BigInt(digitString);
  if (negative) v = -v;
  const bits = BigInt(len * 8);
  if (v < 0n) v += 1n << bits;
  const out = Buffer.alloc(len);
  for (let i = len - 1; i >= 0; i--) {
    out[i] = Number(v & 0xffn);
    v >>= 8n;
  }
  return out;
}

/** Convenience: decode a binary halfword/fullword/doubleword as a JS integer-safe Decimal. */
export function binaryToDecimal(bytes: Uint8Array, signed: boolean): Decimal {
  return decodeBinary(bytes, {
    source: '',
    expanded: '',
    category: 'numeric',
    digits: 18,
    scale: 0,
    signed,
    displayLength: 18,
  }).value;
}

export { Decimal };
