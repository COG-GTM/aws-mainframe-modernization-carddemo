/**
 * Packed decimal (USAGE COMP-3) codec. Sign nibble X'C' (+), X'D' (-), X'F'
 * (unsigned). On decode X'A'/X'E'/X'F' are treated as positive and X'B' as negative.
 */
import { fromScaledDigits, type NumericInput, toScaledDigits } from './decimal.js';
import type { Pic } from './pic.js';
import type { DecodedNumber } from './zoned.js';

export function packedLength(digits: number): number {
  return Math.floor(digits / 2) + 1;
}

export function decodePacked(bytes: Uint8Array, pic: Pic): DecodedNumber {
  let digits = '';
  let valid = true;
  const nibbles: number[] = [];
  for (const b of bytes) nibbles.push(b >> 4, b & 0x0f);
  const sign = nibbles.pop()!;
  for (const n of nibbles) {
    if (n > 9) valid = false;
    digits += String(n % 10);
  }
  if (sign < 0xa) valid = false;
  if (!pic.signed && sign !== 0xf) valid = false;
  const negative = sign === 0xd || sign === 0xb;
  return { value: fromScaledDigits(digits, pic.scale, negative && pic.signed), valid };
}

export function encodePacked(value: NumericInput, pic: Pic): Buffer {
  const len = packedLength(pic.digits);
  const { digitString, negative } = toScaledDigits(value, pic.digits, pic.scale, pic.signed);
  const all = digitString.padStart(len * 2 - 1, '0');
  const sign = pic.signed ? (negative ? 0xd : 0xc) : 0xf;
  const out = Buffer.alloc(len);
  for (let i = 0; i < len; i++) {
    const hi = all.charCodeAt(i * 2) - 0x30;
    const lo = i === len - 1 ? sign : all.charCodeAt(i * 2 + 1) - 0x30;
    out[i] = (hi << 4) | lo;
  }
  return out;
}

export function isPackedNumeric(bytes: Uint8Array, pic: Pic): boolean {
  return decodePacked(bytes, pic).valid;
}
