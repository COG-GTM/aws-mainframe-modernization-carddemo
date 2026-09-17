/**
 * Binary (`USAGE COMP` / `BINARY`) conversions.
 *
 * A binary field holds the value as a two's complement big-endian integer,
 * scaled by the implied decimal positions of its PICTURE clause. The byte
 * width follows the digit count: 1-4 digits occupy 2 bytes, 5-9 digits 4 bytes
 * and 10-18 digits 8 bytes.
 */

import type { ZonedOptions } from "./zoned.js";

/** Byte length of a `COMP` field holding the given number of digits. */
export function binaryLength(digits: number): number {
  if (digits <= 4) {
    return 2;
  }
  if (digits <= 9) {
    return 4;
  }
  if (digits <= 18) {
    return 8;
  }
  throw new Error(`COMP fields support at most 18 digits, got ${digits}`);
}

function scaleFactor(scale: number): bigint {
  return 10n ** BigInt(scale);
}

export function decodeBinary(bytes: Uint8Array, options: ZonedOptions): number {
  let magnitude = 0n;
  for (const byte of bytes) {
    magnitude = (magnitude << 8n) | BigInt(byte);
  }

  const width = BigInt(bytes.length) * 8n;
  const signBit = 1n << (width - 1n);
  if (options.signed && (magnitude & signBit) !== 0n) {
    magnitude -= 1n << width;
  }

  return Number(magnitude) / Number(scaleFactor(options.scale));
}

export function encodeBinary(value: number, options: ZonedOptions): Uint8Array {
  const size = binaryLength(options.digits);
  const scaled = BigInt(Math.round(value * Number(scaleFactor(options.scale))));
  const limit = 10n ** BigInt(options.digits);
  if (scaled >= limit || scaled <= -limit) {
    throw new Error(`value ${value} overflows COMP PIC 9(${options.digits})`);
  }
  if (!options.signed && scaled < 0n) {
    throw new Error(`negative value ${value} does not fit an unsigned COMP field`);
  }

  const width = BigInt(size) * 8n;
  const stored = scaled < 0n ? scaled + (1n << width) : scaled;

  const bytes = new Uint8Array(size);
  for (let index = size - 1; index >= 0; index -= 1) {
    bytes[index] = Number((stored >> BigInt((size - 1 - index) * 8)) & 0xffn);
  }
  return bytes;
}
