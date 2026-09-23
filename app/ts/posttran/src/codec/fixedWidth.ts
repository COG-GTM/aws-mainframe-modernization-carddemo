import type { Money } from './money.ts';
import { decodeZonedDecimal, encodeZonedDecimal } from './zonedDecimal.ts';

/**
 * Helpers for reading and writing the fixed-width record images described by
 * the CardDemo copybooks. Offsets are 1-based, as in the copybook tables, so
 * the TypeScript reads line up one-for-one with the business specification.
 */

export function padRecord(image: string, length: number): string {
  const trimmed = image.replace(/\r?\n$/, '');
  if (trimmed.length > length) {
    throw new RangeError(`record longer than ${length} bytes: ${trimmed.length}`);
  }
  return trimmed.padEnd(length, ' ');
}

function slice(image: string, offset: number, length: number): string {
  const field = image.slice(offset - 1, offset - 1 + length);
  if (field.length !== length) {
    throw new RangeError(`record too short for field at offset ${offset} length ${length}`);
  }
  return field;
}

/** `PIC X(n)` — raw characters, preserved verbatim including padding. */
export function readText(image: string, offset: number, length: number): string {
  return slice(image, offset, length);
}

/**
 * `PIC 9(n)` — fixed-width, zero-padded digit string. Kept as a string so key
 * padding survives round-tripping (see spec §7, "Key formatting").
 */
export function readDigits(image: string, offset: number, length: number): string {
  const field = slice(image, offset, length);
  if (!/^\d+$/.test(field)) {
    throw new RangeError(`non-numeric digits field at offset ${offset}: ${JSON.stringify(field)}`);
  }
  return field;
}

/** `PIC S9(n)V99` — signed zoned decimal returned as scale-2 minor units. */
export function readAmount(image: string, offset: number, length: number): Money {
  return decodeZonedDecimal(slice(image, offset, length), { length, scale: 2, signed: true });
}

export function writeText(value: string, length: number): string {
  if (value.length > length) {
    throw new RangeError(`text value longer than ${length} bytes`);
  }
  return value.padEnd(length, ' ');
}

export function writeDigits(value: string, length: number): string {
  if (!/^\d*$/.test(value)) {
    throw new RangeError(`non-numeric digits value: ${JSON.stringify(value)}`);
  }
  if (value.length > length) {
    throw new RangeError(`digits value longer than ${length} bytes`);
  }
  return value.padStart(length, '0');
}

export function writeAmount(value: Money, length: number): string {
  return encodeZonedDecimal(value, { length, scale: 2, signed: true });
}

/** Concatenates field images and checks the assembled record length. */
export function assembleRecord(parts: readonly string[], length: number): string {
  const image = parts.join('');
  if (image.length !== length) {
    throw new RangeError(`assembled record is ${image.length} bytes, expected ${length}`);
  }
  return image;
}
