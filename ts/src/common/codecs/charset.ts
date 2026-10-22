import { decodeEbcdic, encodeEbcdic } from './ebcdic.js';

/**
 * Character set used for DISPLAY data in a record buffer.
 * - `ascii`: ISO-8859-1 bytes (the `app/data/ASCII` seed files, GnuCOBOL files).
 * - `ebcdic`: IBM CP037 bytes (the `app/data/EBCDIC` seed files).
 */
export type Charset = 'ascii' | 'ebcdic';

export function decodeText(bytes: Uint8Array, charset: Charset): string {
  return charset === 'ebcdic' ? decodeEbcdic(bytes) : Buffer.from(bytes).toString('latin1');
}

export function encodeText(text: string, charset: Charset): Buffer {
  return charset === 'ebcdic' ? encodeEbcdic(text) : Buffer.from(text, 'latin1');
}

/** Byte value of a SPACE in the given charset. */
export function spaceByte(charset: Charset): number {
  return charset === 'ebcdic' ? 0x40 : 0x20;
}

/**
 * COBOL alphanumeric MOVE: left-justify, truncate on the right, pad with spaces.
 */
export function padAlphanumeric(text: string, length: number): string {
  return text.length >= length ? text.slice(0, length) : text + ' '.repeat(length - text.length);
}

/** Figurative constants as JS strings of a given length (Latin-1 view). */
export const spaces = (n: number): string => ' '.repeat(n);
export const lowValues = (n: number): string => '\x00'.repeat(n);
export const highValues = (n: number): string => '\xff'.repeat(n);
export const zeros = (n: number): string => '0'.repeat(n);
