import { describe, expect, it } from 'vitest';
import {
  asciiToEbcdic, decodeBinary, decodeEbcdic, decodePacked, decodeZoned, Decimal, deEdit, ebcdicToAscii, encodeBinary,
  encodeEbcdic, encodePacked, encodeZoned, formatEdited, isZonedNumeric, parsePic, storageLength, toDecimal,
} from '../src/common/codecs/index.js';

const hex = (b: Uint8Array) => Buffer.from(b).toString('hex');

describe('PIC parsing', () => {
  it('expands repeats and computes lengths', () => {
    const p = parsePic('S9(10)V99');
    expect([p.category, p.digits, p.scale, p.signed]).toEqual(['numeric', 12, 2, true]);
    expect(storageLength(p, 'DISPLAY')).toBe(12);
    expect(storageLength(p, 'COMP-3')).toBe(7);
    expect(storageLength(parsePic('9(09)'), 'COMP')).toBe(4);
    expect(storageLength(parsePic('9(11)'), 'COMP')).toBe(8);
    expect(storageLength(parsePic('S9(4)'), 'BINARY')).toBe(2);
    expect(parsePic('X(16)').displayLength).toBe(16);
    expect(parsePic('-ZZZ,ZZZ,ZZZ.ZZ').category).toBe('numeric-edited');
  });
});

describe('zoned decimal (overpunch)', () => {
  const pic = parsePic('S9(10)V99');
  it('decodes ASCII overpunch signs from the seed files', () => {
    expect(decodeZoned(Buffer.from('00000001940{'), pic, 'ascii').value.toString()).toBe('194');
    expect(decodeZoned(Buffer.from('00000000005}'), pic, 'ascii').value.toString()).toBe('-0.5');
    expect(decodeZoned(Buffer.from('00000000123L'), pic, 'ascii').value.toString()).toBe('-12.33');
    expect(decodeZoned(Buffer.from('00000000123C'), pic, 'ascii').value.toString()).toBe('12.33');
  });
  it('encodes ASCII and EBCDIC', () => {
    expect(encodeZoned('-12.33', pic, 'ascii').toString('latin1')).toBe('00000000123L');
    expect(encodeZoned('194', pic, 'ascii').toString('latin1')).toBe('00000001940{');
    expect(hex(encodeZoned('-1', parsePic('S9(2)'), 'ebcdic'))).toBe('f0d1');
    expect(hex(encodeZoned('1', parsePic('S9(2)'), 'ebcdic'))).toBe('f0c1');
    expect(hex(encodeZoned('7', parsePic('9(2)'), 'ebcdic'))).toBe('f0f7');
  });
  it('truncates like MOVE (high-order digits and excess decimals)', () => {
    expect(encodeZoned('12345.678', parsePic('9(3)V99'), 'ascii').toString()).toBe('34567');
    expect(encodeZoned('-5', parsePic('9(3)'), 'ascii').toString()).toBe('005');
  });
  it('class test NUMERIC', () => {
    expect(isZonedNumeric(Buffer.from('00012'), parsePic('9(5)'), 'ascii')).toBe(true);
    expect(isZonedNumeric(Buffer.from('0001 '), parsePic('9(5)'), 'ascii')).toBe(false);
    expect(isZonedNumeric(Buffer.from('0001J'), parsePic('9(5)'), 'ascii')).toBe(false);
    expect(isZonedNumeric(Buffer.from('0001J'), parsePic('S9(5)'), 'ascii')).toBe(true);
  });
});

describe('COMP-3 packed decimal', () => {
  it('round-trips with C/D/F signs', () => {
    const pic = parsePic('S9(09)V99');
    expect(hex(encodePacked('504.77', pic))).toBe('00000050477c');
    expect(hex(encodePacked('-1.5', pic))).toBe('00000000150d');
    expect(hex(encodePacked('3', parsePic('9(03)')))).toBe('003f');
    expect(decodePacked(Buffer.from('00000050477c', 'hex'), pic).value.toString()).toBe('504.77');
    expect(decodePacked(Buffer.from('00000000150d', 'hex'), pic).value.toString()).toBe('-1.5');
    expect(decodePacked(Buffer.from('4040', 'hex'), parsePic('9(3)')).valid).toBe(false);
  });
});

describe('COMP binary', () => {
  it('is big-endian two\'s complement', () => {
    expect(hex(encodeBinary(-1, parsePic('S9(4)')))).toBe('ffff');
    expect(hex(encodeBinary(36, parsePic('9(8)')))).toBe('00000024');
    expect(decodeBinary(Buffer.from('fffe', 'hex'), parsePic('S9(4)')).value.toString()).toBe('-2');
    expect(decodeBinary(Buffer.from('0000000000000032', 'hex'), parsePic('9(11)')).value.toString()).toBe('50');
    expect(decodeBinary(Buffer.from('000004d2', 'hex'), parsePic('S9(7)V99')).value.toString()).toBe('12.34');
  });
});

describe('CP037', () => {
  it('converts both ways and is a bijection', () => {
    expect(decodeEbcdic(Buffer.from('c1c4d4c9d5f0f0f1', 'hex'))).toBe('ADMIN001');
    expect(hex(encodeEbcdic('Hello {}'))).toBe('c885939396' + '40c0d0');
    const all = Buffer.from(Array.from({ length: 256 }, (_, i) => i));
    expect(asciiToEbcdic(ebcdicToAscii(all)).equals(all)).toBe(true);
  });
});

describe('numeric-edited', () => {
  it('formats CVTRA07Y report amounts', () => {
    expect(formatEdited('-ZZZ,ZZZ,ZZZ.ZZ', '504.77')).toBe('         504.77');
    expect(formatEdited('-ZZZ,ZZZ,ZZZ.ZZ', '-1234567.8')).toBe('-  1,234,567.80');
    expect(formatEdited('+ZZZ,ZZZ,ZZZ.ZZ', '1234.5')).toBe('+      1,234.50');
    expect(formatEdited('+ZZZ,ZZZ,ZZZ.ZZ', '-0.01')).toBe('-           .01');
    expect(deEdit('-ZZZ,ZZZ,ZZZ.ZZ', '-  1,234,567.80').toString()).toBe('-1234567.8');
  });
});

describe('decimal guard', () => {
  it('rejects non-integer JS numbers', () => {
    expect(() => toDecimal(0.1)).toThrow(TypeError);
    expect(toDecimal('0.1').plus('0.2').equals(new Decimal('0.3'))).toBe(true);
  });
});
