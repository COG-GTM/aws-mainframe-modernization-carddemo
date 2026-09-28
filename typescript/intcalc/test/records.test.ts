import { readFileSync } from 'node:fs';
import { describe, expect, it } from 'vitest';

import {
  ACCOUNT_RECORD_LENGTH,
  DISCGRP_RECORD_LENGTH,
  TCATBAL_RECORD_LENGTH,
  TRAN_RECORD_LENGTH,
  XREF_RECORD_LENGTH,
  formatAccount,
  formatCardXref,
  formatDisGroup,
  formatTranCatBal,
  parseAccount,
  parseCardXref,
  parseDisGroup,
  parseTranCatBal,
} from '../src/records.ts';
import { splitFixedWidth } from '../src/io.ts';
import { ASCII_DATA_DIR } from './fixtures.ts';

function fixture(name: string, recordLength: number): string[] {
  return splitFixedWidth(readFileSync(`${ASCII_DATA_DIR}/${name}`, 'latin1'), recordLength, name);
}

describe('fixed-width record round-trips over the ASCII fixtures', () => {
  it('reproduces every TCATBALF record byte for byte', () => {
    for (const image of fixture('tcatbal.txt', TCATBAL_RECORD_LENGTH)) {
      expect(formatTranCatBal(parseTranCatBal(image))).toBe(image);
    }
  });

  it('reproduces every DISCGRP record byte for byte', () => {
    for (const image of fixture('discgrp.txt', DISCGRP_RECORD_LENGTH)) {
      expect(formatDisGroup(parseDisGroup(image))).toBe(image);
    }
  });

  it('reproduces every ACCTFILE record byte for byte', () => {
    for (const image of fixture('acctdata.txt', ACCOUNT_RECORD_LENGTH)) {
      expect(formatAccount(parseAccount(image))).toBe(image);
    }
  });

  it('reproduces every XREFFILE record byte for byte', () => {
    for (const image of fixture('cardxref.txt', XREF_RECORD_LENGTH)) {
      expect(formatCardXref(parseCardXref(image))).toBe(image);
    }
  });

  it('keeps the copybook record lengths', () => {
    expect(TCATBAL_RECORD_LENGTH).toBe(50);
    expect(XREF_RECORD_LENGTH).toBe(50);
    expect(DISCGRP_RECORD_LENGTH).toBe(50);
    expect(ACCOUNT_RECORD_LENGTH).toBe(300);
    expect(TRAN_RECORD_LENGTH).toBe(350);
  });
});

describe('splitFixedWidth', () => {
  it('splits a raw RECFM=F image with no line separators', () => {
    const raw = `${'a'.repeat(50)}${'b'.repeat(50)}`;
    expect(splitFixedWidth(raw, 50, 'test')).toEqual(['a'.repeat(50), 'b'.repeat(50)]);
  });

  it('strips CR from CRLF terminated fixtures and pads short records', () => {
    expect(splitFixedWidth('abc\r\n', 5, 'test')).toEqual(['abc  ']);
  });
});
