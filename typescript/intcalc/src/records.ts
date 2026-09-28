import type { Decimal } from './decimal.ts';
import { decodeZoned, encodeZoned } from './zoned.ts';

export const TCATBAL_RECORD_LENGTH = 50;
export const XREF_RECORD_LENGTH = 50;
export const DISCGRP_RECORD_LENGTH = 50;
export const ACCOUNT_RECORD_LENGTH = 300;
export const TRAN_RECORD_LENGTH = 350;

export class RecordFormatError extends Error {}

function field(raw: string, offset: number, length: number): string {
  return raw.slice(offset, offset + length);
}

/** Pads (or verifies) a record image to the fixed length declared by its copybook. */
export function padRecord(raw: string, length: number, what: string): string {
  if (raw.length > length) {
    throw new RecordFormatError(`${what} record is ${raw.length} bytes, expected ${length}`);
  }
  return raw.padEnd(length, ' ');
}

/** Formats an unsigned numeric-display (PIC 9(n)) field: right aligned, zero filled. */
export function unsignedDisplay(value: string, length: number): string {
  return value.replace(/ /g, '').padStart(length, '0').slice(-length);
}

/** Formats an alphanumeric (PIC X(n)) field: left aligned, space filled. */
export function alphanumeric(value: string, length: number): string {
  return value.padEnd(length, ' ').slice(0, length);
}

/** `CVTRA01Y` — transaction category balance, RECLN 50. */
export interface TranCatBalRecord {
  acctId: string;
  tranTypeCd: string;
  tranCatCd: string;
  balance: Decimal;
  filler: string;
}

export function parseTranCatBal(raw: string): TranCatBalRecord {
  const rec = padRecord(raw, TCATBAL_RECORD_LENGTH, 'TCATBALF');
  return {
    acctId: field(rec, 0, 11),
    tranTypeCd: field(rec, 11, 2),
    tranCatCd: field(rec, 13, 4),
    balance: decodeZoned(field(rec, 17, 11), 2),
    filler: field(rec, 28, 22),
  };
}

export function formatTranCatBal(rec: TranCatBalRecord): string {
  return [
    unsignedDisplay(rec.acctId, 11),
    alphanumeric(rec.tranTypeCd, 2),
    unsignedDisplay(rec.tranCatCd, 4),
    encodeZoned(rec.balance, 11, 2, true),
    alphanumeric(rec.filler, 22),
  ].join('');
}

/** `CVACT03Y` — card cross-reference, RECLN 50. */
export interface CardXrefRecord {
  cardNum: string;
  custId: string;
  acctId: string;
  filler: string;
}

export function parseCardXref(raw: string): CardXrefRecord {
  const rec = padRecord(raw, XREF_RECORD_LENGTH, 'XREFFILE');
  return {
    cardNum: field(rec, 0, 16),
    custId: field(rec, 16, 9),
    acctId: field(rec, 25, 11),
    filler: field(rec, 36, 14),
  };
}

export function formatCardXref(rec: CardXrefRecord): string {
  return [
    alphanumeric(rec.cardNum, 16),
    unsignedDisplay(rec.custId, 9),
    unsignedDisplay(rec.acctId, 11),
    alphanumeric(rec.filler, 14),
  ].join('');
}

/** `CVTRA02Y` — disclosure group, RECLN 50. */
export interface DisGroupRecord {
  acctGroupId: string;
  tranTypeCd: string;
  tranCatCd: string;
  intRate: Decimal;
  filler: string;
}

export function parseDisGroup(raw: string): DisGroupRecord {
  const rec = padRecord(raw, DISCGRP_RECORD_LENGTH, 'DISCGRP');
  return {
    acctGroupId: field(rec, 0, 10),
    tranTypeCd: field(rec, 10, 2),
    tranCatCd: field(rec, 12, 4),
    intRate: decodeZoned(field(rec, 16, 6), 2),
    filler: field(rec, 22, 28),
  };
}

export function formatDisGroup(rec: DisGroupRecord): string {
  return [
    alphanumeric(rec.acctGroupId, 10),
    alphanumeric(rec.tranTypeCd, 2),
    unsignedDisplay(rec.tranCatCd, 4),
    encodeZoned(rec.intRate, 6, 2, true),
    alphanumeric(rec.filler, 28),
  ].join('');
}

/** `CVACT01Y` — account master, RECLN 300. */
export interface AccountRecord {
  acctId: string;
  activeStatus: string;
  currBal: Decimal;
  creditLimit: Decimal;
  cashCreditLimit: Decimal;
  openDate: string;
  expirationDate: string;
  reissueDate: string;
  currCycCredit: Decimal;
  currCycDebit: Decimal;
  addrZip: string;
  groupId: string;
  filler: string;
}

export function parseAccount(raw: string): AccountRecord {
  const rec = padRecord(raw, ACCOUNT_RECORD_LENGTH, 'ACCTFILE');
  return {
    acctId: field(rec, 0, 11),
    activeStatus: field(rec, 11, 1),
    currBal: decodeZoned(field(rec, 12, 12), 2),
    creditLimit: decodeZoned(field(rec, 24, 12), 2),
    cashCreditLimit: decodeZoned(field(rec, 36, 12), 2),
    openDate: field(rec, 48, 10),
    expirationDate: field(rec, 58, 10),
    reissueDate: field(rec, 68, 10),
    currCycCredit: decodeZoned(field(rec, 78, 12), 2),
    currCycDebit: decodeZoned(field(rec, 90, 12), 2),
    addrZip: field(rec, 102, 10),
    groupId: field(rec, 112, 10),
    filler: field(rec, 122, 178),
  };
}

export function formatAccount(rec: AccountRecord): string {
  return [
    unsignedDisplay(rec.acctId, 11),
    alphanumeric(rec.activeStatus, 1),
    encodeZoned(rec.currBal, 12, 2, true),
    encodeZoned(rec.creditLimit, 12, 2, true),
    encodeZoned(rec.cashCreditLimit, 12, 2, true),
    alphanumeric(rec.openDate, 10),
    alphanumeric(rec.expirationDate, 10),
    alphanumeric(rec.reissueDate, 10),
    encodeZoned(rec.currCycCredit, 12, 2, true),
    encodeZoned(rec.currCycDebit, 12, 2, true),
    alphanumeric(rec.addrZip, 10),
    alphanumeric(rec.groupId, 10),
    alphanumeric(rec.filler, 178),
  ].join('');
}

/** `CVTRA05Y` — transaction, RECLN 350. */
export interface TranRecord {
  tranId: string;
  tranTypeCd: string;
  tranCatCd: string;
  tranSource: string;
  tranDesc: string;
  tranAmt: Decimal;
  merchantId: string;
  merchantName: string;
  merchantCity: string;
  merchantZip: string;
  cardNum: string;
  origTs: string;
  procTs: string;
  filler: string;
}

export function parseTran(raw: string): TranRecord {
  const rec = padRecord(raw, TRAN_RECORD_LENGTH, 'TRANSACT');
  return {
    tranId: field(rec, 0, 16),
    tranTypeCd: field(rec, 16, 2),
    tranCatCd: field(rec, 18, 4),
    tranSource: field(rec, 22, 10),
    tranDesc: field(rec, 32, 100),
    tranAmt: decodeZoned(field(rec, 132, 11), 2),
    merchantId: field(rec, 143, 9),
    merchantName: field(rec, 152, 50),
    merchantCity: field(rec, 202, 50),
    merchantZip: field(rec, 252, 10),
    cardNum: field(rec, 262, 16),
    origTs: field(rec, 278, 26),
    procTs: field(rec, 304, 26),
    filler: field(rec, 330, 20),
  };
}

export function formatTran(rec: TranRecord): string {
  return [
    alphanumeric(rec.tranId, 16),
    alphanumeric(rec.tranTypeCd, 2),
    unsignedDisplay(rec.tranCatCd, 4),
    alphanumeric(rec.tranSource, 10),
    alphanumeric(rec.tranDesc, 100),
    encodeZoned(rec.tranAmt, 11, 2, true),
    unsignedDisplay(rec.merchantId, 9),
    alphanumeric(rec.merchantName, 50),
    alphanumeric(rec.merchantCity, 50),
    alphanumeric(rec.merchantZip, 10),
    alphanumeric(rec.cardNum, 16),
    alphanumeric(rec.origTs, 26),
    alphanumeric(rec.procTs, 26),
    alphanumeric(rec.filler, 20),
  ].join('');
}
