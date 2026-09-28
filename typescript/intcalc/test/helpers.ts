import { Decimal } from '../src/decimal.ts';
import {
  AccountStore,
  DiscgrpStore,
  TransactionWriter,
  XrefStore,
} from '../src/io.ts';
import type {
  AccountRecord,
  CardXrefRecord,
  DisGroupRecord,
  TranCatBalRecord,
} from '../src/records.ts';

export function balanceRecord(
  acctId: string,
  tranTypeCd: string,
  tranCatCd: string,
  balance: string,
): TranCatBalRecord {
  return {
    acctId: acctId.padStart(11, '0'),
    tranTypeCd,
    tranCatCd: tranCatCd.padStart(4, '0'),
    balance: new Decimal(balance),
    filler: ' '.repeat(22),
  };
}

export function accountRecord(acctId: string, groupId: string, currBal: string): AccountRecord {
  return {
    acctId: acctId.padStart(11, '0'),
    activeStatus: 'Y',
    currBal: new Decimal(currBal),
    creditLimit: new Decimal('5000.00'),
    cashCreditLimit: new Decimal('1000.00'),
    openDate: '2014-11-20',
    expirationDate: '2025-05-20',
    reissueDate: '2025-05-20',
    currCycCredit: new Decimal('11.11'),
    currCycDebit: new Decimal('22.22'),
    addrZip: '30303',
    groupId,
    filler: ' '.repeat(178),
  };
}

export function xrefRecord(acctId: string, cardNum: string): CardXrefRecord {
  return {
    cardNum,
    custId: '000000001',
    acctId: acctId.padStart(11, '0'),
    filler: ' '.repeat(14),
  };
}

export function discgrpRecord(
  acctGroupId: string,
  tranTypeCd: string,
  tranCatCd: string,
  intRate: string,
): DisGroupRecord {
  return {
    acctGroupId: acctGroupId.padEnd(10, ' '),
    tranTypeCd,
    tranCatCd: tranCatCd.padStart(4, '0'),
    intRate: new Decimal(intRate),
    filler: ' '.repeat(28),
  };
}

export interface Harness {
  accounts: AccountStore;
  xref: XrefStore;
  discgrp: DiscgrpStore;
  transactions: TransactionWriter;
}

export function harness(
  accounts: readonly AccountRecord[],
  xrefs: readonly CardXrefRecord[],
  discgrps: readonly DisGroupRecord[],
): Harness {
  return {
    accounts: new AccountStore(accounts),
    xref: new XrefStore(xrefs),
    discgrp: new DiscgrpStore(discgrps),
    transactions: new TransactionWriter(),
  };
}

export const fixedClock = (): Date => new Date(2022, 6, 18, 1, 2, 3, 450);
