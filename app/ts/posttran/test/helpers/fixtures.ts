import { fromDecimalString } from '../../src/codec/money.ts';
import { KeyedStore } from '../../src/io/keyedStore.ts';
import type { AccountRecord } from '../../src/records/account.ts';
import { accountKey } from '../../src/records/account.ts';
import type { CardCrossReference } from '../../src/records/cardCrossReference.ts';
import { cardCrossReferenceKey } from '../../src/records/cardCrossReference.ts';
import type { DailyTransaction } from '../../src/records/dailyTransaction.ts';
import type { TransactionCategoryBalance } from '../../src/records/transactionCategoryBalance.ts';
import { transactionCategoryBalanceKey } from '../../src/records/transactionCategoryBalance.ts';

export const CARD_NUMBER = '4859452612877065';
export const ACCOUNT_ID = '00000000001';

export function daily(overrides: Partial<DailyTransaction> = {}): DailyTransaction {
  return {
    id: '0000000000683580',
    typeCode: '01',
    categoryCode: '0001',
    source: 'POS TERM  ',
    description: 'Purchase'.padEnd(100, ' '),
    amount: fromDecimalString('100.00'),
    merchantId: '800000000',
    merchantName: 'Abshire-Lowe'.padEnd(50, ' '),
    merchantCity: 'North Enoshaven'.padEnd(50, ' '),
    merchantZip: '72112     ',
    cardNumber: CARD_NUMBER,
    originationTimestamp: '2022-06-10 19:27:53.000000',
    processingTimestamp: ' '.repeat(26),
    filler: ' '.repeat(20),
    ...overrides,
  };
}

export function account(overrides: Partial<AccountRecord> = {}): AccountRecord {
  return {
    id: ACCOUNT_ID,
    activeStatus: 'Y',
    currentBalance: fromDecimalString('194.00'),
    creditLimit: fromDecimalString('2020.00'),
    cashCreditLimit: fromDecimalString('1020.00'),
    openDate: '2014-11-20',
    expirationDate: '2025-05-20',
    reissueDate: '2025-05-20',
    currentCycleCredit: fromDecimalString('0.00'),
    currentCycleDebit: fromDecimalString('0.00'),
    addressZip: 'A         ',
    groupId: '000000000 ',
    filler: ' '.repeat(178),
    ...overrides,
  };
}

export function crossReference(overrides: Partial<CardCrossReference> = {}): CardCrossReference {
  return {
    cardNumber: CARD_NUMBER,
    customerId: '000000001',
    accountId: ACCOUNT_ID,
    filler: ' '.repeat(14),
    ...overrides,
  };
}

export function categoryBalance(
  overrides: Partial<TransactionCategoryBalance> = {},
): TransactionCategoryBalance {
  return {
    accountId: ACCOUNT_ID,
    typeCode: '01',
    categoryCode: '0001',
    balance: fromDecimalString('0.00'),
    filler: ' '.repeat(22),
    ...overrides,
  };
}

export function xrefStore(records: readonly CardCrossReference[]): KeyedStore<CardCrossReference> {
  return new KeyedStore({ ddName: 'XREFFILE', keyOf: cardCrossReferenceKey }, records);
}

export function accountStore(records: readonly AccountRecord[]): KeyedStore<AccountRecord> {
  return new KeyedStore({ ddName: 'ACCTFILE', keyOf: accountKey }, records);
}

export function categoryBalanceStore(
  records: readonly TransactionCategoryBalance[],
): KeyedStore<TransactionCategoryBalance> {
  return new KeyedStore({ ddName: 'TCATBALF', keyOf: transactionCategoryBalanceKey }, records);
}
