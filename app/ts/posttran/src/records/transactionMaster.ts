import type { Money } from '../codec/money.ts';
import {
  assembleRecord,
  padRecord,
  readAmount,
  readDigits,
  readText,
  writeAmount,
  writeDigits,
  writeText,
} from '../codec/fixedWidth.ts';

/** `TRAN-RECORD` — copybook CVTRA05Y, 350 bytes, key `TRAN-ID` at offset 1. */
export interface TransactionMasterRecord {
  readonly id: string;
  readonly typeCode: string;
  readonly categoryCode: string;
  readonly source: string;
  readonly description: string;
  readonly amount: Money;
  readonly merchantId: string;
  readonly merchantName: string;
  readonly merchantCity: string;
  readonly merchantZip: string;
  readonly cardNumber: string;
  readonly originationTimestamp: string;
  readonly processingTimestamp: string;
  readonly filler: string;
}

export const TRANSACTION_MASTER_LENGTH = 350;

export function transactionMasterKey(record: TransactionMasterRecord): string {
  return record.id;
}

export function parseTransactionMaster(line: string): TransactionMasterRecord {
  const image = padRecord(line, TRANSACTION_MASTER_LENGTH);
  return {
    id: readText(image, 1, 16),
    typeCode: readText(image, 17, 2),
    categoryCode: readDigits(image, 19, 4),
    source: readText(image, 23, 10),
    description: readText(image, 33, 100),
    amount: readAmount(image, 133, 11),
    merchantId: readDigits(image, 144, 9),
    merchantName: readText(image, 153, 50),
    merchantCity: readText(image, 203, 50),
    merchantZip: readText(image, 253, 10),
    cardNumber: readText(image, 263, 16),
    originationTimestamp: readText(image, 279, 26),
    processingTimestamp: readText(image, 305, 26),
    filler: readText(image, 331, 20),
  };
}

export function serializeTransactionMaster(record: TransactionMasterRecord): string {
  return assembleRecord(
    [
      writeText(record.id, 16),
      writeText(record.typeCode, 2),
      writeDigits(record.categoryCode, 4),
      writeText(record.source, 10),
      writeText(record.description, 100),
      writeAmount(record.amount, 11),
      writeDigits(record.merchantId, 9),
      writeText(record.merchantName, 50),
      writeText(record.merchantCity, 50),
      writeText(record.merchantZip, 10),
      writeText(record.cardNumber, 16),
      writeText(record.originationTimestamp, 26),
      writeText(record.processingTimestamp, 26),
      writeText(record.filler, 20),
    ],
    TRANSACTION_MASTER_LENGTH,
  );
}
