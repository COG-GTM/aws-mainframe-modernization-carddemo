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

/** `TRAN-CAT-BAL-RECORD` — copybook CVTRA01Y, 50 bytes. */
export interface TransactionCategoryBalance {
  readonly accountId: string;
  readonly typeCode: string;
  readonly categoryCode: string;
  readonly balance: Money;
  readonly filler: string;
}

export const TRAN_CAT_BAL_LENGTH = 50;

/** `FD-TRAN-CAT-KEY` — account `9(11)` + type `X(02)` + category `9(04)`. */
export function transactionCategoryBalanceKey(record: {
  readonly accountId: string;
  readonly typeCode: string;
  readonly categoryCode: string;
}): string {
  return `${writeDigits(record.accountId, 11)}${writeText(record.typeCode, 2)}${writeDigits(record.categoryCode, 4)}`;
}

export function parseTransactionCategoryBalance(line: string): TransactionCategoryBalance {
  const image = padRecord(line, TRAN_CAT_BAL_LENGTH);
  return {
    accountId: readDigits(image, 1, 11),
    typeCode: readText(image, 12, 2),
    categoryCode: readDigits(image, 14, 4),
    balance: readAmount(image, 18, 11),
    filler: readText(image, 29, 22),
  };
}

export function serializeTransactionCategoryBalance(record: TransactionCategoryBalance): string {
  return assembleRecord(
    [
      writeDigits(record.accountId, 11),
      writeText(record.typeCode, 2),
      writeDigits(record.categoryCode, 4),
      writeAmount(record.balance, 11),
      writeText(record.filler, 22),
    ],
    TRAN_CAT_BAL_LENGTH,
  );
}
