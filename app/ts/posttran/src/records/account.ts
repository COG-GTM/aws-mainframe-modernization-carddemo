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

/** `ACCOUNT-RECORD` — copybook CVACT01Y, 300 bytes, key `ACCT-ID`. */
export interface AccountRecord {
  readonly id: string;
  readonly activeStatus: string;
  readonly currentBalance: Money;
  readonly creditLimit: Money;
  readonly cashCreditLimit: Money;
  readonly openDate: string;
  /** `ACCT-EXPIRAION-DATE`; spelling kept from the copybook. */
  readonly expirationDate: string;
  readonly reissueDate: string;
  readonly currentCycleCredit: Money;
  readonly currentCycleDebit: Money;
  readonly addressZip: string;
  readonly groupId: string;
  readonly filler: string;
}

export const ACCOUNT_LENGTH = 300;

export function accountKey(record: AccountRecord): string {
  return record.id;
}

export function parseAccount(line: string): AccountRecord {
  const image = padRecord(line, ACCOUNT_LENGTH);
  return {
    id: readDigits(image, 1, 11),
    activeStatus: readText(image, 12, 1),
    currentBalance: readAmount(image, 13, 12),
    creditLimit: readAmount(image, 25, 12),
    cashCreditLimit: readAmount(image, 37, 12),
    openDate: readText(image, 49, 10),
    expirationDate: readText(image, 59, 10),
    reissueDate: readText(image, 69, 10),
    currentCycleCredit: readAmount(image, 79, 12),
    currentCycleDebit: readAmount(image, 91, 12),
    addressZip: readText(image, 103, 10),
    groupId: readText(image, 113, 10),
    filler: readText(image, 123, 178),
  };
}

export function serializeAccount(record: AccountRecord): string {
  return assembleRecord(
    [
      writeDigits(record.id, 11),
      writeText(record.activeStatus, 1),
      writeAmount(record.currentBalance, 12),
      writeAmount(record.creditLimit, 12),
      writeAmount(record.cashCreditLimit, 12),
      writeText(record.openDate, 10),
      writeText(record.expirationDate, 10),
      writeText(record.reissueDate, 10),
      writeAmount(record.currentCycleCredit, 12),
      writeAmount(record.currentCycleDebit, 12),
      writeText(record.addressZip, 10),
      writeText(record.groupId, 10),
      writeText(record.filler, 178),
    ],
    ACCOUNT_LENGTH,
  );
}
