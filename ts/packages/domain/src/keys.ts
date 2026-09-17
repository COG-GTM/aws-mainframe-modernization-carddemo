/**
 * Key builders matching the VSAM KSDS keys used by the COBOL programs.
 */

import type {
  AccountRecord,
  CardRecord,
  CardXrefRecord,
  CustomerRecord,
  DiscGroupRecord,
  TranCatBalRecord,
  TranCatRecord,
  TranTypeRecord,
  TransactionRecord,
  UserSecurityRecord,
} from "./records.js";

export function padKey(value: string | number, width: number): string {
  return typeof value === "number"
    ? String(Math.trunc(value)).padStart(width, "0")
    : value.padEnd(width, " ");
}

export const accountKey = (record: AccountRecord): string => padKey(record.acctId, 11);
export const customerKey = (record: CustomerRecord): string => padKey(record.custId, 9);
export const cardKey = (record: CardRecord): string => padKey(record.cardNum, 16);
export const cardXrefKey = (record: CardXrefRecord): string => padKey(record.xrefCardNum, 16);
export const transactionKey = (record: TransactionRecord): string => padKey(record.tranId, 16);
export const userSecurityKey = (record: UserSecurityRecord): string => padKey(record.secUsrId, 8);

export const tranCatBalKey = (record: TranCatBalRecord): string =>
  `${padKey(record.trancatAcctId, 11)}${padKey(record.trancatTypeCd, 2)}${padKey(record.trancatCd, 4)}`;

export const discGroupKey = (record: DiscGroupRecord): string =>
  `${padKey(record.disAcctGroupId, 10)}${padKey(record.disTranTypeCd, 2)}${padKey(record.disTranCatCd, 4)}`;

export const tranTypeKey = (record: TranTypeRecord): string => padKey(record.tranType, 2);

export const tranCatKey = (record: TranCatRecord): string =>
  `${padKey(record.tranTypeCd, 2)}${padKey(record.tranCatCd, 4)}`;
