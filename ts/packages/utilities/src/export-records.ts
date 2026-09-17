/**
 * `CVEXPORT` — the 500 byte multi-record branch migration layout.
 *
 * The copybook is a common 40 byte prefix followed by 460 bytes of record
 * data that `REDEFINES` into one structure per record type. Each record type
 * is expressed here as a full 500 byte layout whose data fields carry the
 * `@carddemo/domain` property names, so an export record decodes straight
 * into the domain record the COBOL programs `MOVE` it to.
 */

import {
  decodeRecord,
  defineLayout,
  encodeRecord,
  type DecodedRecord,
  type Layout,
} from "@carddemo/copybook";
import type {
  AccountRecord,
  CardRecord,
  CardXrefRecord,
  CustomerRecord,
  TransactionRecord,
} from "@carddemo/domain";

export const EXPORT_RECORD_LENGTH = 500;

/** `EXPORT-REC-TYPE`; `D` is a card record, as `CBEXPORT` writes it. */
export const ExportRecordType = {
  customer: "C",
  account: "A",
  xref: "X",
  transaction: "T",
  card: "D",
} as const;

export type ExportRecordTypeCode = (typeof ExportRecordType)[keyof typeof ExportRecordType];

export interface ExportHeader {
  readonly recType: string;
  readonly timestamp: string;
  readonly sequenceNum: number;
  readonly branchId: string;
  readonly regionCode: string;
}

const headerFields = [
  { name: "recType", pic: "X(1)" },
  { name: "timestamp", pic: "X(26)" },
  { name: "sequenceNum", pic: "9(9)", usage: "comp" },
  { name: "branchId", pic: "X(4)" },
  { name: "regionCode", pic: "X(5)" },
] as const;

export const customerExportLayout: Layout = defineLayout("EXPORT-CUSTOMER-DATA", [
  ...headerFields,
  { name: "custId", pic: "9(09)", usage: "comp" },
  { name: "custFirstName", pic: "X(25)" },
  { name: "custMiddleName", pic: "X(25)" },
  { name: "custLastName", pic: "X(25)" },
  { name: "custAddrLine1", pic: "X(50)" },
  { name: "custAddrLine2", pic: "X(50)" },
  { name: "custAddrLine3", pic: "X(50)" },
  { name: "custAddrStateCd", pic: "X(02)" },
  { name: "custAddrCountryCd", pic: "X(03)" },
  { name: "custAddrZip", pic: "X(10)" },
  { name: "custPhoneNum1", pic: "X(15)" },
  { name: "custPhoneNum2", pic: "X(15)" },
  { name: "custSsn", pic: "9(09)" },
  { name: "custGovtIssuedId", pic: "X(20)" },
  { name: "custDobYyyyMmDd", pic: "X(10)" },
  { name: "custEftAccountId", pic: "X(10)" },
  { name: "custPriCardHolderInd", pic: "X(01)" },
  { name: "custFicoCreditScore", pic: "9(03)", usage: "comp-3" },
  { pic: "X(134)" },
]);

export const accountExportLayout: Layout = defineLayout("EXPORT-ACCOUNT-DATA", [
  ...headerFields,
  { name: "acctId", pic: "9(11)" },
  { name: "acctActiveStatus", pic: "X(01)" },
  { name: "acctCurrBal", pic: "S9(10)V99", usage: "comp-3" },
  { name: "acctCreditLimit", pic: "S9(10)V99" },
  { name: "acctCashCreditLimit", pic: "S9(10)V99", usage: "comp-3" },
  { name: "acctOpenDate", pic: "X(10)" },
  { name: "acctExpiraionDate", pic: "X(10)" },
  { name: "acctReissueDate", pic: "X(10)" },
  { name: "acctCurrCycCredit", pic: "S9(10)V99" },
  { name: "acctCurrCycDebit", pic: "S9(10)V99", usage: "comp" },
  { name: "acctAddrZip", pic: "X(10)" },
  { name: "acctGroupId", pic: "X(10)" },
  { pic: "X(352)" },
]);

export const transactionExportLayout: Layout = defineLayout("EXPORT-TRANSACTION-DATA", [
  ...headerFields,
  { name: "tranId", pic: "X(16)" },
  { name: "tranTypeCd", pic: "X(02)" },
  { name: "tranCatCd", pic: "9(04)" },
  { name: "tranSource", pic: "X(10)" },
  { name: "tranDesc", pic: "X(100)" },
  { name: "tranAmt", pic: "S9(09)V99", usage: "comp-3" },
  { name: "tranMerchantId", pic: "9(09)", usage: "comp" },
  { name: "tranMerchantName", pic: "X(50)" },
  { name: "tranMerchantCity", pic: "X(50)" },
  { name: "tranMerchantZip", pic: "X(10)" },
  { name: "tranCardNum", pic: "X(16)" },
  { name: "tranOrigTs", pic: "X(26)" },
  { name: "tranProcTs", pic: "X(26)" },
  { pic: "X(140)" },
]);

export const xrefExportLayout: Layout = defineLayout("EXPORT-CARD-XREF-DATA", [
  ...headerFields,
  { name: "xrefCardNum", pic: "X(16)" },
  { name: "xrefCustId", pic: "9(09)" },
  { name: "xrefAcctId", pic: "9(11)", usage: "comp" },
  { pic: "X(427)" },
]);

export const cardExportLayout: Layout = defineLayout("EXPORT-CARD-DATA", [
  ...headerFields,
  { name: "cardNum", pic: "X(16)" },
  { name: "cardAcctId", pic: "9(11)", usage: "comp" },
  { name: "cardCvvCd", pic: "9(03)", usage: "comp" },
  { name: "cardEmbossedName", pic: "X(50)" },
  { name: "cardExpiraionDate", pic: "X(10)" },
  { name: "cardActiveStatus", pic: "X(01)" },
  { pic: "X(373)" },
]);

export type ExportRecord =
  | { readonly recType: "C"; readonly header: ExportHeader; readonly customer: CustomerRecord }
  | { readonly recType: "A"; readonly header: ExportHeader; readonly account: AccountRecord }
  | { readonly recType: "X"; readonly header: ExportHeader; readonly xref: CardXrefRecord }
  | {
      readonly recType: "T";
      readonly header: ExportHeader;
      readonly transaction: TransactionRecord;
    }
  | { readonly recType: "D"; readonly header: ExportHeader; readonly card: CardRecord }
  | { readonly recType: "unknown"; readonly header: ExportHeader };

const headerLayout: Layout = defineLayout("EXPORT-RECORD-HEADER", [
  ...headerFields,
  { pic: "X(460)" },
]);

function splitHeader(record: Record<string, unknown>): {
  header: ExportHeader;
  data: Record<string, unknown>;
} {
  const { recType, timestamp, sequenceNum, branchId, regionCode, ...data } = record;
  return {
    header: {
      recType: String(recType),
      timestamp: String(timestamp),
      sequenceNum: Number(sequenceNum),
      branchId: String(branchId),
      regionCode: String(regionCode),
    },
    data,
  };
}

export function decodeExportHeader(line: string): ExportHeader {
  return splitHeader(decodeRecord(headerLayout, line)).header;
}

/** Decodes the `REDEFINES` data area of an export record as `T`. */
function decodeData<T>(layout: Layout, line: string): T {
  return splitHeader(decodeRecord(layout, line)).data as unknown as T;
}

/** Decodes one 500 byte export record into its domain record. */
export function decodeExportRecord(line: string): ExportRecord {
  const header = decodeExportHeader(line);

  switch (header.recType) {
    case ExportRecordType.customer:
      return {
        recType: "C",
        header,
        customer: decodeData<CustomerRecord>(customerExportLayout, line),
      };
    case ExportRecordType.account:
      return {
        recType: "A",
        header,
        account: decodeData<AccountRecord>(accountExportLayout, line),
      };
    case ExportRecordType.xref:
      return {
        recType: "X",
        header,
        xref: decodeData<CardXrefRecord>(xrefExportLayout, line),
      };
    case ExportRecordType.transaction:
      return {
        recType: "T",
        header,
        transaction: decodeData<TransactionRecord>(transactionExportLayout, line),
      };
    case ExportRecordType.card:
      return {
        recType: "D",
        header,
        card: decodeData<CardRecord>(cardExportLayout, line),
      };
    default:
      return { recType: "unknown", header };
  }
}

function encode(layout: Layout, header: ExportHeader, data: object): string {
  return encodeRecord(layout, { ...header, ...data } as DecodedRecord);
}

/** Encodes a domain record as its 500 byte export record. */
export function encodeExportRecord(record: ExportRecord): string {
  switch (record.recType) {
    case "C":
      return encode(customerExportLayout, record.header, record.customer);
    case "A":
      return encode(accountExportLayout, record.header, record.account);
    case "X":
      return encode(xrefExportLayout, record.header, record.xref);
    case "T":
      return encode(transactionExportLayout, record.header, record.transaction);
    case "D":
      return encode(cardExportLayout, record.header, record.card);
    case "unknown":
      return encodeRecord(headerLayout, { ...record.header } as DecodedRecord);
  }
}
