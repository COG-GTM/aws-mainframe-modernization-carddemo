/**
 * `COSTM01` — the transaction record re-keyed by card number for statement
 * reporting, as the `CREASTMT` SORT step builds it from `TRAN-RECORD`.
 */

import {
  decodeRecord,
  defineLayout,
  encodeRecord,
  type DecodedRecord,
  type Layout,
} from "@carddemo/copybook";
import { padKey, type RecordCodec, type TransactionRecord } from "@carddemo/domain";

/** `COSTM01` — TRNX-RECORD, RECLN 350. */
export const trnxLayout: Layout = defineLayout("TRNX-RECORD", [
  { name: "trnxCardNum", pic: "X(16)" },
  { name: "trnxId", pic: "X(16)" },
  { name: "trnxTypeCd", pic: "X(02)" },
  { name: "trnxCatCd", pic: "9(04)" },
  { name: "trnxSource", pic: "X(10)" },
  { name: "trnxDesc", pic: "X(100)" },
  { name: "trnxAmt", pic: "S9(09)V99" },
  { name: "trnxMerchantId", pic: "9(09)" },
  { name: "trnxMerchantName", pic: "X(50)" },
  { name: "trnxMerchantCity", pic: "X(50)" },
  { name: "trnxMerchantZip", pic: "X(10)" },
  { name: "trnxOrigTs", pic: "X(26)" },
  { name: "trnxProcTs", pic: "X(26)" },
  { pic: "X(20)" },
]);

export interface TrnxRecord {
  trnxCardNum: string;
  trnxId: string;
  trnxTypeCd: string;
  trnxCatCd: number;
  trnxSource: string;
  trnxDesc: string;
  trnxAmt: number;
  trnxMerchantId: number;
  trnxMerchantName: string;
  trnxMerchantCity: string;
  trnxMerchantZip: string;
  trnxOrigTs: string;
  trnxProcTs: string;
}

function codec<T>(layout: Layout): RecordCodec<T> {
  return {
    layout,
    decode: (line: string): T => decodeRecord(layout, line) as T,
    encode: (record: T): string => encodeRecord(layout, record as DecodedRecord),
  };
}

export const trnxCodec = codec<TrnxRecord>(trnxLayout);

/** `KEYS(32 0)`: card number followed by transaction id. */
export const trnxKey = (record: TrnxRecord): string =>
  `${padKey(record.trnxCardNum, 16)}${padKey(record.trnxId, 16)}`;

export function toTrnxRecord(transaction: TransactionRecord): TrnxRecord {
  return {
    trnxCardNum: transaction.tranCardNum,
    trnxId: transaction.tranId,
    trnxTypeCd: transaction.tranTypeCd,
    trnxCatCd: transaction.tranCatCd,
    trnxSource: transaction.tranSource,
    trnxDesc: transaction.tranDesc,
    trnxAmt: transaction.tranAmt,
    trnxMerchantId: transaction.tranMerchantId,
    trnxMerchantName: transaction.tranMerchantName,
    trnxMerchantCity: transaction.tranMerchantCity,
    trnxMerchantZip: transaction.tranMerchantZip,
    trnxOrigTs: transaction.tranOrigTs,
    trnxProcTs: transaction.tranProcTs,
  };
}
