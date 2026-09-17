/**
 * Typed views over the copybook layouts, with decode/encode helpers per entity.
 */

import {
  decodeRecord,
  encodeRecord,
  type DecodedRecord,
  type Layout,
} from "@carddemo/copybook";

import {
  accountLayout,
  cardLayout,
  cardXrefLayout,
  customerLayout,
  dailyTransactionLayout,
  discGroupLayout,
  tranCatBalLayout,
  tranCatLayout,
  tranTypeLayout,
  transactionLayout,
  userSecurityLayout,
} from "./layouts.js";

export interface AccountRecord {
  acctId: number;
  acctActiveStatus: string;
  acctCurrBal: number;
  acctCreditLimit: number;
  acctCashCreditLimit: number;
  acctOpenDate: string;
  acctExpiraionDate: string;
  acctReissueDate: string;
  acctCurrCycCredit: number;
  acctCurrCycDebit: number;
  acctAddrZip: string;
  acctGroupId: string;
}

export interface CustomerRecord {
  custId: number;
  custFirstName: string;
  custMiddleName: string;
  custLastName: string;
  custAddrLine1: string;
  custAddrLine2: string;
  custAddrLine3: string;
  custAddrStateCd: string;
  custAddrCountryCd: string;
  custAddrZip: string;
  custPhoneNum1: string;
  custPhoneNum2: string;
  custSsn: number;
  custGovtIssuedId: string;
  custDobYyyyMmDd: string;
  custEftAccountId: string;
  custPriCardHolderInd: string;
  custFicoCreditScore: number;
}

export interface CardRecord {
  cardNum: string;
  cardAcctId: number;
  cardCvvCd: number;
  cardEmbossedName: string;
  cardExpiraionDate: string;
  cardActiveStatus: string;
}

export interface CardXrefRecord {
  xrefCardNum: string;
  xrefCustId: number;
  xrefAcctId: number;
}

export interface TransactionRecord {
  tranId: string;
  tranTypeCd: string;
  tranCatCd: number;
  tranSource: string;
  tranDesc: string;
  tranAmt: number;
  tranMerchantId: number;
  tranMerchantName: string;
  tranMerchantCity: string;
  tranMerchantZip: string;
  tranCardNum: string;
  tranOrigTs: string;
  tranProcTs: string;
}

export interface DailyTransactionRecord {
  dalytranId: string;
  dalytranTypeCd: string;
  dalytranCatCd: number;
  dalytranSource: string;
  dalytranDesc: string;
  dalytranAmt: number;
  dalytranMerchantId: number;
  dalytranMerchantName: string;
  dalytranMerchantCity: string;
  dalytranMerchantZip: string;
  dalytranCardNum: string;
  dalytranOrigTs: string;
  dalytranProcTs: string;
}

export interface TranCatBalRecord {
  trancatAcctId: number;
  trancatTypeCd: string;
  trancatCd: number;
  tranCatBal: number;
}

export interface DiscGroupRecord {
  disAcctGroupId: string;
  disTranTypeCd: string;
  disTranCatCd: number;
  disIntRate: number;
}

export interface TranTypeRecord {
  tranType: string;
  tranTypeDesc: string;
}

export interface TranCatRecord {
  tranTypeCd: string;
  tranCatCd: number;
  tranCatTypeDesc: string;
}

export interface UserSecurityRecord {
  secUsrId: string;
  secUsrFname: string;
  secUsrLname: string;
  secUsrPwd: string;
  secUsrType: string;
}

export interface RecordCodec<T> {
  readonly layout: Layout;
  decode(line: string): T;
  encode(record: T): string;
}

function codec<T>(layout: Layout): RecordCodec<T> {
  return {
    layout,
    decode: (line: string): T => decodeRecord(layout, line) as T,
    encode: (record: T): string => encodeRecord(layout, record as DecodedRecord),
  };
}

export const accountCodec = codec<AccountRecord>(accountLayout);
export const customerCodec = codec<CustomerRecord>(customerLayout);
export const cardCodec = codec<CardRecord>(cardLayout);
export const cardXrefCodec = codec<CardXrefRecord>(cardXrefLayout);
export const transactionCodec = codec<TransactionRecord>(transactionLayout);
export const dailyTransactionCodec = codec<DailyTransactionRecord>(dailyTransactionLayout);
export const tranCatBalCodec = codec<TranCatBalRecord>(tranCatBalLayout);
export const discGroupCodec = codec<DiscGroupRecord>(discGroupLayout);
export const tranTypeCodec = codec<TranTypeRecord>(tranTypeLayout);
export const tranCatCodec = codec<TranCatRecord>(tranCatLayout);
export const userSecurityCodec = codec<UserSecurityRecord>(userSecurityLayout);
