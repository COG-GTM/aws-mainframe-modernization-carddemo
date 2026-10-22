/** CVTRA01Y-CVTRA06Y and COSTM01: transaction-related records. Amounts are zoned decimal. */
import { alnum, defineRecord, group, num, type RecordOf } from '../record/layout.js';

/** CVTRA01Y TRAN-CAT-BAL-RECORD (RECLN 50, key TRAN-CAT-KEY 17 bytes). */
export const TranCatBalRecordLayout = defineRecord('TRAN-CAT-BAL-RECORD', {
  tranCatKey: group('TRAN-CAT-KEY', {
    trancatAcctId: num('TRANCAT-ACCT-ID', '9(11)'),
    trancatTypeCd: alnum('TRANCAT-TYPE-CD', 'X(02)'),
    trancatCd: num('TRANCAT-CD', '9(04)'),
  }),
  tranCatBal: num('TRAN-CAT-BAL', 'S9(09)V99'),
  filler: alnum('FILLER', 'X(22)'),
});
export type TranCatBalRecord = RecordOf<typeof TranCatBalRecordLayout.root.fields>;

/** CVTRA02Y DIS-GROUP-RECORD (RECLN 50, key DIS-GROUP-KEY 16 bytes). */
export const DisGroupRecordLayout = defineRecord('DIS-GROUP-RECORD', {
  disGroupKey: group('DIS-GROUP-KEY', {
    disAcctGroupId: alnum('DIS-ACCT-GROUP-ID', 'X(10)'),
    disTranTypeCd: alnum('DIS-TRAN-TYPE-CD', 'X(02)'),
    disTranCatCd: num('DIS-TRAN-CAT-CD', '9(04)'),
  }),
  disIntRate: num('DIS-INT-RATE', 'S9(04)V99'),
  filler: alnum('FILLER', 'X(28)'),
});
export type DisGroupRecord = RecordOf<typeof DisGroupRecordLayout.root.fields>;

/** CVTRA03Y TRAN-TYPE-RECORD (RECLN 60). */
export const TranTypeRecordLayout = defineRecord('TRAN-TYPE-RECORD', {
  tranType: alnum('TRAN-TYPE', 'X(02)'),
  tranTypeDesc: alnum('TRAN-TYPE-DESC', 'X(50)'),
  filler: alnum('FILLER', 'X(08)'),
});
export type TranTypeRecord = RecordOf<typeof TranTypeRecordLayout.root.fields>;

/** CVTRA04Y TRAN-CAT-RECORD (RECLN 60, key TRAN-CAT-KEY 6 bytes). */
export const TranCatRecordLayout = defineRecord('TRAN-CAT-RECORD', {
  tranCatKey: group('TRAN-CAT-KEY', {
    tranTypeCd: alnum('TRAN-TYPE-CD', 'X(02)'),
    tranCatCd: num('TRAN-CAT-CD', '9(04)'),
  }),
  tranCatTypeDesc: alnum('TRAN-CAT-TYPE-DESC', 'X(50)'),
  filler: alnum('FILLER', 'X(04)'),
});
export type TranCatRecord = RecordOf<typeof TranCatRecordLayout.root.fields>;

/** CVTRA05Y TRAN-RECORD (RECLN 350). */
export const TranRecordLayout = defineRecord('TRAN-RECORD', {
  tranId: alnum('TRAN-ID', 'X(16)'),
  tranTypeCd: alnum('TRAN-TYPE-CD', 'X(02)'),
  tranCatCd: num('TRAN-CAT-CD', '9(04)'),
  tranSource: alnum('TRAN-SOURCE', 'X(10)'),
  tranDesc: alnum('TRAN-DESC', 'X(100)'),
  tranAmt: num('TRAN-AMT', 'S9(09)V99'),
  tranMerchantId: num('TRAN-MERCHANT-ID', '9(09)'),
  tranMerchantName: alnum('TRAN-MERCHANT-NAME', 'X(50)'),
  tranMerchantCity: alnum('TRAN-MERCHANT-CITY', 'X(50)'),
  tranMerchantZip: alnum('TRAN-MERCHANT-ZIP', 'X(10)'),
  tranCardNum: alnum('TRAN-CARD-NUM', 'X(16)'),
  tranOrigTs: alnum('TRAN-ORIG-TS', 'X(26)'),
  tranProcTs: alnum('TRAN-PROC-TS', 'X(26)'),
  filler: alnum('FILLER', 'X(20)'),
});
export type TranRecord = RecordOf<typeof TranRecordLayout.root.fields>;

/** CVTRA06Y DALYTRAN-RECORD (RECLN 350). */
export const DalytranRecordLayout = defineRecord('DALYTRAN-RECORD', {
  dalytranId: alnum('DALYTRAN-ID', 'X(16)'),
  dalytranTypeCd: alnum('DALYTRAN-TYPE-CD', 'X(02)'),
  dalytranCatCd: num('DALYTRAN-CAT-CD', '9(04)'),
  dalytranSource: alnum('DALYTRAN-SOURCE', 'X(10)'),
  dalytranDesc: alnum('DALYTRAN-DESC', 'X(100)'),
  dalytranAmt: num('DALYTRAN-AMT', 'S9(09)V99'),
  dalytranMerchantId: num('DALYTRAN-MERCHANT-ID', '9(09)'),
  dalytranMerchantName: alnum('DALYTRAN-MERCHANT-NAME', 'X(50)'),
  dalytranMerchantCity: alnum('DALYTRAN-MERCHANT-CITY', 'X(50)'),
  dalytranMerchantZip: alnum('DALYTRAN-MERCHANT-ZIP', 'X(10)'),
  dalytranCardNum: alnum('DALYTRAN-CARD-NUM', 'X(16)'),
  dalytranOrigTs: alnum('DALYTRAN-ORIG-TS', 'X(26)'),
  dalytranProcTs: alnum('DALYTRAN-PROC-TS', 'X(26)'),
  filler: alnum('FILLER', 'X(20)'),
});
export type DalytranRecord = RecordOf<typeof DalytranRecordLayout.root.fields>;

/** COSTM01 TRNX-RECORD (RECLN 350, key TRNX-KEY = card num + tran id, 32 bytes). */
export const TrnxRecordLayout = defineRecord('TRNX-RECORD', {
  trnxKey: group('TRNX-KEY', {
    trnxCardNum: alnum('TRNX-CARD-NUM', 'X(16)'),
    trnxId: alnum('TRNX-ID', 'X(16)'),
  }),
  trnxRest: group('TRNX-REST', {
    trnxTypeCd: alnum('TRNX-TYPE-CD', 'X(02)'),
    trnxCatCd: num('TRNX-CAT-CD', '9(04)'),
    trnxSource: alnum('TRNX-SOURCE', 'X(10)'),
    trnxDesc: alnum('TRNX-DESC', 'X(100)'),
    trnxAmt: num('TRNX-AMT', 'S9(09)V99'),
    trnxMerchantId: num('TRNX-MERCHANT-ID', '9(09)'),
    trnxMerchantName: alnum('TRNX-MERCHANT-NAME', 'X(50)'),
    trnxMerchantCity: alnum('TRNX-MERCHANT-CITY', 'X(50)'),
    trnxMerchantZip: alnum('TRNX-MERCHANT-ZIP', 'X(10)'),
    trnxOrigTs: alnum('TRNX-ORIG-TS', 'X(26)'),
    trnxProcTs: alnum('TRNX-PROC-TS', 'X(26)'),
    filler: alnum('FILLER', 'X(20)'),
  }),
});
export type TrnxRecord = RecordOf<typeof TrnxRecordLayout.root.fields>;
