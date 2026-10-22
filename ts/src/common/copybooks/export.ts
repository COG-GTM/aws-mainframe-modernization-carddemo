/**
 * CVEXPORT EXPORT-RECORD (RECLN 500): multi-record export file written by
 * CBEXPORT / read by CBIMPORT. Unlike the master files this layout really does
 * use COMP (big-endian binary) and COMP-3 fields; the seed file is EBCDIC.
 * EXPORT-REC-TYPE selects the REDEFINES view: C customer, A account,
 * X card xref, T transaction, D card.
 */
import { alnum, defineRecord, group, num, occurs, redefines, type RecordOf } from '../record/layout.js';

export const ExportRecordLayout = defineRecord('EXPORT-RECORD', {
  exportRecType: alnum('EXPORT-REC-TYPE', 'X(1)'),
  exportTimestamp: alnum('EXPORT-TIMESTAMP', 'X(26)'),
  exportTimestampR: redefines(
    'exportTimestamp',
    group('EXPORT-TIMESTAMP-R', {
      exportDate: alnum('EXPORT-DATE', 'X(10)'),
      exportDateTimeSep: alnum('EXPORT-DATE-TIME-SEP', 'X(1)'),
      exportTime: alnum('EXPORT-TIME', 'X(15)'),
    }),
  ),
  exportSequenceNum: num('EXPORT-SEQUENCE-NUM', '9(9)', { usage: 'COMP' }),
  exportBranchId: alnum('EXPORT-BRANCH-ID', 'X(4)'),
  exportRegionCode: alnum('EXPORT-REGION-CODE', 'X(5)'),
  exportRecordData: alnum('EXPORT-RECORD-DATA', 'X(460)'),
  exportCustomerData: redefines(
    'exportRecordData',
    group('EXPORT-CUSTOMER-DATA', {
      expCustId: num('EXP-CUST-ID', '9(09)', { usage: 'COMP' }),
      expCustFirstName: alnum('EXP-CUST-FIRST-NAME', 'X(25)'),
      expCustMiddleName: alnum('EXP-CUST-MIDDLE-NAME', 'X(25)'),
      expCustLastName: alnum('EXP-CUST-LAST-NAME', 'X(25)'),
      expCustAddrLines: occurs(3, group('EXP-CUST-ADDR-LINES', { expCustAddrLine: alnum('EXP-CUST-ADDR-LINE', 'X(50)') })),
      expCustAddrStateCd: alnum('EXP-CUST-ADDR-STATE-CD', 'X(02)'),
      expCustAddrCountryCd: alnum('EXP-CUST-ADDR-COUNTRY-CD', 'X(03)'),
      expCustAddrZip: alnum('EXP-CUST-ADDR-ZIP', 'X(10)'),
      expCustPhoneNums: occurs(2, group('EXP-CUST-PHONE-NUMS', { expCustPhoneNum: alnum('EXP-CUST-PHONE-NUM', 'X(15)') })),
      expCustSsn: num('EXP-CUST-SSN', '9(09)'),
      expCustGovtIssuedId: alnum('EXP-CUST-GOVT-ISSUED-ID', 'X(20)'),
      expCustDobYyyyMmDd: alnum('EXP-CUST-DOB-YYYY-MM-DD', 'X(10)'),
      expCustEftAccountId: alnum('EXP-CUST-EFT-ACCOUNT-ID', 'X(10)'),
      expCustPriCardHolderInd: alnum('EXP-CUST-PRI-CARD-HOLDER-IND', 'X(01)'),
      expCustFicoCreditScore: num('EXP-CUST-FICO-CREDIT-SCORE', '9(03)', { usage: 'COMP-3' }),
      filler: alnum('FILLER', 'X(134)'),
    }),
  ),
  exportAccountData: redefines(
    'exportRecordData',
    group('EXPORT-ACCOUNT-DATA', {
      expAcctId: num('EXP-ACCT-ID', '9(11)'),
      expAcctActiveStatus: alnum('EXP-ACCT-ACTIVE-STATUS', 'X(01)'),
      expAcctCurrBal: num('EXP-ACCT-CURR-BAL', 'S9(10)V99', { usage: 'COMP-3' }),
      expAcctCreditLimit: num('EXP-ACCT-CREDIT-LIMIT', 'S9(10)V99'),
      expAcctCashCreditLimit: num('EXP-ACCT-CASH-CREDIT-LIMIT', 'S9(10)V99', { usage: 'COMP-3' }),
      expAcctOpenDate: alnum('EXP-ACCT-OPEN-DATE', 'X(10)'),
      expAcctExpiraionDate: alnum('EXP-ACCT-EXPIRAION-DATE', 'X(10)'),
      expAcctReissueDate: alnum('EXP-ACCT-REISSUE-DATE', 'X(10)'),
      expAcctCurrCycCredit: num('EXP-ACCT-CURR-CYC-CREDIT', 'S9(10)V99'),
      expAcctCurrCycDebit: num('EXP-ACCT-CURR-CYC-DEBIT', 'S9(10)V99', { usage: 'COMP' }),
      expAcctAddrZip: alnum('EXP-ACCT-ADDR-ZIP', 'X(10)'),
      expAcctGroupId: alnum('EXP-ACCT-GROUP-ID', 'X(10)'),
      filler: alnum('FILLER', 'X(352)'),
    }),
  ),
  exportTransactionData: redefines(
    'exportRecordData',
    group('EXPORT-TRANSACTION-DATA', {
      expTranId: alnum('EXP-TRAN-ID', 'X(16)'),
      expTranTypeCd: alnum('EXP-TRAN-TYPE-CD', 'X(02)'),
      expTranCatCd: num('EXP-TRAN-CAT-CD', '9(04)'),
      expTranSource: alnum('EXP-TRAN-SOURCE', 'X(10)'),
      expTranDesc: alnum('EXP-TRAN-DESC', 'X(100)'),
      expTranAmt: num('EXP-TRAN-AMT', 'S9(09)V99', { usage: 'COMP-3' }),
      expTranMerchantId: num('EXP-TRAN-MERCHANT-ID', '9(09)', { usage: 'COMP' }),
      expTranMerchantName: alnum('EXP-TRAN-MERCHANT-NAME', 'X(50)'),
      expTranMerchantCity: alnum('EXP-TRAN-MERCHANT-CITY', 'X(50)'),
      expTranMerchantZip: alnum('EXP-TRAN-MERCHANT-ZIP', 'X(10)'),
      expTranCardNum: alnum('EXP-TRAN-CARD-NUM', 'X(16)'),
      expTranOrigTs: alnum('EXP-TRAN-ORIG-TS', 'X(26)'),
      expTranProcTs: alnum('EXP-TRAN-PROC-TS', 'X(26)'),
      filler: alnum('FILLER', 'X(140)'),
    }),
  ),
  exportCardXrefData: redefines(
    'exportRecordData',
    group('EXPORT-CARD-XREF-DATA', {
      expXrefCardNum: alnum('EXP-XREF-CARD-NUM', 'X(16)'),
      expXrefCustId: num('EXP-XREF-CUST-ID', '9(09)'),
      expXrefAcctId: num('EXP-XREF-ACCT-ID', '9(11)', { usage: 'COMP' }),
      filler: alnum('FILLER', 'X(427)'),
    }),
  ),
  exportCardData: redefines(
    'exportRecordData',
    group('EXPORT-CARD-DATA', {
      expCardNum: alnum('EXP-CARD-NUM', 'X(16)'),
      expCardAcctId: num('EXP-CARD-ACCT-ID', '9(11)', { usage: 'COMP' }),
      expCardCvvCd: num('EXP-CARD-CVV-CD', '9(03)', { usage: 'COMP' }),
      expCardEmbossedName: alnum('EXP-CARD-EMBOSSED-NAME', 'X(50)'),
      expCardExpiraionDate: alnum('EXP-CARD-EXPIRAION-DATE', 'X(10)'),
      expCardActiveStatus: alnum('EXP-CARD-ACTIVE-STATUS', 'X(01)'),
      filler: alnum('FILLER', 'X(373)'),
    }),
  ),
});
export type ExportRecord = RecordOf<typeof ExportRecordLayout.root.fields>;

/** EXPORT-REC-TYPE values and the REDEFINES view each selects. */
export const EXPORT_REC_TYPE_VIEW = {
  C: 'exportCustomerData',
  A: 'exportAccountData',
  X: 'exportCardXrefData',
  T: 'exportTransactionData',
  D: 'exportCardData',
} as const;
