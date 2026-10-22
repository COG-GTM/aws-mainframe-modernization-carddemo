/** CVACT01Y / CVACT02Y / CVACT03Y: account, card and card-xref master records. */
import { alnum, defineRecord, num, type RecordOf } from '../record/layout.js';

/** CVACT01Y ACCOUNT-RECORD (RECLN 300). Amounts are zoned decimal (no USAGE clause). */
export const AccountRecordLayout = defineRecord('ACCOUNT-RECORD', {
  acctId: num('ACCT-ID', '9(11)'),
  acctActiveStatus: alnum('ACCT-ACTIVE-STATUS', 'X(01)'),
  acctCurrBal: num('ACCT-CURR-BAL', 'S9(10)V99'),
  acctCreditLimit: num('ACCT-CREDIT-LIMIT', 'S9(10)V99'),
  acctCashCreditLimit: num('ACCT-CASH-CREDIT-LIMIT', 'S9(10)V99'),
  acctOpenDate: alnum('ACCT-OPEN-DATE', 'X(10)'),
  acctExpiraionDate: alnum('ACCT-EXPIRAION-DATE', 'X(10)'),
  acctReissueDate: alnum('ACCT-REISSUE-DATE', 'X(10)'),
  acctCurrCycCredit: num('ACCT-CURR-CYC-CREDIT', 'S9(10)V99'),
  acctCurrCycDebit: num('ACCT-CURR-CYC-DEBIT', 'S9(10)V99'),
  acctAddrZip: alnum('ACCT-ADDR-ZIP', 'X(10)'),
  acctGroupId: alnum('ACCT-GROUP-ID', 'X(10)'),
  filler: alnum('FILLER', 'X(178)'),
});
export type AccountRecord = RecordOf<typeof AccountRecordLayout.root.fields>;

/** CVACT02Y CARD-RECORD (RECLN 150). */
export const CardRecordLayout = defineRecord('CARD-RECORD', {
  cardNum: alnum('CARD-NUM', 'X(16)'),
  cardAcctId: num('CARD-ACCT-ID', '9(11)'),
  cardCvvCd: num('CARD-CVV-CD', '9(03)'),
  cardEmbossedName: alnum('CARD-EMBOSSED-NAME', 'X(50)'),
  cardExpiraionDate: alnum('CARD-EXPIRAION-DATE', 'X(10)'),
  cardActiveStatus: alnum('CARD-ACTIVE-STATUS', 'X(01)'),
  filler: alnum('FILLER', 'X(59)'),
});
export type CardRecord = RecordOf<typeof CardRecordLayout.root.fields>;

/** CVACT03Y CARD-XREF-RECORD (RECLN 50). */
export const CardXrefRecordLayout = defineRecord('CARD-XREF-RECORD', {
  xrefCardNum: alnum('XREF-CARD-NUM', 'X(16)'),
  xrefCustId: num('XREF-CUST-ID', '9(09)'),
  xrefAcctId: num('XREF-ACCT-ID', '9(11)'),
  filler: alnum('FILLER', 'X(14)'),
});
export type CardXrefRecord = RecordOf<typeof CardXrefRecordLayout.root.fields>;
