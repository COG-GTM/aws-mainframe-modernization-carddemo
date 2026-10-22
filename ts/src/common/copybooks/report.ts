/** CVTRA07Y: daily transaction report (DALYREPT) line layouts. */
import { alnum, defineRecord, edited, num, type RecordOf } from '../record/layout.js';

export const ReportNameHeaderLayout = defineRecord('REPORT-NAME-HEADER', {
  reptShortName: alnum('REPT-SHORT-NAME', 'X(38)', 'DALYREPT'),
  reptLongName: alnum('REPT-LONG-NAME', 'X(41)', 'Daily Transaction Report'),
  reptDateHeader: alnum('REPT-DATE-HEADER', 'X(12)', 'Date Range: '),
  reptStartDate: alnum('REPT-START-DATE', 'X(10)'),
  filler: alnum('FILLER', 'X(04)', ' to '),
  reptEndDate: alnum('REPT-END-DATE', 'X(10)'),
});

export const TransactionDetailReportLayout = defineRecord('TRANSACTION-DETAIL-REPORT', {
  tranReportTransId: alnum('TRAN-REPORT-TRANS-ID', 'X(16)'),
  filler: alnum('FILLER', 'X(01)'),
  tranReportAccountId: alnum('TRAN-REPORT-ACCOUNT-ID', 'X(11)'),
  filler2: alnum('FILLER', 'X(01)'),
  tranReportTypeCd: alnum('TRAN-REPORT-TYPE-CD', 'X(02)'),
  filler3: alnum('FILLER', 'X(01)', '-'),
  tranReportTypeDesc: alnum('TRAN-REPORT-TYPE-DESC', 'X(15)'),
  filler4: alnum('FILLER', 'X(01)'),
  tranReportCatCd: num('TRAN-REPORT-CAT-CD', '9(04)'),
  filler5: alnum('FILLER', 'X(01)', '-'),
  tranReportCatDesc: alnum('TRAN-REPORT-CAT-DESC', 'X(29)'),
  filler6: alnum('FILLER', 'X(01)'),
  tranReportSource: alnum('TRAN-REPORT-SOURCE', 'X(10)'),
  filler7: alnum('FILLER', 'X(04)'),
  tranReportAmt: edited('TRAN-REPORT-AMT', '-ZZZ,ZZZ,ZZZ.ZZ'),
  filler8: alnum('FILLER', 'X(02)'),
});
export type TransactionDetailReport = RecordOf<typeof TransactionDetailReportLayout.root.fields>;

export const TransactionHeader1Layout = defineRecord('TRANSACTION-HEADER-1', {
  filler: alnum('FILLER', 'X(17)', 'Transaction ID'),
  filler2: alnum('FILLER', 'X(12)', 'Account ID'),
  filler3: alnum('FILLER', 'X(19)', 'Transaction Type'),
  filler4: alnum('FILLER', 'X(35)', 'Tran Category'),
  filler5: alnum('FILLER', 'X(14)', 'Tran Source'),
  filler6: alnum('FILLER', 'X'),
  filler7: alnum('FILLER', 'X(16)', '        Amount'),
});

export const TransactionHeader2Layout = defineRecord('TRANSACTION-HEADER-2', {
  transactionHeader2: alnum('TRANSACTION-HEADER-2', 'X(133)', { all: '-' }),
});

export const ReportPageTotalsLayout = defineRecord('REPORT-PAGE-TOTALS', {
  filler: alnum('FILLER', 'X(11)', 'Page Total'),
  filler2: alnum('FILLER', 'X(86)', { all: '.' }),
  reptPageTotal: edited('REPT-PAGE-TOTAL', '+ZZZ,ZZZ,ZZZ.ZZ'),
});

export const ReportAccountTotalsLayout = defineRecord('REPORT-ACCOUNT-TOTALS', {
  filler: alnum('FILLER', 'X(13)', 'Account Total'),
  filler2: alnum('FILLER', 'X(84)', { all: '.' }),
  reptAccountTotal: edited('REPT-ACCOUNT-TOTAL', '+ZZZ,ZZZ,ZZZ.ZZ'),
});

export const ReportGrandTotalsLayout = defineRecord('REPORT-GRAND-TOTALS', {
  filler: alnum('FILLER', 'X(11)', 'Grand Total'),
  filler2: alnum('FILLER', 'X(86)', { all: '.' }),
  reptGrandTotal: edited('REPT-GRAND-TOTAL', '+ZZZ,ZZZ,ZZZ.ZZ'),
});
