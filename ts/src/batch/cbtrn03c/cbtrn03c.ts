/**
 * CBTRN03C: daily transaction detail report (TRANREPT step 10).
 * Reads the date-filtered, card-sorted transaction file (see `tranreptSortInput`
 * for the STEP05R SORT), keyed-reads CARDXREF / TRANTYPE / TRANCATG and writes
 * 133-byte report lines with account, page and grand totals.
 */
import { type Decimal } from '../../common/codecs/index.js';
import {
  ReportAccountTotalsLayout,
  ReportGrandTotalsLayout,
  ReportNameHeaderLayout,
  ReportPageTotalsLayout,
  TranCatRecordLayout,
  TranRecordLayout,
  TranTypeRecordLayout,
  TransactionDetailReportLayout,
  TransactionHeader1Layout,
  TransactionHeader2Layout,
} from '../../common/copybooks/index.js';
import { SequentialFile, type SequentialOrganization } from '../../common/io/sequential.js';
import { type IndexedFile, type VsamCatalog } from '../../common/vsam/index.js';
import { type BatchContext, type BatchOptions, type BatchResult, displaySigned, isInvalidKey, runBatch, PrimeKeyOnlyIndexedFile, slice, truncateTo } from '../cbtrn-common/index.js';

export interface Cbtrn03cOptions extends BatchOptions {
  /** Catalog holding CARDXREF, TRANTYPE and TRANCATG. */
  catalog: VsamCatalog;
  /** TRANFILE DD: sequential posted transactions (350 bytes), normally sorted by card number. */
  tranfile: string;
  /** Default 'LINE'. */
  tranfileOrganization?: SequentialOrganization;
  /** DATEPARM DD: 80-byte record 'YYYY-MM-DD YYYY-MM-DD'. */
  dateparm: string;
  /** TRANREPT DD: report output (133-byte records). */
  tranrept: string;
  /** Default 'LINE' (full 133 bytes per line, not trimmed). */
  tranreptOrganization?: SequentialOrganization;
}

export const REPORT_LINE_LENGTH = 133;
export const PAGE_SIZE = 20;

export function cbtrn03c(opts: Cbtrn03cOptions): BatchResult {
  return runBatch(opts, (ctx) => {
    ctx.display('START OF EXECUTION OF PROGRAM CBTRN03C');
    const tranFile = ctx.track(new SequentialFile(opts.tranfile, { organization: opts.tranfileOrganization ?? 'LINE', recordLength: 350 }));
    const reptFile = ctx.track(
      new SequentialFile(opts.tranrept, { organization: opts.tranreptOrganization ?? 'LINE', recordLength: REPORT_LINE_LENGTH }),
    );
    const xrefFile = ctx.track(new PrimeKeyOnlyIndexedFile(opts.catalog, 'CARDXREF', { access: 'RANDOM' }));
    const tranTypeFile = ctx.track(new PrimeKeyOnlyIndexedFile(opts.catalog, 'TRANTYPE', { access: 'RANDOM' }));
    const tranCatgFile = ctx.track(new PrimeKeyOnlyIndexedFile(opts.catalog, 'TRANCATG', { access: 'RANDOM' }));
    const dateParmFile = ctx.track(new SequentialFile(opts.dateparm, { organization: 'LINE', recordLength: 80 }));

    check(ctx, tranFile.open('INPUT').status, 'ERROR OPENING TRANFILE');
    check(ctx, reptFile.open('OUTPUT').status, 'ERROR OPENING REPTFILE');
    check(ctx, xrefFile.open('INPUT').status, 'ERROR OPENING CROSS REF FILE');
    check(ctx, tranTypeFile.open('INPUT').status, 'ERROR OPENING TRANSACTION TYPE FILE');
    check(ctx, tranCatgFile.open('INPUT').status, 'ERROR OPENING TRANSACTION CATG FILE');
    check(ctx, dateParmFile.open('INPUT').status, 'ERROR OPENING DATE PARM FILE');

    let endOfFile = false;
    let startDate = ' '.repeat(10);
    let endDate = ' '.repeat(10);

    // 0550-DATEPARM-READ
    const p = dateParmFile.read();
    if (p.status === '00') {
      startDate = slice(p.record!, 0, 10);
      endDate = slice(p.record!, 11, 10);
      ctx.display('Reporting from ', startDate, ' to ', endDate);
    } else if (p.status === '10') {
      endOfFile = true;
    } else {
      ctx.fail('ERROR READING DATEPARM FILE', p.status);
    }

    // WS-REPORT-VARS and record areas
    let firstTime = true;
    let lineCounter = 0;
    let pageTotal: Decimal = truncateTo(0, 9, 2);
    let accountTotal: Decimal = truncateTo(0, 9, 2);
    let grandTotal: Decimal = truncateTo(0, 9, 2);
    let currCardNum = ' '.repeat(16);
    let tranRec: Buffer = TranRecordLayout.serialize({});
    let xrefRec: Buffer = Buffer.alloc(50, 0x20);
    const nameHeader = { reptStartDate: '', reptEndDate: '' };

    // MOVE <report line> TO FD-REPTFILE-REC (X(133)): shorter groups are space padded.
    const write = (line: Buffer | string): void => {
      const text = (typeof line === 'string' ? line : line.toString('latin1')).padEnd(REPORT_LINE_LENGTH).slice(0, REPORT_LINE_LENGTH);
      const w = reptFile.write(text);
      if (w.status !== '00') ctx.fail('ERROR WRITING REPTFILE', w.status);
    };
    const header2 = TransactionHeader2Layout.serialize({});
    const writePageTotals = (): void => {
      write(ReportPageTotalsLayout.serialize({ reptPageTotal: pageTotal }));
      grandTotal = truncateTo(grandTotal.plus(pageTotal), 9, 2);
      pageTotal = truncateTo(0, 9, 2);
      lineCounter++;
      write(header2);
      lineCounter++;
    };
    const writeAccountTotals = (): void => {
      write(ReportAccountTotalsLayout.serialize({ reptAccountTotal: accountTotal }));
      accountTotal = truncateTo(0, 9, 2);
      lineCounter++;
      write(header2);
      lineCounter++;
    };
    const writeHeaders = (): void => {
      write(ReportNameHeaderLayout.serialize(nameHeader));
      lineCounter++;
      write(' '.repeat(REPORT_LINE_LENGTH));
      lineCounter++;
      write(TransactionHeader1Layout.serialize({}));
      lineCounter++;
      write(header2);
      lineCounter++;
    };
    const addToTotals = (amt: Decimal): void => {
      pageTotal = truncateTo(pageTotal.plus(amt), 9, 2);
      accountTotal = truncateTo(accountTotal.plus(amt), 9, 2);
    };
    const lookup = (file: IndexedFile, key: string, message: string): Buffer => {
      const r = file.read(key);
      if (isInvalidKey(r.status)) {
        ctx.display(message, key);
        ctx.displayIoStatus('23');
        ctx.abend();
      }
      return r.record!;
    };

    while (!endOfFile) {
      // 1000-TRANFILE-GET-NEXT
      const r = tranFile.read();
      if (r.status === '00') tranRec = r.record!;
      else if (r.status === '10') endOfFile = true;
      else ctx.fail('ERROR READING TRANSACTION FILE', r.status);

      const procDate = slice(tranRec, 304, 10);
      // Bug-for-bug: ELSE NEXT SENTENCE leaves the whole PERFORM UNTIL (the
      // sentence ends at END-PERFORM.), so the first out-of-range record ends
      // the report without any totals.
      if (!(procDate >= startDate && procDate <= endDate)) break;

      const tran = TranRecordLayout.parse(tranRec);
      if (!endOfFile) {
        ctx.display(slice(tranRec, 0, 350));
        if (currCardNum !== tran.tranCardNum) {
          if (!firstTime) writeAccountTotals();
          currCardNum = tran.tranCardNum;
          xrefRec = lookup(xrefFile, tran.tranCardNum, 'INVALID CARD NUMBER : ');
        }
        const tranTypeRec = lookup(tranTypeFile, tran.tranTypeCd, 'INVALID TRANSACTION TYPE : ');
        const tranCatRec = lookup(tranCatgFile, slice(tranRec, 16, 6), 'INVALID TRAN CATG KEY : ');

        // 1100-WRITE-TRANSACTION-REPORT
        if (firstTime) {
          firstTime = false;
          nameHeader.reptStartDate = startDate;
          nameHeader.reptEndDate = endDate;
          writeHeaders();
        }
        if (lineCounter % PAGE_SIZE === 0) {
          writePageTotals();
          writeHeaders();
        }
        addToTotals(tran.tranAmt);
        // 1120-WRITE-DETAIL
        write(
          TransactionDetailReportLayout.serialize({
            tranReportTransId: tran.tranId,
            tranReportAccountId: slice(xrefRec, 25, 11),
            tranReportTypeCd: tran.tranTypeCd,
            tranReportTypeDesc: TranTypeRecordLayout.parse(tranTypeRec).tranTypeDesc,
            tranReportCatCd: tran.tranCatCd,
            tranReportCatDesc: TranCatRecordLayout.parse(tranCatRec).tranCatTypeDesc,
            tranReportSource: tran.tranSource,
            tranReportAmt: tran.tranAmt,
          }),
        );
        lineCounter++;
      } else {
        // Bug-for-bug: at EOF TRAN-RECORD still holds the last record, whose
        // amount is added a second time; the last account's totals are never written.
        ctx.display('TRAN-AMT ', displaySigned(tran.tranAmt, 9, 2));
        ctx.display('WS-PAGE-TOTAL', displaySigned(pageTotal, 9, 2));
        addToTotals(tran.tranAmt);
        writePageTotals();
        write(ReportGrandTotalsLayout.serialize({ reptGrandTotal: grandTotal }));
      }
    }

    check(ctx, tranFile.close().status, 'ERROR CLOSING POSTED TRANSACTION FILE');
    check(ctx, reptFile.close().status, 'ERROR CLOSING REPORT FILE');
    check(ctx, xrefFile.close().status, 'ERROR CLOSING CROSS REF FILE');
    check(ctx, tranTypeFile.close().status, 'ERROR CLOSING TRANSACTION TYPE FILE');
    check(ctx, tranCatgFile.close().status, 'ERROR CLOSING TRANSACTION CATG FILE');
    check(ctx, dateParmFile.close().status, 'ERROR CLOSING DATE PARM FILE');
    ctx.display('END OF EXECUTION OF PROGRAM CBTRN03C');
    return 0;
  });
}

function check(ctx: BatchContext, status: string, message: string): void {
  if (status !== '00') ctx.fail(message, status);
}

/**
 * TRANREPT STEP05R stand-in (DFSORT): INCLUDE TRAN-PROC-DT (305,10,CH) between
 * the dates, SORT FIELDS=(TRAN-CARD-NUM,A) with TRAN-CARD-NUM 263,16,ZD.
 * DFSORT keeps equal keys in input order (EQUALS); Array.prototype.sort is stable.
 */
export function tranreptSortInput(records: readonly Uint8Array[], startDate: string, endDate: string): Buffer[] {
  const zd = (rec: Uint8Array): bigint => {
    const digits = slice(rec, 262, 16).replace(/[^0-9]/g, '0');
    return BigInt(digits);
  };
  return records
    .filter((r) => {
      const d = slice(r, 304, 10);
      return d >= startDate && d <= endDate;
    })
    .map((r) => Buffer.from(r))
    .sort((a, b) => {
      const ka = zd(a);
      const kb = zd(b);
      return ka < kb ? -1 : ka > kb ? 1 : 0;
    });
}
