/**
 * CBACT04C: interest calculator. Reads the transaction-category-balance KSDS
 * (TCATBALF) sequentially; per account it reads the account (ACCTFILE,
 * random) and the card xref by account id (XREFFILE alternate key), looks up
 * the disclosure-group interest rate (DISCGRP, falling back to group
 * 'DEFAULT'), writes one interest transaction per non-zero-rate category to
 * TRANSACT and finally REWRITEs the account with the accumulated interest.
 * Port of app/cbl/CBACT04C.cbl (JCL: app/jcl/INTCALC.jcl, PARM='2022071800').
 */
import { AccountRecordLayout, CardXrefRecordLayout } from '../../common/copybooks/account.js';
import { DisGroupRecordLayout, TranCatBalRecordLayout, TranRecordLayout } from '../../common/copybooks/transaction.js';
import { IndexedFile } from '../../common/vsam/cobol-file.js';
import { fit } from '../acctcust-common/ksds-print.js';
import {
  abendProgram,
  Decimal,
  displayIoStatus,
  OutputFile,
  resolveCatalog,
  runProgram,
  storeNumeric,
  type BatchRunOptions,
  type BatchRunResult,
  type Sysout,
} from '../acctcust-common/support.js';
import type { NumericInput } from '../../common/codecs/decimal.js';

export interface Cbact04cOptions extends BatchRunOptions {
  /** PARM-DATE (EXEC PARM, X(10)); default the INTCALC.jcl value '2022071800'. */
  parmDate?: string;
  /** TRANSACT path (ORGANIZATION SEQUENTIAL, fixed 350-byte records, no delimiters). Not written when omitted. */
  transactFile?: string;
  /** FUNCTION CURRENT-DATE source (local time); default `new Date()`. */
  now?: () => Date;
}

export interface Cbact04cResult extends BatchRunResult {
  /** TRANSACT records written (350 bytes each), in write order. */
  transactRecords: Buffer[];
  /** WS-RECORD-COUNT: TCATBAL records read. */
  recordCount: number;
}

/** 1300-COMPUTE-INTEREST: COMPUTE WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200, no ROUNDED. */
export function computeMonthlyInterest(tranCatBal: NumericInput, disIntRate: NumericInput): Decimal {
  const exact = new Decimal(tranCatBal).times(disIntRate).div(1200);
  return storeNumeric(exact.toDecimalPlaces(2, Decimal.ROUND_DOWN), 11, 2);
}

/** Z-GET-DB2-FORMAT-TIMESTAMP: YYYY-MM-DD-HH.MM.SS.hh0000 from FUNCTION CURRENT-DATE. */
export function db2FormatTimestamp(d: Date): string {
  const p = (n: number, w = 2) => String(n).padStart(w, '0');
  const hundredths = Math.floor(d.getMilliseconds() / 10);
  return `${p(d.getFullYear(), 4)}-${p(d.getMonth() + 1)}-${p(d.getDate())}-${p(d.getHours())}.${p(d.getMinutes())}.${p(d.getSeconds())}.${p(hundredths)}0000`;
}

export function runCbact04c(opts: Cbact04cOptions = {}): Cbact04cResult {
  const transactRecords: Buffer[] = [];
  let recordCount = 0;
  const parmDate = (opts.parmDate ?? '2022071800').padEnd(10, ' ').slice(0, 10);
  const now = opts.now ?? (() => new Date());
  const transact = new OutputFile(opts.transactFile, false);
  try {
    return runProgram(
      opts,
      (out) => {
        const catalog = resolveCatalog(opts);
        const tcatbal = new IndexedFile(catalog, 'TCATBALF', { access: 'SEQUENTIAL' });
        const xref = new IndexedFile(catalog, 'XREFFILE', { access: 'RANDOM' });
        const discgrp = new IndexedFile(catalog, 'DISCGRP', { access: 'RANDOM' });
        const acct = new IndexedFile(catalog, 'ACCTFILE', { access: 'RANDOM' });

        // WORKING-STORAGE record areas (READ ... INTO targets), GnuCOBOL-initialized.
        let tcbRec = TranCatBalRecordLayout.serialize({});
        let xrefRec = CardXrefRecordLayout.serialize({});
        let disRec = DisGroupRecordLayout.serialize({});
        let acctRec = AccountRecordLayout.serialize({});
        const tranRec = TranRecordLayout.serialize({});
        let wsLastAcctNum = ' '.repeat(11);
        let wsTotalInt = new Decimal(0);
        let wsFirstTime = true;
        let wsTranidSuffix = 0;

        out.display('START OF EXECUTION OF PROGRAM CBACT04C');
        openCheck(out, tcatbal.open('INPUT').status, 'ERROR OPENING TRANSACTION CATEGORY BALANCE');
        const xs = xref.open('INPUT').status;
        if (xs !== '00') fail(out, 'ERROR OPENING CROSS REF FILE', xs, xs);
        openCheck(out, discgrp.open('INPUT').status, 'ERROR OPENING DALY REJECTS FILE');
        openCheck(out, acct.open('I-O').status, 'ERROR OPENING ACCOUNT MASTER FILE');
        openCheck(out, transact.open(), 'ERROR OPENING TRANSACTION FILE');

        const updateAccount = () => {
          // 1050-UPDATE-ACCOUNT
          const a = AccountRecordLayout.parse(acctRec);
          acctRec = AccountRecordLayout.serialize({
            ...a,
            acctCurrBal: storeNumeric(a.acctCurrBal.plus(wsTotalInt), 12, 2),
            acctCurrCycCredit: 0,
            acctCurrCycDebit: 0,
          });
          const st = acct.rewrite(acctRec).status;
          if (st !== '00') fail(out, 'ERROR RE-WRITING ACCOUNT FILE', st);
        };

        const writeTx = (monthlyInt: Decimal) => {
          // 1300-B-WRITE-TX (STRING does not pad: unwritten bytes of TRAN-RECORD keep their content)
          wsTranidSuffix = (wsTranidSuffix + 1) % 1_000_000;
          const desc = 'Int. for a/c ' + acctRec.toString('latin1', 0, 11);
          const fields = TranRecordLayout.serialize({
            tranId: parmDate + String(wsTranidSuffix).padStart(6, '0'),
            tranTypeCd: '01',
            tranCatCd: 5,
            tranSource: 'System',
            tranAmt: monthlyInt,
            tranMerchantId: 0,
            tranCardNum: CardXrefRecordLayout.parse(xrefRec).xrefCardNum,
            tranOrigTs: db2FormatTimestamp(now()),
            tranProcTs: '',
          });
          const descRange = TranRecordLayout.range('TRAN-DESC');
          for (const name of ['TRAN-ID', 'TRAN-TYPE-CD', 'TRAN-CAT-CD', 'TRAN-SOURCE', 'TRAN-AMT', 'TRAN-MERCHANT-ID', 'TRAN-MERCHANT-NAME', 'TRAN-MERCHANT-CITY', 'TRAN-MERCHANT-ZIP', 'TRAN-CARD-NUM', 'TRAN-ORIG-TS']) {
            const r = TranRecordLayout.range(name);
            fields.copy(tranRec, r.offset, r.offset, r.offset + r.length);
          }
          tranRec.write(desc, descRange.offset, Math.min(desc.length, descRange.length), 'latin1');
          const ts = TranRecordLayout.range('TRAN-ORIG-TS');
          const proc = TranRecordLayout.range('TRAN-PROC-TS');
          tranRec.copy(tranRec, proc.offset, ts.offset, ts.offset + ts.length);
          const st = transact.write(tranRec);
          transactRecords.push(Buffer.from(tranRec));
          if (st !== '00') fail(out, 'ERROR WRITING TRANSACTION RECORD', st);
        };

        let endOfFile = false;
        while (!endOfFile) {
          // 1000-TCATBALF-GET-NEXT
          const r = tcatbal.readNext();
          if (r.status === '00') {
            tcbRec = fit(r.record!, TranCatBalRecordLayout.length);
          } else if (r.status === '10') {
            endOfFile = true;
          } else {
            fail(out, 'ERROR READING TRANSACTION CATEGORY FILE', r.status);
          }
          // The COBOL's `ELSE PERFORM 1050-UPDATE-ACCOUNT` belongs to the outer
          // `IF END-OF-FILE = 'N'`, which is always true inside PERFORM UNTIL
          // END-OF-FILE = 'Y': the last account is never rewritten (bug-for-bug).
          if (endOfFile) break;
          recordCount += 1;
          out.display(tcbRec);
          const tcb = TranCatBalRecordLayout.parse(tcbRec);
          const trancatAcctId = tcbRec.toString('latin1', 0, 11);
          if (trancatAcctId !== wsLastAcctNum) {
            if (!wsFirstTime) updateAccount();
            else wsFirstTime = false;
            wsTotalInt = new Decimal(0);
            wsLastAcctNum = trancatAcctId;
            // 1100-GET-ACCT-DATA
            const ar = acct.read(trancatAcctId);
            if (ar.status === '23') out.display('ACCOUNT NOT FOUND: ', trancatAcctId);
            else if (ar.record) acctRec = fit(ar.record, AccountRecordLayout.length);
            if (ar.status !== '00') fail(out, 'ERROR READING ACCOUNT FILE', ar.status);
            // 1110-GET-XREF-DATA (READ ... KEY IS FD-XREF-ACCT-ID)
            const xr = xref.read(trancatAcctId, 'CXACAIX');
            if (xr.status === '23') out.display('ACCOUNT NOT FOUND: ', trancatAcctId);
            else if (xr.record) xrefRec = fit(xr.record, CardXrefRecordLayout.length);
            if (xr.status !== '00') fail(out, 'ERROR READING XREF FILE', xr.status);
          }
          // 1200-GET-INTEREST-RATE
          const groupId = AccountRecordLayout.parse(acctRec).acctGroupId;
          const typeCd = tcb.tranCatKey.trancatTypeCd;
          const catCd = tcbRec.toString('latin1', 13, 17);
          const dr = discgrp.read(groupId.padEnd(10, ' ').slice(0, 10) + typeCd + catCd);
          if (dr.status === '23') {
            out.display('DISCLOSURE GROUP RECORD MISSING');
            out.display('TRY WITH DEFAULT GROUP CODE');
          } else if (dr.record) {
            disRec = fit(dr.record, DisGroupRecordLayout.length);
          }
          if (dr.status !== '00' && dr.status !== '23') fail(out, 'ERROR READING DISCLOSURE GROUP FILE', dr.status);
          if (dr.status === '23') {
            // 1200-A-GET-DEFAULT-INT-RATE
            const dd = discgrp.read('DEFAULT   ' + typeCd + catCd);
            if (dd.record) disRec = fit(dd.record, DisGroupRecordLayout.length);
            if (dd.status !== '00') fail(out, 'ERROR READING DEFAULT DISCLOSURE GROUP', dd.status);
          }
          const rate = DisGroupRecordLayout.parse(disRec).disIntRate;
          if (!rate.isZero()) {
            const monthly = computeMonthlyInterest(tcb.tranCatBal, rate);
            wsTotalInt = storeNumeric(wsTotalInt.plus(monthly), 11, 2);
            writeTx(monthly);
            // 1400-COMPUTE-FEES: "To be implemented" in the COBOL (empty paragraph).
          }
        }

        closeCheck(out, tcatbal.close().status, 'ERROR CLOSING TRANSACTION BALANCE FILE');
        closeCheck(out, xref.close().status, 'ERROR CLOSING CROSS REF FILE');
        closeCheck(out, discgrp.close().status, 'ERROR CLOSING DISCLOSURE GROUP FILE');
        closeCheck(out, acct.close().status, 'ERROR CLOSING ACCOUNT FILE');
        transact.close();
        out.display('END OF EXECUTION OF PROGRAM CBACT04C');
        return { transactRecords, recordCount };
      },
      () => ({ transactRecords, recordCount }),
    );
  } finally {
    transact.close();
  }
}

function fail(out: Sysout, message: string, status: string, suffix = ''): never {
  out.display(message, suffix);
  displayIoStatus(out, status);
  return abendProgram(out);
}

function openCheck(out: Sysout, status: string, message: string): void {
  if (status !== '00') fail(out, message, status);
}

const closeCheck = openCheck;
