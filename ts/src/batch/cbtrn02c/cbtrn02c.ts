/**
 * CBTRN02C: post the daily transaction file (POSTTRAN step 15).
 * DALYTRAN (sequential) -> validate against XREFFILE/ACCTFILE -> post to
 * TRANFILE + TCATBALF + ACCTFILE, or write DALYREJS (350-byte input + 80-byte trailer).
 */
import { Decimal } from '../../common/codecs/index.js';
import { AccountRecordLayout, DalytranRecordLayout, TranCatBalRecordLayout, TranRecordLayout } from '../../common/copybooks/index.js';
import { SequentialFile, type SequentialOrganization } from '../../common/io/sequential.js';
import { type IndexedFile, type VsamCatalog } from '../../common/vsam/index.js';
import { type BatchContext, type BatchOptions, type BatchResult, db2FormatTimestamp, isInvalidKey, runBatch, PrimeKeyOnlyIndexedFile, slice, truncateTo } from '../cbtrn-common/index.js';

export interface Cbtrn02cOptions extends BatchOptions {
  /** Catalog holding TRANFILE, XREFFILE, ACCTFILE and TCATBALF (e.g. `loadSeedCatalog().catalog`). */
  catalog: VsamCatalog;
  /** DALYTRAN DD: daily transaction file path. */
  dalytran: string;
  /** Default 'LINE' (the ASCII seed `dailytran.txt`); 'FIXED' for RECFM=FB images. */
  dalytranOrganization?: SequentialOrganization;
  /** DALYREJS DD: reject file path (430-byte records). */
  dalyrejs: string;
  /** Default 'LINE'. */
  dalyrejsOrganization?: SequentialOrganization;
  /** FUNCTION CURRENT-DATE source (TRAN-PROC-TS). Default: wall clock. */
  now?: () => Date;
}

export interface Cbtrn02cResult extends BatchResult {
  transactionCount: number;
  rejectCount: number;
}

export const REJECT_RECORD_LENGTH = 430;

export const ValidationFailure = {
  INVALID_CARD: { reason: 100, desc: 'INVALID CARD NUMBER FOUND' },
  ACCOUNT_NOT_FOUND: { reason: 101, desc: 'ACCOUNT RECORD NOT FOUND' },
  OVERLIMIT: { reason: 102, desc: 'OVERLIMIT TRANSACTION' },
  EXPIRED: { reason: 103, desc: 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION' },
} as const;

export function cbtrn02c(opts: Cbtrn02cOptions): Cbtrn02cResult {
  let transactionCount = 0;
  let rejectCount = 0;
  const result = runBatch(opts, (ctx) => {
    const now = opts.now ?? (() => new Date());
    ctx.display('START OF EXECUTION OF PROGRAM CBTRN02C');
    const dalytran = ctx.track(new SequentialFile(opts.dalytran, { organization: opts.dalytranOrganization ?? 'LINE', recordLength: 350 }));
    const tranFile = ctx.track(new PrimeKeyOnlyIndexedFile(opts.catalog, 'TRANFILE', { access: 'RANDOM' }));
    const xrefFile = ctx.track(new PrimeKeyOnlyIndexedFile(opts.catalog, 'XREFFILE', { access: 'RANDOM' }));
    const dalyrejs = ctx.track(
      new SequentialFile(opts.dalyrejs, { organization: opts.dalyrejsOrganization ?? 'LINE', recordLength: REJECT_RECORD_LENGTH }),
    );
    const acctFile = ctx.track(new PrimeKeyOnlyIndexedFile(opts.catalog, 'ACCTFILE', { access: 'RANDOM' }));
    const tcatbalFile = ctx.track(new PrimeKeyOnlyIndexedFile(opts.catalog, 'TCATBALF', { access: 'RANDOM' }));

    open(ctx, dalytran.open('INPUT').status, 'ERROR OPENING DALYTRAN');
    open(ctx, tranFile.open('OUTPUT').status, 'ERROR OPENING TRANSACTION FILE');
    open(ctx, xrefFile.open('INPUT').status, 'ERROR OPENING CROSS REF FILE');
    open(ctx, dalyrejs.open('OUTPUT').status, 'ERROR OPENING DALY REJECTS FILE');
    open(ctx, acctFile.open('I-O').status, 'ERROR OPENING ACCOUNT MASTER FILE');
    open(ctx, tcatbalFile.open('I-O').status, 'ERROR OPENING TRANSACTION BALANCE FILE');

    // Working storage (persists across records, as in COBOL).
    let dalytranRec: Buffer = DalytranRecordLayout.serialize({});
    let xrefRec: Buffer = Buffer.alloc(50, 0x20);
    let acctRec: Buffer = AccountRecordLayout.serialize({});
    const tranRecFiller = slice(TranRecordLayout.serialize({}), 330, 20);
    const ws = { tcatbalRec: TranCatBalRecordLayout.serialize({}) };
    let endOfFile = false;

    while (!endOfFile) {
      // 1000-DALYTRAN-GET-NEXT
      const r = dalytran.read();
      if (r.status === '00') dalytranRec = r.record!;
      else if (r.status === '10') endOfFile = true;
      else ctx.fail('ERROR READING DALYTRAN FILE', r.status);
      if (endOfFile) break;

      transactionCount++;
      const daly = DalytranRecordLayout.parse(dalytranRec);
      let failure: { reason: number; desc: string } | undefined;

      // 1500-A-LOOKUP-XREF
      const x = xrefFile.read(daly.dalytranCardNum);
      if (isInvalidKey(x.status)) failure = ValidationFailure.INVALID_CARD;
      else if (x.record) xrefRec = x.record;

      // 1500-B-LOOKUP-ACCT
      if (!failure) {
        const a = acctFile.read(slice(xrefRec, 25, 11));
        if (isInvalidKey(a.status)) {
          failure = ValidationFailure.ACCOUNT_NOT_FOUND;
        } else {
          if (a.record) acctRec = a.record;
          const acct = AccountRecordLayout.parse(acctRec);
          const tempBal = truncateTo(acct.acctCurrCycCredit.minus(acct.acctCurrCycDebit).plus(daly.dalytranAmt), 9, 2);
          if (!acct.acctCreditLimit.greaterThanOrEqualTo(tempBal)) failure = ValidationFailure.OVERLIMIT;
          if (!(acct.acctExpiraionDate >= daly.dalytranOrigTs.slice(0, 10))) failure = ValidationFailure.EXPIRED;
        }
      }

      if (!failure) {
        postTransaction(ctx, ws, { dalytranRec, xrefRec, acctRec, tranRecFiller, procTs: db2FormatTimestamp(now()), tranFile, acctFile, tcatbalFile });
      } else {
        rejectCount++;
        // 2500-WRITE-REJECT-REC
        const trailer = String(failure.reason).padStart(4, '0') + failure.desc.padEnd(76);
        const w = dalyrejs.write(Buffer.concat([dalytranRec, Buffer.from(trailer, 'latin1')]));
        if (w.status !== '00') ctx.fail('ERROR WRITING TO REJECTS FILE', w.status);
      }
    }

    close(ctx, dalytran.close().status, 'ERROR CLOSING DALYTRAN FILE');
    close(ctx, tranFile.close().status, 'ERROR CLOSING TRANSACTION FILE');
    close(ctx, xrefFile.close().status, 'ERROR CLOSING CROSS REF FILE');
    close(ctx, dalyrejs.close().status, 'ERROR CLOSING DAILY REJECTS FILE');
    close(ctx, acctFile.close().status, 'ERROR CLOSING ACCOUNT FILE');
    close(ctx, tcatbalFile.close().status, 'ERROR CLOSING TRANSACTION BALANCE FILE');
    ctx.display('TRANSACTIONS PROCESSED :', String(transactionCount).padStart(9, '0'));
    ctx.display('TRANSACTIONS REJECTED  :', String(rejectCount).padStart(9, '0'));
    const rc = rejectCount > 0 ? 4 : 0;
    ctx.display('END OF EXECUTION OF PROGRAM CBTRN02C');
    return rc;
  });
  return { ...result, transactionCount, rejectCount };
}

const open = (ctx: BatchContext, status: string, message: string): void => {
  if (status !== '00') ctx.fail(message, status);
};
const close = open;

interface PostInput {
  dalytranRec: Buffer;
  xrefRec: Buffer;
  acctRec: Buffer;
  tranRecFiller: string;
  procTs: string;
  tranFile: IndexedFile;
  acctFile: IndexedFile;
  tcatbalFile: IndexedFile;
}

/** 2000-POST-TRANSACTION: 2700-UPDATE-TCATBAL, 2800-UPDATE-ACCOUNT-REC, 2900-WRITE-TRANSACTION-FILE. */
function postTransaction(ctx: BatchContext, ws: { tcatbalRec: Buffer }, p: PostInput): void {
  const daly = DalytranRecordLayout.parse(p.dalytranRec);
  const tranRec = TranRecordLayout.serialize({
    tranId: daly.dalytranId,
    tranTypeCd: daly.dalytranTypeCd,
    tranCatCd: daly.dalytranCatCd,
    tranSource: daly.dalytranSource,
    tranDesc: daly.dalytranDesc,
    tranAmt: daly.dalytranAmt,
    tranMerchantId: daly.dalytranMerchantId,
    tranMerchantName: daly.dalytranMerchantName,
    tranMerchantCity: daly.dalytranMerchantCity,
    tranMerchantZip: daly.dalytranMerchantZip,
    tranCardNum: daly.dalytranCardNum,
    tranOrigTs: daly.dalytranOrigTs,
    tranProcTs: p.procTs,
    filler: p.tranRecFiller,
  });

  // 2700-UPDATE-TCATBAL
  const key = slice(p.xrefRec, 25, 11) + slice(p.dalytranRec, 16, 2) + slice(p.dalytranRec, 18, 4);
  const t = p.tcatbalFile.read(key);
  let create = false;
  if (isInvalidKey(t.status)) {
    ctx.display('TCATBAL record not found for key : ', key, '.. Creating.');
    create = true;
  }
  else if (t.record) ws.tcatbalRec = t.record;
  if (t.status !== '00' && t.status !== '23') ctx.fail('ERROR READING TRANSACTION BALANCE FILE', t.status);
  if (create) {
    // INITIALIZE skips FILLER: it keeps whatever the last successful READ INTO left there.
    const rec = TranCatBalRecordLayout.serialize({
      ...TranCatBalRecordLayout.parse(ws.tcatbalRec),
      tranCatKey: { trancatAcctId: slice(p.xrefRec, 25, 11), trancatTypeCd: daly.dalytranTypeCd, trancatCd: daly.dalytranCatCd },
      tranCatBal: new Decimal(0).plus(daly.dalytranAmt),
    });
    ws.tcatbalRec = rec;
    const w = p.tcatbalFile.write(rec);
    if (w.status !== '00') ctx.fail('ERROR WRITING TRANSACTION BALANCE FILE', w.status);
  } else {
    const bal = TranCatBalRecordLayout.parse(ws.tcatbalRec);
    ws.tcatbalRec = TranCatBalRecordLayout.serialize({ ...bal, tranCatBal: bal.tranCatBal.plus(daly.dalytranAmt) });
    const w = p.tcatbalFile.rewrite(ws.tcatbalRec);
    if (w.status !== '00') ctx.fail('ERROR REWRITING TRANSACTION BALANCE FILE', w.status);
  }

  // 2800-UPDATE-ACCOUNT-REC (REWRITE INVALID KEY only sets reason 109, which nothing reads)
  const acct = AccountRecordLayout.parse(p.acctRec);
  const amt = daly.dalytranAmt;
  const updated = {
    ...acct,
    acctCurrBal: acct.acctCurrBal.plus(amt),
    acctCurrCycCredit: amt.greaterThanOrEqualTo(0) ? acct.acctCurrCycCredit.plus(amt) : acct.acctCurrCycCredit,
    acctCurrCycDebit: amt.greaterThanOrEqualTo(0) ? acct.acctCurrCycDebit : acct.acctCurrCycDebit.plus(amt),
  };
  p.acctFile.rewrite(AccountRecordLayout.serialize(updated));

  // 2900-WRITE-TRANSACTION-FILE
  const w = p.tranFile.write(tranRec);
  if (w.status !== '00') ctx.fail('ERROR WRITING TO TRANSACTION FILE', w.status);
}
