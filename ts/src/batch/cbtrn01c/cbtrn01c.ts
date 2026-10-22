/**
 * CBTRN01C: daily transaction verification (DISPLAY only, no output files).
 * Reads DALYTRAN sequentially and, for each record, keyed-reads XREFFILE and
 * ACCTFILE. CUSTFILE, CARDFILE and TRANFILE are opened and closed but never read.
 */
import { DalytranRecordLayout } from '../../common/copybooks/index.js';
import { SequentialFile, type SequentialOrganization } from '../../common/io/sequential.js';
import { type VsamCatalog } from '../../common/vsam/index.js';
import { type BatchContext, type BatchOptions, type BatchResult, isInvalidKey, runBatch, PrimeKeyOnlyIndexedFile, slice } from '../cbtrn-common/index.js';

export interface Cbtrn01cOptions extends BatchOptions {
  /** Catalog holding CUSTFILE, XREFFILE, CARDFILE, ACCTFILE and TRANFILE. */
  catalog: VsamCatalog;
  /** DALYTRAN DD path. */
  dalytran: string;
  /** Default 'LINE' (the ASCII seed). */
  dalytranOrganization?: SequentialOrganization;
}

export function cbtrn01c(opts: Cbtrn01cOptions): BatchResult {
  return runBatch(opts, (ctx) => {
    ctx.display('START OF EXECUTION OF PROGRAM CBTRN01C');
    const dalytran = ctx.track(new SequentialFile(opts.dalytran, { organization: opts.dalytranOrganization ?? 'LINE', recordLength: 350 }));
    const keyed = (dd: string) => ctx.track(new PrimeKeyOnlyIndexedFile(opts.catalog, dd, { access: 'RANDOM' }));
    const custFile = keyed('CUSTFILE');
    const xrefFile = keyed('XREFFILE');
    const cardFile = keyed('CARDFILE');
    const acctFile = keyed('ACCTFILE');
    const tranFile = keyed('TRANFILE');

    check(ctx, dalytran.open('INPUT').status, 'ERROR OPENING DAILY TRANSACTION FILE');
    check(ctx, custFile.open('INPUT').status, 'ERROR OPENING CUSTOMER FILE');
    check(ctx, xrefFile.open('INPUT').status, 'ERROR OPENING CROSS REF FILE');
    check(ctx, cardFile.open('INPUT').status, 'ERROR OPENING CARD FILE');
    check(ctx, acctFile.open('INPUT').status, 'ERROR OPENING ACCOUNT FILE');
    check(ctx, tranFile.open('INPUT').status, 'ERROR OPENING TRANSACTION FILE');

    let dalytranRec: Buffer = DalytranRecordLayout.serialize({});
    const xrefRec: Buffer = Buffer.alloc(50, 0x20);
    let endOfFile = false;

    while (!endOfFile) {
      const r = dalytran.read();
      if (r.status === '00') dalytranRec = r.record!;
      else if (r.status === '10') endOfFile = true;
      else ctx.fail('ERROR READING DAILY TRANSACTION FILE', r.status);
      if (!endOfFile) ctx.display(slice(dalytranRec, 0, 350));
      // Bug-for-bug: the lookups also run after end of file, with the last record read.

      // 2000-LOOKUP-XREF
      const cardNum = slice(dalytranRec, 262, 16);
      xrefRec.write(cardNum, 0, 'latin1');
      const x = xrefFile.read(cardNum);
      if (isInvalidKey(x.status)) {
        ctx.display('INVALID CARD NUMBER FOR XREF');
        ctx.display('CARD NUMBER ', cardNum, ' COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-', slice(dalytranRec, 0, 16));
        continue;
      }
      x.record!.copy(xrefRec);
      ctx.display('SUCCESSFUL READ OF XREF');
      ctx.display('CARD NUMBER: ', slice(xrefRec, 0, 16));
      ctx.display('ACCOUNT ID : ', slice(xrefRec, 25, 11));
      ctx.display('CUSTOMER ID: ', slice(xrefRec, 16, 9));

      // 3000-READ-ACCOUNT
      const acctId = slice(xrefRec, 25, 11);
      const a = acctFile.read(acctId);
      if (isInvalidKey(a.status)) {
        ctx.display('INVALID ACCOUNT NUMBER FOUND');
        ctx.display('ACCOUNT ', acctId, ' NOT FOUND');
      } else {
        ctx.display('SUCCESSFUL READ OF ACCOUNT FILE');
      }
    }

    // Close messages as coded (9000-DALYTRAN-CLOSE says CUSTOMER FILE).
    check(ctx, dalytran.close().status, 'ERROR CLOSING CUSTOMER FILE');
    check(ctx, custFile.close().status, 'ERROR CLOSING CUSTOMER FILE');
    check(ctx, xrefFile.close().status, 'ERROR CLOSING CROSS REF FILE');
    check(ctx, cardFile.close().status, 'ERROR CLOSING CARD FILE');
    check(ctx, acctFile.close().status, 'ERROR CLOSING ACCOUNT FILE');
    check(ctx, tranFile.close().status, 'ERROR CLOSING TRANSACTION FILE');
    ctx.display('END OF EXECUTION OF PROGRAM CBTRN01C');
    return 0;
  });
}

function check(ctx: BatchContext, status: string, message: string): void {
  if (status !== '00') ctx.fail(message, status);
}
