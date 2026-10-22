/**
 * CBIMPORT: reads the CVEXPORT export file sequentially (key order) and
 * splits it by EXPORT-REC-TYPE into the normalized customer, account, xref,
 * transaction and card files; unknown record types go to the error file.
 */
import { type Charset, spaceByte } from '../../common/codecs/charset.js';
import { AccountRecordLayout, CardRecordLayout, CardXrefRecordLayout } from '../../common/copybooks/account.js';
import { CustomerRecordLayout } from '../../common/copybooks/customer.js';
import { ExportRecordLayout } from '../../common/copybooks/export.js';
import { TranRecordLayout } from '../../common/copybooks/transaction.js';
import { SequentialFile } from '../../common/io/sequential.js';
import { alnum, defineRecord, num } from '../../common/record/layout.js';
import type { VsamCatalog } from '../../common/vsam/catalog.js';
import { IndexedFile } from '../../common/vsam/cobol-file.js';
import { FileStatus } from '../../common/vsam/status.js';
import { currentDate, EXPORT_DD, pic9, ProgramAbend } from '../cbexport/export-file.js';

/** Output DDs in OPEN order, with the FD file name and RECORD CONTAINS length. */
export const CBIMPORT_OUTPUTS = [
  { dd: 'CUSTOUT', fileName: 'CUSTOMER-OUTPUT', recordLength: 500 },
  { dd: 'ACCTOUT', fileName: 'ACCOUNT-OUTPUT', recordLength: 300 },
  { dd: 'XREFOUT', fileName: 'XREF-OUTPUT', recordLength: 50 },
  { dd: 'TRNXOUT', fileName: 'TRANSACTION-OUTPUT', recordLength: 350 },
  { dd: 'CARDOUT', fileName: 'CARD-OUTPUT', recordLength: 150 },
  { dd: 'ERROUT', fileName: 'ERROR-OUTPUT', recordLength: 132 },
] as const;
export type CbimportOutputDd = (typeof CBIMPORT_OUTPUTS)[number]['dd'];

/** WS-ERROR-RECORD (132 bytes). */
export const ErrorRecordLayout = defineRecord('WS-ERROR-RECORD', {
  errTimestamp: alnum('ERR-TIMESTAMP', 'X(26)'),
  filler1: alnum('FILLER', 'X(01)', '|'),
  errRecordType: alnum('ERR-RECORD-TYPE', 'X(01)'),
  filler2: alnum('FILLER', 'X(01)', '|'),
  errSequence: num('ERR-SEQUENCE', '9(07)'),
  filler3: alnum('FILLER', 'X(01)', '|'),
  errMessage: alnum('ERR-MESSAGE', 'X(50)'),
  filler4: alnum('FILLER', 'X(43)', ''),
});

export interface CbimportOptions {
  /** Catalog resolving EXPFILE. */
  catalog: VsamCatalog;
  /** Output DD name -> file path (FIXED record sequential). A missing DD fails its OPEN with status 35. */
  dd: Partial<Record<CbimportOutputDd, string>>;
  /** Encoding of the export file's display fields and of the output files. Default 'ascii'. */
  charset?: Charset;
  /** Clock for FUNCTION CURRENT-DATE. Default: now. */
  now?: Date;
  /** Receives each DISPLAY line as it is produced. */
  display?: (line: string) => void;
}

export interface CbimportStats {
  read: number;
  customers: number;
  accounts: number;
  xrefs: number;
  transactions: number;
  cards: number;
  errors: number;
  unknown: number;
}

export interface CbimportResult {
  abended: boolean;
  displays: string[];
  stats: CbimportStats;
}

/**
 * INITIALIZE skips FILLER, so the FD record's FILLER keeps whatever the record
 * area held; that is LOW-VALUES under GnuCOBOL (see README LIMITATIONS).
 */
function lowValues(layout: { range(name: string): { length: number } }): string {
  return '\0'.repeat(layout.range('filler').length);
}

type Stat = keyof Omit<CbimportStats, 'read' | 'errors' | 'unknown'>;

export function runCbimport(opts: CbimportOptions): CbimportResult {
  const charset = opts.charset ?? 'ascii';
  const now = () => opts.now ?? new Date();
  const displays: string[] = [];
  const display = (line: string) => {
    displays.push(line);
    opts.display?.(line);
  };
  const stats: CbimportStats = { read: 0, customers: 0, accounts: 0, xrefs: 0, transactions: 0, cards: 0, errors: 0, unknown: 0 };
  const abend = (): never => {
    display('CBIMPORT: ABENDING PROGRAM');
    throw new ProgramAbend('CBIMPORT');
  };

  const input = new IndexedFile(opts.catalog, EXPORT_DD, { access: 'SEQUENTIAL' });
  const outputs = new Map<CbimportOutputDd, SequentialFile | undefined>();
  for (const o of CBIMPORT_OUTPUTS) {
    const path = opts.dd[o.dd];
    outputs.set(o.dd, path === undefined ? undefined : new SequentialFile(path, { organization: 'FIXED', recordLength: o.recordLength, padByte: spaceByte(charset) }));
  }
  const out = (dd: CbimportOutputDd) => outputs.get(dd)!;

  const writeOrAbend = (dd: CbimportOutputDd, what: string, stat: Stat, record: Buffer) => {
    const st = out(dd).write(record).status;
    if (st !== '00') {
      display(`ERROR: Writing ${what} record, Status: ${st}`);
      abend();
    }
    stats[stat]++;
  };

  try {
    // 1000-INITIALIZE
    display('CBIMPORT: Starting Customer Data Import');
    const cd = currentDate(now());
    const importDate = `${cd.slice(0, 4)}-${cd.slice(4, 6)}-${cd.slice(6, 8)}`;
    const importTime = `${cd.slice(8, 10)}:${cd.slice(10, 12)}:${cd.slice(12, 14)}`;
    const ist = input.open('INPUT').status;
    if (ist !== '00') {
      display(`ERROR: Cannot open EXPORT-INPUT, Status: ${ist}`);
      abend();
    }
    for (const o of CBIMPORT_OUTPUTS) {
      const f = outputs.get(o.dd);
      const st = f ? f.open('OUTPUT').status : FileStatus.FILE_NOT_FOUND;
      if (st !== '00') {
        display(`ERROR: Cannot open ${o.fileName}, Status: ${st}`);
        abend();
      }
    }
    display(`CBIMPORT: Import Date: ${importDate}`);
    display(`CBIMPORT: Import Time: ${importTime}`);

    // 2000-PROCESS-EXPORT-FILE
    const read = () => {
      const r = input.readNext();
      if (r.status !== '00' && r.status !== '10') {
        display(`ERROR: Reading EXPORT-INPUT, Status: ${r.status}`);
        abend();
      }
      return r;
    };
    for (let r = read(); r.status !== '10'; r = read()) {
      stats.read++;
      const rec = r.record!;
      const type = ExportRecordLayout.parse(rec, { charset }).exportRecType;
      switch (type) {
        case 'C': {
          const v = ExportRecordLayout.parse(rec, { charset, views: ['exportCustomerData'] }).exportCustomerData!;
          const record = CustomerRecordLayout.serialize(
            {
              filler: lowValues(CustomerRecordLayout),
              custId: v.expCustId,
              custFirstName: v.expCustFirstName,
              custMiddleName: v.expCustMiddleName,
              custLastName: v.expCustLastName,
              custAddrLine1: v.expCustAddrLines[0]!.expCustAddrLine,
              custAddrLine2: v.expCustAddrLines[1]!.expCustAddrLine,
              custAddrLine3: v.expCustAddrLines[2]!.expCustAddrLine,
              custAddrStateCd: v.expCustAddrStateCd,
              custAddrCountryCd: v.expCustAddrCountryCd,
              custAddrZip: v.expCustAddrZip,
              custPhoneNum1: v.expCustPhoneNums[0]!.expCustPhoneNum,
              custPhoneNum2: v.expCustPhoneNums[1]!.expCustPhoneNum,
              custSsn: v.expCustSsn,
              custGovtIssuedId: v.expCustGovtIssuedId,
              custDobYyyyMmDd: v.expCustDobYyyyMmDd,
              custEftAccountId: v.expCustEftAccountId,
              custPriCardHolderInd: v.expCustPriCardHolderInd,
              custFicoCreditScore: v.expCustFicoCreditScore,
            },
            { charset },
          );
          writeOrAbend('CUSTOUT', 'customer', 'customers', record);
          break;
        }
        case 'A': {
          const v = ExportRecordLayout.parse(rec, { charset, views: ['exportAccountData'] }).exportAccountData!;
          const record = AccountRecordLayout.serialize(
            {
              filler: lowValues(AccountRecordLayout),
              acctId: v.expAcctId,
              acctActiveStatus: v.expAcctActiveStatus,
              acctCurrBal: v.expAcctCurrBal,
              acctCreditLimit: v.expAcctCreditLimit,
              acctCashCreditLimit: v.expAcctCashCreditLimit,
              acctOpenDate: v.expAcctOpenDate,
              acctExpiraionDate: v.expAcctExpiraionDate,
              acctReissueDate: v.expAcctReissueDate,
              acctCurrCycCredit: v.expAcctCurrCycCredit,
              acctCurrCycDebit: v.expAcctCurrCycDebit,
              acctAddrZip: v.expAcctAddrZip,
              acctGroupId: v.expAcctGroupId,
            },
            { charset },
          );
          writeOrAbend('ACCTOUT', 'account', 'accounts', record);
          break;
        }
        case 'X': {
          const v = ExportRecordLayout.parse(rec, { charset, views: ['exportCardXrefData'] }).exportCardXrefData!;
          const record = CardXrefRecordLayout.serialize(
            { filler: lowValues(CardXrefRecordLayout), xrefCardNum: v.expXrefCardNum, xrefCustId: v.expXrefCustId, xrefAcctId: v.expXrefAcctId },
            { charset },
          );
          writeOrAbend('XREFOUT', 'xref', 'xrefs', record);
          break;
        }
        case 'T': {
          const v = ExportRecordLayout.parse(rec, { charset, views: ['exportTransactionData'] }).exportTransactionData!;
          const record = TranRecordLayout.serialize(
            {
              filler: lowValues(TranRecordLayout),
              tranId: v.expTranId,
              tranTypeCd: v.expTranTypeCd,
              tranCatCd: v.expTranCatCd,
              tranSource: v.expTranSource,
              tranDesc: v.expTranDesc,
              tranAmt: v.expTranAmt,
              tranMerchantId: v.expTranMerchantId,
              tranMerchantName: v.expTranMerchantName,
              tranMerchantCity: v.expTranMerchantCity,
              tranMerchantZip: v.expTranMerchantZip,
              tranCardNum: v.expTranCardNum,
              tranOrigTs: v.expTranOrigTs,
              tranProcTs: v.expTranProcTs,
            },
            { charset },
          );
          writeOrAbend('TRNXOUT', 'transaction', 'transactions', record);
          break;
        }
        case 'D': {
          const v = ExportRecordLayout.parse(rec, { charset, views: ['exportCardData'] }).exportCardData!;
          const record = CardRecordLayout.serialize(
            {
              filler: lowValues(CardRecordLayout),
              cardNum: v.expCardNum,
              cardAcctId: v.expCardAcctId,
              cardCvvCd: v.expCardCvvCd,
              cardEmbossedName: v.expCardEmbossedName,
              cardExpiraionDate: v.expCardExpiraionDate,
              cardActiveStatus: v.expCardActiveStatus,
            },
            { charset },
          );
          writeOrAbend('CARDOUT', 'card', 'cards', record);
          break;
        }
        default: {
          // 2700-PROCESS-UNKNOWN-RECORD / 2750-WRITE-ERROR (a failed WRITE is reported, not fatal)
          stats.unknown++;
          const seq = ExportRecordLayout.parse(rec, { charset }).exportSequenceNum;
          const err = ErrorRecordLayout.serialize(
            { errTimestamp: currentDate(now()), errRecordType: type, errSequence: seq, errMessage: 'Unknown record type encountered' },
            { charset },
          );
          const st = out('ERROUT').write(err).status;
          if (st !== '00') display(`ERROR: Writing error record, Status: ${st}`);
          stats.errors++;
        }
      }
    }

    // 3000-VALIDATE-IMPORT
    display('CBIMPORT: Import validation completed');
    display('CBIMPORT: No validation errors detected');

    // 4000-FINALIZE
    input.close();
    for (const f of outputs.values()) f?.close();
    display('CBIMPORT: Import completed');
    display(`CBIMPORT: Total Records Read: ${pic9(stats.read)}`);
    display(`CBIMPORT: Customers Imported: ${pic9(stats.customers)}`);
    display(`CBIMPORT: Accounts Imported: ${pic9(stats.accounts)}`);
    display(`CBIMPORT: XRefs Imported: ${pic9(stats.xrefs)}`);
    display(`CBIMPORT: Transactions Imported: ${pic9(stats.transactions)}`);
    display(`CBIMPORT: Cards Imported: ${pic9(stats.cards)}`);
    display(`CBIMPORT: Errors Written: ${pic9(stats.errors)}`);
    display(`CBIMPORT: Unknown Record Types: ${pic9(stats.unknown)}`);
    return { abended: false, displays, stats };
  } catch (e) {
    if (!(e instanceof ProgramAbend)) throw e;
    if (input.isOpen) input.close();
    for (const f of outputs.values()) f?.close();
    return { abended: true, displays, stats };
  }
}
