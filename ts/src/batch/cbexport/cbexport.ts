/**
 * CBEXPORT: reads the customer, account, xref, transaction and card KSDS
 * files sequentially (key order) and writes one 500-byte CVEXPORT record per
 * input record to the indexed export file, in that file order, numbering the
 * records 1..n in EXPORT-SEQUENCE-NUM.
 */
import type { Charset } from '../../common/codecs/charset.js';
import { AccountRecordLayout, CardRecordLayout, CardXrefRecordLayout } from '../../common/copybooks/account.js';
import { CustomerRecordLayout } from '../../common/copybooks/customer.js';
import { type ExportRecord, ExportRecordLayout } from '../../common/copybooks/export.js';
import { TranRecordLayout } from '../../common/copybooks/transaction.js';
import type { RecordInput } from '../../common/record/layout.js';
import type { VsamCatalog } from '../../common/vsam/catalog.js';
import { IndexedFile } from '../../common/vsam/cobol-file.js';
import { acceptDateTime, EXPORT_DD, pic9, ProgramAbend } from './export-file.js';

export interface CbexportOptions {
  /** Catalog resolving the DDs CUSTFILE, ACCTFILE, XREFFILE, TRANSACT, CARDFILE (ASCII data) and EXPFILE. */
  catalog: VsamCatalog;
  /** Clock for ACCEPT DATE/TIME. Default: now. */
  now?: Date;
  /** Encoding of the display fields written to EXPFILE: 'ebcdic' gives the z/OS bytes. Default 'ascii'. */
  charset?: Charset;
  /** Receives each DISPLAY line as it is produced. */
  display?: (line: string) => void;
}

export interface CbexportStats {
  customers: number;
  accounts: number;
  xrefs: number;
  transactions: number;
  cards: number;
  total: number;
}

export interface CbexportResult {
  /** True when the run ended in 9999-ABEND-PROGRAM. */
  abended: boolean;
  /** SYSOUT DISPLAY lines. */
  displays: string[];
  stats: CbexportStats;
  /** Final WS-SEQUENCE-COUNTER. */
  sequence: number;
}

type HeaderFields = Pick<ExportRecord, 'exportRecType' | 'exportTimestamp' | 'exportSequenceNum' | 'exportBranchId' | 'exportRegionCode'>;
type ExportInput = RecordInput<typeof ExportRecordLayout.root.fields>;

interface Phase {
  dd: string;
  fileName: string;
  heading: string;
  countLabel: string;
  stat: keyof Omit<CbexportStats, 'total'>;
  build(rec: Buffer, header: Omit<HeaderFields, 'exportSequenceNum'> & { exportSequenceNum: number }): ExportInput;
}

const PHASES: readonly Phase[] = [
  {
    dd: 'CUSTFILE',
    fileName: 'CUSTOMER-INPUT',
    heading: 'customer records',
    countLabel: 'Customers exported',
    stat: 'customers',
    build(rec, header) {
      const c = CustomerRecordLayout.parse(rec);
      return {
        ...header,
        exportRecType: 'C',
        exportCustomerData: {
          expCustId: c.custId,
          expCustFirstName: c.custFirstName,
          expCustMiddleName: c.custMiddleName,
          expCustLastName: c.custLastName,
          expCustAddrLines: [{ expCustAddrLine: c.custAddrLine1 }, { expCustAddrLine: c.custAddrLine2 }, { expCustAddrLine: c.custAddrLine3 }],
          expCustAddrStateCd: c.custAddrStateCd,
          expCustAddrCountryCd: c.custAddrCountryCd,
          expCustAddrZip: c.custAddrZip,
          expCustPhoneNums: [{ expCustPhoneNum: c.custPhoneNum1 }, { expCustPhoneNum: c.custPhoneNum2 }],
          expCustSsn: c.custSsn,
          expCustGovtIssuedId: c.custGovtIssuedId,
          expCustDobYyyyMmDd: c.custDobYyyyMmDd,
          expCustEftAccountId: c.custEftAccountId,
          expCustPriCardHolderInd: c.custPriCardHolderInd,
          expCustFicoCreditScore: c.custFicoCreditScore,
        },
      };
    },
  },
  {
    dd: 'ACCTFILE',
    fileName: 'ACCOUNT-INPUT',
    heading: 'account records',
    countLabel: 'Accounts exported',
    stat: 'accounts',
    build(rec, header) {
      const a = AccountRecordLayout.parse(rec);
      return {
        ...header,
        exportRecType: 'A',
        exportAccountData: {
          expAcctId: a.acctId,
          expAcctActiveStatus: a.acctActiveStatus,
          expAcctCurrBal: a.acctCurrBal,
          expAcctCreditLimit: a.acctCreditLimit,
          expAcctCashCreditLimit: a.acctCashCreditLimit,
          expAcctOpenDate: a.acctOpenDate,
          expAcctExpiraionDate: a.acctExpiraionDate,
          expAcctReissueDate: a.acctReissueDate,
          expAcctCurrCycCredit: a.acctCurrCycCredit,
          expAcctCurrCycDebit: a.acctCurrCycDebit,
          expAcctAddrZip: a.acctAddrZip,
          expAcctGroupId: a.acctGroupId,
        },
      };
    },
  },
  {
    dd: 'XREFFILE',
    fileName: 'XREF-INPUT',
    heading: 'cross-reference records',
    countLabel: 'Cross-references exported',
    stat: 'xrefs',
    build(rec, header) {
      const x = CardXrefRecordLayout.parse(rec);
      return {
        ...header,
        exportRecType: 'X',
        exportCardXrefData: { expXrefCardNum: x.xrefCardNum, expXrefCustId: x.xrefCustId, expXrefAcctId: x.xrefAcctId },
      };
    },
  },
  {
    dd: 'TRANSACT',
    fileName: 'TRANSACTION-INPUT',
    heading: 'transaction records',
    countLabel: 'Transactions exported',
    stat: 'transactions',
    build(rec, header) {
      const t = TranRecordLayout.parse(rec);
      return {
        ...header,
        exportRecType: 'T',
        exportTransactionData: {
          expTranId: t.tranId,
          expTranTypeCd: t.tranTypeCd,
          expTranCatCd: t.tranCatCd,
          expTranSource: t.tranSource,
          expTranDesc: t.tranDesc,
          expTranAmt: t.tranAmt,
          expTranMerchantId: t.tranMerchantId,
          expTranMerchantName: t.tranMerchantName,
          expTranMerchantCity: t.tranMerchantCity,
          expTranMerchantZip: t.tranMerchantZip,
          expTranCardNum: t.tranCardNum,
          expTranOrigTs: t.tranOrigTs,
          expTranProcTs: t.tranProcTs,
        },
      };
    },
  },
  {
    dd: 'CARDFILE',
    fileName: 'CARD-INPUT',
    heading: 'card records',
    countLabel: 'Cards exported',
    stat: 'cards',
    build(rec, header) {
      const d = CardRecordLayout.parse(rec);
      return {
        ...header,
        exportRecType: 'D',
        exportCardData: {
          expCardNum: d.cardNum,
          expCardAcctId: d.cardAcctId,
          expCardCvvCd: d.cardCvvCd,
          expCardEmbossedName: d.cardEmbossedName,
          expCardExpiraionDate: d.cardExpiraionDate,
          expCardActiveStatus: d.cardActiveStatus,
        },
      };
    },
  },
];

/** WS-FORMATTED-TIMESTAMP: 'YYYY-MM-DD HH:MM:SS.00' in a PIC X(26). */
export function exportTimestamp(now: Date): { date: string; time: string; formatted: string } {
  const { date: d, time: t } = acceptDateTime(now);
  const date = `${d.slice(0, 4)}-${d.slice(4, 6)}-${d.slice(6, 8)}`;
  const time = `${t.slice(0, 2)}:${t.slice(2, 4)}:${t.slice(4, 6)}`;
  return { date, time, formatted: `${date} ${time}.00`.padEnd(26) };
}

export function runCbexport(opts: CbexportOptions): CbexportResult {
  const charset = opts.charset ?? 'ascii';
  const displays: string[] = [];
  const display = (line: string) => {
    displays.push(line);
    opts.display?.(line);
  };
  const stats: CbexportStats = { customers: 0, accounts: 0, xrefs: 0, transactions: 0, cards: 0, total: 0 };
  let sequence = 0;
  const abend = (): never => {
    display('CBEXPORT: ABENDING PROGRAM');
    throw new ProgramAbend('CBEXPORT');
  };

  const inputs = PHASES.map((p) => new IndexedFile(opts.catalog, p.dd, { access: 'SEQUENTIAL' }));
  const output = new IndexedFile(opts.catalog, EXPORT_DD, { access: 'SEQUENTIAL' });

  try {
    // 1000-INITIALIZE
    display('CBEXPORT: Starting Customer Data Export');
    const ts = exportTimestamp(opts.now ?? new Date());
    PHASES.forEach((p, i) => {
      const st = inputs[i]!.open('INPUT').status;
      if (st !== '00') {
        display(`ERROR: Cannot open ${p.fileName}, Status: ${st}`);
        abend();
      }
    });
    const ost = output.open('OUTPUT').status;
    if (ost !== '00') {
      display(`ERROR: Cannot open EXPORT-OUTPUT, Status: ${ost}`);
      abend();
    }
    display(`CBEXPORT: Export Date: ${ts.date}`);
    display(`CBEXPORT: Export Time: ${ts.time}`);

    // 2000 .. 5500: one pass per input file
    PHASES.forEach((p, i) => {
      const file = inputs[i]!;
      display(`CBEXPORT: Processing ${p.heading}`);
      const read = () => {
        const r = file.readNext();
        if (r.status !== '00' && r.status !== '10') {
          display(`ERROR: Reading ${p.fileName}, Status: ${r.status}`);
          abend();
        }
        return r;
      };
      for (let r = read(); r.status !== '10'; r = read()) {
        sequence++;
        const record = p.build(r.record!, {
          exportRecType: '',
          exportTimestamp: ts.formatted,
          exportSequenceNum: sequence,
          exportBranchId: '0001',
          exportRegionCode: 'NORTH',
        });
        const ws = output.write(ExportRecordLayout.serialize(record, { charset })).status;
        if (ws !== '00') {
          display(`ERROR: Writing export record, Status: ${ws}`);
          abend();
        }
        stats[p.stat]++;
        stats.total++;
      }
      display(`CBEXPORT: ${p.countLabel}: ${pic9(stats[p.stat])}`);
    });

    // 6000-FINALIZE
    for (const f of inputs) f.close();
    output.close();
    display('CBEXPORT: Export completed');
    display(`CBEXPORT: Customers Exported: ${pic9(stats.customers)}`);
    display(`CBEXPORT: Accounts Exported: ${pic9(stats.accounts)}`);
    display(`CBEXPORT: XRefs Exported: ${pic9(stats.xrefs)}`);
    display(`CBEXPORT: Transactions Exported: ${pic9(stats.transactions)}`);
    display(`CBEXPORT: Cards Exported: ${pic9(stats.cards)}`);
    display(`CBEXPORT: Total Records Exported: ${pic9(stats.total)}`);
    return { abended: false, displays, stats, sequence };
  } catch (e) {
    if (!(e instanceof ProgramAbend)) throw e;
    // Language Environment closes the files at abnormal termination.
    for (const f of [...inputs, output]) if (f.isOpen) f.close();
    return { abended: true, displays, stats, sequence };
  }
}
