/**
 * `CBACT04C` — interest calculator.
 *
 * Drives the transaction category balance file in key order, looks up the
 * account, its card cross-reference and the disclosure group interest rate,
 * writes one interest transaction per category balance and posts the accrued
 * interest back to the account master.
 *
 * File handling mirrors the COBOL: every I/O checks the file status the same
 * way the program does and raises {@link AbendError} where the COBOL performs
 * `9999-ABEND-PROGRAM`.
 */

import {
  discGroupKey,
  padKey,
  tranCatBalCodec,
  transactionCodec,
  type AccountRecord,
  type CardXrefRecord,
  type DiscGroupRecord,
  type TranCatBalRecord,
  type TransactionRecord,
} from "@carddemo/domain";
import {
  FileStatus,
  SequentialWriter,
  dataFiles,
  openAccountFile,
  openCardXrefFile,
  openDiscGroupFile,
  openTranCatBalFile,
  type FileStatusCode,
  type Ksds,
} from "@carddemo/vsam";

import { fromCents, monthlyInterestCents, toCents } from "./money.js";
import { db2FormatTimestamp } from "./timestamp.js";

/** `MOVE 999 TO ABCODE ... CALL 'CEE3ABD'`. */
export const ABEND_CODE = 999;

const DEFAULT_ACCT_GROUP_ID = "DEFAULT";
const INTEREST_TRAN_TYPE_CD = "01";
const INTEREST_TRAN_CAT_CD = 5;
const INTEREST_TRAN_SOURCE = "System";
const PARM_DATE_LENGTH = 10;
const TRAN_ID_SUFFIX_LENGTH = 6;

export class AbendError extends Error {
  readonly abendCode = ABEND_CODE;

  constructor(
    readonly fileName: string,
    readonly status: FileStatusCode,
  ) {
    super(`ABENDING PROGRAM: ${fileName} file status ${status}`);
    this.name = "AbendError";
  }
}

export interface InterestOptions {
  /** `PARM-DATE`, the 10 character job parameter prefixed to every `TRAN-ID`. */
  readonly parmDate: string;
  /** `ACCTFILE`, opened `I-O`: interest is posted back to this file. */
  readonly accountPath: string;
  /** `TRANSACT`, opened `OUTPUT`: the interest transactions written by the run. */
  readonly transactionPath: string;
  readonly tcatbalPath?: string;
  readonly xrefPath?: string;
  readonly discgrpPath?: string;
  /** `FUNCTION CURRENT-DATE`, injectable so transaction timestamps are testable. */
  readonly now?: () => Date;
  /** `DISPLAY`; defaults to standard output. */
  readonly display?: (line: string) => void;
}

export interface InterestSummary {
  /** `WS-RECORD-COUNT`: category balances read. */
  readonly recordCount: number;
  /** `WS-TRANID-SUFFIX`: interest transactions written. */
  readonly transactionCount: number;
  readonly accountsUpdated: number;
  readonly totalInterest: number;
}

export function runInterestCalculation(options: InterestOptions): InterestSummary {
  return new InterestCalculator(options).run();
}

interface OpenableFile {
  readonly name: string;
  openFile(): FileStatusCode;
}

interface ClosableFile {
  readonly name: string;
  close(): FileStatusCode;
}

class InterestCalculator {
  private readonly tranCatBalFile: Ksds<TranCatBalRecord>;
  private readonly xrefFile: Ksds<CardXrefRecord>;
  private readonly discGroupFile: Ksds<DiscGroupRecord>;
  private readonly accountFile: Ksds<AccountRecord>;
  private readonly transactionFile: SequentialWriter<TransactionRecord>;
  private readonly display: (line: string) => void;
  private readonly now: () => Date;
  private readonly parmDate: string;

  private endOfFile = false;
  private firstTime = true;
  private lastAcctNum = "";
  private totalInterestCents = 0n;
  private postedInterestCents = 0n;
  private recordCount = 0;
  private tranIdSuffix = 0;
  private accountsUpdated = 0;
  private account: AccountRecord | undefined;
  private xref: CardXrefRecord | undefined;

  constructor(options: InterestOptions) {
    if (options.parmDate.length > PARM_DATE_LENGTH) {
      throw new Error(`PARM-DATE exceeds PIC X(${PARM_DATE_LENGTH}): ${options.parmDate}`);
    }
    this.parmDate = padKey(options.parmDate, PARM_DATE_LENGTH);
    this.display = options.display ?? ((line: string): void => console.log(line));
    this.now = options.now ?? ((): Date => new Date());
    this.tranCatBalFile = openTranCatBalFile(options.tcatbalPath ?? dataFiles.tcatbal);
    this.xrefFile = openCardXrefFile(options.xrefPath ?? dataFiles.cardxref);
    this.discGroupFile = openDiscGroupFile(options.discgrpPath ?? dataFiles.discgrp);
    this.accountFile = openAccountFile(options.accountPath);
    this.transactionFile = new SequentialWriter({
      name: "TRANSACT",
      path: options.transactionPath,
      codec: transactionCodec,
    });
  }

  run(): InterestSummary {
    this.display("START OF EXECUTION OF PROGRAM CBACT04C");
    this.openInput(this.tranCatBalFile, "ERROR OPENING TRANSACTION CATEGORY BALANCE");
    this.openInput(this.xrefFile, "ERROR OPENING CROSS REF FILE");
    this.openInput(this.discGroupFile, "ERROR OPENING DALY REJECTS FILE");
    this.openInput(this.accountFile, "ERROR OPENING ACCOUNT MASTER FILE");
    this.openTransactionFile();
    this.tranCatBalFile.startBrowse();

    while (!this.endOfFile) {
      const balance = this.getNextTranCatBal();
      if (balance === undefined) {
        // End of file. As in the COBOL, the loop exits here, so the interest
        // accrued for the last account is never posted by 1050-UPDATE-ACCOUNT.
        continue;
      }

      this.recordCount += 1;
      const acctNum = padKey(balance.trancatAcctId, 11);
      if (acctNum !== this.lastAcctNum) {
        if (this.firstTime) {
          this.firstTime = false;
        } else {
          this.updateAccount();
        }
        this.totalInterestCents = 0n;
        this.lastAcctNum = acctNum;
        this.account = this.getAcctData(acctNum);
        this.xref = this.getXrefData(acctNum);
      }

      const discGroup = this.getInterestRate(this.requireAccount().acctGroupId, balance);
      if (discGroup.disIntRate !== 0) {
        this.computeInterest(balance, discGroup);
      }
    }

    this.closeFile(this.tranCatBalFile, "ERROR CLOSING TRANSACTION BALANCE FILE");
    this.closeFile(this.xrefFile, "ERROR CLOSING CROSS REF FILE");
    this.closeFile(this.discGroupFile, "ERROR CLOSING DISCLOSURE GROUP FILE");
    this.accountFile.save();
    this.closeFile(this.accountFile, "ERROR CLOSING ACCOUNT FILE");
    this.closeFile(this.transactionFile, "ERROR CLOSING TRANSACTION FILE");

    this.display("END OF EXECUTION OF PROGRAM CBACT04C");

    return {
      recordCount: this.recordCount,
      transactionCount: this.tranIdSuffix,
      accountsUpdated: this.accountsUpdated,
      totalInterest: fromCents(this.postedInterestCents),
    };
  }

  /** `0000`/`0100`/`0200`/`0300`: `OPEN INPUT` and `OPEN I-O`. */
  private openInput(file: OpenableFile, errorMessage: string): void {
    const status = file.openFile();
    if (status !== FileStatus.ok) {
      this.display(errorMessage);
      throw this.abend(file.name, status);
    }
  }

  /** `0400-TRANFILE-OPEN`: `OPEN OUTPUT`. */
  private openTransactionFile(): void {
    const status = this.transactionFile.openFile();
    if (status !== FileStatus.ok) {
      this.display("ERROR OPENING TRANSACTION FILE");
      throw this.abend(this.transactionFile.name, status);
    }
  }

  /** `1000-TCATBALF-GET-NEXT`; `undefined` once file status `10` is reported. */
  private getNextTranCatBal(): TranCatBalRecord | undefined {
    const result = this.tranCatBalFile.readNext();
    if (result.status === FileStatus.endOfFile) {
      this.endOfFile = true;
      return undefined;
    }
    if (result.status !== FileStatus.ok || result.record === undefined) {
      this.display("ERROR READING TRANSACTION CATEGORY FILE");
      throw this.abend(this.tranCatBalFile.name, result.status);
    }
    this.display(tranCatBalCodec.encode(result.record));
    return result.record;
  }

  /** `1050-UPDATE-ACCOUNT`: post the accrued interest and reset the cycle totals. */
  private updateAccount(): void {
    const account = this.requireAccount();
    const updated: AccountRecord = {
      ...account,
      acctCurrBal: fromCents(toCents(account.acctCurrBal) + this.totalInterestCents),
      acctCurrCycCredit: 0,
      acctCurrCycDebit: 0,
    };

    const status = this.accountFile.rewrite(updated);
    if (status !== FileStatus.ok) {
      this.display("ERROR RE-WRITING ACCOUNT FILE");
      throw this.abend(this.accountFile.name, status);
    }

    this.account = updated;
    this.accountsUpdated += 1;
    this.postedInterestCents += this.totalInterestCents;
  }

  /** `1100-GET-ACCT-DATA`. */
  private getAcctData(acctNum: string): AccountRecord {
    const result = this.accountFile.read(acctNum);
    if (result.status === FileStatus.notFound) {
      this.display(`ACCOUNT NOT FOUND: ${acctNum}`);
    }
    if (result.status !== FileStatus.ok || result.record === undefined) {
      this.display("ERROR READING ACCOUNT FILE");
      throw this.abend(this.accountFile.name, result.status);
    }
    return result.record;
  }

  /**
   * `1110-GET-XREF-DATA`: read on the `FD-XREF-ACCT-ID` alternate key. The
   * store is keyed on the card number, so the alternate key is resolved by
   * scanning in primary key order, which is the record a duplicate alternate
   * key would return on VSAM.
   */
  private getXrefData(acctNum: string): CardXrefRecord {
    const record = this.xrefFile
      .toArray()
      .find((candidate) => padKey(candidate.xrefAcctId, 11) === acctNum);
    if (record === undefined) {
      this.display(`ACCOUNT NOT FOUND: ${acctNum}`);
      this.display("ERROR READING XREF FILE");
      throw this.abend(this.xrefFile.name, FileStatus.notFound);
    }
    return record;
  }

  /** `1200-GET-INTEREST-RATE`, falling back to `1200-A-GET-DEFAULT-INT-RATE`. */
  private getInterestRate(acctGroupId: string, balance: TranCatBalRecord): DiscGroupRecord {
    const result = this.discGroupFile.read(this.discGroupKeyFor(acctGroupId, balance));
    if (result.status === FileStatus.notFound) {
      this.display("DISCLOSURE GROUP RECORD MISSING");
      this.display("TRY WITH DEFAULT GROUP CODE");
      return this.getDefaultInterestRate(balance);
    }
    if (result.status !== FileStatus.ok || result.record === undefined) {
      this.display("ERROR READING DISCLOSURE GROUP FILE");
      throw this.abend(this.discGroupFile.name, result.status);
    }
    return result.record;
  }

  private getDefaultInterestRate(balance: TranCatBalRecord): DiscGroupRecord {
    const result = this.discGroupFile.read(this.discGroupKeyFor(DEFAULT_ACCT_GROUP_ID, balance));
    if (result.status !== FileStatus.ok || result.record === undefined) {
      this.display("ERROR READING DEFAULT DISCLOSURE GROUP");
      throw this.abend(this.discGroupFile.name, result.status);
    }
    return result.record;
  }

  private discGroupKeyFor(acctGroupId: string, balance: TranCatBalRecord): string {
    return discGroupKey({
      disAcctGroupId: acctGroupId,
      disTranTypeCd: balance.trancatTypeCd,
      disTranCatCd: balance.trancatCd,
      disIntRate: 0,
    });
  }

  /** `1300-COMPUTE-INTEREST`. */
  private computeInterest(balance: TranCatBalRecord, discGroup: DiscGroupRecord): void {
    const monthlyInterestInCents = monthlyInterestCents(
      toCents(balance.tranCatBal),
      toCents(discGroup.disIntRate),
    );
    this.totalInterestCents += monthlyInterestInCents;
    this.writeTransaction(fromCents(monthlyInterestInCents));
  }

  /** `1300-B-WRITE-TX`. */
  private writeTransaction(monthlyInterest: number): void {
    this.tranIdSuffix += 1;
    const timestamp = db2FormatTimestamp(this.now());
    const record: TransactionRecord = {
      tranId: `${this.parmDate}${padKey(this.tranIdSuffix, TRAN_ID_SUFFIX_LENGTH)}`,
      tranTypeCd: INTEREST_TRAN_TYPE_CD,
      tranCatCd: INTEREST_TRAN_CAT_CD,
      tranSource: INTEREST_TRAN_SOURCE,
      tranDesc: `Int. for a/c ${padKey(this.requireAccount().acctId, 11)}`,
      tranAmt: monthlyInterest,
      tranMerchantId: 0,
      tranMerchantName: "",
      tranMerchantCity: "",
      tranMerchantZip: "",
      tranCardNum: this.requireXref().xrefCardNum,
      tranOrigTs: timestamp,
      tranProcTs: timestamp,
    };

    const status = this.transactionFile.write(record);
    if (status !== FileStatus.ok) {
      this.display("ERROR WRITING TRANSACTION RECORD");
      throw this.abend(this.transactionFile.name, status);
    }
  }

  private closeFile(file: ClosableFile, errorMessage: string): void {
    const status = file.close();
    if (status !== FileStatus.ok) {
      this.display(errorMessage);
      throw this.abend(file.name, status);
    }
  }

  private requireAccount(): AccountRecord {
    if (this.account === undefined) {
      throw new Error("account record is not in working storage");
    }
    return this.account;
  }

  private requireXref(): CardXrefRecord {
    if (this.xref === undefined) {
      throw new Error("card cross-reference record is not in working storage");
    }
    return this.xref;
  }

  /** `9910-DISPLAY-IO-STATUS` followed by `9999-ABEND-PROGRAM`. */
  private abend(fileName: string, status: FileStatusCode): AbendError {
    this.display(`FILE STATUS IS: NNNN00${status}`);
    this.display("ABENDING PROGRAM");
    return new AbendError(fileName, status);
  }
}
