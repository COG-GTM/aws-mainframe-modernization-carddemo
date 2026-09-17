/**
 * `CBSTM03A` — account statement generation.
 *
 * All file access goes through the {@link Cbstm03b} port, as in COBOL: the
 * transaction file is read sequentially into the `WS-TRNX-TABLE` two
 * dimensional table, the cross reference file drives the statement loop and the
 * customer and account files are read by key. Both outputs are produced in the
 * same pass: the `STMTFILE` plain text records (`PIC X(80)`) and the `HTMLFILE`
 * records (`PIC X(100)`).
 */

import {
  accountCodec,
  cardXrefCodec,
  customerCodec,
  type AccountRecord,
  type CardXrefRecord,
  type CustomerRecord,
} from "@carddemo/domain";
import { FileStatus } from "@carddemo/vsam";

import { AbendError } from "./abend.js";
import {
  M03bDdName,
  M03bOperation,
  newM03bArea,
  type Cbstm03b,
  type M03bArea,
} from "./cbstm03b.js";
import { addMoney } from "./money.js";
import { alphanumeric, delimitedBy, displayDigits, formatEdited } from "./picture.js";
import { trnxCodec, type TrnxRecord } from "./trnx.js";

/** `FD-STMTFILE-REC PIC X(80)`. */
export const STATEMENT_RECORD_LENGTH = 80;

/** `FD-HTMLFILE-REC PIC X(100)`. */
export const HTML_RECORD_LENGTH = 100;

/** `WS-CARD-TBL OCCURS 51 TIMES`. */
export const MAX_CARDS = 51;

/** `WS-TRAN-TBL OCCURS 10 TIMES`. */
export const MAX_TRANSACTIONS_PER_CARD = 10;

const CURRENT_BALANCE_PICTURE = "9(9).99-";
const TRAN_AMOUNT_PICTURE = "Z(9).99-";

export interface StatementOutput {
  /** `STMTFILE` records. */
  readonly statement: string[];
  /** `HTMLFILE` records. */
  readonly html: string[];
}

const statementRecord = (text: string): string => alphanumeric(text, STATEMENT_RECORD_LENGTH);
const htmlRecord = (text: string): string => alphanumeric(text, HTML_RECORD_LENGTH);

/** `HTML-FIXED-LN` condition names. */
const Html = {
  l01: "<!DOCTYPE html>",
  l02: '<html lang="en">',
  l03: "<head>",
  l04: '<meta charset="utf-8">',
  l05: "<title>HTML Table Layout</title>",
  l06: "</head>",
  l07: '<body style="margin:0px;">',
  l08: '<table  align="center" frame="box" style="width:70%; font:12px Segoe UI,sans-serif;">',
  trs: "<tr>",
  tre: "</tr>",
  tde: "</td>",
  l10: '<td colspan="3" style="padding:0px 5px;background-color:#1d1d96b3;">',
  l15: '<td colspan="3" style="padding:0px 5px;background-color:#FFAF33;">',
  l16: '<p style="font-size:16px">Bank of XYZ</p>',
  l17: "<p>410 Terry Ave N</p>",
  l18: "<p>Seattle WA 99999</p>",
  l22_35: '<td colspan="3" style="padding:0px 5px;background-color:#f2f2f2;">',
  l30_42: '<td colspan="3" style="padding:0px 5px;background-color:#33FFD1; text-align:center;">',
  l31: '<p style="font-size:16px">Basic Details</p>',
  l43: '<p style="font-size:16px">Transaction Summary</p>',
  l47: '<td style="width:25%; padding:0px 5px; background-color:#33FF5E; text-align:left;">',
  l48: '<p style="font-size:16px">Tran ID</p>',
  l50: '<td style="width:55%; padding:0px 5px; background-color:#33FF5E; text-align:left;">',
  l51: '<p style="font-size:16px">Tran Details</p>',
  l53: '<td style="width:20%; padding:0px 5px; background-color:#33FF5E; text-align:right;">',
  l54: '<p style="font-size:16px">Amount</p>',
  l58: '<td style="width:25%; padding:0px 5px; background-color:#f2f2f2; text-align:left;">',
  l61: '<td style="width:55%; padding:0px 5px; background-color:#f2f2f2; text-align:left;">',
  l64: '<td style="width:20%; padding:0px 5px; background-color:#f2f2f2; text-align:right;">',
  l75: "<h3>End of Statement</h3>",
  l78: "</table>",
  l79: "</body>",
  l80: "</html>",
} as const;

/** `ST-LINE0`. */
const ST_LINE0 = statementRecord(`${"*".repeat(31)}START OF STATEMENT${"*".repeat(31)}`);
/** `ST-LINE5`, `ST-LINE10` and `ST-LINE12`. */
const ST_RULE = statementRecord("-".repeat(80));
/** `ST-LINE6`. */
const ST_LINE6 = statementRecord(`${" ".repeat(33)}${alphanumeric("Basic Details", 14)}`);
/** `ST-LINE11`. */
const ST_LINE11 = statementRecord(`${" ".repeat(30)}TRANSACTION SUMMARY `);
/** `ST-LINE13`. */
const ST_LINE13 = statementRecord(
  `${alphanumeric("Tran ID", 16)}${alphanumeric("Tran Details", 51)}  Tran Amount`,
);
/** `ST-LINE15`. */
const ST_LINE15 = statementRecord(`${"*".repeat(32)}END OF STATEMENT${"*".repeat(32)}`);

/** One `WS-CARD-TBL` entry. */
interface CardTransactions {
  readonly cardNum: string;
  readonly transactions: TrnxRecord[];
}

class StatementWriter {
  readonly statement: string[] = [];
  readonly html: string[] = [];

  writeStatement(line: string): void {
    this.statement.push(statementRecord(line));
  }

  writeHtml(line: string): void {
    this.html.push(htmlRecord(line));
  }
}

function callIo(io: Cbstm03b, area: M03bArea): void {
  io.call(area);
}

/** `8100-TRNXFILE-OPEN` and `8500-READTRNX-READ`: loads `WS-TRNX-TABLE`. */
function readTransactionTable(io: Cbstm03b, area: M03bArea): CardTransactions[] {
  area.dd = M03bDdName.trnxFile;
  area.oper = M03bOperation.open;
  callIo(io, area);
  expectOk(area, "OPENING TRNXFILE");

  area.oper = M03bOperation.read;
  area.fldt = "";
  callIo(io, area);
  expectOk(area, "READING TRNXFILE");

  const cards: CardTransactions[] = [];
  let current: CardTransactions | undefined;

  for (;;) {
    const record = trnxCodec.decode(area.fldt);
    if (current === undefined || current.cardNum !== record.trnxCardNum) {
      current = { cardNum: record.trnxCardNum, transactions: [] };
      cards.push(current);
      if (cards.length > MAX_CARDS) {
        throw new AbendError(`WS-CARD-TBL holds ${String(MAX_CARDS)} cards`);
      }
    }
    current.transactions.push(record);
    if (current.transactions.length > MAX_TRANSACTIONS_PER_CARD) {
      throw new AbendError(
        `WS-TRAN-TBL holds ${String(MAX_TRANSACTIONS_PER_CARD)} transactions per card`,
      );
    }

    area.oper = M03bOperation.read;
    area.fldt = "";
    callIo(io, area);
    if (area.rc === FileStatus.endOfFile) {
      return cards;
    }
    if (area.rc !== FileStatus.ok) {
      throw new AbendError(`ERROR READING TRNXFILE, RETURN CODE: ${area.rc}`);
    }
  }
}

function expectOk(area: M03bArea, operation: string): void {
  if (area.rc !== FileStatus.ok && area.rc !== "04") {
    throw new AbendError(`ERROR ${operation}, RETURN CODE: ${area.rc}`);
  }
}

function openFile(io: Cbstm03b, area: M03bArea, dd: string, operation: string): void {
  area.dd = dd;
  area.oper = M03bOperation.open;
  callIo(io, area);
  expectOk(area, operation);
}

function closeFile(io: Cbstm03b, area: M03bArea, dd: string, operation: string): void {
  area.dd = dd;
  area.oper = M03bOperation.close;
  callIo(io, area);
  expectOk(area, operation);
}

/** `2000-CUSTFILE-GET` / `3000-ACCTFILE-GET`: `READ ... KEY` through the subroutine. */
function readByKey(io: Cbstm03b, area: M03bArea, dd: string, key: string, operation: string): void {
  area.dd = dd;
  area.oper = M03bOperation.readKeyed;
  area.key = alphanumeric(key, 25);
  area.keyLength = key.length;
  area.fldt = "";
  callIo(io, area);
  if (area.rc !== FileStatus.ok) {
    throw new AbendError(`ERROR READING ${operation}, RETURN CODE: ${area.rc}`);
  }
}

/** `5000-CREATE-STATEMENT` plus `5100-WRITE-HTML-HEADER` and `5200-WRITE-HTML-NMADBS`. */
function createStatement(
  writer: StatementWriter,
  customer: CustomerRecord,
  account: AccountRecord,
): void {
  writer.writeStatement(ST_LINE0);

  for (const line of [
    Html.l01,
    Html.l02,
    Html.l03,
    Html.l04,
    Html.l05,
    Html.l06,
    Html.l07,
    Html.l08,
    Html.trs,
    Html.l10,
  ]) {
    writer.writeHtml(line);
  }

  const acctId = alphanumeric(displayDigits(account.acctId, 11), 20);
  writer.writeHtml(`<h3>Statement for Account Number: ${acctId}</h3>`);
  for (const line of [
    Html.tde,
    Html.tre,
    Html.trs,
    Html.l15,
    Html.l16,
    Html.l17,
    Html.l18,
    Html.tde,
    Html.tre,
    Html.trs,
    Html.l22_35,
  ]) {
    writer.writeHtml(line);
  }

  const name = alphanumeric(
    `${delimitedBy(alphanumeric(customer.custFirstName, 25), " ")} ` +
      `${delimitedBy(alphanumeric(customer.custMiddleName, 25), " ")} ` +
      `${delimitedBy(alphanumeric(customer.custLastName, 25), " ")} `,
    75,
  );
  const addr1 = alphanumeric(customer.custAddrLine1, 50);
  const addr2 = alphanumeric(customer.custAddrLine2, 50);
  const addr3 = alphanumeric(
    `${delimitedBy(alphanumeric(customer.custAddrLine3, 50), " ")} ` +
      `${delimitedBy(alphanumeric(customer.custAddrStateCd, 2), " ")} ` +
      `${delimitedBy(alphanumeric(customer.custAddrCountryCd, 3), " ")} ` +
      `${delimitedBy(alphanumeric(customer.custAddrZip, 10), " ")} `,
    80,
  );
  const currBal = formatEdited(CURRENT_BALANCE_PICTURE, account.acctCurrBal);
  const ficoScore = alphanumeric(displayDigits(customer.custFicoCreditScore, 3), 20);

  writer.writeHtml(
    `<p style="font-size:16px">${delimitedBy(alphanumeric(name, 50), "  ")}  </p>`,
  );
  writer.writeHtml(`<p>${delimitedBy(addr1, "  ")}  </p>`);
  writer.writeHtml(`<p>${delimitedBy(addr2, "  ")}  </p>`);
  writer.writeHtml(`<p>${delimitedBy(addr3, "  ")}  </p>`);
  for (const line of [
    Html.tde,
    Html.tre,
    Html.trs,
    Html.l30_42,
    Html.l31,
    Html.tde,
    Html.tre,
    Html.trs,
    Html.l22_35,
  ]) {
    writer.writeHtml(line);
  }
  writer.writeHtml(`<p>Account ID         : ${acctId}</p>`);
  writer.writeHtml(`<p>Current Balance    : ${currBal}</p>`);
  writer.writeHtml(`<p>FICO Score         : ${ficoScore}</p>`);
  for (const line of [
    Html.tde,
    Html.tre,
    Html.trs,
    Html.l30_42,
    Html.l43,
    Html.tde,
    Html.tre,
    Html.trs,
    Html.l47,
    Html.l48,
    Html.tde,
    Html.l50,
    Html.l51,
    Html.tde,
    Html.l53,
    Html.l54,
    Html.tde,
    Html.tre,
  ]) {
    writer.writeHtml(line);
  }

  writer.writeStatement(name);
  writer.writeStatement(addr1);
  writer.writeStatement(addr2);
  writer.writeStatement(addr3);
  writer.writeStatement(ST_RULE);
  writer.writeStatement(ST_LINE6);
  writer.writeStatement(ST_RULE);
  writer.writeStatement(`Account ID         :${acctId}`);
  writer.writeStatement(`Current Balance    :${currBal}`);
  writer.writeStatement(`FICO Score         :${ficoScore}`);
  writer.writeStatement(ST_RULE);
  writer.writeStatement(ST_LINE11);
  writer.writeStatement(ST_RULE);
  writer.writeStatement(ST_LINE13);
  writer.writeStatement(ST_RULE);
}

/** `6000-WRITE-TRANS`. */
function writeTransaction(writer: StatementWriter, transaction: TrnxRecord): void {
  const tranId = alphanumeric(transaction.trnxId, 16);
  const tranDetails = alphanumeric(transaction.trnxDesc, 49);
  const tranAmount = formatEdited(TRAN_AMOUNT_PICTURE, transaction.trnxAmt);

  writer.writeStatement(`${tranId} ${tranDetails}$${tranAmount}`);

  writer.writeHtml(Html.trs);
  writer.writeHtml(Html.l58);
  writer.writeHtml(`<p>${tranId}</p>`);
  writer.writeHtml(Html.tde);
  writer.writeHtml(Html.l61);
  writer.writeHtml(`<p>${tranDetails}</p>`);
  writer.writeHtml(Html.tde);
  writer.writeHtml(Html.l64);
  writer.writeHtml(`<p>${tranAmount}</p>`);
  writer.writeHtml(Html.tde);
  writer.writeHtml(Html.tre);
}

/** `4000-TRNXFILE-GET`: the transactions of one card, then the statement trailer. */
function writeTransactions(
  writer: StatementWriter,
  cards: readonly CardTransactions[],
  cardNum: string,
): void {
  let total = 0;
  for (const card of cards) {
    if (card.cardNum > cardNum) {
      break;
    }
    if (card.cardNum === cardNum) {
      for (const transaction of card.transactions) {
        writeTransaction(writer, transaction);
        total = addMoney(total, transaction.trnxAmt);
      }
    }
  }

  writer.writeStatement(ST_RULE);
  writer.writeStatement(
    `Total EXP:${" ".repeat(56)}$${formatEdited(TRAN_AMOUNT_PICTURE, total)}`,
  );
  writer.writeStatement(ST_LINE15);

  for (const line of [
    Html.trs,
    Html.l10,
    Html.l75,
    Html.tde,
    Html.tre,
    Html.l78,
    Html.l79,
    Html.l80,
  ]) {
    writer.writeHtml(line);
  }
}

/**
 * Runs `CBSTM03A` over the files behind `io`, returning the two output files.
 */
export function generateStatements(io: Cbstm03b): StatementOutput {
  const area = newM03bArea();
  const writer = new StatementWriter();

  const cards = readTransactionTable(io, area);
  openFile(io, area, M03bDdName.xrefFile, "OPENING XREFFILE");
  openFile(io, area, M03bDdName.custFile, "OPENING CUSTFILE");
  openFile(io, area, M03bDdName.acctFile, "OPENING ACCTFILE");

  for (;;) {
    // 1000-XREFFILE-GET-NEXT
    area.dd = M03bDdName.xrefFile;
    area.oper = M03bOperation.read;
    area.fldt = "";
    callIo(io, area);
    if (area.rc === FileStatus.endOfFile) {
      break;
    }
    if (area.rc !== FileStatus.ok) {
      throw new AbendError(`ERROR READING XREFFILE, RETURN CODE: ${area.rc}`);
    }
    const xref: CardXrefRecord = cardXrefCodec.decode(area.fldt);

    readByKey(io, area, M03bDdName.custFile, displayDigits(xref.xrefCustId, 9), "CUSTFILE");
    const customer: CustomerRecord = customerCodec.decode(area.fldt);

    readByKey(io, area, M03bDdName.acctFile, displayDigits(xref.xrefAcctId, 11), "ACCTFILE");
    const account: AccountRecord = accountCodec.decode(area.fldt);

    createStatement(writer, customer, account);
    writeTransactions(writer, cards, xref.xrefCardNum);
  }

  closeFile(io, area, M03bDdName.trnxFile, "CLOSING TRNXFILE");
  closeFile(io, area, M03bDdName.xrefFile, "CLOSING XREFFILE");
  closeFile(io, area, M03bDdName.custFile, "CLOSING CUSTFILE");
  closeFile(io, area, M03bDdName.acctFile, "CLOSING ACCTFILE");

  return { statement: writer.statement, html: writer.html };
}
