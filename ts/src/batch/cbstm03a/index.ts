/**
 * CBSTM03A (CREASTMT STEP040): prints an account statement for every card in
 * XREFFILE, as fixed 80-byte text records (STMTFILE) and 100-byte HTML records
 * (HTMLFILE). All file I/O goes through the CBSTM03B subprogram (`M03bArea`).
 *
 * Control flow is a faithful paragraph state machine: 0000-START dispatches on
 * WS-FL-DD, `ALTER 8100-FILE-OPEN TO PROCEED TO ...` is the `alter8100` label
 * variable, and every `GO TO` returns the next paragraph label.
 *
 * Flow: open TRNXFILE and load every TRNX record into WS-TRNX-TABLE
 * (51 cards x 10 transactions, grouped on card number change), open
 * XREF/CUST/ACCT, then per XREF record: read customer and account, write the
 * statement header, list that card's transactions from the table and the total.
 */
import { writeLineRecords } from '../../common/io/sequential.js';
import { asciiToEbcdic } from '../../common/codecs/ebcdic.js';
import { type Decimal, fromScaledDigits, toScaledDigits } from '../../common/codecs/decimal.js';
import { formatEdited } from '../../common/codecs/edited.js';
import { AccountRecordLayout, CardXrefRecordLayout, CustrecLayout, TrnxRecordLayout } from '../../common/copybooks/index.js';
import type { AnyRecordLayout } from '../../common/record/layout.js';
import type { VsamCatalog } from '../../common/vsam/catalog.js';
import { Cbstm03b, M03B_OPER, type M03bArea, newM03bArea } from '../cbstm03b/index.js';
import { SIZE, cobolString, stringInto } from './cobol-string.js';
import { HTML, type HtmlFixedLine } from './html.js';

export { HTML } from './html.js';
export { buildTrnxFile, readTransact, sortTransactToTrnx, TRXFL_DSN } from './trnx-file.js';
export { cobolString, stringInto } from './cobol-string.js';

/** One TIOT DD entry: name and whether its UCB address is non-null. */
export interface TiotDd {
  ddName: string;
  /** Default true ("valid UCB"). */
  ucb?: boolean;
}

/** z/OS job context that the PSA -> TCB -> TIOT walk would find. */
export interface JobContext {
  jobName: string;
  stepName: string;
  /** TIOT DD entries in JCL order (at least one). */
  dds: readonly TiotDd[];
}

/** CREASTMT STEP040's DD statements (app/jcl/CREASTMT.JCL). UCB presence is not modelled: all are reported valid. */
export const CREASTMT_STEP040: JobContext = {
  jobName: 'CREASTMT',
  stepName: 'STEP040',
  dds: ['STEPLIB', 'SYSPRINT', 'SYSOUT', 'TRNXFILE', 'XREFFILE', 'ACCTFILE', 'CUSTFILE', 'STMTFILE', 'HTMLFILE'].map((ddName) => ({ ddName })),
};

export interface Cbstm03aOptions {
  /** Must resolve TRNXFILE (see `buildTrnxFile`), XREFFILE, CUSTFILE and ACCTFILE. */
  catalog: VsamCatalog;
  /** TIOT context for the opening DISPLAYs; `null` skips them. Default `CREASTMT_STEP040`. */
  job?: JobContext | null;
  /** The CALLed subprogram (default: a fresh `Cbstm03b` over `catalog`). */
  subprogram?: { call(area: M03bArea): void };
  /** Output paths (written as LINE SEQUENTIAL, one record per line, at normal end only). */
  stmtFile?: string;
  htmlFile?: string;
  /** Strip trailing spaces when writing the files (GnuCOBOL line-sequential style). Default false. */
  trimTrailing?: boolean;
  /**
   * HTML-escape (`& < > " '`) the data values written into HTMLFILE. Default
   * false: the COBOL inserts customer/transaction text verbatim (HTML injection
   * if the data contains markup). Escaped values may be truncated at 100 bytes.
   */
  escapeHtml?: boolean;
}

export interface Cbstm03aResult {
  /** STMTFILE records (80 bytes each). */
  stmtRecords: string[];
  /** HTMLFILE records (100 bytes each). */
  htmlRecords: string[];
  /** SYSOUT (DISPLAY) lines. */
  display: string[];
}

/** 9999-ABEND-PROGRAM (CALL 'CEE3ABD'). STMTFILE/HTMLFILE are DISP=(NEW,CATLG,DELETE): not written. */
export class Cbstm03aAbend extends Error {
  constructor(
    message: string,
    readonly display: string[],
    readonly stmtRecords: string[],
    readonly htmlRecords: string[],
  ) {
    super(message);
    this.name = 'Cbstm03aAbend';
  }
}

/** An out-of-bounds subscript that would overlay storage outside WS-TRNX-TABLE / WS-TRN-TBL-CNTR. */
export class StorageOverlayError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'StorageOverlayError';
  }
}

const CARDS = 51;
const TRANS = 10;
const TRAN_REST_LEN = 318;
const TRAN_ENTRY_LEN = 16 + TRAN_REST_LEN;
const CARD_ENTRY_LEN = 16 + TRANS * TRAN_ENTRY_LEN;
const TABLE_LEN = CARDS * CARD_ENTRY_LEN;

const PIC_CURR_BAL = '9(9).99-';
const PIC_TRANAMT = 'Z(9).99-';

type Label =
  | '0000-START'
  | '1000-MAINLINE'
  | '8100-FILE-OPEN'
  | '8100-TRNXFILE-OPEN'
  | '8200-XREFFILE-OPEN'
  | '8300-CUSTFILE-OPEN'
  | '8400-ACCTFILE-OPEN'
  | '8500-READTRNX-READ'
  | '8599-EXIT';

const pad = (s: string, n: number): string => (s.length >= n ? s.slice(0, n) : s.padEnd(n, ' '));
const field = (layout: Pick<AnyRecordLayout, 'range'>, rec: Buffer, name: string): string => {
  const { offset, length } = layout.range(name);
  return rec.toString('latin1', offset, offset + length);
};
/** Alphanumeric comparison in the EBCDIC collating sequence (z/OS). */
const compareAlnum = (a: string, b: string): number => {
  const x = asciiToEbcdic(Buffer.from(a, 'latin1')).toString('latin1');
  const y = asciiToEbcdic(Buffer.from(b, 'latin1')).toString('latin1');
  return x < y ? -1 : x > y ? 1 : 0;
};
/** ADD into PIC S9(9)V99 COMP-3 without ON SIZE ERROR: high-order digits are truncated. */
const addS9v99 = (a: Decimal, b: Decimal): Decimal => {
  const { digitString, negative } = toScaledDigits(a.plus(b), 11, 2, true);
  return fromScaledDigits(digitString, 2, negative);
};

const HTML_ENTITIES: Record<string, string> = { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' };
export const escapeHtml = (v: string): string => v.replace(/[&<>"']/g, (c) => HTML_ENTITIES[c]!);

/** STATEMENT-LINES named items (FILLERs are constants and are rebuilt by the line functions). */
interface StatementItems {
  stName: string;
  stAdd1: string;
  stAdd2: string;
  stAdd3: string;
  stAcctId: string;
  stCurrBal: string;
  stFicoScore: string;
  stTranId: string;
  stTranDt: string;
  stTranAmt: string;
  stTotalTramt: string;
}

/** INITIALIZE STATEMENT-LINES: alphanumeric items to SPACES, numeric-edited items to ZERO, FILLERs untouched. */
function initializeStatementItems(): StatementItems {
  return {
    stName: pad('', 75),
    stAdd1: pad('', 50),
    stAdd2: pad('', 50),
    stAdd3: pad('', 80),
    stAcctId: pad('', 20),
    stCurrBal: formatEdited(PIC_CURR_BAL, 0),
    stFicoScore: pad('', 20),
    stTranId: pad('', 16),
    stTranDt: pad('', 49),
    stTranAmt: formatEdited(PIC_TRANAMT, 0),
    stTotalTramt: formatEdited(PIC_TRANAMT, 0),
  };
}

const ST = {
  line0: () => '*'.repeat(31) + 'START OF STATEMENT' + '*'.repeat(31),
  line1: (s: StatementItems) => s.stName + pad('', 5),
  line2: (s: StatementItems) => s.stAdd1 + pad('', 30),
  line3: (s: StatementItems) => s.stAdd2 + pad('', 30),
  line4: (s: StatementItems) => s.stAdd3,
  line5: () => '-'.repeat(80),
  line6: () => pad('', 33) + pad('Basic Details', 14) + pad('', 33),
  line7: (s: StatementItems) => 'Account ID         :' + s.stAcctId + pad('', 40),
  line8: (s: StatementItems) => 'Current Balance    :' + s.stCurrBal + pad('', 7) + pad('', 40),
  line9: (s: StatementItems) => 'FICO Score         :' + s.stFicoScore + pad('', 40),
  line10: () => '-'.repeat(80),
  line11: () => pad('', 30) + 'TRANSACTION SUMMARY ' + pad('', 30),
  line12: () => '-'.repeat(80),
  line13: () => pad('Tran ID         ', 16) + pad('Tran Details    ', 51) + '  Tran Amount',
  line14: (s: StatementItems) => s.stTranId + ' ' + s.stTranDt + '$' + s.stTranAmt,
  line14A: (s: StatementItems) => 'Total EXP:' + pad('', 56) + '$' + s.stTotalTramt,
  line15: () => '*'.repeat(32) + 'END OF STATEMENT' + '*'.repeat(32),
};

class Cbstm03a {
  private readonly display: string[] = [];
  private readonly stmt: string[] = [];
  private readonly html: string[] = [];
  private readonly sub: { call(area: M03bArea): void };
  private readonly area: M03bArea = newM03bArea();
  /** Applied to every data value placed in an HTML line. */
  private readonly h: (v: string) => string;

  // COMP-VARIABLES
  private crCnt = 0;
  private trCnt = 0;
  private crJmp = 0;
  private trJmp = 0;
  // COMP3-VARIABLES / MISC-VARIABLES
  private wsTotalAmt: Decimal = fromScaledDigits('0', 2, false);
  private wsFlDd = 'TRNXFILE';
  private wsSaveCard = pad('', 16);
  private endOfFile = 'N';
  // Records (COPY COSTM01 / CVACT03Y / CUSTREC / CVACT01Y)
  private trnxRecord = Buffer.alloc(TrnxRecordLayout.length, 0x20);
  private xrefRecord = Buffer.alloc(CardXrefRecordLayout.length, 0x20);
  private custRecord = Buffer.alloc(CustrecLayout.length, 0x20);
  private acctRecord = Buffer.alloc(AccountRecordLayout.length, 0x20);
  private st: StatementItems = initializeStatementItems();
  // WS-TRNX-TABLE / WS-TRN-TBL-CNTR (INITIALIZE: spaces / zero)
  private readonly table = Buffer.alloc(TABLE_LEN, 0x20);
  private readonly trct = new Array<number>(CARDS).fill(0);
  /** Target of the ALTERable GO TO in 8100-FILE-OPEN (compiled as GO TO 8100-TRNXFILE-OPEN). */
  private alter8100: Label = '8100-TRNXFILE-OPEN';

  constructor(private readonly opts: Cbstm03aOptions) {
    this.sub = opts.subprogram ?? new Cbstm03b(opts.catalog);
    this.h = opts.escapeHtml ? escapeHtml : (v) => v;
  }

  run(): Cbstm03aResult {
    const job = this.opts.job === undefined ? CREASTMT_STEP040 : this.opts.job;
    if (job) this.displayTiot(job);
    // OPEN OUTPUT STMT-FILE HTML-FILE. INITIALIZE WS-TRNX-TABLE WS-TRN-TBL-CNTR (done at construction).
    let next: Label | null = '0000-START';
    while (next) next = this.perform(next);
    // 9999-GOBACK (after CLOSE STMT-FILE HTML-FILE in 1000-MAINLINE)
    const { stmtFile, htmlFile, trimTrailing } = this.opts;
    if (stmtFile) writeLineRecords(stmtFile, this.stmt, { trimTrailing: trimTrailing ?? false });
    if (htmlFile) writeLineRecords(htmlFile, this.html, { trimTrailing: trimTrailing ?? false });
    return { stmtRecords: this.stmt, htmlRecords: this.html, display: this.display };
  }

  // ---------------------------------------------------------------- TIOT walk
  /**
   * The loop tests END-OF-TIOT on the 4 bytes *after* the current 16-byte
   * segment (the next entry's TIO-LEN..), so it stops on the last real entry,
   * which is then displayed by the statement after the loop (with the
   * slightly different '-- null  UCB' text).
   */
  private displayTiot(job: JobContext): void {
    if (job.dds.length === 0) throw new Error('JobContext.dds must contain at least one DD');
    this.display.push(`Running JCL : ${pad(job.jobName, 8)} Step ${pad(job.stepName, 8)}`);
    this.display.push('DD Names from TIOT: ');
    const dds = job.dds;
    for (let i = 0; i < dds.length - 1; i++) {
      const dd = dds[i]!;
      this.display.push(`: ${pad(dd.ddName, 8)} ${dd.ucb === false ? '--  null UCB' : '-- valid UCB'}`);
    }
    const last = dds[dds.length - 1]!;
    this.display.push(`: ${pad(last.ddName, 8)} ${last.ucb === false ? '-- null  UCB' : '-- valid UCB'}`);
  }

  // ---------------------------------------------------------------- paragraphs
  private perform(label: Label): Label | null {
    switch (label) {
      case '0000-START':
        return this.p0000Start();
      case '1000-MAINLINE':
        this.p1000Mainline();
        return null;
      case '8100-FILE-OPEN':
        return this.alter8100;
      case '8100-TRNXFILE-OPEN':
        return this.p8100TrnxfileOpen();
      case '8200-XREFFILE-OPEN':
        return this.openFile('XREFFILE', 'CUSTFILE');
      case '8300-CUSTFILE-OPEN':
        return this.openFile('CUSTFILE', 'ACCTFILE');
      case '8400-ACCTFILE-OPEN':
        this.openFile('ACCTFILE', null);
        return '1000-MAINLINE';
      case '8500-READTRNX-READ':
        return this.p8500ReadtrnxRead();
      case '8599-EXIT':
        this.setTrct(this.crCnt, this.trCnt);
        this.wsFlDd = 'XREFFILE';
        return '0000-START';
    }
  }

  private p0000Start(): Label | null {
    switch (this.wsFlDd) {
      case 'TRNXFILE':
        this.alter8100 = '8100-TRNXFILE-OPEN';
        return '8100-FILE-OPEN';
      case 'XREFFILE':
        this.alter8100 = '8200-XREFFILE-OPEN';
        return '8100-FILE-OPEN';
      case 'CUSTFILE':
        this.alter8100 = '8300-CUSTFILE-OPEN';
        return '8100-FILE-OPEN';
      case 'ACCTFILE':
        this.alter8100 = '8400-ACCTFILE-OPEN';
        return '8100-FILE-OPEN';
      case 'READTRNX':
        return '8500-READTRNX-READ';
      default:
        return null; // GO TO 9999-GOBACK
    }
  }

  private p1000Mainline(): void {
    while (this.endOfFile !== 'Y') {
      if (this.endOfFile === 'N') {
        this.p1000XreffileGetNext();
        if (this.endOfFile === 'N') {
          this.p2000CustfileGet();
          this.p3000AcctfileGet();
          this.p5000CreateStatement();
          this.crJmp = 1;
          this.wsTotalAmt = fromScaledDigits('0', 2, false);
          this.p4000TrnxfileGet();
        }
      }
    }
    this.closeFile('TRNXFILE');
    this.closeFile('XREFFILE');
    this.closeFile('CUSTFILE');
    this.closeFile('ACCTFILE');
  }

  private callM03b(): void {
    this.sub.call(this.area);
  }

  private abend(lines: string[]): never {
    this.display.push(...lines, 'ABENDING PROGRAM');
    throw new Cbstm03aAbend(lines.join('; '), this.display, this.stmt, this.html);
  }

  private p1000XreffileGetNext(): void {
    const a = this.area;
    a.dd = 'XREFFILE';
    a.oper = M03B_OPER.READ;
    a.rc = '00';
    a.fldt.fill(0x20);
    this.callM03b();
    if (a.rc === '10') this.endOfFile = 'Y';
    else if (a.rc !== '00') this.abend(['ERROR READING XREFFILE', `RETURN CODE: ${a.rc}`]);
    this.xrefRecord = Buffer.from(a.fldt.subarray(0, CardXrefRecordLayout.length));
  }

  private readKeyed(dd: 'CUSTFILE' | 'ACCTFILE', key: string): Buffer {
    const a = this.area;
    a.dd = dd;
    a.oper = M03B_OPER.READ_K;
    a.key = pad(key, 25);
    a.keyLn = key.length; // COMPUTE WS-M03B-KEY-LN = LENGTH OF XREF-...-ID
    a.rc = '00';
    a.fldt.fill(0x20);
    this.callM03b();
    if (a.rc !== '00') this.abend([`ERROR READING ${dd}`, `RETURN CODE: ${a.rc}`]);
    return a.fldt;
  }

  private p2000CustfileGet(): void {
    const fldt = this.readKeyed('CUSTFILE', field(CardXrefRecordLayout, this.xrefRecord, 'XREF-CUST-ID'));
    this.custRecord = Buffer.from(fldt.subarray(0, CustrecLayout.length));
  }

  private p3000AcctfileGet(): void {
    const fldt = this.readKeyed('ACCTFILE', field(CardXrefRecordLayout, this.xrefRecord, 'XREF-ACCT-ID'));
    this.acctRecord = Buffer.from(fldt.subarray(0, AccountRecordLayout.length));
  }

  // ---------------------------------------------------------------- WS-TRNX-TABLE (no subscript checking)
  private tableOffset(cr: number, tr: number | null, len: number): number {
    const off = (cr - 1) * CARD_ENTRY_LEN + (tr === null ? 0 : 16 + (tr - 1) * TRAN_ENTRY_LEN);
    if (off < 0 || off + len > TABLE_LEN) {
      throw new StorageOverlayError(`WS-TRNX-TABLE subscript (${cr}${tr === null ? '' : `, ${tr}`}) is outside the table`);
    }
    return off;
  }

  private getCardNum(cr: number): string {
    const o = this.tableOffset(cr, null, 16);
    return this.table.toString('latin1', o, o + 16);
  }

  private setCardNum(cr: number, v: Buffer): void {
    v.copy(this.table, this.tableOffset(cr, null, 16), 0, 16);
  }

  private getTran(cr: number, tr: number): Buffer {
    const o = this.tableOffset(cr, tr, TRAN_ENTRY_LEN);
    return this.table.subarray(o, o + TRAN_ENTRY_LEN);
  }

  private trctIndex(cr: number): number {
    if (cr < 1 || cr > CARDS) throw new StorageOverlayError(`WS-TRCT subscript ${cr} is outside WS-TRN-TBL-CNTR`);
    return cr - 1;
  }

  private setTrct(cr: number, v: number): void {
    this.trct[this.trctIndex(cr)] = v;
  }

  private getTrct(cr: number): number {
    return this.trct[this.trctIndex(cr)]!;
  }

  // ---------------------------------------------------------------- 4000 / 5000 / 6000
  private writeStmt(line: string): void {
    this.stmt.push(pad(line, 80));
  }

  private writeHtml(line: string): void {
    this.html.push(pad(line, 100));
  }

  private writeFixed(...lines: HtmlFixedLine[]): void {
    for (const l of lines) this.writeHtml(HTML[l]);
  }

  private p4000TrnxfileGet(): void {
    const xrefCard = field(CardXrefRecordLayout, this.xrefRecord, 'XREF-CARD-NUM');
    for (this.crJmp = 1; !(this.crJmp > this.crCnt || compareAlnum(this.getCardNum(this.crJmp), xrefCard) > 0); this.crJmp++) {
      if (xrefCard === this.getCardNum(this.crJmp)) {
        this.trnxRecord.write(this.getCardNum(this.crJmp), 0, 16, 'latin1');
        for (this.trJmp = 1; !(this.trJmp > this.getTrct(this.crJmp)); this.trJmp++) {
          this.getTran(this.crJmp, this.trJmp).copy(this.trnxRecord, 16);
          this.p6000WriteTrans();
          this.wsTotalAmt = addS9v99(this.wsTotalAmt, TrnxRecordLayout.parse(this.trnxRecord).trnxRest.trnxAmt);
        }
      }
    }
    // MOVE WS-TOTAL-AMT TO WS-TRN-AMT (same picture), MOVE WS-TRN-AMT TO ST-TOTAL-TRAMT
    this.st.stTotalTramt = formatEdited(PIC_TRANAMT, this.wsTotalAmt);
    this.writeStmt(ST.line12());
    this.writeStmt(ST.line14A(this.st));
    this.writeStmt(ST.line15());
    this.writeFixed('LTRS', 'L10', 'L75', 'LTDE', 'LTRE', 'L78', 'L79', 'L80');
  }

  private p5000CreateStatement(): void {
    this.st = initializeStatementItems();
    this.writeStmt(ST.line0());
    this.p5100WriteHtmlHeader();
    const c = (name: string) => field(CustrecLayout, this.custRecord, name);
    this.st.stName = cobolString(this.st.stName, [
      { value: c('CUST-FIRST-NAME'), delimitedBy: ' ' },
      { value: ' ', delimitedBy: SIZE },
      { value: c('CUST-MIDDLE-NAME'), delimitedBy: ' ' },
      { value: ' ', delimitedBy: SIZE },
      { value: c('CUST-LAST-NAME'), delimitedBy: ' ' },
      { value: ' ', delimitedBy: SIZE },
    ]).result;
    this.st.stAdd1 = pad(c('CUST-ADDR-LINE-1'), 50);
    this.st.stAdd2 = pad(c('CUST-ADDR-LINE-2'), 50);
    this.st.stAdd3 = cobolString(this.st.stAdd3, [
      { value: c('CUST-ADDR-LINE-3'), delimitedBy: ' ' },
      { value: ' ', delimitedBy: SIZE },
      { value: c('CUST-ADDR-STATE-CD'), delimitedBy: ' ' },
      { value: ' ', delimitedBy: SIZE },
      { value: c('CUST-ADDR-COUNTRY-CD'), delimitedBy: ' ' },
      { value: ' ', delimitedBy: SIZE },
      { value: c('CUST-ADDR-ZIP'), delimitedBy: ' ' },
      { value: ' ', delimitedBy: SIZE },
    ]).result;
    // MOVE ACCT-ID (9(11)) / CUST-FICO-CREDIT-SCORE (9(3)) TO X(20): digits moved as characters.
    this.st.stAcctId = pad(field(AccountRecordLayout, this.acctRecord, 'ACCT-ID'), 20);
    this.st.stCurrBal = formatEdited(PIC_CURR_BAL, AccountRecordLayout.parse(this.acctRecord).acctCurrBal);
    this.st.stFicoScore = pad(c('CUST-FICO-CREDIT-SCORE'), 20);
    this.p5200WriteHtmlNmadbs();
    const s = this.st;
    for (const line of [ST.line1(s), ST.line2(s), ST.line3(s), ST.line4(s), ST.line5(), ST.line6(), ST.line5(), ST.line7(s),
      ST.line8(s), ST.line9(s), ST.line10(), ST.line11(), ST.line12(), ST.line13(), ST.line12()]) {
      this.writeStmt(line);
    }
  }

  private p5100WriteHtmlHeader(): void {
    this.writeFixed('L01', 'L02', 'L03', 'L04', 'L05', 'L06', 'L07', 'L08', 'LTRS', 'L10');
    // HTML-L11: X(34) + L11-ACCT X(20) + '</h3>'
    this.writeHtml('<h3>Statement for Account Number: ' + this.h(pad(field(AccountRecordLayout, this.acctRecord, 'ACCT-ID'), 20)) + '</h3>');
    this.writeFixed('LTDE', 'LTRE', 'LTRS', 'L15', 'L16', 'L17', 'L18', 'LTDE', 'LTRE', 'LTRS', 'L22_35');
  }

  private p5200WriteHtmlNmadbs(): void {
    const l23Name = pad(this.st.stName, 50); // MOVE ST-NAME TO L23-NAME
    const para = (open: string, value: string, delim: string, len: number) =>
      stringInto(len, [
        { value: open, delimitedBy: '*' },
        { value: this.h(value), delimitedBy: delim },
        ...(delim === '  ' ? [{ value: '  ', delimitedBy: SIZE }] : []),
        { value: '</p>', delimitedBy: '*' },
      ]);
    this.writeHtml(para('<p style="font-size:16px">', l23Name, '  ', 100));
    this.writeHtml(para('<p>', this.st.stAdd1, '  ', 100));
    this.writeHtml(para('<p>', this.st.stAdd2, '  ', 100));
    this.writeHtml(para('<p>', this.st.stAdd3, '  ', 100));
    this.writeFixed('LTDE', 'LTRE', 'LTRS', 'L30_42', 'L31', 'LTDE', 'LTRE', 'LTRS', 'L22_35');
    this.writeHtml(para('<p>Account ID         : ', this.st.stAcctId, '*', 100));
    this.writeHtml(para('<p>Current Balance    : ', this.st.stCurrBal, '*', 100));
    this.writeHtml(para('<p>FICO Score         : ', this.st.stFicoScore, '*', 100));
    this.writeFixed('LTDE', 'LTRE', 'LTRS', 'L30_42', 'L43', 'LTDE', 'LTRE', 'LTRS');
    this.writeFixed('L47', 'L48', 'LTDE', 'L50', 'L51', 'LTDE', 'L53', 'L54', 'LTDE', 'LTRE');
  }

  private p6000WriteTrans(): void {
    const t = TrnxRecordLayout;
    this.st.stTranId = field(t, this.trnxRecord, 'TRNX-ID');
    this.st.stTranDt = pad(field(t, this.trnxRecord, 'TRNX-DESC'), 49);
    this.st.stTranAmt = formatEdited(PIC_TRANAMT, t.parse(this.trnxRecord).trnxRest.trnxAmt);
    this.writeStmt(ST.line14(this.st));
    const tranLn = (v: string) => stringInto(100, [
      { value: '<p>', delimitedBy: '*' },
      { value: this.h(v), delimitedBy: '*' },
      { value: '</p>', delimitedBy: '*' },
    ]);
    this.writeFixed('LTRS', 'L58');
    this.writeHtml(tranLn(this.st.stTranId));
    this.writeFixed('LTDE', 'L61');
    this.writeHtml(tranLn(this.st.stTranDt));
    this.writeFixed('LTDE', 'L64');
    this.writeHtml(tranLn(this.st.stTranAmt));
    this.writeFixed('LTDE', 'LTRE');
  }

  // ---------------------------------------------------------------- 8100-8599 / 9100-9400
  private p8100TrnxfileOpen(): Label {
    const a = this.area;
    a.dd = 'TRNXFILE';
    a.oper = M03B_OPER.OPEN;
    a.rc = '00';
    this.callM03b();
    if (a.rc !== '00' && a.rc !== '04') this.abend(['ERROR OPENING TRNXFILE', `RETURN CODE: ${a.rc}`]);
    a.oper = M03B_OPER.READ;
    a.fldt.fill(0x20);
    this.callM03b();
    if (a.rc !== '00' && a.rc !== '04') this.abend(['ERROR READING TRNXFILE', `RETURN CODE: ${a.rc}`]);
    this.trnxRecord = Buffer.from(a.fldt.subarray(0, TrnxRecordLayout.length));
    this.wsSaveCard = this.trnxRecord.toString('latin1', 0, 16);
    this.crCnt = 1;
    this.trCnt = 0;
    this.wsFlDd = 'READTRNX';
    return '0000-START';
  }

  /** 8200 / 8300 / 8400: open one file, then MOVE nextDd TO WS-FL-DD. */
  private openFile(dd: 'XREFFILE' | 'CUSTFILE' | 'ACCTFILE', nextDd: string | null): Label {
    const a = this.area;
    a.dd = dd;
    a.oper = M03B_OPER.OPEN;
    a.rc = '00';
    this.callM03b();
    if (a.rc !== '00' && a.rc !== '04') this.abend([`ERROR OPENING ${dd}`, `RETURN CODE: ${a.rc}`]);
    if (nextDd) this.wsFlDd = nextDd;
    return '0000-START';
  }

  private p8500ReadtrnxRead(): Label {
    const card = this.trnxRecord.toString('latin1', 0, 16);
    if (this.wsSaveCard === card) this.trCnt += 1;
    else {
      this.setTrct(this.crCnt, this.trCnt);
      this.crCnt += 1;
      this.trCnt = 1;
    }
    this.setCardNum(this.crCnt, this.trnxRecord.subarray(0, 16));
    this.trnxRecord.copy(this.table, this.tableOffset(this.crCnt, this.trCnt, TRAN_ENTRY_LEN), 16, 16 + TRAN_ENTRY_LEN);
    this.wsSaveCard = card;

    const a = this.area;
    a.dd = 'TRNXFILE';
    a.oper = M03B_OPER.READ;
    a.fldt.fill(0x20);
    this.callM03b();
    if (a.rc === '00') {
      this.trnxRecord = Buffer.from(a.fldt.subarray(0, TrnxRecordLayout.length));
      return '8500-READTRNX-READ';
    }
    if (a.rc === '10') return '8599-EXIT';
    return this.abend(['ERROR READING TRNXFILE', `RETURN CODE: ${a.rc}`]);
  }

  private closeFile(dd: 'TRNXFILE' | 'XREFFILE' | 'CUSTFILE' | 'ACCTFILE'): void {
    const a = this.area;
    a.dd = dd;
    a.oper = M03B_OPER.CLOSE;
    a.rc = '00';
    this.callM03b();
    if (a.rc !== '00' && a.rc !== '04') this.abend([`ERROR CLOSING ${dd}`, `RETURN CODE: ${a.rc}`]);
  }
}

/** Runs CBSTM03A. Throws `Cbstm03aAbend` where the COBOL calls CEE3ABD. */
export function runCbstm03a(opts: Cbstm03aOptions): Cbstm03aResult {
  return new Cbstm03a(opts).run();
}
