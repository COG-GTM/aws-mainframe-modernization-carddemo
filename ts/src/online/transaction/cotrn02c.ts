/**
 * COTRN02C - add a transaction (transaction CT02, map COTRN2A / mapset COTRN02).
 * Validates the input (CALL 'CSUTLDTC' for both dates), derives the next
 * TRAN-ID from the last TRANSACT record (STARTBR HIGH-VALUES + READPREV) and
 * WRITEs the new record. Every SEND-TRNADD-SCREEN ends the task (RETURN TRANSID).
 */
import { TranRecordLayout } from '../../common/copybooks/index.js';
import {
  CCDA_MSG_INVALID_KEY,
  DFHAID,
  DFHCOLOR,
  DFHRESP,
  SymbolicMap,
  cobolString,
  csutldtc,
  formatEdited,
  isNumericText,
  moveAlnum,
  numval,
  numvalC,
  populateHeaderInfo,
  type CicsContext,
  type OnlineProgram,
} from '../framework/index.js';
import { EXT_COMMAREA_LENGTH, HIGH_VALUES_16, ProgramCommarea, isBlankField, moveAlnumToUnsigned, moveNumeric, requireNormal } from './common.js';

const WS_PGMNAME = 'COTRN02C';
const WS_TRANID = 'CT02';
const WS_TRANSACT_FILE = 'TRANSACT';
const WS_CCXREF_FILE = 'CCXREF';
const WS_CXACAIX_FILE = 'CXACAIX';
const WS_DATE_FORMAT = 'YYYY-MM-DD';
const DATA_FIELDS = ['TTYPCD', 'TCATCD', 'TRNSRC', 'TRNAMT', 'TDESC', 'TORIGDT', 'TPROCDT', 'MID', 'MNAME', 'MCITY', 'MZIP'] as const;

/** CALL 'CSUTLDTC' USING CSUTLDTC-DATE CSUTLDTC-DATE-FORMAT CSUTLDTC-RESULT -> (SEV-CD, MSG-NUM). */
export function callCsutldtc(date: string, format: string): { sevCd: string; msgNum: string } {
  const { result } = csutldtc(date, format);
  return { sevCd: result.slice(0, 4), msgNum: result.slice(15, 19) };
}

function run(ctx: CicsContext): void {
  const map = new SymbolicMap('COTRN2A', 'COTRN02');
  let commarea = ProgramCommarea.fresh(EXT_COMMAREA_LENGTH);
  let errFlg = false;
  let wsMessage = moveAlnum('', 80);
  // TRAN-RECORD / CARD-XREF-RECORD: fresh WORKING-STORAGE (LOW-VALUES).
  let tranId = '\0'.repeat(16);
  let tranRecord: Buffer = Buffer.alloc(TranRecordLayout.length, 0);
  let xrefRecord: Buffer = Buffer.alloc(50, 0);

  const setMessage = (m: string): void => {
    wsMessage = moveAlnum(m, 80);
  };

  const sendTrnaddScreen = (): never => {
    populateHeaderInfo(map, { tranid: WS_TRANID, pgmname: WS_PGMNAME, now: ctx.now() });
    map.set('ERRMSG', wsMessage);
    ctx.sendMap(map, { mapset: 'COTRN02', erase: true, cursor: true });
    return ctx.return({ transid: WS_TRANID, commarea: commarea.toBuffer() });
  };

  /** MOVE 'Y' TO WS-ERR-FLG, message, cursor, PERFORM SEND-TRNADD-SCREEN. */
  const fail = (message: string, cursorField: string): never => {
    errFlg = true;
    setMessage(message);
    map.setLength(cursorField, -1);
    return sendTrnaddScreen();
  };

  const returnToPrevScreen = (): void => {
    ctx.xctl(commarea.prepareReturn(WS_TRANID, WS_PGMNAME), commarea.toBuffer());
  };

  // ------------------------------------------------------------ files
  const readXref = (file: string, ridfld: string, notFound: string, other: string, cursorField: string): void => {
    const r = ctx.track(ctx.files.read({ file, ridfld }));
    if (r.resp === DFHRESP.NORMAL) {
      if (r.record) xrefRecord = Buffer.from(r.record);
      return;
    }
    fail(r.resp === DFHRESP.NOTFND ? notFound : other, cursorField);
  };

  const startbrTransactFile = (): void => {
    const r = ctx.track(ctx.files.startbr({ file: WS_TRANSACT_FILE, ridfld: tranId, keylength: 16 }));
    if (r.resp === DFHRESP.NORMAL) return;
    fail(r.resp === DFHRESP.NOTFND ? 'Transaction ID NOT found...' : 'Unable to lookup Transaction...', 'ACTIDIN');
  };

  const readprevTransactFile = (): void => {
    const r = ctx.track(ctx.files.readprev({ file: WS_TRANSACT_FILE }));
    if (r.resp === DFHRESP.NORMAL) {
      if (r.record) tranRecord = Buffer.from(r.record);
      if (r.ridfld !== undefined) tranId = r.ridfld;
    } else if (r.resp === DFHRESP.ENDFILE) {
      tranId = '0'.repeat(16);
    } else {
      fail('Unable to lookup Transaction...', 'ACTIDIN');
    }
  };

  const endbrTransactFile = (): void => {
    requireNormal(`ENDBR DATASET(${WS_TRANSACT_FILE})`, ctx.track(ctx.files.endbr({ file: WS_TRANSACT_FILE })));
  };

  /** STARTBR HIGH-VALUES / READPREV / ENDBR: the last TRANSACT record. */
  const readLastTransaction = (): void => {
    tranId = HIGH_VALUES_16;
    startbrTransactFile();
    readprevTransactFile();
    endbrTransactFile();
  };

  const initializeAllFields = (): void => {
    map.setLength('ACTIDIN', -1);
    for (const f of ['ACTIDIN', 'CARDNIN', ...DATA_FIELDS, 'CONFIRM']) map.set(f, '');
    setMessage('');
  };

  const writeTransactFile = (): void => {
    const r = ctx.track(ctx.files.write({ file: WS_TRANSACT_FILE, ridfld: tranId, from: tranRecord }));
    if (r.resp === DFHRESP.NORMAL) {
      initializeAllFields();
      map.setColor('ERRMSG', DFHCOLOR.DFHGREEN);
      wsMessage = cobolString(
        ['Transaction added successfully. ', ' Your Tran ID is ', { value: tranId, delimitedBy: ' ' }, '.'],
        moveAlnum('', 80),
      );
      sendTrnaddScreen();
    }
    if (r.resp === DFHRESP.DUPKEY || r.resp === DFHRESP.DUPREC) fail('Tran ID already exist...', 'ACTIDIN');
    fail('Unable to Add Transaction...', 'ACTIDIN');
  };

  // ------------------------------------------------------------ validation
  const validateInputKeyFields = (): void => {
    const actidin = map.get('ACTIDIN');
    const cardnin = map.get('CARDNIN');
    if (!isBlankField(actidin)) {
      if (!isNumericText(actidin)) fail('Account ID must be Numeric...', 'ACTIDIN');
      const wsAcctIdN = moveNumeric(numval(actidin), 11, 0, false).toFixed(0).padStart(11, '0');
      map.set('ACTIDIN', wsAcctIdN);
      readXref(WS_CXACAIX_FILE, wsAcctIdN, 'Account ID NOT found...', 'Unable to lookup Acct in XREF AIX file...', 'ACTIDIN');
      map.set('CARDNIN', xrefRecord.toString('latin1', 0, 16));
    } else if (!isBlankField(cardnin)) {
      if (!isNumericText(cardnin)) fail('Card Number must be Numeric...', 'CARDNIN');
      const wsCardNumN = moveNumeric(numval(cardnin), 16, 0, false).toFixed(0).padStart(16, '0');
      map.set('CARDNIN', wsCardNumN);
      readXref(WS_CCXREF_FILE, wsCardNumN, 'Card Number NOT found...', 'Unable to lookup Card # in XREF file...', 'CARDNIN');
      map.set('ACTIDIN', xrefRecord.toString('latin1', 25, 36));
    } else {
      fail('Account or Card Number must be entered...', 'ACTIDIN');
    }
  };

  const isDateShaped = (d: string): boolean =>
    isNumericText(d.slice(0, 4)) && d[4] === '-' && isNumericText(d.slice(5, 7)) && d[7] === '-' && isNumericText(d.slice(8, 10));

  const validateInputDataFields = (): void => {
    if (errFlg) for (const f of DATA_FIELDS) map.set(f, '');

    const emptyChecks: [string, string][] = [
      ['TTYPCD', 'Type CD can NOT be empty...'],
      ['TCATCD', 'Category CD can NOT be empty...'],
      ['TRNSRC', 'Source can NOT be empty...'],
      ['TDESC', 'Description can NOT be empty...'],
      ['TRNAMT', 'Amount can NOT be empty...'],
      ['TORIGDT', 'Orig Date can NOT be empty...'],
      ['TPROCDT', 'Proc Date can NOT be empty...'],
      ['MID', 'Merchant ID can NOT be empty...'],
      ['MNAME', 'Merchant Name can NOT be empty...'],
      ['MCITY', 'Merchant City can NOT be empty...'],
      ['MZIP', 'Merchant Zip can NOT be empty...'],
    ];
    for (const [f, msg] of emptyChecks) if (isBlankField(map.get(f))) fail(msg, f);

    if (!isNumericText(map.get('TTYPCD'))) fail('Type CD must be Numeric...', 'TTYPCD');
    if (!isNumericText(map.get('TCATCD'))) fail('Category CD must be Numeric...', 'TCATCD');

    const amt = map.get('TRNAMT');
    if ((amt[0] !== '-' && amt[0] !== '+') || !isNumericText(amt.slice(1, 9)) || amt[9] !== '.' || !isNumericText(amt.slice(10, 12))) {
      fail('Amount should be in format -99999999.99', 'TRNAMT');
    }
    if (!isDateShaped(map.get('TORIGDT'))) fail('Orig Date should be in format YYYY-MM-DD', 'TORIGDT');
    if (!isDateShaped(map.get('TPROCDT'))) fail('Proc Date should be in format YYYY-MM-DD', 'TPROCDT');

    const wsTranAmtN = moveNumeric(numvalC(map.get('TRNAMT')), 11, 2, true);
    map.set('TRNAMT', formatEdited('+99999999.99', wsTranAmtN));

    const orig = callCsutldtc(map.get('TORIGDT'), WS_DATE_FORMAT);
    if (orig.sevCd !== '0000' && orig.msgNum !== '2513') fail('Orig Date - Not a valid date...', 'TORIGDT');
    const proc = callCsutldtc(map.get('TPROCDT'), WS_DATE_FORMAT);
    if (proc.sevCd !== '0000' && proc.msgNum !== '2513') fail('Proc Date - Not a valid date...', 'TPROCDT');

    if (!isNumericText(map.get('MID'))) fail('Merchant ID must be Numeric...', 'MID');
  };

  // ------------------------------------------------------------ actions
  const addTransaction = (): void => {
    readLastTransaction();
    const wsTranIdN = (BigInt(moveAlnumToUnsigned(tranId, 16)) + 1n) % 10n ** 16n;
    tranId = wsTranIdN.toString().padStart(16, '0');
    // INITIALIZE TRAN-RECORD leaves the FILLER as last read.
    const filler = tranRecord.toString('latin1', TranRecordLayout.range('TRAN-PROC-TS').offset + 26);
    tranRecord = TranRecordLayout.serialize({
      tranId,
      tranTypeCd: map.get('TTYPCD'),
      tranCatCd: moveAlnumToUnsigned(map.get('TCATCD'), 4),
      tranSource: map.get('TRNSRC'),
      tranDesc: map.get('TDESC'),
      tranAmt: moveNumeric(numvalC(map.get('TRNAMT')), 11, 2, true),
      tranCardNum: map.get('CARDNIN'),
      tranMerchantId: moveAlnumToUnsigned(map.get('MID'), 9),
      tranMerchantName: map.get('MNAME'),
      tranMerchantCity: map.get('MCITY'),
      tranMerchantZip: map.get('MZIP'),
      tranOrigTs: map.get('TORIGDT'),
      tranProcTs: map.get('TPROCDT'),
      filler,
    });
    writeTransactFile();
  };

  const processEnterKey = (): void => {
    validateInputKeyFields();
    validateInputDataFields();
    const confirm = map.get('CONFIRM');
    switch (confirm) {
      case 'Y':
      case 'y':
        addTransaction();
        break;
      case 'N':
      case 'n':
      case ' ':
      case '\0':
        fail('Confirm to add this transaction...', 'CONFIRM');
        break;
      default:
        fail('Invalid value. Valid values are (Y/N)...', 'CONFIRM');
    }
  };

  const copyLastTranData = (): void => {
    validateInputKeyFields();
    readLastTransaction();
    if (!errFlg) {
      const tran = TranRecordLayout.parse(tranRecord);
      map.set('TTYPCD', tran.tranTypeCd);
      map.set('TCATCD', tran.tranCatCd.toFixed(0).padStart(4, '0'));
      map.set('TRNSRC', tran.tranSource);
      map.set('TRNAMT', formatEdited('+99999999.99', tran.tranAmt));
      map.set('TDESC', tran.tranDesc);
      map.set('TORIGDT', tran.tranOrigTs);
      map.set('TPROCDT', tran.tranProcTs);
      map.set('MID', tran.tranMerchantId.toFixed(0).padStart(9, '0'));
      map.set('MNAME', tran.tranMerchantName);
      map.set('MCITY', tran.tranMerchantCity);
      map.set('MZIP', tran.tranMerchantZip);
    }
    processEnterKey();
  };

  // ------------------------------------------------------------ MAIN-PARA
  map.set('ERRMSG', '');
  if (ctx.eib.eibcalen === 0) {
    commarea.toProgram = 'COSGN00C';
    returnToPrevScreen();
  } else {
    commarea = ProgramCommarea.fromDfhCommarea(ctx.commarea, EXT_COMMAREA_LENGTH);
    if (!commarea.isPgmReenter) {
      commarea.setPgmReenter();
      map.lowValues();
      map.setLength('ACTIDIN', -1);
      if (!isBlankField(commarea.get('TRN-SELECTED'))) {
        map.set('CARDNIN', commarea.get('TRN-SELECTED'));
        processEnterKey();
      }
      sendTrnaddScreen();
    } else {
      ctx.receiveMap(map);
      switch (ctx.eib.eibaid) {
        case DFHAID.DFHENTER:
          processEnterKey();
          break;
        case DFHAID.DFHPF3:
          commarea.toProgram = isBlankField(commarea.fromProgram) ? 'COMEN01C' : commarea.fromProgram;
          returnToPrevScreen();
          break;
        case DFHAID.DFHPF4:
          initializeAllFields();
          sendTrnaddScreen();
          break;
        case DFHAID.DFHPF5:
          copyLastTranData();
          break;
        default:
          errFlg = true;
          setMessage(CCDA_MSG_INVALID_KEY);
          sendTrnaddScreen();
      }
    }
  }
  ctx.return({ transid: WS_TRANID, commarea: commarea.toBuffer() });
}

export const COTRN02C: OnlineProgram = { program: WS_PGMNAME, transid: WS_TRANID, run };
