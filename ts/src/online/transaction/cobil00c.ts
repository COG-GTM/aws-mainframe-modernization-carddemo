/**
 * COBIL00C - online bill payment (transaction CB00, map COBIL0A / mapset COBIL00).
 * Pays the full ACCT-CURR-BAL: writes a type-02 TRANSACT record and REWRITEs
 * the account with the balance reduced by the payment (decimal arithmetic).
 */
import { AccountRecordLayout, TranRecordLayout } from '../../common/copybooks/index.js';
import {
  CCDA_MSG_INVALID_KEY,
  DFHAID,
  DFHCOLOR,
  DFHRESP,
  SymbolicMap,
  cobolString,
  formatEdited,
  formatTime,
  moveAlnum,
  populateHeaderInfo,
  type CicsContext,
  type OnlineProgram,
} from '../framework/index.js';
import {
  Decimal,
  EXT_COMMAREA_LENGTH,
  HIGH_VALUES_16,
  ProgramCommarea,
  isBlankField,
  moveAlnumToUnsigned,
  moveNumeric,
  requireNormal,
} from './common.js';

const WS_PGMNAME = 'COBIL00C';
const WS_TRANID = 'CB00';
const WS_TRANSACT_FILE = 'TRANSACT';
const WS_ACCTDAT_FILE = 'ACCTDAT';
const WS_CXACAIX_FILE = 'CXACAIX';

function run(ctx: CicsContext): void {
  const map = new SymbolicMap('COBIL0A', 'COBIL00');
  let commarea = ProgramCommarea.fresh(EXT_COMMAREA_LENGTH);
  let errFlg = false;
  let confPay = false;
  let wsMessage = moveAlnum('', 80);
  // ACCOUNT-RECORD / CARD-XREF-RECORD / TRAN-RECORD: fresh WORKING-STORAGE (LOW-VALUES).
  let accountRecord: Buffer | undefined;
  let acctCurrBal = new Decimal(0);
  let acctId = '';
  let xrefAcctId = '';
  let xrefRecord: Buffer = Buffer.alloc(50, 0);
  let tranId = '\0'.repeat(16);
  let tranRecord: Buffer = Buffer.alloc(TranRecordLayout.length, 0);

  const setMessage = (m: string): void => {
    wsMessage = moveAlnum(m, 80);
  };

  const sendBillpayScreen = (): void => {
    populateHeaderInfo(map, { tranid: WS_TRANID, pgmname: WS_PGMNAME, now: ctx.now() });
    map.set('ERRMSG', wsMessage);
    ctx.sendMap(map, { mapset: 'COBIL00', erase: true, cursor: true });
  };

  const fail = (message: string): void => {
    errFlg = true;
    setMessage(message);
    map.setLength('ACTIDIN', -1);
    sendBillpayScreen();
  };

  const returnToPrevScreen = (): void => {
    ctx.xctl(commarea.prepareReturn(WS_TRANID, WS_PGMNAME), commarea.toBuffer());
  };

  const initializeAllFields = (): void => {
    map.setLength('ACTIDIN', -1);
    for (const f of ['ACTIDIN', 'CURBAL', 'CONFIRM']) map.set(f, '');
    setMessage('');
  };

  const clearCurrentScreen = (): void => {
    initializeAllFields();
    sendBillpayScreen();
  };

  // ------------------------------------------------------------ files
  const readAcctdatFile = (): void => {
    const r = ctx.track(ctx.files.read({ file: WS_ACCTDAT_FILE, ridfld: acctId, keylength: 11, update: true }));
    if (r.resp === DFHRESP.NORMAL) {
      if (r.record) {
        accountRecord = Buffer.from(r.record);
        acctCurrBal = AccountRecordLayout.parse(accountRecord).acctCurrBal;
      }
      return;
    }
    fail(r.resp === DFHRESP.NOTFND ? 'Account ID NOT found...' : 'Unable to lookup Account...');
  };

  const updateAcctdatFile = (): void => {
    const acct = AccountRecordLayout.parse(accountRecord ?? Buffer.alloc(AccountRecordLayout.length, 0x20));
    acct.acctCurrBal = acctCurrBal;
    const r = ctx.track(ctx.files.rewrite({ file: WS_ACCTDAT_FILE, from: AccountRecordLayout.serialize(acct) }));
    if (r.resp === DFHRESP.NORMAL) return;
    fail(r.resp === DFHRESP.NOTFND ? 'Account ID NOT found...' : 'Unable to Update Account...');
  };

  const readCxacaixFile = (): void => {
    const r = ctx.track(ctx.files.read({ file: WS_CXACAIX_FILE, ridfld: xrefAcctId, keylength: 11 }));
    if (r.resp === DFHRESP.NORMAL) {
      if (r.record) xrefRecord = Buffer.from(r.record);
      return;
    }
    fail(r.resp === DFHRESP.NOTFND ? 'Account ID NOT found...' : 'Unable to lookup XREF AIX file...');
  };

  const startbrTransactFile = (): void => {
    const r = ctx.track(ctx.files.startbr({ file: WS_TRANSACT_FILE, ridfld: tranId, keylength: 16 }));
    if (r.resp === DFHRESP.NORMAL) return;
    fail(r.resp === DFHRESP.NOTFND ? 'Transaction ID NOT found...' : 'Unable to lookup Transaction...');
  };

  const readprevTransactFile = (): void => {
    const r = ctx.track(ctx.files.readprev({ file: WS_TRANSACT_FILE }));
    if (r.resp === DFHRESP.NORMAL) {
      if (r.record) tranRecord = Buffer.from(r.record);
      if (r.ridfld !== undefined) tranId = r.ridfld;
    } else if (r.resp === DFHRESP.ENDFILE) {
      tranId = '0'.repeat(16);
    } else {
      fail('Unable to lookup Transaction...');
    }
  };

  const endbrTransactFile = (): void => {
    requireNormal(`ENDBR DATASET(${WS_TRANSACT_FILE})`, ctx.track(ctx.files.endbr({ file: WS_TRANSACT_FILE })));
  };

  const writeTransactFile = (): void => {
    const r = ctx.track(ctx.files.write({ file: WS_TRANSACT_FILE, ridfld: tranId, from: tranRecord }));
    if (r.resp === DFHRESP.NORMAL) {
      initializeAllFields();
      map.setColor('ERRMSG', DFHCOLOR.DFHGREEN);
      wsMessage = cobolString(
        ['Payment successful. ', ' Your Transaction ID is ', { value: tranId, delimitedBy: ' ' }, '.'],
        moveAlnum('', 80),
      );
      sendBillpayScreen();
    } else if (r.resp === DFHRESP.DUPKEY || r.resp === DFHRESP.DUPREC) {
      fail('Tran ID already exist...');
    } else {
      fail('Unable to Add Bill pay Transaction...');
    }
  };

  /** GET-CURRENT-TIMESTAMP: ASKTIME + FORMATTIME YYYYMMDD DATESEP('-') TIME TIMESEP(':') into WS-TIMESTAMP. */
  const getCurrentTimestamp = (): string => {
    const t = formatTime(ctx.asktime(), { datesep: '-', timesep: ':' });
    return `${t.yyyymmdd} ${t.time}.000000`;
  };

  // ------------------------------------------------------------ PROCESS-ENTER-KEY
  const processEnterKey = (): void => {
    confPay = false;
    if (isBlankField(map.get('ACTIDIN'))) {
      errFlg = true;
      setMessage('Acct ID can NOT be empty...');
      map.setLength('ACTIDIN', -1);
      sendBillpayScreen();
    }

    if (!errFlg) {
      acctId = moveAlnumToUnsigned(map.get('ACTIDIN'), 11);
      xrefAcctId = acctId;
      switch (map.get('CONFIRM')) {
        case 'Y':
        case 'y':
          confPay = true;
          readAcctdatFile();
          break;
        case 'N':
        case 'n':
          clearCurrentScreen();
          errFlg = true;
          break;
        case ' ':
        case '\0':
          readAcctdatFile();
          break;
        default:
          errFlg = true;
          setMessage('Invalid value. Valid values are (Y/N)...');
          map.setLength('CONFIRM', -1);
          sendBillpayScreen();
      }
      map.set('CURBAL', formatEdited('+9999999999.99', acctCurrBal));
    }

    if (!errFlg && acctCurrBal.lte(0) && !isBlankField(map.get('ACTIDIN'))) fail('You have nothing to pay...');

    if (!errFlg) {
      if (confPay) {
        readCxacaixFile();
        tranId = HIGH_VALUES_16;
        startbrTransactFile();
        readprevTransactFile();
        endbrTransactFile();
        const wsTranIdNum = (BigInt(moveAlnumToUnsigned(tranId, 16)) + 1n) % 10n ** 16n;
        tranId = wsTranIdNum.toString().padStart(16, '0');
        const tranAmt = moveNumeric(acctCurrBal, 11, 2, true);
        const ts = getCurrentTimestamp();
        // INITIALIZE TRAN-RECORD leaves the FILLER as last read.
        const filler = tranRecord.toString('latin1', TranRecordLayout.range('TRAN-PROC-TS').offset + 26);
        tranRecord = TranRecordLayout.serialize({
          tranId,
          tranTypeCd: '02',
          tranCatCd: 2,
          tranSource: 'POS TERM',
          tranDesc: 'BILL PAYMENT - ONLINE',
          tranAmt,
          tranCardNum: xrefRecord.toString('latin1', 0, 16),
          tranMerchantId: 999999999,
          tranMerchantName: 'BILL PAYMENT',
          tranMerchantCity: 'N/A',
          tranMerchantZip: 'N/A',
          tranOrigTs: ts,
          tranProcTs: ts,
          filler,
        });
        writeTransactFile();
        acctCurrBal = moveNumeric(acctCurrBal.minus(tranAmt), 12, 2, true);
        updateAcctdatFile();
      } else {
        setMessage('Confirm to make a bill payment...');
        map.setLength('CONFIRM', -1);
      }
      sendBillpayScreen();
    }
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
        map.set('ACTIDIN', commarea.get('TRN-SELECTED'));
        processEnterKey();
      }
      sendBillpayScreen();
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
          clearCurrentScreen();
          break;
        default:
          errFlg = true;
          setMessage(CCDA_MSG_INVALID_KEY);
          sendBillpayScreen();
      }
    }
  }
  ctx.return({ transid: WS_TRANID, commarea: commarea.toBuffer() });
}

export const COBIL00C: OnlineProgram = { program: WS_PGMNAME, transid: WS_TRANID, run };
