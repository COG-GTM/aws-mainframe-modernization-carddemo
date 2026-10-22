/**
 * COTRN01C - view a transaction (transaction CT01, map COTRN1A / mapset COTRN01).
 * READs TRANSACT (with UPDATE, never rewritten) by the entered/selected TRAN-ID.
 */
import { TranRecordLayout } from '../../common/copybooks/index.js';
import {
  CCDA_MSG_INVALID_KEY,
  DFHAID,
  DFHRESP,
  SymbolicMap,
  formatEdited,
  moveAlnum,
  populateHeaderInfo,
  type CicsContext,
  type OnlineProgram,
} from '../framework/index.js';
import { EXT_COMMAREA_LENGTH, ProgramCommarea, isBlankField } from './common.js';

const WS_PGMNAME = 'COTRN01C';
const WS_TRANID = 'CT01';
const WS_TRANSACT_FILE = 'TRANSACT';
const DETAIL_FIELDS = ['TRNID', 'CARDNUM', 'TTYPCD', 'TCATCD', 'TRNSRC', 'TRNAMT', 'TDESC', 'TORIGDT', 'TPROCDT', 'MID', 'MNAME', 'MCITY', 'MZIP'] as const;

function run(ctx: CicsContext): void {
  const map = new SymbolicMap('COTRN1A', 'COTRN01');
  let commarea = ProgramCommarea.fresh(EXT_COMMAREA_LENGTH);
  let errFlg = false;
  let wsMessage = moveAlnum('', 80);
  let tranRecord: Buffer = Buffer.alloc(TranRecordLayout.length, 0);

  const setMessage = (m: string): void => {
    wsMessage = moveAlnum(m, 80);
  };

  const sendTrnviewScreen = (): void => {
    populateHeaderInfo(map, { tranid: WS_TRANID, pgmname: WS_PGMNAME, now: ctx.now() });
    map.set('ERRMSG', wsMessage);
    ctx.sendMap(map, { mapset: 'COTRN01', erase: true, cursor: true });
  };

  const returnToPrevScreen = (): void => {
    ctx.xctl(commarea.prepareReturn(WS_TRANID, WS_PGMNAME), commarea.toBuffer());
  };

  const readTransactFile = (tranId: string): void => {
    const r = ctx.track(ctx.files.read({ file: WS_TRANSACT_FILE, ridfld: tranId, keylength: 16, update: true }));
    if (r.resp === DFHRESP.NORMAL) {
      if (r.record) tranRecord = Buffer.from(r.record);
      return;
    }
    errFlg = true;
    setMessage(r.resp === DFHRESP.NOTFND ? 'Transaction ID NOT found...' : 'Unable to lookup Transaction...');
    map.setLength('TRNIDIN', -1);
    sendTrnviewScreen();
  };

  const processEnterKey = (): void => {
    if (isBlankField(map.get('TRNIDIN'))) {
      errFlg = true;
      setMessage('Tran ID can NOT be empty...');
      map.setLength('TRNIDIN', -1);
      sendTrnviewScreen();
    } else {
      map.setLength('TRNIDIN', -1);
    }

    if (!errFlg) {
      for (const f of DETAIL_FIELDS) map.set(f, '');
      readTransactFile(map.get('TRNIDIN'));
    }

    if (!errFlg) {
      const tran = TranRecordLayout.parse(tranRecord);
      map.set('TRNID', tran.tranId);
      map.set('CARDNUM', tran.tranCardNum);
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
      sendTrnviewScreen();
    }
  };

  const initializeAllFields = (): void => {
    map.setLength('TRNIDIN', -1);
    for (const f of ['TRNIDIN', ...DETAIL_FIELDS]) map.set(f, '');
    setMessage('');
  };

  // MAIN-PARA
  map.set('ERRMSG', '');
  if (ctx.eib.eibcalen === 0) {
    commarea.toProgram = 'COSGN00C';
    returnToPrevScreen();
  } else {
    commarea = ProgramCommarea.fromDfhCommarea(ctx.commarea, EXT_COMMAREA_LENGTH);
    if (!commarea.isPgmReenter) {
      commarea.setPgmReenter();
      map.lowValues();
      map.setLength('TRNIDIN', -1);
      if (!isBlankField(commarea.get('TRN-SELECTED'))) {
        map.set('TRNIDIN', commarea.get('TRN-SELECTED'));
        processEnterKey();
      }
      sendTrnviewScreen();
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
          sendTrnviewScreen();
          break;
        case DFHAID.DFHPF5:
          commarea.toProgram = 'COTRN00C';
          returnToPrevScreen();
          break;
        default:
          errFlg = true;
          setMessage(CCDA_MSG_INVALID_KEY);
          sendTrnviewScreen();
      }
    }
  }
  ctx.return({ transid: WS_TRANID, commarea: commarea.toBuffer() });
}

export const COTRN01C: OnlineProgram = { program: WS_PGMNAME, transid: WS_TRANID, run };
