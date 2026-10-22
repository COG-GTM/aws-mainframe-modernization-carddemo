/**
 * COTRN00C - list transactions (transaction CT00, map COTRN0A / mapset COTRN00).
 * Browses TRANSACT ten records per page (STARTBR/READNEXT/READPREV, PF7/PF8),
 * 'S' on a row XCTLs to COTRN01C.
 */
import { TranRecordLayout } from '../../common/copybooks/index.js';
import {
  CCDA_MSG_INVALID_KEY,
  DFHAID,
  DFHRESP,
  SymbolicMap,
  formatEdited,
  isNumericText,
  moveAlnum,
  populateHeaderInfo,
  type CicsContext,
  type OnlineProgram,
} from '../framework/index.js';
import { EXT_COMMAREA_LENGTH, LOW_VALUES_16, ProgramCommarea, isBlankField, requireNormal } from './common.js';

const WS_PGMNAME = 'COTRN00C';
const WS_TRANID = 'CT00';
const WS_TRANSACT_FILE = 'TRANSACT';
const ROWS = 10;

const p2 = (i: number): string => String(i).padStart(2, '0');
const rowFields = (i: number): { sel: string; trnid: string; tdate: string; tdesc: string; tamt: string } => ({
  sel: `SEL${String(i).padStart(4, '0')}`,
  trnid: `TRNID${p2(i)}`,
  tdate: `TDATE${p2(i)}`,
  tdesc: `TDESC${p2(i)}`,
  tamt: `TAMT${String(i).padStart(3, '0')}`,
});

function run(ctx: CicsContext): void {
  const map = new SymbolicMap('COTRN0A', 'COTRN00');
  let commarea = ProgramCommarea.fresh(EXT_COMMAREA_LENGTH);
  let errFlg = false;
  let transactEof = false;
  let sendErase = true;
  let wsMessage = moveAlnum('', 80);
  // TRAN-RECORD (CVTRA05Y): fresh WORKING-STORAGE, LOW-VALUES.
  let tranId = LOW_VALUES_16;
  let tranRecord: Buffer = Buffer.alloc(TranRecordLayout.length, 0);

  const setMessage = (m: string): void => {
    wsMessage = moveAlnum(m, 80);
  };

  // ------------------------------------------------------------ screen I/O
  const sendTrnlstScreen = (): void => {
    populateHeaderInfo(map, { tranid: WS_TRANID, pgmname: WS_PGMNAME, now: ctx.now() });
    map.set('ERRMSG', wsMessage);
    ctx.sendMap(map, { mapset: 'COTRN00', erase: sendErase, cursor: true });
  };

  const returnToPrevScreen = (): void => {
    ctx.xctl(commarea.prepareReturn(WS_TRANID, WS_PGMNAME), commarea.toBuffer());
  };

  // ------------------------------------------------------------ TRANSACT browse
  const startbrTransactFile = (): void => {
    const r = ctx.track(ctx.files.startbr({ file: WS_TRANSACT_FILE, ridfld: tranId, keylength: 16 }));
    if (r.resp === DFHRESP.NORMAL) return;
    if (r.resp === DFHRESP.NOTFND) {
      transactEof = true;
      setMessage('You are at the top of the page...');
    } else {
      errFlg = true;
      setMessage('Unable to lookup transaction...');
    }
    map.setLength('TRNIDIN', -1);
    sendTrnlstScreen();
  };

  const readTransact = (dir: 'next' | 'prev'): void => {
    const r = ctx.track(
      dir === 'next'
        ? ctx.files.readnext({ file: WS_TRANSACT_FILE, ridfld: tranId })
        : ctx.files.readprev({ file: WS_TRANSACT_FILE }),
    );
    if (r.resp === DFHRESP.NORMAL) {
      if (r.record) tranRecord = Buffer.from(r.record);
      if (r.ridfld !== undefined) tranId = r.ridfld;
      return;
    }
    if (r.resp === DFHRESP.ENDFILE) {
      transactEof = true;
      setMessage(dir === 'next' ? 'You have reached the bottom of the page...' : 'You have reached the top of the page...');
    } else {
      errFlg = true;
      setMessage('Unable to lookup transaction...');
    }
    map.setLength('TRNIDIN', -1);
    sendTrnlstScreen();
  };

  const endbrTransactFile = (): void => {
    requireNormal(`ENDBR DATASET(${WS_TRANSACT_FILE})`, ctx.track(ctx.files.endbr({ file: WS_TRANSACT_FILE })));
  };

  // ------------------------------------------------------------ rows
  const populateTranData = (idx: number): void => {
    if (idx < 1 || idx > ROWS) return;
    const tran = TranRecordLayout.parse(tranRecord);
    const wsTranAmt = formatEdited('+99999999.99', tran.tranAmt);
    const ts = tran.tranOrigTs;
    // WS-TIMESTAMP-DT-YYYY(3:2) / -MM / -DD -> WS-CURDATE-MM-DD-YY
    const wsTranDate = `${ts.slice(5, 7)}/${ts.slice(8, 10)}/${ts.slice(2, 4)}`;
    const f = rowFields(idx);
    map.set(f.trnid, tran.tranId);
    if (idx === 1) commarea.set('TRNID-FIRST', tran.tranId);
    if (idx === ROWS) commarea.set('TRNID-LAST', tran.tranId);
    map.set(f.tdate, wsTranDate);
    map.set(f.tdesc, tran.tranDesc);
    map.set(f.tamt, wsTranAmt);
  };

  const initializeTranData = (idx: number): void => {
    const f = rowFields(idx);
    for (const n of [f.trnid, f.tdate, f.tdesc, f.tamt]) map.set(n, '');
  };

  const okToContinue = (): boolean => !transactEof && !errFlg;

  // ------------------------------------------------------------ paging
  const processPageForward = (): void => {
    startbrTransactFile();
    if (errFlg) return;
    const aid = ctx.eib.eibaid;
    if (aid !== DFHAID.DFHENTER && aid !== DFHAID.DFHPF7 && aid !== DFHAID.DFHPF3) readTransact('next');
    if (okToContinue()) for (let i = 1; i <= ROWS; i++) initializeTranData(i);
    let wsIdx = 1;
    while (!(wsIdx >= 11 || transactEof || errFlg)) {
      readTransact('next');
      if (okToContinue()) {
        populateTranData(wsIdx);
        wsIdx += 1;
      }
    }
    if (okToContinue()) {
      commarea.setNum('PAGE-NUM', commarea.getNum('PAGE-NUM') + 1);
      readTransact('next');
      commarea.set('NEXT-PAGE-FLG', okToContinue() ? 'Y' : 'N');
    } else {
      commarea.set('NEXT-PAGE-FLG', 'N');
      if (wsIdx > 1) commarea.setNum('PAGE-NUM', commarea.getNum('PAGE-NUM') + 1);
    }
    endbrTransactFile();
    map.set('PAGENUM', commarea.get('PAGE-NUM'));
    map.set('TRNIDIN', ' ');
    sendTrnlstScreen();
  };

  const processPageBackward = (): void => {
    startbrTransactFile();
    if (errFlg) return;
    const aid = ctx.eib.eibaid;
    if (aid !== DFHAID.DFHENTER && aid !== DFHAID.DFHPF8) readTransact('prev');
    if (okToContinue()) for (let i = 1; i <= ROWS; i++) initializeTranData(i);
    let wsIdx = 10;
    while (!(wsIdx <= 0 || transactEof || errFlg)) {
      readTransact('prev');
      if (okToContinue()) {
        populateTranData(wsIdx);
        wsIdx -= 1;
      }
    }
    if (okToContinue()) {
      readTransact('prev');
      if (commarea.get('NEXT-PAGE-FLG') === 'Y') {
        if (okToContinue() && commarea.getNum('PAGE-NUM') > 1) commarea.setNum('PAGE-NUM', commarea.getNum('PAGE-NUM') - 1);
        else commarea.setNum('PAGE-NUM', 1);
      }
    }
    endbrTransactFile();
    map.set('PAGENUM', commarea.get('PAGE-NUM'));
    sendTrnlstScreen();
  };

  // ------------------------------------------------------------ keys
  const processEnterKey = (): void => {
    let selFlg = ' ';
    let selected = ' ';
    for (let i = 1; i <= ROWS; i++) {
      const f = rowFields(i);
      if (!isBlankField(map.get(f.sel))) {
        selFlg = map.get(f.sel);
        selected = map.get(f.trnid);
        break;
      }
    }
    commarea.set('TRN-SEL-FLG', selFlg);
    commarea.set('TRN-SELECTED', selected);
    if (!isBlankField(commarea.get('TRN-SEL-FLG')) && !isBlankField(commarea.get('TRN-SELECTED'))) {
      const flg = commarea.get('TRN-SEL-FLG');
      if (flg === 'S' || flg === 's') {
        commarea.toProgram = 'COTRN01C';
        commarea.set('CDEMO-FROM-TRANID', WS_TRANID);
        commarea.set('CDEMO-FROM-PROGRAM', WS_PGMNAME);
        commarea.set('CDEMO-PGM-CONTEXT', '0');
        ctx.xctl(commarea.toProgram.trim(), commarea.toBuffer());
      } else {
        setMessage('Invalid selection. Valid value is S');
        map.setLength('TRNIDIN', -1);
      }
    }

    const trnidin = map.get('TRNIDIN');
    if (isBlankField(trnidin)) tranId = LOW_VALUES_16;
    else if (isNumericText(trnidin)) tranId = trnidin;
    else {
      errFlg = true;
      setMessage('Tran ID must be Numeric ...');
      map.setLength('TRNIDIN', -1);
      sendTrnlstScreen();
    }

    map.setLength('TRNIDIN', -1);
    commarea.setNum('PAGE-NUM', 0);
    processPageForward();
    if (!errFlg) map.set('TRNIDIN', ' ');
  };

  const processPf7Key = (): void => {
    const first = commarea.get('TRNID-FIRST');
    tranId = isBlankField(first) ? LOW_VALUES_16 : first;
    commarea.set('NEXT-PAGE-FLG', 'Y');
    map.setLength('TRNIDIN', -1);
    if (commarea.getNum('PAGE-NUM') > 1) processPageBackward();
    else {
      setMessage('You are already at the top of the page...');
      sendErase = false;
      sendTrnlstScreen();
    }
  };

  const processPf8Key = (): void => {
    const last = commarea.get('TRNID-LAST');
    tranId = isBlankField(last) ? '\xff'.repeat(16) : last;
    map.setLength('TRNIDIN', -1);
    if (commarea.get('NEXT-PAGE-FLG') === 'Y') processPageForward();
    else {
      setMessage('You are already at the bottom of the page...');
      sendErase = false;
      sendTrnlstScreen();
    }
  };

  // ------------------------------------------------------------ MAIN-PARA
  map.set('ERRMSG', '');
  map.setLength('TRNIDIN', -1);
  if (ctx.eib.eibcalen === 0) {
    commarea.toProgram = 'COSGN00C';
    returnToPrevScreen();
  } else {
    commarea = ProgramCommarea.fromDfhCommarea(ctx.commarea, EXT_COMMAREA_LENGTH);
    if (!commarea.isPgmReenter) {
      commarea.setPgmReenter();
      map.lowValues();
      processEnterKey();
      sendTrnlstScreen();
    } else {
      ctx.receiveMap(map);
      switch (ctx.eib.eibaid) {
        case DFHAID.DFHENTER:
          processEnterKey();
          break;
        case DFHAID.DFHPF3:
          commarea.toProgram = 'COMEN01C';
          returnToPrevScreen();
          break;
        case DFHAID.DFHPF7:
          processPf7Key();
          break;
        case DFHAID.DFHPF8:
          processPf8Key();
          break;
        default:
          errFlg = true;
          map.setLength('TRNIDIN', -1);
          setMessage(CCDA_MSG_INVALID_KEY);
          sendTrnlstScreen();
      }
    }
  }
  ctx.return({ transid: WS_TRANID, commarea: commarea.toBuffer() });
}

export const COTRN00C: OnlineProgram = { program: WS_PGMNAME, transid: WS_TRANID, run };
