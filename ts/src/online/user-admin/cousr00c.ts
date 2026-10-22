/**
 * COUSR00C - list all users from USRSEC (transaction CU00, map COUSR0A /
 * mapset COUSR00): 10 rows per page, PF7/PF8 paging, U/D row selection.
 */
import {
  CCDA_MSG_INVALID_KEY,
  CicsCondition,
  DFHAID,
  DFHRESP,
  SymbolicMap,
  isSpacesOrLowValues,
  moveAlnum,
  populateHeaderInfo,
  type CicsContext,
  type OnlineProgram,
} from '../framework/index.js';
import { SecUserData, UserAdminCommarea, WS_USRSEC_FILE } from './common.js';

const WS_PGMNAME = 'COUSR00C';
const WS_TRANID = 'CU00';
const nn = (i: number): string => String(i).padStart(2, '0');

function run(ctx: CicsContext): void {
  const map = new SymbolicMap('COUSR0A', 'COUSR00');
  let ca = new UserAdminCommarea();
  const sec = new SecUserData();
  let errFlg = false;
  let eof = false;
  let sendErase = true;
  let wsMessage = moveAlnum('', 80);
  let wsIdx = 0;

  // CDEMO-CU00-NEXT-PAGE-FLG lives in the commarea ('Y' = NEXT-PAGE-YES).
  const isNextPage = (): boolean => ca.get('nextPageFlg') === 'Y';
  const setNextPage = (yes: boolean): void => {
    ca.set('nextPageFlg', yes ? 'Y' : 'N');
  };

  const setMessage = (m: string): void => {
    wsMessage = moveAlnum(m, 80);
  };

  const sendUsrlstScreen = (): void => {
    populateHeaderInfo(map, { tranid: WS_TRANID, pgmname: WS_PGMNAME, now: ctx.now() });
    map.set('ERRMSG', wsMessage);
    ctx.sendMap(map, { mapset: 'COUSR00', erase: sendErase, cursor: true });
  };

  const returnToPrevScreen = (): never => {
    if (ca.base.isBlank('CDEMO-TO-PROGRAM')) ca.base.toProgram = 'COSGN00C';
    ca.base.fromTranid = WS_TRANID;
    ca.base.fromProgram = WS_PGMNAME;
    ca.base.setPgmEnter();
    ctx.xctl(ca.base.toProgram, ca.toBuffer());
    throw new Error('unreachable: XCTL returned');
  };

  const browseError = (): void => {
    errFlg = true;
    setMessage('Unable to lookup User...');
    map.setLength('USRIDIN', -1);
    sendUsrlstScreen();
  };

  const startbrUserSecFile = (): void => {
    const r = ctx.track(ctx.files.startbr({ file: WS_USRSEC_FILE, ridfld: sec.id, keylength: 8 }));
    if (r.resp === DFHRESP.NORMAL) return;
    if (r.resp === DFHRESP.NOTFND) {
      eof = true;
      setMessage('You are at the top of the page...');
      map.setLength('USRIDIN', -1);
      sendUsrlstScreen();
      return;
    }
    browseError();
  };

  const readUserSecFile = (dir: 'next' | 'prev'): void => {
    const opts = { file: WS_USRSEC_FILE, ridfld: sec.id, keylength: 8 };
    const r = ctx.track(dir === 'next' ? ctx.files.readnext(opts) : ctx.files.readprev(opts));
    if (r.resp === DFHRESP.NORMAL) {
      if (r.record) sec.load(r.record);
      return;
    }
    if (r.resp === DFHRESP.ENDFILE) {
      eof = true;
      setMessage(dir === 'next' ? 'You have reached the bottom of the page...' : 'You have reached the top of the page...');
      map.setLength('USRIDIN', -1);
      sendUsrlstScreen();
      return;
    }
    browseError();
  };

  // EXEC CICS ENDBR without RESP: any failure raises the condition (abend).
  const endbrUserSecFile = (): void => {
    const r = ctx.track(ctx.files.endbr({ file: WS_USRSEC_FILE }));
    if (r.resp !== DFHRESP.NORMAL) throw new CicsCondition('ENDBR', r.resp, r.resp2);
  };

  const initializeUserData = (i: number): void => {
    for (const f of ['USRID', 'FNAME', 'LNAME', 'UTYPE']) map.set(`${f}${nn(i)}`, '');
  };

  const populateUserData = (i: number): void => {
    if (i < 1 || i > 10) return;
    map.set(`USRID${nn(i)}`, sec.id);
    if (i === 1) ca.set('usridFirst', sec.id);
    if (i === 10) ca.set('usridLast', sec.id);
    map.set(`FNAME${nn(i)}`, sec.fname);
    map.set(`LNAME${nn(i)}`, sec.lname);
    map.set(`UTYPE${nn(i)}`, sec.type);
  };

  const processPageForward = (): void => {
    startbrUserSecFile();
    if (errFlg) return;
    const aid = ctx.eib.eibaid;
    if (aid !== DFHAID.DFHENTER && aid !== DFHAID.DFHPF7 && aid !== DFHAID.DFHPF3) readUserSecFile('next');
    if (!eof && !errFlg) for (let i = 1; i <= 10; i++) initializeUserData(i);
    wsIdx = 1;
    while (!(wsIdx >= 11 || eof || errFlg)) {
      readUserSecFile('next');
      if (!eof && !errFlg) {
        populateUserData(wsIdx);
        wsIdx += 1;
      }
    }
    if (!eof && !errFlg) {
      ca.pageNum = ca.pageNum + 1;
      readUserSecFile('next');
      setNextPage(!eof && !errFlg);
    } else {
      setNextPage(false);
      if (wsIdx > 1) ca.pageNum = ca.pageNum + 1;
    }
    endbrUserSecFile();
    map.set('PAGENUM', ca.get('pageNum'));
    map.set('USRIDIN', ' ');
    sendUsrlstScreen();
  };

  const processPageBackward = (): void => {
    startbrUserSecFile();
    if (errFlg) return;
    const aid = ctx.eib.eibaid;
    if (aid !== DFHAID.DFHENTER && aid !== DFHAID.DFHPF8) readUserSecFile('prev');
    if (!eof && !errFlg) for (let i = 1; i <= 10; i++) initializeUserData(i);
    wsIdx = 10;
    while (!(wsIdx <= 0 || eof || errFlg)) {
      readUserSecFile('prev');
      if (!eof && !errFlg) {
        populateUserData(wsIdx);
        wsIdx -= 1;
      }
    }
    if (!eof && !errFlg) {
      readUserSecFile('prev');
      if (isNextPage()) {
        if (!eof && !errFlg && ca.pageNum > 1) ca.pageNum = ca.pageNum - 1;
        else ca.pageNum = 1;
      }
    }
    endbrUserSecFile();
    map.set('PAGENUM', ca.get('pageNum'));
    sendUsrlstScreen();
  };

  const processEnterKey = (): void => {
    let selected = false;
    for (let i = 1; i <= 10; i++) {
      const sel = map.get(`SEL${String(i).padStart(4, '0')}`);
      if (!isSpacesOrLowValues(sel)) {
        ca.set('usrSelFlg', sel);
        ca.set('usrSelected', map.get(`USRID${nn(i)}`));
        selected = true;
        break;
      }
    }
    if (!selected) {
      ca.set('usrSelFlg', ' ');
      ca.set('usrSelected', ' ');
    }
    if (!ca.isBlank('usrSelFlg') && !ca.isBlank('usrSelected')) {
      const flg = ca.get('usrSelFlg');
      const target = flg === 'U' || flg === 'u' ? 'COUSR02C' : flg === 'D' || flg === 'd' ? 'COUSR03C' : undefined;
      if (target) {
        ca.base.toProgram = target;
        ca.base.fromTranid = WS_TRANID;
        ca.base.fromProgram = WS_PGMNAME;
        ca.base.setPgmEnter();
        ctx.xctl(target, ca.toBuffer());
      } else {
        setMessage('Invalid selection. Valid values are U and D');
        map.setLength('USRIDIN', -1);
      }
    }
    if (isSpacesOrLowValues(map.get('USRIDIN'))) sec.buffer.fill(0x00, 0, 8);
    else sec.id = map.get('USRIDIN');
    map.setLength('USRIDIN', -1);
    ca.pageNum = 0;
    processPageForward();
    if (!errFlg) map.set('USRIDIN', ' ');
  };

  const processPf7Key = (): void => {
    if (ca.isBlank('usridFirst')) sec.buffer.fill(0x00, 0, 8);
    else sec.id = ca.get('usridFirst');
    setNextPage(true);
    map.setLength('USRIDIN', -1);
    if (ca.pageNum > 1) processPageBackward();
    else {
      setMessage('You are already at the top of the page...');
      sendErase = false;
      sendUsrlstScreen();
    }
  };

  const processPf8Key = (): void => {
    if (ca.isBlank('usridLast')) sec.buffer.fill(0xff, 0, 8);
    else sec.id = ca.get('usridLast');
    map.setLength('USRIDIN', -1);
    if (isNextPage()) processPageForward();
    else {
      setMessage('You are already at the bottom of the page...');
      sendErase = false;
      sendUsrlstScreen();
    }
  };

  // MAIN-PARA (SET NEXT-PAGE-NO acts on the WORKING-STORAGE copy, overwritten below)
  setNextPage(false);
  map.set('ERRMSG', '');
  map.setLength('USRIDIN', -1);

  if (ctx.eib.eibcalen === 0) {
    ca.base.toProgram = 'COSGN00C';
    returnToPrevScreen();
  }
  ca = UserAdminCommarea.fromDfhCommarea(ctx.commarea);
  if (!ca.base.isPgmReenter) {
    ca.base.setPgmReenter();
    map.lowValues();
    processEnterKey();
    sendUsrlstScreen();
  } else {
    ctx.receiveMap(map);
    switch (ctx.eib.eibaid) {
      case DFHAID.DFHENTER:
        processEnterKey();
        break;
      case DFHAID.DFHPF3:
        ca.base.toProgram = 'COADM01C';
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
        map.setLength('USRIDIN', -1);
        setMessage(CCDA_MSG_INVALID_KEY);
        sendUsrlstScreen();
    }
  }
  ctx.return({ transid: WS_TRANID, commarea: ca.toBuffer() });
}

export const COUSR00C: OnlineProgram = { program: 'COUSR00C', transid: 'CU00', run };
