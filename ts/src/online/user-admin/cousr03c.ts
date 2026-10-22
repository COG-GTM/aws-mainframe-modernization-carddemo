/**
 * COUSR03C - delete a user from USRSEC (transaction CU03, map COUSR3A /
 * mapset COUSR03).
 */
import {
  CCDA_MSG_INVALID_KEY,
  DFHAID,
  DFHCOLOR,
  DFHRESP,
  SymbolicMap,
  cobolString,
  isSpacesOrLowValues,
  moveAlnum,
  populateHeaderInfo,
  type CicsContext,
  type OnlineProgram,
} from '../framework/index.js';
import { SecUserData, UserAdminCommarea, WS_USRSEC_FILE } from './common.js';

const WS_PGMNAME = 'COUSR03C';
const WS_TRANID = 'CU03';

function run(ctx: CicsContext): void {
  const map = new SymbolicMap('COUSR3A', 'COUSR03');
  let ca = new UserAdminCommarea();
  const sec = new SecUserData();
  let errFlg = false;
  let wsMessage = moveAlnum('', 80);

  const setMessage = (m: string): void => {
    wsMessage = moveAlnum(m, 80);
  };

  const sendUsrdelScreen = (): void => {
    populateHeaderInfo(map, { tranid: WS_TRANID, pgmname: WS_PGMNAME, now: ctx.now() });
    map.set('ERRMSG', wsMessage);
    ctx.sendMap(map, { mapset: 'COUSR03', erase: true, cursor: true });
  };

  const returnToPrevScreen = (): never => {
    if (ca.base.isBlank('CDEMO-TO-PROGRAM')) ca.base.toProgram = 'COSGN00C';
    ca.base.fromTranid = WS_TRANID;
    ca.base.fromProgram = WS_PGMNAME;
    ca.base.setPgmEnter();
    ctx.xctl(ca.base.toProgram, ca.toBuffer());
    throw new Error('unreachable: XCTL returned');
  };

  const fail = (msg: string, cursor: string): void => {
    errFlg = true;
    setMessage(msg);
    map.setLength(cursor, -1);
    sendUsrdelScreen();
  };

  const initializeAllFields = (): void => {
    map.setLength('USRIDIN', -1);
    for (const f of ['USRIDIN', 'FNAME', 'LNAME', 'USRTYPE']) map.set(f, '');
    setMessage('');
  };

  const readUserSecFile = (): void => {
    const r = ctx.track(ctx.files.read({ file: WS_USRSEC_FILE, ridfld: sec.id, keylength: 8, update: true }));
    if (r.resp === DFHRESP.NORMAL) {
      if (r.record) sec.load(r.record);
      setMessage('Press PF5 key to delete this user ...');
      map.setColor('ERRMSG', DFHCOLOR.DFHNEUTR);
      sendUsrdelScreen();
    } else if (r.resp === DFHRESP.NOTFND) {
      fail('User ID NOT found...', 'USRIDIN');
    } else {
      fail('Unable to lookup User...', 'FNAME');
    }
  };

  const deleteUserSecFile = (): void => {
    const r = ctx.track(ctx.files.delete({ file: WS_USRSEC_FILE }));
    if (r.resp === DFHRESP.NORMAL) {
      initializeAllFields();
      setMessage('');
      map.setColor('ERRMSG', DFHCOLOR.DFHGREEN);
      wsMessage = cobolString(['User ', { value: sec.id, delimitedBy: ' ' }, ' has been deleted ...'], wsMessage);
      sendUsrdelScreen();
    } else if (r.resp === DFHRESP.NOTFND) {
      fail('User ID NOT found...', 'USRIDIN');
    } else {
      fail('Unable to Update User...', 'FNAME');
    }
  };

  const validateUserId = (): void => {
    if (isSpacesOrLowValues(map.get('USRIDIN'))) fail('User ID can NOT be empty...', 'USRIDIN');
    else map.setLength('USRIDIN', -1);
  };

  const processEnterKey = (): void => {
    validateUserId();
    if (!errFlg) {
      for (const f of ['FNAME', 'LNAME', 'USRTYPE']) map.set(f, '');
      sec.id = map.get('USRIDIN');
      readUserSecFile();
    }
    if (!errFlg) {
      map.set('FNAME', sec.fname);
      map.set('LNAME', sec.lname);
      map.set('USRTYPE', sec.type);
      sendUsrdelScreen();
    }
  };

  const deleteUserInfo = (): void => {
    validateUserId();
    if (!errFlg) {
      sec.id = map.get('USRIDIN');
      // Bug-for-bug: DELETE runs whatever the READ outcome was.
      readUserSecFile();
      deleteUserSecFile();
    }
  };

  // MAIN-PARA
  map.set('ERRMSG', '');
  if (ctx.eib.eibcalen === 0) {
    ca.base.toProgram = 'COSGN00C';
    returnToPrevScreen();
  }
  ca = UserAdminCommarea.fromDfhCommarea(ctx.commarea);
  if (!ca.base.isPgmReenter) {
    ca.base.setPgmReenter();
    map.lowValues();
    map.setLength('USRIDIN', -1);
    if (!ca.isBlank('usrSelected')) {
      map.set('USRIDIN', ca.get('usrSelected'));
      processEnterKey();
    }
    sendUsrdelScreen();
  } else {
    ctx.receiveMap(map);
    switch (ctx.eib.eibaid) {
      case DFHAID.DFHENTER:
        processEnterKey();
        break;
      case DFHAID.DFHPF3:
        ca.base.toProgram = ca.base.isBlank('CDEMO-FROM-PROGRAM') ? 'COADM01C' : ca.base.fromProgram;
        returnToPrevScreen();
        break;
      case DFHAID.DFHPF4:
        initializeAllFields();
        sendUsrdelScreen();
        break;
      case DFHAID.DFHPF5:
        deleteUserInfo();
        break;
      case DFHAID.DFHPF12:
        ca.base.toProgram = 'COADM01C';
        returnToPrevScreen();
        break;
      default:
        errFlg = true;
        setMessage(CCDA_MSG_INVALID_KEY);
        sendUsrdelScreen();
    }
  }
  ctx.return({ transid: WS_TRANID, commarea: ca.toBuffer() });
}

export const COUSR03C: OnlineProgram = { program: 'COUSR03C', transid: 'CU03', run };
