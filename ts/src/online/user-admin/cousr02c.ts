/**
 * COUSR02C - update a user in USRSEC (transaction CU02, map COUSR2A /
 * mapset COUSR02).
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

const WS_PGMNAME = 'COUSR02C';
const WS_TRANID = 'CU02';

function run(ctx: CicsContext): void {
  const map = new SymbolicMap('COUSR2A', 'COUSR02');
  let ca = new UserAdminCommarea();
  const sec = new SecUserData();
  let errFlg = false;
  let usrModified = false;
  let wsMessage = moveAlnum('', 80);

  const setMessage = (m: string): void => {
    wsMessage = moveAlnum(m, 80);
  };

  const sendUsrupdScreen = (): void => {
    populateHeaderInfo(map, { tranid: WS_TRANID, pgmname: WS_PGMNAME, now: ctx.now() });
    map.set('ERRMSG', wsMessage);
    ctx.sendMap(map, { mapset: 'COUSR02', erase: true, cursor: true });
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
    sendUsrupdScreen();
  };

  const readUserSecFile = (): void => {
    const r = ctx.track(ctx.files.read({ file: WS_USRSEC_FILE, ridfld: sec.id, keylength: 8, update: true }));
    if (r.resp === DFHRESP.NORMAL) {
      if (r.record) sec.load(r.record);
      setMessage('Press PF5 key to save your updates ...');
      map.setColor('ERRMSG', DFHCOLOR.DFHNEUTR);
      sendUsrupdScreen();
    } else if (r.resp === DFHRESP.NOTFND) {
      fail('User ID NOT found...', 'USRIDIN');
    } else {
      fail('Unable to lookup User...', 'FNAME');
    }
  };

  const updateUserSecFile = (): void => {
    const r = ctx.track(ctx.files.rewrite({ file: WS_USRSEC_FILE, from: sec.buffer }));
    if (r.resp === DFHRESP.NORMAL) {
      setMessage('');
      map.setColor('ERRMSG', DFHCOLOR.DFHGREEN);
      wsMessage = cobolString(['User ', { value: sec.id, delimitedBy: ' ' }, ' has been updated ...'], wsMessage);
      sendUsrupdScreen();
    } else if (r.resp === DFHRESP.NOTFND) {
      fail('User ID NOT found...', 'USRIDIN');
    } else {
      fail('Unable to Update User...', 'FNAME');
    }
  };

  const processEnterKey = (): void => {
    if (isSpacesOrLowValues(map.get('USRIDIN'))) fail('User ID can NOT be empty...', 'USRIDIN');
    else map.setLength('USRIDIN', -1);
    if (!errFlg) {
      for (const f of ['FNAME', 'LNAME', 'PASSWD', 'USRTYPE']) map.set(f, '');
      sec.id = map.get('USRIDIN');
      readUserSecFile();
    }
    if (!errFlg) {
      map.set('FNAME', sec.fname);
      map.set('LNAME', sec.lname);
      map.set('PASSWD', sec.pwd);
      map.set('USRTYPE', sec.type);
      sendUsrupdScreen();
    }
  };

  const updateUserInfo = (): void => {
    const required: [string, string][] = [
      ['USRIDIN', 'User ID can NOT be empty...'],
      ['FNAME', 'First Name can NOT be empty...'],
      ['LNAME', 'Last Name can NOT be empty...'],
      ['PASSWD', 'Password can NOT be empty...'],
      ['USRTYPE', 'User Type can NOT be empty...'],
    ];
    const missing = required.find(([f]) => isSpacesOrLowValues(map.get(f)));
    if (missing) fail(missing[1], missing[0]);
    else map.setLength('FNAME', -1);
    if (errFlg) return;
    sec.id = map.get('USRIDIN');
    // Bug-for-bug: the comparison/REWRITE below runs whatever the READ outcome was.
    readUserSecFile();
    const pairs: [string, 'fname' | 'lname' | 'pwd' | 'type'][] = [
      ['FNAME', 'fname'],
      ['LNAME', 'lname'],
      ['PASSWD', 'pwd'],
      ['USRTYPE', 'type'],
    ];
    for (const [f, k] of pairs) {
      if (map.get(f) !== sec[k]) {
        sec[k] = map.get(f);
        usrModified = true;
      }
    }
    if (usrModified) updateUserSecFile();
    else {
      setMessage('Please modify to update ...');
      map.setColor('ERRMSG', DFHCOLOR.DFHRED);
      sendUsrupdScreen();
    }
  };

  const clearCurrentScreen = (): void => {
    map.setLength('USRIDIN', -1);
    for (const f of ['USRIDIN', 'FNAME', 'LNAME', 'PASSWD', 'USRTYPE']) map.set(f, '');
    setMessage('');
    sendUsrupdScreen();
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
    sendUsrupdScreen();
  } else {
    ctx.receiveMap(map);
    switch (ctx.eib.eibaid) {
      case DFHAID.DFHENTER:
        processEnterKey();
        break;
      case DFHAID.DFHPF3:
        updateUserInfo();
        ca.base.toProgram = ca.base.isBlank('CDEMO-FROM-PROGRAM') ? 'COADM01C' : ca.base.fromProgram;
        returnToPrevScreen();
        break;
      case DFHAID.DFHPF4:
        clearCurrentScreen();
        break;
      case DFHAID.DFHPF5:
        updateUserInfo();
        break;
      case DFHAID.DFHPF12:
        ca.base.toProgram = 'COADM01C';
        returnToPrevScreen();
        break;
      default:
        errFlg = true;
        setMessage(CCDA_MSG_INVALID_KEY);
        sendUsrupdScreen();
    }
  }
  ctx.return({ transid: WS_TRANID, commarea: ca.toBuffer() });
}

export const COUSR02C: OnlineProgram = { program: 'COUSR02C', transid: 'CU02', run };
