/**
 * COUSR01C - add a regular/admin user to USRSEC (transaction CU01, map
 * COUSR1A / mapset COUSR01).
 */
import {
  CCDA_MSG_INVALID_KEY,
  CarddemoCommarea,
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
import { SecUserData, WS_USRSEC_FILE } from './common.js';

const WS_PGMNAME = 'COUSR01C';
const WS_TRANID = 'CU01';

function run(ctx: CicsContext): void {
  const map = new SymbolicMap('COUSR1A', 'COUSR01');
  let ca = new CarddemoCommarea();
  const sec = new SecUserData();
  let errFlg = false;
  let wsMessage = moveAlnum('', 80);

  const setMessage = (m: string): void => {
    wsMessage = moveAlnum(m, 80);
  };

  const sendUsraddScreen = (): void => {
    populateHeaderInfo(map, { tranid: WS_TRANID, pgmname: WS_PGMNAME, now: ctx.now() });
    map.set('ERRMSG', wsMessage);
    ctx.sendMap(map, { mapset: 'COUSR01', erase: true, cursor: true });
  };

  const returnToPrevScreen = (): never => {
    if (ca.isBlank('CDEMO-TO-PROGRAM')) ca.toProgram = 'COSGN00C';
    ca.fromTranid = WS_TRANID;
    ca.fromProgram = WS_PGMNAME;
    ca.setPgmEnter();
    ctx.xctl(ca.toProgram, ca.toBuffer());
    throw new Error('unreachable: XCTL returned');
  };

  const initializeAllFields = (): void => {
    map.setLength('FNAME', -1);
    for (const f of ['USERID', 'FNAME', 'LNAME', 'PASSWD', 'USRTYPE']) map.set(f, '');
    setMessage('');
  };

  const writeUserSecFile = (): void => {
    const r = ctx.track(ctx.files.write({ file: WS_USRSEC_FILE, ridfld: sec.id, from: sec.buffer }));
    if (r.resp === DFHRESP.NORMAL) {
      initializeAllFields();
      setMessage('');
      map.setColor('ERRMSG', DFHCOLOR.DFHGREEN);
      wsMessage = cobolString(['User ', { value: sec.id, delimitedBy: ' ' }, ' has been added ...'], wsMessage);
      sendUsraddScreen();
    } else if (r.resp === DFHRESP.DUPKEY || r.resp === DFHRESP.DUPREC) {
      errFlg = true;
      setMessage('User ID already exist...');
      map.setLength('USERID', -1);
      sendUsraddScreen();
    } else {
      errFlg = true;
      setMessage('Unable to Add User...');
      map.setLength('FNAME', -1);
      sendUsraddScreen();
    }
  };

  const processEnterKey = (): void => {
    const required: [string, string][] = [
      ['FNAME', 'First Name can NOT be empty...'],
      ['LNAME', 'Last Name can NOT be empty...'],
      ['USERID', 'User ID can NOT be empty...'],
      ['PASSWD', 'Password can NOT be empty...'],
      ['USRTYPE', 'User Type can NOT be empty...'],
    ];
    const missing = required.find(([f]) => isSpacesOrLowValues(map.get(f)));
    if (missing) {
      errFlg = true;
      setMessage(missing[1]);
      map.setLength(missing[0], -1);
      sendUsraddScreen();
    } else {
      map.setLength('FNAME', -1);
    }
    if (!errFlg) {
      sec.id = map.get('USERID');
      sec.fname = map.get('FNAME');
      sec.lname = map.get('LNAME');
      sec.pwd = map.get('PASSWD');
      sec.type = map.get('USRTYPE');
      writeUserSecFile();
    }
  };

  // MAIN-PARA
  map.set('ERRMSG', '');
  if (ctx.eib.eibcalen === 0) {
    ca.toProgram = 'COSGN00C';
    returnToPrevScreen();
  }
  ca = CarddemoCommarea.fromDfhCommarea(ctx.commarea);
  if (!ca.isPgmReenter) {
    ca.setPgmReenter();
    map.lowValues();
    map.setLength('FNAME', -1);
    sendUsraddScreen();
  } else {
    ctx.receiveMap(map);
    switch (ctx.eib.eibaid) {
      case DFHAID.DFHENTER:
        processEnterKey();
        break;
      case DFHAID.DFHPF3:
        ca.toProgram = 'COADM01C';
        returnToPrevScreen();
        break;
      case DFHAID.DFHPF4:
        initializeAllFields();
        sendUsraddScreen();
        break;
      default:
        errFlg = true;
        map.setLength('FNAME', -1);
        setMessage(CCDA_MSG_INVALID_KEY);
        sendUsraddScreen();
    }
  }
  ctx.return({ transid: WS_TRANID, commarea: ca.toBuffer() });
}

export const COUSR01C: OnlineProgram = { program: 'COUSR01C', transid: 'CU01', run };
