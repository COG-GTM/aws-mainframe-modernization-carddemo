/**
 * COSGN00C - CardDemo signon (transaction CC00, map COSGN0A / mapset COSGN00).
 * Validates the user against USRSEC and XCTLs to COADM01C (admin) or COMEN01C (user).
 */
import { SecUserDataLayout } from '../../common/copybooks/index.js';
import {
  CCDA_MSG_INVALID_KEY,
  CCDA_MSG_THANK_YOU,
  CarddemoCommarea,
  DFHAID,
  DFHRESP,
  SymbolicMap,
  isSpacesOrLowValues,
  moveAlnum,
  populateHeaderInfo,
  upperCase,
  type CicsContext,
  type OnlineProgram,
} from '../framework/index.js';

const WS_PGMNAME = 'COSGN00C';
const WS_TRANID = 'CC00';
const WS_USRSEC_FILE = 'USRSEC  ';
const PWD = SecUserDataLayout.range('SEC-USR-PWD');
const TYPE = SecUserDataLayout.range('SEC-USR-TYPE');

function run(ctx: CicsContext): void {
  const map = new SymbolicMap('COSGN0A', 'COSGN00');
  const commarea = new CarddemoCommarea();
  let errFlg = false;
  let wsMessage = moveAlnum('', 80);
  let wsUserId = '';
  let wsUserPwd = '';

  const sendSignonScreen = (): void => {
    populateHeaderInfo(map, { tranid: WS_TRANID, pgmname: WS_PGMNAME, now: ctx.now() });
    const { applid, sysid } = ctx.assign();
    map.setRaw('APPLID', applid.padEnd(8, ' '));
    // ASSIGN SYSID returns 4 bytes; the rest of the 8-byte SYSIDO keeps its prior content.
    map.setRaw('SYSID', sysid.padEnd(4, ' ').slice(0, 4) + map.get('SYSID').slice(4));
    map.set('ERRMSG', wsMessage);
    ctx.sendMap(map, { mapset: 'COSGN00', erase: true, cursor: true });
  };

  const readUserSecFile = (): void => {
    const r = ctx.track(ctx.files.read({ file: WS_USRSEC_FILE.trim(), ridfld: wsUserId }));
    if (r.resp === DFHRESP.NORMAL && r.record) {
      const secUsrPwd = r.record.toString('latin1', PWD.offset, PWD.offset + PWD.length);
      const secUsrType = r.record.toString('latin1', TYPE.offset, TYPE.offset + TYPE.length);
      if (secUsrPwd === wsUserPwd) {
        commarea.fromTranid = WS_TRANID;
        commarea.fromProgram = WS_PGMNAME;
        commarea.userId = wsUserId;
        commarea.userType = secUsrType;
        commarea.setPgmEnter();
        if (commarea.isAdmin) ctx.xctl('COADM01C', commarea.toBuffer());
        else ctx.xctl('COMEN01C', commarea.toBuffer());
      } else {
        wsMessage = moveAlnum('Wrong Password. Try again ...', 80);
        map.setLength('PASSWD', -1);
        sendSignonScreen();
      }
    } else if (r.resp === DFHRESP.NOTFND) {
      errFlg = true;
      wsMessage = moveAlnum('User not found. Try again ...', 80);
      map.setLength('USERID', -1);
      sendSignonScreen();
    } else {
      errFlg = true;
      wsMessage = moveAlnum('Unable to verify the User ...', 80);
      map.setLength('USERID', -1);
      sendSignonScreen();
    }
  };

  const processEnterKey = (): void => {
    ctx.receiveMap(map);
    if (isSpacesOrLowValues(map.get('USERID'))) {
      errFlg = true;
      wsMessage = moveAlnum('Please enter User ID ...', 80);
      map.setLength('USERID', -1);
      sendSignonScreen();
    } else if (isSpacesOrLowValues(map.get('PASSWD'))) {
      errFlg = true;
      wsMessage = moveAlnum('Please enter Password ...', 80);
      map.setLength('PASSWD', -1);
      sendSignonScreen();
    }
    wsUserId = upperCase(map.get('USERID'));
    commarea.userId = wsUserId;
    wsUserPwd = upperCase(map.get('PASSWD'));
    if (!errFlg) readUserSecFile();
  };

  // MAIN-PARA
  map.set('ERRMSG', '');
  if (ctx.eib.eibcalen === 0) {
    map.lowValues();
    map.setLength('USERID', -1);
    sendSignonScreen();
  } else {
    switch (ctx.eib.eibaid) {
      case DFHAID.DFHENTER:
        processEnterKey();
        break;
      case DFHAID.DFHPF3:
        wsMessage = moveAlnum(CCDA_MSG_THANK_YOU, 80);
        ctx.sendText(wsMessage);
        ctx.return();
        break;
      default:
        errFlg = true;
        wsMessage = moveAlnum(CCDA_MSG_INVALID_KEY, 80);
        sendSignonScreen();
    }
  }
  ctx.return({ transid: WS_TRANID, commarea: commarea.toBuffer() });
}

export const COSGN00C: OnlineProgram = { program: WS_PGMNAME, transid: WS_TRANID, run };
