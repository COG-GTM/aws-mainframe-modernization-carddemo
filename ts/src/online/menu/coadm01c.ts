/**
 * COADM01C - CardDemo admin menu (transaction CA00, map COADM1A / mapset
 * COADM01, options from COADM02Y).
 */
import {
  ADMIN_MENU_OPTIONS,
  ADMIN_MENU_OPT_COUNT,
  CCDA_MSG_INVALID_KEY,
  CarddemoCommarea,
  CicsCondition,
  DFHAID,
  DFHCOLOR,
  DFHRESP,
  SymbolicMap,
  cobolString,
  moveAlnum,
  populateHeaderInfo,
  type CicsContext,
  type OnlineProgram,
} from '../framework/index.js';
import { buildMenuOptions, parseOption } from './menu-common.js';

const WS_PGMNAME = 'COADM01C';
const WS_TRANID = 'CA00';

function run(ctx: CicsContext): void {
  const map = new SymbolicMap('COADM1A', 'COADM01');
  let commarea = new CarddemoCommarea();
  let errFlg = false;
  let wsMessage = moveAlnum('', 80);
  let optionName = '';

  const sendMenuScreen = (): void => {
    populateHeaderInfo(map, { tranid: WS_TRANID, pgmname: WS_PGMNAME, now: ctx.now() });
    buildMenuOptions(map, ADMIN_MENU_OPTIONS);
    map.set('ERRMSG', wsMessage);
    ctx.sendMap(map, { mapset: 'COADM01', erase: true });
  };

  const returnToSignonScreen = (): void => {
    if (commarea.isBlank('CDEMO-TO-PROGRAM')) commarea.toProgram = 'COSGN00C';
    ctx.xctl(commarea.toProgram);
  };

  const notInstalledMessage = (): void => {
    wsMessage = moveAlnum('', 80);
    map.setColor('ERRMSG', DFHCOLOR.DFHGREEN);
    wsMessage = cobolString(['This option ', moveAlnum(optionName, 35), 'is not installed ...'], wsMessage);
  };

  // HANDLE CONDITION PGMIDERR(PGMIDERR-ERR-PARA)
  const pgmiderrErrPara = (): never => {
    notInstalledMessage();
    sendMenuScreen();
    return ctx.return({ transid: WS_TRANID, commarea: commarea.toBuffer() });
  };

  const processEnterKey = (): void => {
    const opt = parseOption(map.get('OPTION'));
    map.set('OPTION', opt.text);
    if (!opt.numeric || opt.value > ADMIN_MENU_OPT_COUNT || opt.value === 0) {
      errFlg = true;
      wsMessage = moveAlnum('Please enter a valid option number...', 80);
      sendMenuScreen();
    }
    const entry = opt.numeric && opt.value >= 1 && opt.value <= ADMIN_MENU_OPT_COUNT ? ADMIN_MENU_OPTIONS[opt.value - 1] : undefined;
    if (!errFlg && entry) {
      optionName = entry.name;
      if (!entry.pgmName.startsWith('DUMMY')) {
        commarea.fromTranid = WS_TRANID;
        commarea.fromProgram = WS_PGMNAME;
        commarea.setPgmEnter();
        try {
          ctx.xctl(entry.pgmName, commarea.toBuffer());
        } catch (e) {
          if (e instanceof CicsCondition && e.resp === DFHRESP.PGMIDERR) pgmiderrErrPara();
          throw e;
        }
      }
      notInstalledMessage();
      sendMenuScreen();
    }
  };

  // MAIN-PARA
  map.set('ERRMSG', '');
  if (ctx.eib.eibcalen === 0) {
    commarea.fromProgram = 'COSGN00C';
    returnToSignonScreen();
  } else {
    commarea = CarddemoCommarea.fromDfhCommarea(ctx.commarea);
    if (!commarea.isPgmReenter) {
      commarea.setPgmReenter();
      map.lowValues();
      sendMenuScreen();
    } else {
      ctx.receiveMap(map);
      switch (ctx.eib.eibaid) {
        case DFHAID.DFHENTER:
          processEnterKey();
          break;
        case DFHAID.DFHPF3:
          commarea.toProgram = 'COSGN00C';
          returnToSignonScreen();
          break;
        default:
          errFlg = true;
          wsMessage = moveAlnum(CCDA_MSG_INVALID_KEY, 80);
          sendMenuScreen();
      }
    }
  }
  ctx.return({ transid: WS_TRANID, commarea: commarea.toBuffer() });
}

export const COADM01C: OnlineProgram = { program: WS_PGMNAME, transid: WS_TRANID, run };
