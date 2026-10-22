/**
 * COMEN01C - CardDemo main menu for regular users (transaction CM00, map COMEN1A
 * / mapset COMEN01, options from COMEN02Y).
 */
import {
  CCDA_MSG_INVALID_KEY,
  CarddemoCommarea,
  DFHAID,
  DFHCOLOR,
  DFHRESP,
  MAIN_MENU_OPTIONS,
  MAIN_MENU_OPT_COUNT,
  SymbolicMap,
  cobolString,
  moveAlnum,
  populateHeaderInfo,
  type CicsContext,
  type OnlineProgram,
} from '../framework/index.js';
import { buildMenuOptions, parseOption } from './menu-common.js';

const WS_PGMNAME = 'COMEN01C';
const WS_TRANID = 'CM00';

function run(ctx: CicsContext): void {
  const map = new SymbolicMap('COMEN1A', 'COMEN01');
  let commarea = new CarddemoCommarea();
  let errFlg = false;
  let wsMessage = moveAlnum('', 80);

  const sendMenuScreen = (): void => {
    populateHeaderInfo(map, { tranid: WS_TRANID, pgmname: WS_PGMNAME, now: ctx.now() });
    buildMenuOptions(map, MAIN_MENU_OPTIONS);
    map.set('ERRMSG', wsMessage);
    ctx.sendMap(map, { mapset: 'COMEN01', erase: true });
  };

  const returnToSignonScreen = (): void => {
    if (commarea.isBlank('CDEMO-TO-PROGRAM')) commarea.toProgram = 'COSGN00C';
    ctx.xctl(commarea.toProgram);
  };

  const notInstalled = (name: string): void => {
    wsMessage = moveAlnum('', 80);
    map.setColor('ERRMSG', DFHCOLOR.DFHRED);
    wsMessage = cobolString(['This option ', { value: name, delimitedBy: '  ' }, ' is not installed...'], wsMessage);
  };

  const xctlTo = (pgm: string): void => {
    commarea.fromTranid = WS_TRANID;
    commarea.fromProgram = WS_PGMNAME;
    commarea.setPgmEnter();
    ctx.xctl(pgm, commarea.toBuffer());
  };

  const processEnterKey = (): void => {
    const opt = parseOption(map.get('OPTION'));
    map.set('OPTION', opt.text);
    if (!opt.numeric || opt.value > MAIN_MENU_OPT_COUNT || opt.value === 0) {
      errFlg = true;
      wsMessage = moveAlnum('Please enter a valid option number...', 80);
      sendMenuScreen();
    }
    // Out-of-range subscripts read storage outside the table in COBOL; modelled as not 'A'.
    const entry = opt.numeric && opt.value >= 1 && opt.value <= MAIN_MENU_OPT_COUNT ? MAIN_MENU_OPTIONS[opt.value - 1] : undefined;
    if (commarea.isUser && entry?.usrType === 'A') {
      errFlg = true;
      wsMessage = moveAlnum('No access - Admin Only option... ', 80);
      sendMenuScreen();
    }
    if (!errFlg && entry) {
      const pgm = entry.pgmName;
      if (pgm === 'COPAUS0C') {
        if (ctx.inquireProgram(pgm).resp === DFHRESP.NORMAL) xctlTo(pgm);
        else notInstalled(entry.name);
      } else if (pgm.startsWith('DUMMY')) {
        wsMessage = moveAlnum('', 80);
        map.setColor('ERRMSG', DFHCOLOR.DFHGREEN);
        wsMessage = cobolString(['This option ', { value: entry.name, delimitedBy: ' ' }, 'is coming soon ...'], wsMessage);
      } else if (ctx.inquireProgram(pgm).resp !== DFHRESP.NORMAL) {
        // Deviation (see README LIMITATIONS): an option whose program is not ported yet
        // gets the COPAUS0C "not installed" handling instead of abending AEI0 on XCTL.
        notInstalled(entry.name);
      } else {
        xctlTo(pgm);
      }
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

export const COMEN01C: OnlineProgram = { program: WS_PGMNAME, transid: WS_TRANID, run };
