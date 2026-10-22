/**
 * COCRDSLC - credit card detail (transaction CCDL, map CCRDSLA / mapset COCRDSL).
 * Entered from COCRDLIC ('S' selection) or from the menu; reads CARDDAT by card number.
 */
import { CCARD_AID, CardRecordLayout, type CardRecord } from '../../common/copybooks/index.js';
import {
  CarddemoCommarea,
  DFHBMSCA,
  DFHCOLOR,
  DFHRESP,
  SymbolicMap,
  isSpacesOrLowValues,
  moveAlnum,
  populateHeaderInfo,
  storePfKey,
  type CicsContext,
  type OnlineProgram,
} from '../framework/index.js';
import { buildWsCommarea, fileErrorMessage, initializedCommarea, installAbendRoutine, isNumeric, programCommarea, sendPlainText, zonedValue } from './common.js';

const LIT_THISPGM = 'COCRDSLC';
const LIT_THISTRANID = 'CCDL';
const LIT_THISMAPSET = 'COCRDSL';
const LIT_THISMAP = 'CCRDSLA';
const LIT_CCLISTPGM = 'COCRDLIC';
const LIT_CCLISTMAPSET = 'COCRDLI';
const LIT_MENUPGM = 'COMEN01C';
const LIT_MENUTRANID = 'CM00';
const PROG_COMMAREA_LEN = 12;

const MSG_PROMPT_FOR_INPUT = 'Please enter Account and Card Number';
const MSG_FOUND_CARDS = '   Displaying requested details';

type Flag = ' ' | '0' | '1';

function run(ctx: CicsContext): void {
  installAbendRoutine(ctx, LIT_THISPGM);
  const map = new SymbolicMap(LIT_THISMAP, LIT_THISMAPSET);
  let returnMsg = moveAlnum('', 75);
  let infoMsg = moveAlnum('', 40);
  let inputError = false;
  let acctFlag: Flag = ' ';
  let cardFlag: Flag = ' ';
  let ccAcctId = moveAlnum('', 11);
  let ccCardNum = moveAlnum('', 16);
  let card: CardRecord | undefined;
  const msgOff = (): boolean => /^ *$/.test(returnMsg);
  const setMsg = (m: string): void => { returnMsg = moveAlnum(m, 75); };

  let cdemo: CarddemoCommarea;
  let prog: Buffer;
  const wsCdemo = new CarddemoCommarea();
  if (ctx.eib.eibcalen === 0 || (wsCdemo.fromProgram === LIT_MENUPGM && !wsCdemo.isPgmReenter)) {
    cdemo = initializedCommarea();
    prog = Buffer.alloc(PROG_COMMAREA_LEN, 0x20);
  } else {
    cdemo = CarddemoCommarea.fromDfhCommarea(ctx.commarea);
    prog = programCommarea(ctx.commarea, PROG_COMMAREA_LEN);
  }

  let aid = storePfKey(ctx.eib.eibaid, '     ');
  if (!(aid === CCARD_AID.ENTER || aid === CCARD_AID.PFK03)) aid = CCARD_AID.ENTER;

  const commonReturn = (): never => ctx.return({ transid: LIT_THISTRANID, commarea: buildWsCommarea(cdemo, prog) });

  const sendMapScreen = (): void => {
    map.lowValues();
    populateHeaderInfo(map, { tranid: LIT_THISTRANID, pgmname: LIT_THISPGM, now: ctx.now() });
    // 1200-SETUP-SCREEN-VARS
    if (ctx.eib.eibcalen === 0) {
      infoMsg = moveAlnum(MSG_PROMPT_FOR_INPUT, 40);
    } else {
      if (zonedValue(cdemo.get('CDEMO-ACCT-ID')) === 0n) map.setRaw('ACCTSID', '');
      else map.set('ACCTSID', ccAcctId);
      if (zonedValue(cdemo.get('CDEMO-CARD-NUM')) === 0n) map.setRaw('CARDSID', '');
      else map.set('CARDSID', ccCardNum);
      if (infoMsg === moveAlnum(MSG_FOUND_CARDS, 40) && card) {
        map.set('CRDNAME', card.cardEmbossedName);
        map.set('EXPMON', card.cardExpiraionDate.slice(5, 7));
        map.set('EXPYEAR', card.cardExpiraionDate.slice(0, 4));
        map.set('CRDSTCD', card.cardActiveStatus);
      }
    }
    if (isSpacesOrLowValues(infoMsg)) infoMsg = moveAlnum(MSG_PROMPT_FOR_INPUT, 40);
    map.set('ERRMSG', returnMsg);
    map.set('INFOMSG', infoMsg);
    // 1300-SETUP-SCREEN-ATTRS
    const fromList = cdemo.get('CDEMO-LAST-MAPSET') === LIT_CCLISTMAPSET && cdemo.fromProgram === LIT_CCLISTPGM;
    map.setAttr('ACCTSID', fromList ? DFHBMSCA.DFHBMPRF : DFHBMSCA.DFHBMFSE);
    map.setAttr('CARDSID', fromList ? DFHBMSCA.DFHBMPRF : DFHBMSCA.DFHBMFSE);
    if (acctFlag === '0' || acctFlag === ' ') map.setLength('ACCTSID', -1);
    else if (cardFlag === '0' || cardFlag === ' ') map.setLength('CARDSID', -1);
    else map.setLength('ACCTSID', -1);
    if (fromList) {
      map.setColor('ACCTSID', DFHCOLOR.DFHDFCOL);
      map.setColor('CARDSID', DFHCOLOR.DFHDFCOL);
    }
    if (acctFlag === '0') map.setColor('ACCTSID', DFHCOLOR.DFHRED);
    if (cardFlag === '0') map.setColor('CARDSID', DFHCOLOR.DFHRED);
    if (acctFlag === ' ' && cdemo.isPgmReenter) {
      map.set('ACCTSID', '*');
      map.setColor('ACCTSID', DFHCOLOR.DFHRED);
    }
    if (cardFlag === ' ' && cdemo.isPgmReenter) {
      map.set('CARDSID', '*');
      map.setColor('CARDSID', DFHCOLOR.DFHRED);
    }
    map.setColor('INFOMSG', isSpacesOrLowValues(infoMsg) ? undefined : DFHCOLOR.DFHNEUTR);
    // 1400-SEND-SCREEN
    cdemo.setPgmReenter();
    ctx.sendMap(map, { mapset: LIT_THISMAPSET, cursor: true, erase: true });
  };

  const editAccount = (): void => {
    acctFlag = '0';
    if (isSpacesOrLowValues(ccAcctId) || zonedValue(ccAcctId) === 0n) {
      inputError = true;
      acctFlag = ' ';
      if (msgOff()) setMsg('Account number not provided');
      cdemo.setNumber('CDEMO-ACCT-ID', 0);
      return;
    }
    if (!isNumeric(ccAcctId)) {
      inputError = true;
      acctFlag = '0';
      if (msgOff()) setMsg('ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER');
      cdemo.setNumber('CDEMO-ACCT-ID', 0);
    } else {
      cdemo.setNumber('CDEMO-ACCT-ID', ccAcctId);
      acctFlag = '1';
    }
  };

  const editCard = (): void => {
    cardFlag = '0';
    if (isSpacesOrLowValues(ccCardNum) || zonedValue(ccCardNum) === 0n) {
      inputError = true;
      cardFlag = ' ';
      if (msgOff()) setMsg('Card number not provided');
      cdemo.setNumber('CDEMO-CARD-NUM', 0);
      return;
    }
    if (!isNumeric(ccCardNum)) {
      inputError = true;
      cardFlag = '0';
      if (msgOff()) setMsg('CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER');
      cdemo.setNumber('CDEMO-CARD-NUM', 0);
    } else {
      cdemo.setNumber('CDEMO-CARD-NUM', ccCardNum);
      cardFlag = '1';
    }
  };

  const processInputs = (): void => {
    ctx.receiveMap(map);
    inputError = false;
    cardFlag = '1';
    acctFlag = '1';
    const acctIn = map.get('ACCTSID');
    ccAcctId = acctIn === moveAlnum('*', 11) || /^ *$/.test(acctIn) ? '\0'.repeat(11) : acctIn;
    const cardIn = map.get('CARDSID');
    ccCardNum = cardIn === moveAlnum('*', 16) || /^ *$/.test(cardIn) ? '\0'.repeat(16) : cardIn;
    editAccount();
    editCard();
    if ((acctFlag as Flag) === ' ' && (cardFlag as Flag) === ' ') setMsg('No input received');
  };

  // 9100-GETCARD-BYACCTCARD (the account number is not used for the read).
  const readData = (): void => {
    const r = ctx.track(ctx.files.read({ file: 'CARDDAT', ridfld: ccCardNum, keylength: 16 }));
    if (r.resp === DFHRESP.NORMAL) {
      infoMsg = moveAlnum(MSG_FOUND_CARDS, 40);
      card = CardRecordLayout.parse(r.record!);
    } else if (r.resp === DFHRESP.NOTFND) {
      inputError = true;
      acctFlag = '0';
      cardFlag = '0';
      if (msgOff()) setMsg('Did not find cards for this search condition');
    } else {
      inputError = true;
      if (msgOff()) acctFlag = '0';
      setMsg(fileErrorMessage('READ', 'CARDDAT', r.resp, r.resp2));
    }
  };

  if (aid === CCARD_AID.PFK03) {
    cdemo.toTranid = cdemo.isBlank('CDEMO-FROM-TRANID') ? LIT_MENUTRANID : cdemo.fromTranid;
    cdemo.toProgram = cdemo.isBlank('CDEMO-FROM-PROGRAM') ? LIT_MENUPGM : cdemo.fromProgram;
    cdemo.fromTranid = LIT_THISTRANID;
    cdemo.fromProgram = LIT_THISPGM;
    cdemo.userType = 'U';
    cdemo.setPgmEnter();
    cdemo.set('CDEMO-LAST-MAPSET', LIT_THISMAPSET);
    cdemo.set('CDEMO-LAST-MAP', LIT_THISMAP);
    ctx.xctl(cdemo.toProgram, cdemo.toBuffer());
  } else if (cdemo.isPgmEnter && cdemo.fromProgram === LIT_CCLISTPGM) {
    ccAcctId = cdemo.get('CDEMO-ACCT-ID');
    ccCardNum = cdemo.get('CDEMO-CARD-NUM');
    readData();
    sendMapScreen();
    commonReturn();
  } else if (cdemo.isPgmEnter) {
    sendMapScreen();
    commonReturn();
  } else if (cdemo.isPgmReenter) {
    processInputs();
    if (!inputError) readData();
    sendMapScreen();
    commonReturn();
  } else {
    sendPlainText(ctx, 'UNEXPECTED DATA SCENARIO');
  }
}

export const COCRDSLC: OnlineProgram = { program: LIT_THISPGM, transid: LIT_THISTRANID, run };
