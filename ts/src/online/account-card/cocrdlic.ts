/**
 * COCRDLIC - credit card list (transaction CCLI, map CCRDLIA / mapset COCRDLI).
 * Browses CARDDAT 7 rows per page with optional account/card filters; 'S' row
 * selection XCTLs to COCRDSLC, 'U' to COCRDUPC. PF7/PF8 page, PF3 to menu.
 */
import { CCARD_AID } from '../../common/copybooks/index.js';
import type { CicsResult } from '../../common/vsam/index.js';
import {
  CarddemoCommarea,
  DFHBMSCA,
  DFHCOLOR,
  DFHRESP,
  SymbolicMap,
  moveAlnum,
  populateHeaderInfo,
  storePfKey,
  type CicsContext,
  type OnlineProgram,
} from '../framework/index.js';
import { buildWsCommarea, fileErrorMessage, initializedCommarea, installAbendRoutine, isNumeric, programCommarea, sendPlainText, zonedValue } from './common.js';

const LIT_THISPGM = 'COCRDLIC';
const LIT_THISTRANID = 'CCLI';
const LIT_THISMAPSET = 'COCRDLI';
const LIT_THISMAP = 'CCRDLIA';
const LIT_MENUPGM = 'COMEN01C';
const LIT_CARDDTLPGM = 'COCRDSLC';
const LIT_CARDUPDPGM = 'COCRDUPC';
const LIT_CARD_FILE = 'CARDDAT';
const MAX_SCREEN_LINES = 7;
const ROW_LEN = 28;

const MSG_INFORM_REC_ACTIONS = 'TYPE S FOR DETAIL, U TO UPDATE ANY RECORD';
const MSG_NO_RECORDS_FOUND = 'NO RECORDS FOUND FOR THIS SEARCH CONDITION.';
const MSG_MORE_THAN_1_ACTION = 'PLEASE SELECT ONLY ONE RECORD TO VIEW OR UPDATE';
const MSG_INVALID_ACTION_CODE = 'INVALID ACTION CODE';

/** WS-THIS-PROGCOMMAREA (254 bytes, including WS-SCREEN-DATA). */
export const COCRDLIC_PROGCOMMAREA_LEN = 27 + 27 + 1 + 1 + 1 + 1 + MAX_SCREEN_LINES * ROW_LEN;

interface ListState {
  lastCardNum: string;
  lastCardAcctId: string;
  firstCardNum: string;
  firstCardAcctId: string;
  /** WS-CA-SCREEN-NUM PIC 9(1). */
  screenNum: number;
  /** WS-CA-LAST-PAGE-DISPLAYED PIC 9(1): 0 shown, 9 not shown. */
  lastPageDisplayed: number;
  /** WS-CA-NEXT-PAGE-IND: 'Y' exists, LOW-VALUES not exists. */
  nextPageInd: string;
  returnFlag: string;
  /** WS-EACH-CARD(1..7): acct X(11) + card X(16) + status X(1). */
  rows: string[];
}

const digitOf = (c: string): number => Number(zonedValue(c));

function parseState(buf: Buffer): ListState {
  const s = buf.toString('latin1');
  const rows: string[] = [];
  for (let i = 0; i < MAX_SCREEN_LINES; i++) rows.push(s.slice(58 + i * ROW_LEN, 58 + (i + 1) * ROW_LEN));
  return {
    lastCardNum: s.slice(0, 16),
    lastCardAcctId: s.slice(16, 27),
    firstCardNum: s.slice(27, 43),
    firstCardAcctId: s.slice(43, 54),
    screenNum: digitOf(s.charAt(54)),
    lastPageDisplayed: digitOf(s.charAt(55)),
    nextPageInd: s.charAt(56),
    returnFlag: s.charAt(57),
    rows,
  };
}

function serializeState(st: ListState): Buffer {
  const s = st.lastCardNum + st.lastCardAcctId + st.firstCardNum + st.firstCardAcctId + String(st.screenNum % 10) +
    String(st.lastPageDisplayed % 10) + st.nextPageInd + st.returnFlag + st.rows.join('');
  return Buffer.from(s, 'latin1');
}

/** INITIALIZE WS-THIS-PROGCOMMAREA (alphanumerics spaces, numerics zero). */
function initialState(): ListState {
  return parseState(Buffer.from(' '.repeat(16) + '0'.repeat(11) + ' '.repeat(16) + '0'.repeat(11) + '00' + '  ' + ' '.repeat(196), 'latin1'));
}

const LOW_ROW = '\0'.repeat(ROW_LEN);
const isLow = (s: string): boolean => /^\0*$/.test(s);

function run(ctx: CicsContext): void {
  installAbendRoutine(ctx, LIT_THISPGM);
  const map = new SymbolicMap(LIT_THISMAP, LIT_THISMAPSET);
  let errorMsg = moveAlnum('', 75);
  let infoMsg = moveAlnum('', 45);
  /** WS-INPUT-FLAG: INPUT-OK is ' ', '0' or LOW-VALUES. */
  let inputError = false;
  let acctFlag = ' ' as ' ' | '0' | '1';
  let cardFlag = ' ' as ' ' | '0' | '1';
  let protectRows = ' ';
  const selectFlags = ' '.repeat(MAX_SCREEN_LINES).split('');
  let selectErrors = ' '.repeat(MAX_SCREEN_LINES).split('');
  let iSelected = 0;
  let ccAcctId = moveAlnum('', 11);
  let ccCardNum = moveAlnum('', 16);
  /** WS-CARD-RID-CARDNUM (INITIALIZEd to spaces). */
  let ridCardNum = moveAlnum('', 16);
  /** CARD-RECORD (last record read into it during this task). */
  let cardRecord = '\0'.repeat(150);
  const msgOff = (): boolean => /^ *$/.test(errorMsg);
  const setMsg = (m: string): void => { errorMsg = moveAlnum(m, 75); };

  let cdemo: CarddemoCommarea;
  let st: ListState;
  if (ctx.eib.eibcalen === 0) {
    cdemo = initializedCommarea();
    st = initialState();
    cdemo.fromTranid = LIT_THISTRANID;
    cdemo.fromProgram = LIT_THISPGM;
    cdemo.userType = 'U';
    cdemo.setPgmEnter();
    cdemo.set('CDEMO-LAST-MAP', LIT_THISMAP);
    cdemo.set('CDEMO-LAST-MAPSET', LIT_THISMAPSET);
    st.screenNum = 1;
    st.lastPageDisplayed = 9;
  } else {
    cdemo = CarddemoCommarea.fromDfhCommarea(ctx.commarea);
    st = parseState(programCommarea(ctx.commarea, COCRDLIC_PROGCOMMAREA_LEN));
  }
  if (cdemo.isPgmEnter && cdemo.fromProgram !== LIT_THISPGM) {
    st = initialState();
    cdemo.setPgmEnter();
    cdemo.set('CDEMO-LAST-MAP', LIT_THISMAP);
    st.screenNum = 1;
    st.lastPageDisplayed = 9;
  }

  let aid = storePfKey(ctx.eib.eibaid, '     ');

  // 2000-RECEIVE-MAP
  const editInputs = (): void => {
    ctx.receiveMap(map);
    ccAcctId = map.get('ACCTSID');
    ccCardNum = map.get('CARDSID');
    for (let i = 0; i < MAX_SCREEN_LINES; i++) selectFlags[i] = map.get(`CRDSEL${i + 1}`).charAt(0) || '\0';
    inputError = false;
    protectRows = '0';
    // 2210-EDIT-ACCOUNT
    acctFlag = ' ';
    if (/^\0*$/.test(ccAcctId) || /^ *$/.test(ccAcctId) || zonedValue(ccAcctId) === 0n) {
      cdemo.setNumber('CDEMO-ACCT-ID', 0);
    } else if (!isNumeric(ccAcctId)) {
      inputError = true;
      acctFlag = '0';
      protectRows = '1';
      setMsg('ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER');
      cdemo.setNumber('CDEMO-ACCT-ID', 0);
    } else {
      cdemo.setNumber('CDEMO-ACCT-ID', ccAcctId);
      acctFlag = '1';
    }
    // 2220-EDIT-CARD
    cardFlag = ' ';
    if (/^\0*$/.test(ccCardNum) || /^ *$/.test(ccCardNum) || zonedValue(ccCardNum) === 0n) {
      cdemo.setNumber('CDEMO-CARD-NUM', 0);
    } else if (!isNumeric(ccCardNum)) {
      inputError = true;
      cardFlag = '0';
      protectRows = '1';
      if (msgOff()) setMsg('CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER');
      cdemo.setNumber('CDEMO-CARD-NUM', 0);
    } else {
      cdemo.setNumber('CDEMO-CARD-NUM', ccCardNum);
      cardFlag = '1';
    }
    // 2250-EDIT-ARRAY
    if (inputError) return;
    const tally = selectFlags.filter((c) => c === 'S' || c === 'U').length;
    if (tally > 1) {
      inputError = true;
      setMsg(MSG_MORE_THAN_1_ACTION);
      selectErrors = selectFlags.map((c) => (c === 'S' || c === 'U' ? '1' : '0'));
    }
    iSelected = 0;
    for (let i = 0; i < MAX_SCREEN_LINES; i++) {
      const c = selectFlags[i]!;
      if (c === 'S' || c === 'U') {
        iSelected = i + 1;
        if (errorMsg === moveAlnum(MSG_MORE_THAN_1_ACTION, 75)) selectErrors[i] = '1';
      } else if (c === ' ' || c === '\0') {
        continue;
      } else {
        inputError = true;
        selectErrors[i] = '1';
        if (msgOff()) setMsg(MSG_INVALID_ACTION_CODE);
      }
    }
  };

  if (ctx.eib.eibcalen > 0 && cdemo.fromProgram === LIT_THISPGM) editInputs();

  if (!(aid === CCARD_AID.ENTER || aid === CCARD_AID.PFK03 || aid === CCARD_AID.PFK07 || aid === CCARD_AID.PFK08)) aid = CCARD_AID.ENTER;

  if (aid === CCARD_AID.PFK03 && cdemo.fromProgram === LIT_THISPGM) {
    cdemo.fromTranid = LIT_THISTRANID;
    cdemo.fromProgram = LIT_THISPGM;
    cdemo.userType = 'U';
    cdemo.setPgmEnter();
    cdemo.set('CDEMO-LAST-MAPSET', LIT_THISMAPSET);
    cdemo.set('CDEMO-LAST-MAP', LIT_THISMAP);
    cdemo.toProgram = LIT_MENUPGM;
    ctx.xctl(LIT_MENUPGM, cdemo.toBuffer());
  }
  if (aid !== CCARD_AID.PFK08) st.lastPageDisplayed = 9;

  const cardNumOf = (rec: string): string => rec.slice(0, 16);
  const acctOf = (rec: string): string => rec.slice(16, 27);
  const statusOf = (rec: string): string => rec.slice(90, 91);
  const rowOf = (rec: string): string => acctOf(rec) + cardNumOf(rec) + statusOf(rec);

  // 9500-FILTER-RECORDS
  const include = (rec: string): boolean => {
    if (acctFlag === '1' && acctOf(rec) !== ccAcctId) return false;
    if (cardFlag === '1' && cardNumOf(rec) !== ccCardNum) return false;
    return true;
  };
  const fileError = (r: CicsResult): void => setMsg(fileErrorMessage('READ', LIT_CARD_FILE, r.resp, r.resp2));
  const ok = (r: CicsResult): boolean => r.resp === DFHRESP.NORMAL || r.resp === DFHRESP.DUPREC;
  const browseRead = (prev: boolean): CicsResult => {
    const r = prev ? ctx.files.readprev({ file: LIT_CARD_FILE }) : ctx.files.readnext({ file: LIT_CARD_FILE });
    if (r.record) cardRecord = r.record.toString('latin1');
    if (r.ridfld !== undefined) ridCardNum = r.ridfld;
    return r;
  };

  const readForward = (): void => {
    st.rows = Array.from({ length: MAX_SCREEN_LINES }, () => LOW_ROW);
    ctx.files.startbr({ file: LIT_CARD_FILE, ridfld: ridCardNum, keylength: 16, gteq: true });
    let counter = 0;
    st.nextPageInd = 'Y';
    for (;;) {
      const r = browseRead(false);
      if (ok(r)) {
        if (include(cardRecord)) {
          counter++;
          st.rows[counter - 1] = rowOf(cardRecord);
          if (counter === 1) {
            st.firstCardAcctId = acctOf(cardRecord);
            st.firstCardNum = cardNumOf(cardRecord);
            if (st.screenNum === 0) st.screenNum += 1;
          }
        }
        if (counter === MAX_SCREEN_LINES) {
          st.lastCardAcctId = acctOf(cardRecord);
          st.lastCardNum = cardNumOf(cardRecord);
          const r2 = browseRead(false);
          if (ok(r2)) {
            st.nextPageInd = 'Y';
            st.lastCardAcctId = acctOf(cardRecord);
            st.lastCardNum = cardNumOf(cardRecord);
          } else if (r2.resp === DFHRESP.ENDFILE) {
            st.nextPageInd = '\0';
            if (msgOff()) setMsg('NO MORE RECORDS TO SHOW');
          } else {
            fileError(r2);
          }
          break;
        }
      } else if (r.resp === DFHRESP.ENDFILE) {
        st.nextPageInd = '\0';
        st.lastCardAcctId = acctOf(cardRecord);
        st.lastCardNum = cardNumOf(cardRecord);
        if (msgOff()) setMsg('NO MORE RECORDS TO SHOW');
        if (st.screenNum === 1 && counter === 0) setMsg(MSG_NO_RECORDS_FOUND);
        break;
      } else {
        fileError(r);
        break;
      }
    }
    ctx.files.endbr({ file: LIT_CARD_FILE });
  };

  const readBackwards = (): void => {
    st.rows = Array.from({ length: MAX_SCREEN_LINES }, () => LOW_ROW);
    st.lastCardNum = st.firstCardNum;
    st.lastCardAcctId = st.firstCardAcctId;
    ctx.files.startbr({ file: LIT_CARD_FILE, ridfld: ridCardNum, keylength: 16, gteq: true });
    let counter = MAX_SCREEN_LINES + 1;
    st.nextPageInd = 'Y';
    const r = browseRead(true);
    if (ok(r)) {
      counter--;
      for (;;) {
        const r2 = browseRead(true);
        if (!ok(r2)) {
          fileError(r2);
          break;
        }
        if (include(cardRecord)) {
          st.rows[counter - 1] = rowOf(cardRecord);
          counter--;
          if (counter === 0) {
            st.firstCardAcctId = acctOf(cardRecord);
            st.firstCardNum = cardNumOf(cardRecord);
            break;
          }
        }
      }
    } else {
      fileError(r);
    }
    ctx.files.endbr({ file: LIT_CARD_FILE });
  };

  const sendMapScreen = (): void => {
    // 1100-SCREEN-INIT
    map.lowValues();
    populateHeaderInfo(map, { tranid: LIT_THISTRANID, pgmname: LIT_THISPGM, now: ctx.now() });
    map.set('PAGENO', String(st.screenNum % 10));
    infoMsg = moveAlnum('', 45);
    map.set('INFOMSG', infoMsg);
    // 1200-SCREEN-ARRAY-INIT
    for (let i = 0; i < MAX_SCREEN_LINES; i++) {
      const row = st.rows[i]!;
      if (isLow(row)) continue;
      map.setRaw(`CRDSEL${i + 1}`, selectFlags[i]!);
      map.set(`ACCTNO${i + 1}`, row.slice(0, 11));
      map.set(`CRDNUM${i + 1}`, row.slice(11, 27));
      map.set(`CRDSTS${i + 1}`, row.slice(27, 28));
    }
    // 1250-SETUP-ARRAY-ATTRIBS (row 1 differs from rows 2-7 in the COBOL)
    for (let i = 0; i < MAX_SCREEN_LINES; i++) {
      const name = `CRDSEL${i + 1}`;
      if (isLow(st.rows[i]!) || protectRows === '1') {
        map.setAttr(name, i === 0 ? DFHBMSCA.DFHBMPRF : DFHBMSCA.DFHBMPRO);
      } else {
        if (selectErrors[i] === '1') {
          map.setColor(name, DFHCOLOR.DFHRED);
          if (i === 0) {
            if (selectFlags[0] === ' ' || selectFlags[0] === '\0') map.set(name, '*');
          } else {
            map.setLength(name, -1);
          }
        }
        map.setAttr(name, DFHBMSCA.DFHBMFSE);
      }
    }
    // 1300-SETUP-SCREEN-ATTRS
    if (!(ctx.eib.eibcalen === 0 || (cdemo.isPgmEnter && cdemo.fromProgram === LIT_MENUPGM))) {
      if (acctFlag === '1' || acctFlag === '0') {
        map.set('ACCTSID', ccAcctId);
        map.setAttr('ACCTSID', DFHBMSCA.DFHBMFSE);
      } else if (zonedValue(cdemo.get('CDEMO-ACCT-ID')) === 0n) {
        map.setRaw('ACCTSID', '');
      } else {
        map.set('ACCTSID', cdemo.get('CDEMO-ACCT-ID'));
        map.setAttr('ACCTSID', DFHBMSCA.DFHBMFSE);
      }
      if (cardFlag === '1' || cardFlag === '0') {
        map.set('CARDSID', ccCardNum);
        map.setAttr('CARDSID', DFHBMSCA.DFHBMFSE);
      } else if (zonedValue(cdemo.get('CDEMO-CARD-NUM')) === 0n) {
        map.setRaw('CARDSID', '');
      } else {
        map.set('CARDSID', cdemo.get('CDEMO-CARD-NUM'));
        map.setAttr('CARDSID', DFHBMSCA.DFHBMFSE);
      }
    }
    if (acctFlag === '0') {
      map.setColor('ACCTSID', DFHCOLOR.DFHRED);
      map.setLength('ACCTSID', -1);
    }
    if (cardFlag === '0') {
      map.setColor('CARDSID', DFHCOLOR.DFHRED);
      map.setLength('CARDSID', -1);
    }
    if (!inputError) map.setLength('ACCTSID', -1);
    // 1400-SETUP-MESSAGE
    const nextNotExists = st.nextPageInd === '\0';
    if (acctFlag === '0' || cardFlag === '0') {
      // CONTINUE
    } else if (aid === CCARD_AID.PFK07 && st.screenNum === 1) {
      setMsg('NO PREVIOUS PAGES TO DISPLAY');
    } else if (aid === CCARD_AID.PFK08 && nextNotExists && st.lastPageDisplayed === 0) {
      setMsg('NO MORE PAGES TO DISPLAY');
    } else if (aid === CCARD_AID.PFK08 && nextNotExists) {
      infoMsg = moveAlnum(MSG_INFORM_REC_ACTIONS, 45);
      if (st.lastPageDisplayed === 9) st.lastPageDisplayed = 0;
    } else if (/^( *|\0*)$/.test(infoMsg) || st.nextPageInd === 'Y') {
      infoMsg = moveAlnum(MSG_INFORM_REC_ACTIONS, 45);
    } else {
      infoMsg = moveAlnum('', 45);
    }
    map.set('ERRMSG', errorMsg);
    if (!/^( *|\0*)$/.test(infoMsg) && errorMsg !== moveAlnum(MSG_NO_RECORDS_FOUND, 75)) {
      map.set('INFOMSG', infoMsg);
      map.setColor('INFOMSG', DFHCOLOR.DFHNEUTR);
    }
    // 1500-SEND-SCREEN
    ctx.sendMap(map, { mapset: LIT_THISMAPSET, cursor: true, erase: true });
  };

  const commonReturn = (): never => {
    cdemo.fromTranid = LIT_THISTRANID;
    cdemo.fromProgram = LIT_THISPGM;
    cdemo.set('CDEMO-LAST-MAPSET', LIT_THISMAPSET);
    cdemo.set('CDEMO-LAST-MAP', LIT_THISMAP);
    return ctx.return({ transid: LIT_THISTRANID, commarea: buildWsCommarea(cdemo, serializeState(st)) });
  };

  const selected = (code: string): boolean => iSelected >= 1 && iSelected <= MAX_SCREEN_LINES && selectFlags[iSelected - 1] === code;
  const xctlToRow = (program: string): never => {
    cdemo.fromTranid = LIT_THISTRANID;
    cdemo.fromProgram = LIT_THISPGM;
    cdemo.userType = 'U';
    cdemo.setPgmEnter();
    cdemo.set('CDEMO-LAST-MAPSET', LIT_THISMAPSET);
    cdemo.set('CDEMO-LAST-MAP', LIT_THISMAP);
    const row = st.rows[iSelected - 1]!;
    cdemo.setNumber('CDEMO-ACCT-ID', zonedValue(row.slice(0, 11)));
    cdemo.setNumber('CDEMO-CARD-NUM', zonedValue(row.slice(11, 27)));
    ctx.xctl(program, cdemo.toBuffer());
    throw new Error('unreachable');
  };

  if (inputError) {
    cdemo.fromProgram = LIT_THISPGM;
    cdemo.set('CDEMO-LAST-MAPSET', LIT_THISMAPSET);
    cdemo.set('CDEMO-LAST-MAP', LIT_THISMAP);
    if (acctFlag !== '0' && cardFlag !== '0') readForward();
    sendMapScreen();
    commonReturn();
  } else if (aid === CCARD_AID.PFK07 && st.screenNum === 1) {
    ridCardNum = st.firstCardNum;
    readForward();
    sendMapScreen();
    commonReturn();
  } else if (aid === CCARD_AID.PFK03 || (cdemo.isPgmReenter && cdemo.fromProgram !== LIT_THISPGM)) {
    cdemo = initializedCommarea();
    st = initialState();
    cdemo.fromTranid = LIT_THISTRANID;
    cdemo.fromProgram = LIT_THISPGM;
    cdemo.userType = 'U';
    cdemo.setPgmEnter();
    cdemo.set('CDEMO-LAST-MAP', LIT_THISMAP);
    cdemo.set('CDEMO-LAST-MAPSET', LIT_THISMAPSET);
    st.screenNum = 1;
    st.lastPageDisplayed = 9;
    ridCardNum = st.firstCardNum;
    readForward();
    sendMapScreen();
    commonReturn();
  } else if (aid === CCARD_AID.PFK08 && st.nextPageInd === 'Y') {
    ridCardNum = st.lastCardNum;
    st.screenNum = (st.screenNum + 1) % 10;
    readForward();
    sendMapScreen();
    commonReturn();
  } else if (aid === CCARD_AID.PFK07 && st.screenNum !== 1) {
    ridCardNum = st.firstCardNum;
    st.screenNum = (st.screenNum + 9) % 10;
    readBackwards();
    sendMapScreen();
    commonReturn();
  } else if (aid === CCARD_AID.ENTER && selected('S') && cdemo.fromProgram === LIT_THISPGM) {
    xctlToRow(LIT_CARDDTLPGM);
  } else if (aid === CCARD_AID.ENTER && selected('U') && cdemo.fromProgram === LIT_THISPGM) {
    xctlToRow(LIT_CARDUPDPGM);
  } else {
    ridCardNum = st.firstCardNum;
    readForward();
    sendMapScreen();
    commonReturn();
  }
  sendPlainText(ctx, errorMsg);
}

export const COCRDLIC: OnlineProgram = { program: LIT_THISPGM, transid: LIT_THISTRANID, run };
