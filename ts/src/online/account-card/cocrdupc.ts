/**
 * COCRDUPC - credit card update (transaction CCUP, map CCRDUPA / mapset COCRDUP).
 * Fetch by account+card, edit name/status/expiry, confirm with PF5, then
 * READ UPDATE + "changed by someone else" check + REWRITE of CARDDAT.
 */
import { CCARD_AID } from '../../common/copybooks/index.js';
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
  upperCase,
  type CicsContext,
  type FieldAttribute,
  type OnlineProgram,
} from '../framework/index.js';
import { abendRoutine, buildWsCommarea, fileErrorMessage, initializedCommarea, installAbendRoutine, isNumeric, programCommarea, zonedValue } from './common.js';

const LIT_THISPGM = 'COCRDUPC';
const LIT_THISTRANID = 'CCUP';
const LIT_THISMAPSET = 'COCRDUP';
const LIT_THISMAP = 'CCRDUPA';
const LIT_CCLISTPGM = 'COCRDLIC';
const LIT_CCLISTMAPSET = 'COCRDLI';
const LIT_MENUPGM = 'COMEN01C';
const LIT_MENUTRANID = 'CM00';
const LIT_CARDFILENAME = 'CARDDAT';

const INFO = {
  FOUND_CARDS: 'Details of selected card shown above',
  PROMPT_FOR_SEARCH_KEYS: 'Please enter Account and Card Number',
  PROMPT_FOR_CHANGES: 'Update card details presented above.',
  PROMPT_FOR_CONFIRMATION: 'Changes validated.Press F5 to save',
  CONFIRM_UPDATE_SUCCESS: 'Changes committed to database',
  INFORM_FAILURE: 'Changes unsuccessful. Please try again',
} as const;
const MSG = {
  NO_CHANGES_DETECTED: 'No change detected with respect to values fetched.',
  COULD_NOT_LOCK_FOR_UPDATE: 'Could not lock record for update',
  DATA_WAS_CHANGED_BEFORE_UPDATE: 'Record changed by some one else. Please review',
  LOCKED_BUT_UPDATE_FAILED: 'Update of record failed',
} as const;

/** CCUP-OLD-DETAILS / CCUP-NEW-DETAILS (89 bytes each). */
interface CardDetails {
  acctId: string; // X(11)
  cardId: string; // X(16)
  cvvCd: string; // X(3)
  crdName: string; // X(50)
  expYear: string; // X(4)
  expMon: string; // X(2)
  expDay: string; // X(2)
  crdStcd: string; // X(1)
}
const DETAIL_FIELDS: readonly [keyof CardDetails, number][] = [
  ['acctId', 11], ['cardId', 16], ['cvvCd', 3], ['crdName', 50], ['expYear', 4], ['expMon', 2], ['expDay', 2], ['crdStcd', 1],
];
const DETAILS_LEN = 89;
const UPDATE_RECORD_LEN = 150;
/** WS-THIS-PROGCOMMAREA: change action + old + new + CARD-UPDATE-RECORD. */
export const COCRDUPC_PROGCOMMAREA_LEN = 1 + DETAILS_LEN * 2 + UPDATE_RECORD_LEN;

const spaces = (n: number): string => ' '.repeat(n);
const low = (n: number): string => '\0'.repeat(n);

function parseDetails(s: string): CardDetails {
  const d = {} as CardDetails;
  let off = 0;
  for (const [k, n] of DETAIL_FIELDS) {
    d[k] = s.slice(off, off + n);
    off += n;
  }
  return d;
}
const serializeDetails = (d: CardDetails): string => DETAIL_FIELDS.map(([k, n]) => moveAlnum(d[k], n)).join('');
/** INITIALIZE CCUP-xxx-DETAILS. */
const blankDetails = (): CardDetails => parseDetails(spaces(DETAILS_LEN));
/** CCUP-xxx-CARDDATA (name + expiry date + status). */
const cardData = (d: CardDetails): string => d.crdName + d.expYear + d.expMon + d.expDay + d.crdStcd;

interface UpdState {
  changeAction: string;
  old: CardDetails;
  nu: CardDetails;
  updateRecord: string;
}

function parseState(buf: Buffer): UpdState {
  const s = buf.toString('latin1');
  return {
    changeAction: s.charAt(0),
    old: parseDetails(s.slice(1, 1 + DETAILS_LEN)),
    nu: parseDetails(s.slice(1 + DETAILS_LEN, 1 + 2 * DETAILS_LEN)),
    updateRecord: s.slice(1 + 2 * DETAILS_LEN, COCRDUPC_PROGCOMMAREA_LEN),
  };
}
const serializeState = (st: UpdState): Buffer =>
  Buffer.from(st.changeAction + serializeDetails(st.old) + serializeDetails(st.nu) + st.updateRecord, 'latin1');

/** INITIALIZE WS-THIS-PROGCOMMAREA (CARD-UPDATE-RECORD FILLER is not touched by INITIALIZE). */
function initialState(prev?: UpdState): UpdState {
  const filler = prev ? prev.updateRecord.slice(91) : spaces(59);
  return {
    changeAction: ' ',
    old: blankDetails(),
    nu: blankDetails(),
    updateRecord: spaces(16) + '0'.repeat(11) + '000' + spaces(50) + spaces(10) + ' ' + filler,
  };
}

type Flag = ' ' | '0' | '1';
const isNotFetched = (a: string): boolean => a === '\0' || a === ' ';
const isChangesMade = (a: string): boolean => 'ENCLF'.includes(a) && a.length === 1;

function run(ctx: CicsContext): void {
  installAbendRoutine(ctx, LIT_THISPGM);
  const map = new SymbolicMap(LIT_THISMAP, LIT_THISMAPSET);
  let returnMsg = spaces(75);
  let infoMsg = spaces(40);
  /** WS-INPUT-FLAG: '0' ok, '1' error, LOW-VALUES pending (INITIALIZE gives ' '). */
  let inputFlag = ' ';
  let acctFlag = ' ' as Flag;
  let cardFlag = ' ' as Flag;
  let nameFlag = ' ' as Flag;
  let statusFlag = ' ' as Flag;
  let monFlag = ' ' as Flag;
  let yearFlag = ' ' as Flag;
  let ccAcctId = spaces(11);
  let ccCardNum = spaces(16);
  const msgOff = (): boolean => /^ *$/.test(returnMsg);
  const setMsg = (m: string): void => { returnMsg = moveAlnum(m, 75); };
  const setInfo = (m: string): void => { infoMsg = moveAlnum(m, 40); };
  const inputError = (): boolean => inputFlag === '1';
  const resetMisc = (): void => {
    returnMsg = spaces(75);
    infoMsg = spaces(40);
    inputFlag = ' ';
    acctFlag = cardFlag = nameFlag = statusFlag = monFlag = yearFlag = ' ';
  };

  let cdemo: CarddemoCommarea;
  let st: UpdState;
  const wsCdemo = new CarddemoCommarea();
  if (ctx.eib.eibcalen === 0 || (wsCdemo.fromProgram === LIT_MENUPGM && !wsCdemo.isPgmReenter)) {
    cdemo = initializedCommarea();
    st = initialState();
    cdemo.setPgmEnter();
    st.changeAction = '\0';
  } else {
    cdemo = CarddemoCommarea.fromDfhCommarea(ctx.commarea);
    st = parseState(programCommarea(ctx.commarea, COCRDUPC_PROGCOMMAREA_LEN));
  }

  let aid = storePfKey(ctx.eib.eibaid, '     ');
  const pfkValid =
    aid === CCARD_AID.ENTER ||
    aid === CCARD_AID.PFK03 ||
    (aid === CCARD_AID.PFK05 && st.changeAction === 'N') ||
    (aid === CCARD_AID.PFK12 && !isNotFetched(st.changeAction));
  if (!pfkValid) aid = CCARD_AID.ENTER;

  const commonReturn = (): never => ctx.return({ transid: LIT_THISTRANID, commarea: buildWsCommarea(cdemo, serializeState(st)) });

  // 9100-GETCARD-BYACCTCARD
  let cardRecord = '';
  const getCard = (): void => {
    const r = ctx.track(ctx.files.read({ file: LIT_CARDFILENAME, ridfld: ccCardNum, keylength: 16 }));
    if (r.resp === DFHRESP.NORMAL) {
      cardRecord = r.record!.toString('latin1');
      setInfo(INFO.FOUND_CARDS);
    } else if (r.resp === DFHRESP.NOTFND) {
      inputFlag = '1';
      acctFlag = '0';
      cardFlag = '0';
      if (msgOff()) setMsg('Did not find cards for this search condition');
    } else {
      inputFlag = '1';
      if (msgOff()) acctFlag = '0';
      setMsg(fileErrorMessage('READ', LIT_CARDFILENAME, r.resp, r.resp2));
    }
  };

  // 9000-READ-DATA
  const readData = (): void => {
    st.old = blankDetails();
    st.old.acctId = ccAcctId;
    st.old.cardId = ccCardNum;
    getCard();
    if (infoMsg === moveAlnum(INFO.FOUND_CARDS, 40)) {
      st.old.cvvCd = cardRecord.slice(27, 30);
      st.old.crdName = upperCase(cardRecord.slice(30, 80));
      st.old.expYear = cardRecord.slice(80, 84);
      st.old.expMon = cardRecord.slice(85, 87);
      st.old.expDay = cardRecord.slice(88, 90);
      st.old.crdStcd = cardRecord.slice(90, 91);
    }
  };

  // 9200-WRITE-PROCESSING
  const writeProcessing = (): void => {
    const r = ctx.track(ctx.files.read({ file: LIT_CARDFILENAME, ridfld: ccCardNum, keylength: 16, update: true }));
    if (r.resp !== DFHRESP.NORMAL) {
      inputFlag = '1';
      if (msgOff()) setMsg(MSG.COULD_NOT_LOCK_FOR_UPDATE);
      return;
    }
    // 9300-CHECK-CHANGE-IN-REC
    const rec = r.record!.toString('latin1');
    const cvv = rec.slice(27, 30);
    const name = upperCase(rec.slice(30, 80));
    const exp = rec.slice(80, 90);
    const status = rec.slice(90, 91);
    const o = st.old;
    if (!(cvv === o.cvvCd && name === o.crdName && exp.slice(0, 4) === o.expYear && exp.slice(5, 7) === o.expMon &&
      exp.slice(8, 10) === o.expDay && status === o.crdStcd)) {
      setMsg(MSG.DATA_WAS_CHANGED_BEFORE_UPDATE);
      st.old = { ...o, cvvCd: cvv, crdName: name, expYear: exp.slice(0, 4), expMon: exp.slice(5, 7), expDay: exp.slice(8, 10), crdStcd: status };
      return;
    }
    // INITIALIZE CARD-UPDATE-RECORD + moves. CCUP-NEW-CVV-CD is never filled (spaces); the
    // MOVE CARD-CVV-CD-N TO CARD-UPDATE-CVV-CD repairs only the sign nibble of the last byte.
    const nu = st.nu;
    const cvvOut = (nu.cvvCd.slice(0, 2) + (nu.cvvCd.charAt(2) === ' ' ? '0' : nu.cvvCd.charAt(2))).padEnd(3, ' ');
    const acctN = zonedValue(ccAcctId).toString().padStart(11, '0').slice(-11);
    const expiry = moveAlnum(nu.expYear + '-' + nu.expMon + '-' + nu.expDay, 10);
    st.updateRecord = moveAlnum(nu.cardId, 16) + acctN + cvvOut + moveAlnum(nu.crdName, 50) + expiry + moveAlnum(nu.crdStcd, 1) + st.updateRecord.slice(91);
    const w = ctx.track(ctx.files.rewrite({ file: LIT_CARDFILENAME, from: Buffer.from(st.updateRecord, 'latin1') }));
    if (w.resp !== DFHRESP.NORMAL) setMsg(MSG.LOCKED_BUT_UPDATE_FAILED);
  };

  // 1100-RECEIVE-MAP
  const receive = (): void => {
    ctx.receiveMap(map);
    st.nu = blankDetails();
    const star = (name: string, len: number): string | undefined => {
      const v = map.get(name);
      return v === moveAlnum('*', len) || /^ *$/.test(v) ? undefined : v;
    };
    const acct = star('ACCTSID', 11);
    ccAcctId = acct ?? low(11);
    st.nu.acctId = ccAcctId;
    const card = star('CARDSID', 16);
    ccCardNum = card ?? low(16);
    st.nu.cardId = ccCardNum;
    st.nu.crdName = star('CRDNAME', 50) ?? low(50);
    st.nu.crdStcd = star('CRDSTCD', 1) ?? low(1);
    st.nu.expDay = map.get('EXPDAY');
    st.nu.expMon = star('EXPMON', 2) ?? low(2);
    st.nu.expYear = star('EXPYEAR', 4) ?? low(4);
  };

  const blankOrZeros = (s: string): boolean => /^\0*$/.test(s) || /^ *$/.test(s) || /^0*$/.test(s);

  // 1200-EDIT-MAP-INPUTS
  const editInputs = (): void => {
    inputFlag = '0';
    if (isNotFetched(st.changeAction)) {
      // 1210-EDIT-ACCOUNT
      acctFlag = '0';
      if (isSpacesOrLowValues(ccAcctId) || zonedValue(ccAcctId) === 0n) {
        inputFlag = '1';
        acctFlag = ' ';
        if (msgOff()) setMsg('Account number not provided');
        cdemo.setNumber('CDEMO-ACCT-ID', 0);
        st.nu.acctId = low(11);
      } else if (!isNumeric(ccAcctId)) {
        inputFlag = '1';
        acctFlag = '0';
        if (msgOff()) setMsg('ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER');
        cdemo.setNumber('CDEMO-ACCT-ID', 0);
        st.nu.acctId = low(11);
      } else {
        cdemo.setNumber('CDEMO-ACCT-ID', ccAcctId);
        st.nu.acctId = ccAcctId;
        acctFlag = '1';
      }
      // 1220-EDIT-CARD
      cardFlag = '0';
      if (isSpacesOrLowValues(ccCardNum) || zonedValue(ccCardNum) === 0n) {
        inputFlag = '1';
        cardFlag = ' ';
        if (msgOff()) setMsg('Card number not provided');
        cdemo.setNumber('CDEMO-CARD-NUM', 0);
        st.nu.cardId = '0'.repeat(16);
      } else if (!isNumeric(ccCardNum)) {
        inputFlag = '1';
        cardFlag = '0';
        if (msgOff()) setMsg('CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER');
        cdemo.setNumber('CDEMO-CARD-NUM', 0);
        st.nu.cardId = low(16);
      } else {
        cdemo.setNumber('CDEMO-CARD-NUM', ccCardNum);
        st.nu.cardId = ccCardNum;
        cardFlag = '1';
      }
      // MOVE LOW-VALUES TO CCUP-NEW-CARDDATA
      st.nu = { ...st.nu, crdName: low(50), expYear: low(4), expMon: low(2), expDay: low(2), crdStcd: low(1) };
      if (acctFlag === ' ' && cardFlag === ' ') setMsg('No input received');
      return;
    }
    setInfo(INFO.FOUND_CARDS);
    acctFlag = '1';
    cardFlag = '1';
    cdemo.setNumber('CDEMO-ACCT-ID', zonedValue(st.old.acctId));
    cdemo.setNumber('CDEMO-CARD-NUM', zonedValue(st.old.cardId));
    if (upperCase(cardData(st.nu)) === upperCase(cardData(st.old))) setMsg(MSG.NO_CHANGES_DETECTED);
    if (returnMsg === moveAlnum(MSG.NO_CHANGES_DETECTED, 75) || st.changeAction === 'N' || st.changeAction === 'C') {
      nameFlag = statusFlag = monFlag = yearFlag = '1';
      return;
    }
    st.changeAction = 'E';
    // 1230-EDIT-NAME
    nameFlag = '0';
    if (blankOrZeros(st.nu.crdName)) {
      inputFlag = '1';
      nameFlag = ' ';
      if (msgOff()) setMsg('Card name not provided');
    } else if (st.nu.crdName.replace(/[A-Za-z]/g, ' ').trim().length !== 0) {
      inputFlag = '1';
      nameFlag = '0';
      if (msgOff()) setMsg('Card name can only contain alphabets and spaces');
    } else {
      nameFlag = '1';
    }
    // 1240-EDIT-CARDSTATUS
    statusFlag = '0';
    if (blankOrZeros(st.nu.crdStcd)) {
      inputFlag = '1';
      statusFlag = ' ';
      if (msgOff()) setMsg('Card Active Status must be Y or N');
    } else if (st.nu.crdStcd === 'Y' || st.nu.crdStcd === 'N') {
      statusFlag = '1';
    } else {
      inputFlag = '1';
      statusFlag = '0';
      if (msgOff()) setMsg('Card Active Status must be Y or N');
    }
    // 1250-EDIT-EXPIRY-MON
    monFlag = '0';
    if (blankOrZeros(st.nu.expMon)) {
      inputFlag = '1';
      monFlag = ' ';
      if (msgOff()) setMsg('Card expiry month must be between 1 and 12');
    } else {
      const m = zonedValue(st.nu.expMon);
      if (m >= 1n && m <= 12n) {
        monFlag = '1';
      } else {
        inputFlag = '1';
        monFlag = '0';
        if (msgOff()) setMsg('Card expiry month must be between 1 and 12');
      }
    }
    // 1260-EDIT-EXPIRY-YEAR (flag stays BLANK-or-previous until after the blank test)
    if (blankOrZeros(st.nu.expYear)) {
      inputFlag = '1';
      yearFlag = ' ';
      if (msgOff()) setMsg('Invalid card expiry year');
    } else {
      yearFlag = '0';
      const y = zonedValue(st.nu.expYear);
      if (y >= 1950n && y <= 2099n) {
        yearFlag = '1';
      } else {
        inputFlag = '1';
        yearFlag = '0';
        if (msgOff()) setMsg('Invalid card expiry year');
      }
    }
    if (!inputError()) st.changeAction = 'N';
  };

  // 2000-DECIDE-ACTION
  const decideAction = (): void => {
    const a = st.changeAction;
    if (isNotFetched(a) || aid === CCARD_AID.PFK12) {
      if (acctFlag === '1' && cardFlag === '1') {
        readData();
        if (infoMsg === moveAlnum(INFO.FOUND_CARDS, 40)) st.changeAction = 'S';
      }
    } else if (a === 'S') {
      if (!(inputError() || returnMsg === moveAlnum(MSG.NO_CHANGES_DETECTED, 75))) st.changeAction = 'N';
    } else if (a === 'E') {
      // CONTINUE
    } else if (a === 'N' && aid === CCARD_AID.PFK05) {
      writeProcessing();
      if (returnMsg === moveAlnum(MSG.COULD_NOT_LOCK_FOR_UPDATE, 75)) st.changeAction = 'L';
      else if (returnMsg === moveAlnum(MSG.LOCKED_BUT_UPDATE_FAILED, 75)) st.changeAction = 'F';
      else if (returnMsg === moveAlnum(MSG.DATA_WAS_CHANGED_BEFORE_UPDATE, 75)) st.changeAction = 'S';
      else st.changeAction = 'C';
    } else if (a === 'N') {
      // CONTINUE
    } else if (a === 'C') {
      st.changeAction = 'S';
      if (cdemo.isBlank('CDEMO-FROM-TRANID')) {
        cdemo.setNumber('CDEMO-ACCT-ID', 0);
        cdemo.setNumber('CDEMO-CARD-NUM', 0);
        cdemo.set('CDEMO-ACCT-STATUS', '\0');
      }
    } else {
      abendRoutine(ctx, LIT_THISPGM, '0001', 'UNEXPECTED DATA SCENARIO');
    }
  };

  // 3000-SEND-MAP
  const sendMapScreen = (): void => {
    map.lowValues();
    populateHeaderInfo(map, { tranid: LIT_THISTRANID, pgmname: LIT_THISPGM, now: ctx.now() });
    const a = st.changeAction;
    // 3200-SETUP-SCREEN-VARS
    if (!cdemo.isPgmEnter) {
      if (zonedValue(ccAcctId) === 0n) map.setRaw('ACCTSID', '');
      else map.set('ACCTSID', ccAcctId);
      if (zonedValue(ccCardNum) === 0n) map.setRaw('CARDSID', '');
      else map.set('CARDSID', ccCardNum);
      const show = (d: CardDetails, day: string): void => {
        map.setRaw('CRDNAME', d.crdName);
        map.setRaw('CRDSTCD', d.crdStcd);
        map.setRaw('EXPDAY', day);
        map.setRaw('EXPMON', d.expMon);
        map.setRaw('EXPYEAR', d.expYear);
      };
      if (isNotFetched(a)) {
        for (const f of ['CRDNAME', 'CRDSTCD', 'EXPDAY', 'EXPMON', 'EXPYEAR']) map.setRaw(f, '');
      } else if (a === 'S') {
        show(st.old, st.old.expDay);
      } else if (isChangesMade(a)) {
        show(st.nu, st.old.expDay);
      } else {
        show(st.old, st.old.expDay);
      }
    }
    // 3250-SETUP-INFOMSG
    if (cdemo.isPgmEnter || isNotFetched(a)) setInfo(INFO.PROMPT_FOR_SEARCH_KEYS);
    else if (a === 'S') setInfo(INFO.FOUND_CARDS);
    else if (a === 'E') setInfo(INFO.PROMPT_FOR_CHANGES);
    else if (a === 'N') setInfo(INFO.PROMPT_FOR_CONFIRMATION);
    else if (a === 'C') setInfo(INFO.CONFIRM_UPDATE_SUCCESS);
    else if (a === 'L' || a === 'F') setInfo(INFO.INFORM_FAILURE);
    else if (isSpacesOrLowValues(infoMsg)) setInfo(INFO.PROMPT_FOR_SEARCH_KEYS);
    map.set('INFOMSG', infoMsg);
    map.set('ERRMSG', returnMsg);
    // 3300-SETUP-SCREEN-ATTRS
    const detailFields = ['CRDNAME', 'CRDSTCD', 'EXPMON', 'EXPYEAR'];
    const attrs = (keys: FieldAttribute, details: FieldAttribute): void => {
      map.setAttr('ACCTSID', keys);
      map.setAttr('CARDSID', keys);
      for (const f of detailFields) map.setAttr(f, details);
    };
    if (isNotFetched(a)) attrs(DFHBMSCA.DFHBMFSE, DFHBMSCA.DFHBMPRF);
    else if (a === 'S' || a === 'E') attrs(DFHBMSCA.DFHBMPRF, DFHBMSCA.DFHBMFSE);
    else if (a === 'N' || a === 'C') attrs(DFHBMSCA.DFHBMPRF, DFHBMSCA.DFHBMPRF);
    else attrs(DFHBMSCA.DFHBMFSE, DFHBMSCA.DFHBMPRF);
    const notOkOrBlank = (f: Flag): boolean => f === '0' || f === ' ';
    if (infoMsg === moveAlnum(INFO.FOUND_CARDS, 40) || returnMsg === moveAlnum(MSG.NO_CHANGES_DETECTED, 75)) map.setLength('CRDNAME', -1);
    else if (notOkOrBlank(acctFlag)) map.setLength('ACCTSID', -1);
    else if (notOkOrBlank(cardFlag)) map.setLength('CARDSID', -1);
    else if (notOkOrBlank(nameFlag)) map.setLength('CRDNAME', -1);
    else if (notOkOrBlank(statusFlag)) map.setLength('CRDSTCD', -1);
    else if (notOkOrBlank(monFlag)) map.setLength('EXPMON', -1);
    else if (notOkOrBlank(yearFlag)) map.setLength('EXPYEAR', -1);
    else map.setLength('ACCTSID', -1);
    if (cdemo.get('CDEMO-LAST-MAPSET') === LIT_CCLISTMAPSET) {
      map.setColor('ACCTSID', DFHCOLOR.DFHDFCOL);
      map.setColor('CARDSID', DFHCOLOR.DFHDFCOL);
    }
    const flagColour = (field: string, flag: Flag, gate: boolean, star: boolean): void => {
      if (flag === '0' && gate) map.setColor(field, DFHCOLOR.DFHRED);
      if (flag === ' ' && star) {
        map.set(field, '*');
        map.setColor(field, DFHCOLOR.DFHRED);
      }
    };
    flagColour('ACCTSID', acctFlag, true, cdemo.isPgmReenter);
    flagColour('CARDSID', cardFlag, true, cdemo.isPgmReenter);
    const editing = st.changeAction === 'E';
    flagColour('CRDNAME', nameFlag, editing, editing);
    flagColour('CRDSTCD', statusFlag, editing, editing);
    flagColour('EXPMON', monFlag, editing, editing);
    flagColour('EXPYEAR', yearFlag, editing, editing);
    // MOVE DFHBMDAR TO EXPDAYC (attribute into colour slot): no-op here.
    map.setAttr('INFOMSG', isSpacesOrLowValues(infoMsg) ? DFHBMSCA.DFHBMDAR : DFHBMSCA.DFHBMBRY);
    if (infoMsg === moveAlnum(INFO.PROMPT_FOR_CONFIRMATION, 40)) map.setAttr('FKEYSC', DFHBMSCA.DFHBMBRY);
    // 3400-SEND-SCREEN
    ctx.sendMap(map, { mapset: LIT_THISMAPSET, cursor: true, erase: true });
  };

  const exitBack = (): never => {
    aid = CCARD_AID.PFK03;
    cdemo.toTranid = cdemo.isBlank('CDEMO-FROM-TRANID') ? LIT_MENUTRANID : cdemo.fromTranid;
    cdemo.toProgram = cdemo.isBlank('CDEMO-FROM-PROGRAM') ? LIT_MENUPGM : cdemo.fromProgram;
    cdemo.fromTranid = LIT_THISTRANID;
    cdemo.fromProgram = LIT_THISPGM;
    if (cdemo.get('CDEMO-LAST-MAPSET') === LIT_CCLISTMAPSET) {
      cdemo.setNumber('CDEMO-ACCT-ID', 0);
      cdemo.setNumber('CDEMO-CARD-NUM', 0);
    }
    cdemo.userType = 'U';
    cdemo.setPgmEnter();
    cdemo.set('CDEMO-LAST-MAPSET', LIT_THISMAPSET);
    cdemo.set('CDEMO-LAST-MAP', LIT_THISMAP);
    ctx.syncpoint();
    ctx.xctl(cdemo.toProgram, cdemo.toBuffer());
    throw new Error('unreachable');
  };

  const fromListMapset = cdemo.get('CDEMO-LAST-MAPSET') === LIT_CCLISTMAPSET;
  const a = st.changeAction;
  if (aid === CCARD_AID.PFK03 || (a === 'C' && fromListMapset) || ((a === 'L' || a === 'F') && fromListMapset)) {
    exitBack();
  } else if ((cdemo.isPgmEnter && cdemo.fromProgram === LIT_CCLISTPGM) || (aid === CCARD_AID.PFK12 && cdemo.fromProgram === LIT_CCLISTPGM)) {
    cdemo.setPgmReenter();
    inputFlag = '0';
    acctFlag = '1';
    cardFlag = '1';
    ccAcctId = cdemo.get('CDEMO-ACCT-ID');
    ccCardNum = cdemo.get('CDEMO-CARD-NUM');
    readData();
    st.changeAction = 'S';
    sendMapScreen();
    commonReturn();
  } else if ((isNotFetched(a) && cdemo.isPgmEnter) || (cdemo.fromProgram === LIT_MENUPGM && !cdemo.isPgmReenter)) {
    st = initialState(st);
    sendMapScreen();
    cdemo.setPgmReenter();
    st.changeAction = '\0';
    commonReturn();
  } else if (a === 'C' || a === 'L' || a === 'F') {
    st = initialState(st);
    resetMisc();
    cdemo.setNumber('CDEMO-ACCT-ID', 0);
    cdemo.setNumber('CDEMO-CARD-NUM', 0);
    cdemo.setPgmEnter();
    sendMapScreen();
    cdemo.setPgmReenter();
    st.changeAction = '\0';
    commonReturn();
  } else {
    receive();
    editInputs();
    decideAction();
    sendMapScreen();
    commonReturn();
  }
}

export const COCRDUPC: OnlineProgram = { program: LIT_THISPGM, transid: LIT_THISTRANID, run };
