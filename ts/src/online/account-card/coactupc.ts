/**
 * COACTUPC - account + customer update (transaction CAUP, map CACTUPA / mapset COACTUP).
 * Fetch by account (CXACAIX -> ACCTDAT -> CUSTDAT), edit every field (CSUTLDPY dates,
 * CSLKPCDY phone/state/zip lookups, SSN, FICO), confirm with PF5, then READ UPDATE both
 * records, check for concurrent change and REWRITE ACCTDAT then CUSTDAT.
 */
import { Decimal } from 'decimal.js';
import { CCARD_AID } from '../../common/copybooks/index.js';
import { decodeZoned, encodeZoned } from '../../common/codecs/zoned.js';
import { parsePic } from '../../common/codecs/pic.js';
import {
  CarddemoCommarea,
  DFHBMSCA,
  DFHCOLOR,
  DFHRESP,
  SymbolicMap,
  cobolString,
  csutldtc,
  formatEdited,
  isSpacesOrLowValues,
  isValidGeneralPurposeCode,
  isValidUsStateCode,
  isValidUsStateZipCombo,
  moveAlnum,
  numval,
  numvalC,
  populateHeaderInfo,
  storePfKey,
  testNumval,
  testNumvalC,
  upperCase,
  type CicsContext,
  type FieldAttribute,
  type OnlineProgram,
} from '../framework/index.js';
import { abendRoutine, buildWsCommarea, fileErrorMessage, initializedCommarea, installAbendRoutine, isNumeric, programCommarea, respText, zonedValue } from './common.js';

const LIT_THISPGM = 'COACTUPC';
const LIT_THISTRANID = 'CAUP';
const LIT_THISMAPSET = 'COACTUP';
const LIT_THISMAP = 'CACTUPA';
const LIT_CCLISTMAPSET = 'COCRDLI';
const LIT_MENUPGM = 'COMEN01C';
const LIT_MENUTRANID = 'CM00';
const AMOUNT_PIC = '+ZZZ,ZZZ,ZZZ.99';
const S9_10V99 = parsePic('S9(10)V99');

const INFO = {
  PROMPT_FOR_SEARCH_KEYS: 'Enter or update id of account to update',
  PROMPT_FOR_CHANGES: 'Update account details presented above.',
  PROMPT_FOR_CONFIRMATION: 'Changes validated.Press F5 to save',
  CONFIRM_UPDATE_SUCCESS: 'Changes committed to database',
  INFORM_FAILURE: 'Changes unsuccessful. Please try again',
  FOUND_ACCOUNT_DATA: 'Details of selected account shown above',
} as const;
const MSG = {
  NO_CHANGES_DETECTED: 'No change detected with respect to values fetched.',
  COULD_NOT_LOCK_ACCT: 'Could not lock account record for update',
  COULD_NOT_LOCK_CUST: 'Could not lock customer record for update',
  DATA_WAS_CHANGED: 'Record changed by some one else. Please review',
  UPDATE_FAILED: 'Update of record failed',
} as const;

/** ACUP-OLD-DETAILS / ACUP-NEW-DETAILS field layout (436 bytes). */
const FIELDS = [
  ['acctId', 11], ['status', 1], ['currBal', 12], ['creditLimit', 12], ['cashCreditLimit', 12], ['openDate', 8], ['expDate', 8],
  ['reissueDate', 8], ['cycCredit', 12], ['cycDebit', 12], ['groupId', 10], ['custId', 9], ['fname', 25], ['mname', 25],
  ['lname', 25], ['addr1', 50], ['addr2', 50], ['addr3', 50], ['state', 2], ['country', 3], ['zip', 10], ['phone1', 15],
  ['phone2', 15], ['ssn', 9], ['govt', 20], ['dob', 8], ['eft', 10], ['pri', 1], ['fico', 3],
] as const;
type Key = (typeof FIELDS)[number][0];
type Details = Record<Key, string>;
const DETAILS_LEN = FIELDS.reduce((n, [, l]) => n + l, 0);
const ACCT_DATA_LEN = 106;
export const COACTUPC_PROGCOMMAREA_LEN = 1 + 2 * DETAILS_LEN;
const MONEY = ['currBal', 'creditLimit', 'cashCreditLimit', 'cycCredit', 'cycDebit'] as const;
type MoneyKey = (typeof MONEY)[number];

function parseDetails(s: string): Details {
  const d = {} as Details;
  let off = 0;
  for (const [k, n] of FIELDS) {
    d[k] = s.slice(off, off + n);
    off += n;
  }
  return d;
}
const serializeDetails = (d: Details): string => FIELDS.map(([k, n]) => moveAlnum(d[k], n)).join('');
const spaces = (n: number): string => ' '.repeat(n);
const low = (n: number): string => '\0'.repeat(n);
const blankDetails = (): Details => parseDetails(spaces(DETAILS_LEN));

/** Decimal value of an S9(10)V99 zoned item (invalid data -> nibble value). */
function money(raw: string): Decimal {
  const buf = Buffer.from(raw, 'latin1');
  try {
    return new Decimal(decodeZoned(buf, S9_10V99, 'ascii').value.toString());
  } catch {
    return new Decimal(zonedValue(raw).toString()).div(100);
  }
}
/** COMPUTE x-N = value (S9(10)V99, truncating high-order digits and extra decimals). */
function encodeMoney(v: Decimal): string {
  const t = v.toDecimalPlaces(2, Decimal.ROUND_DOWN);
  const mod = t.abs().mod(new Decimal('1e10'));
  return encodeZoned(t.isNeg() ? mod.neg() : mod, S9_10V99, 'ascii').toString('latin1');
}

interface ActState { action: string; old: Details; nu: Details }
const parseState = (b: Buffer): ActState => {
  const s = b.toString('latin1');
  return { action: s.charAt(0), old: parseDetails(s.slice(1, 1 + DETAILS_LEN)), nu: parseDetails(s.slice(1 + DETAILS_LEN)) };
};
const serializeState = (st: ActState): Buffer => Buffer.from(st.action + serializeDetails(st.old) + serializeDetails(st.nu), 'latin1');
const initialState = (): ActState => ({ action: ' ', old: blankDetails(), nu: blankDetails() });

const isNotFetched = (a: string): boolean => a === '\0' || a === ' ';
const isChangesMade = (a: string): boolean => a.length === 1 && 'ENCLF'.includes(a);
const LOW = '\0';

/** Non-key edit flags: LOW-VALUES valid, '0' not ok, 'B' blank (Y/N flags hold the Y/N value). */
const FLAG_FIELDS = [
  ['ACCT-STATUS', 'ACSTTUS'], ['OPEN-YEAR', 'OPNYEAR'], ['OPEN-MONTH', 'OPNMON'], ['OPEN-DAY', 'OPNDAY'], ['CRED-LIMIT', 'ACRDLIM'],
  ['EXPIRY-YEAR', 'EXPYEAR'], ['EXPIRY-MONTH', 'EXPMON'], ['EXPIRY-DAY', 'EXPDAY'], ['CASH-CREDIT-LIMIT', 'ACSHLIM'],
  ['REISSUE-YEAR', 'RISYEAR'], ['REISSUE-MONTH', 'RISMON'], ['REISSUE-DAY', 'RISDAY'], ['CURR-BAL', 'ACURBAL'],
  ['CURR-CYC-CREDIT', 'ACRCYCR'], ['CURR-CYC-DEBIT', 'ACRCYDB'], ['SSN1', 'ACTSSN1'], ['SSN2', 'ACTSSN2'], ['SSN3', 'ACTSSN3'],
  ['DOB-YEAR', 'DOBYEAR'], ['DOB-MONTH', 'DOBMON'], ['DOB-DAY', 'DOBDAY'], ['FICO', 'ACSTFCO'], ['FIRST-NAME', 'ACSFNAM'],
  ['MIDDLE-NAME', 'ACSMNAM'], ['LAST-NAME', 'ACSLNAM'], ['ADDRESS-LINE-1', 'ACSADL1'], ['STATE', 'ACSSTTE'],
  ['ADDRESS-LINE-2', 'ACSADL2'], ['ZIPCODE', 'ACSZIPC'], ['CITY', 'ACSCITY'], ['COUNTRY', 'ACSCTRY'], ['PHONE-1A', 'ACSPH1A'],
  ['PHONE-1B', 'ACSPH1B'], ['PHONE-1C', 'ACSPH1C'], ['PHONE-2A', 'ACSPH2A'], ['PHONE-2B', 'ACSPH2B'], ['PHONE-2C', 'ACSPH2C'],
  ['PRI-CARDHOLDER', 'ACSPFLG'], ['EFT', 'ACSEFTC'],
] as const;
type FlagKey = (typeof FLAG_FIELDS)[number][0];
/** Order of the cursor-positioning EVALUATE in 3300-SETUP-SCREEN-ATTRS. */
const CURSOR_ORDER: readonly FlagKey[] = [
  'ACCT-STATUS', 'OPEN-YEAR', 'OPEN-MONTH', 'OPEN-DAY', 'CRED-LIMIT', 'EXPIRY-YEAR', 'EXPIRY-MONTH', 'EXPIRY-DAY',
  'CASH-CREDIT-LIMIT', 'REISSUE-YEAR', 'REISSUE-MONTH', 'REISSUE-DAY', 'CURR-BAL', 'CURR-CYC-CREDIT', 'CURR-CYC-DEBIT',
  'SSN1', 'SSN2', 'SSN3', 'DOB-YEAR', 'DOB-MONTH', 'DOB-DAY', 'FICO', 'FIRST-NAME', 'MIDDLE-NAME', 'LAST-NAME',
  'ADDRESS-LINE-1', 'STATE', 'ZIPCODE', 'CITY', 'COUNTRY', 'PHONE-1A', 'PHONE-1B', 'PHONE-1C', 'PHONE-2A', 'PHONE-2B',
  'PHONE-2C', 'EFT', 'PRI-CARDHOLDER',
];
const MAP_OF = new Map<FlagKey, string>(FLAG_FIELDS.map(([k, m]) => [k, m]));

const trimU = (s: string): string => upperCase(s).replace(/^ +/, '').replace(/ +$/, '');
const isBlank = (s: string): boolean => /^\0*$/.test(s) || /^ *$/.test(s);
const ALPHA = /[A-Za-z]/g;

function run(ctx: CicsContext): void {
  installAbendRoutine(ctx, LIT_THISPGM);
  const map = new SymbolicMap(LIT_THISMAP, LIT_THISMAPSET);
  let returnMsg = spaces(75);
  let infoMsg = spaces(40);
  let inputFlag = ' ';
  let acctFilter = ' ';
  let _custFilter = ' ';
  let foundAcct = ' ';
  let foundCust = ' ';
  let dataChanged = ' ';
  let ccAcctId = spaces(11);
  let flags = new Map<FlagKey, string>(FLAG_FIELDS.map(([k]) => [k, ' ']));
  /** ALPHA-VARS-FOR-DATA-EDITING: raw text of the five amount fields. */
  const rawX = new Map<MoneyKey, string>(MONEY.map((k) => [k, spaces(15)]));
  let account: Buffer | undefined;
  let customer: Buffer | undefined;
  let xrefCardNum = '';

  const msgOff = (): boolean => /^ *$/.test(returnMsg);
  const setMsg = (m: string): void => { returnMsg = moveAlnum(m, 75); };
  const setInfo = (m: string): void => { infoMsg = moveAlnum(m, 40); };
  const err = (): void => { inputFlag = '1'; };
  const isErr = (): boolean => inputFlag === '1';
  let varName = '';
  const say = (suffix: string): void => { if (msgOff()) setMsg(varName + suffix); };
  const setName = (n: string): void => { varName = moveAlnum(n, 25).replace(/ +$/, '').replace(/^ +/, ''); };
  const setFlag = (k: FlagKey, v: string): void => { flags.set(k, v); };
  const flag = (k: FlagKey): string => flags.get(k) ?? ' ';
  const allFlagsLow = (): void => { flags = new Map(FLAG_FIELDS.map(([k]) => [k, LOW])); };

  let cdemo: CarddemoCommarea;
  let st: ActState;
  const wsCdemo = new CarddemoCommarea();
  if (ctx.eib.eibcalen === 0 || (wsCdemo.fromProgram === LIT_MENUPGM && !wsCdemo.isPgmReenter)) {
    cdemo = initializedCommarea();
    st = initialState();
    cdemo.setPgmEnter();
    st.action = LOW;
  } else {
    cdemo = CarddemoCommarea.fromDfhCommarea(ctx.commarea);
    st = parseState(programCommarea(ctx.commarea, COACTUPC_PROGCOMMAREA_LEN));
  }
  let aid = storePfKey(ctx.eib.eibaid, '     ');
  const pfkValid = aid === CCARD_AID.ENTER || aid === CCARD_AID.PFK03 || (aid === CCARD_AID.PFK05 && st.action === 'N') ||
    (aid === CCARD_AID.PFK12 && !isNotFetched(st.action));
  if (!pfkValid) aid = CCARD_AID.ENTER;

  // ---- generic edits (1215-1280) ------------------------------------------------
  const sub = (v: string, len: number): string => v.padEnd(256, ' ').slice(0, len);
  const blankField = (s: string): boolean => isBlank(s) || s.replace(/^ +| +$/g, '').length === 0;
  const mandatory = (v: string, len: number): string => {
    if (blankField(sub(v, len))) { err(); say(' must be supplied.'); return 'B'; }
    return LOW;
  };
  const alphaReqd = (v: string, len: number, optional = false): string => {
    const s = sub(v, len);
    if (blankField(s)) {
      if (optional) return LOW;
      err(); say(' must be supplied.'); return 'B';
    }
    if (s.replace(ALPHA, ' ').trim().length !== 0) { err(); say(' can have alphabets only.'); return '0'; }
    return LOW;
  };
  const numReqd = (v: string, len: number): string => {
    const s = sub(v, len);
    if (blankField(s)) { err(); say(' must be supplied.'); return 'B'; }
    if (!isNumeric(s)) { err(); say(' must be all numeric.'); return '0'; }
    if (numval(s).isZero()) { err(); say(' must not be zero.'); return '0'; }
    return LOW;
  };
  const yesNo = (v: string): string => {
    if (/^\0$|^ $|^0$/.test(v)) { err(); say(' must be supplied.'); return 'B'; }
    if (v === 'Y' || v === 'N') return v;
    err(); say(' must be Y or N.'); return '0';
  };
  const signed9v2 = (x: string): string => {
    if (/^\0*$/.test(x) || /^ *$/.test(x)) { err(); say(' must be supplied.'); return 'B'; }
    if (testNumvalC(x) !== 0) { err(); say(' is not valid'); return '0'; }
    return LOW;
  };
  // CSUTLDPY EDIT-DATE-CCYYMMDD THRU EDIT-DATE-CCYYMMDD-EXIT; returns [year, month, day] flags.
  let editedDate = '';
  const editDate = (ccyymmdd: string): [string, string, string] => {
    const ccyy = ccyymmdd.slice(0, 4);
    let mm = ccyymmdd.slice(4, 6);
    let dd = ccyymmdd.slice(6, 8);
    let y = '0';
    let m = '0';
    let d = LOW;
    if (isBlank(ccyy)) { err(); y = 'B'; say(' : Year must be supplied.'); }
    else if (!isNumeric(ccyy)) { err(); say(' must be 4 digit number.'); }
    else if (!(ccyy.startsWith('20') || ccyy.startsWith('19'))) { err(); say(' : Century is not valid.'); }
    else y = LOW;
    if (isBlank(mm)) { err(); m = 'B'; say(' : Month must be supplied.'); }
    else if (!(zonedValue(mm) >= 1n && zonedValue(mm) <= 12n)) { err(); say(': Month must be a number between 1 and 12.'); }
    else if (testNumval(mm) !== 0) { err(); say(': Month must be a number between 1 and 12.'); }
    else { mm = numval(mm).abs().trunc().mod(100).toFixed(0).padStart(2, '0'); m = LOW; }
    if (isBlank(dd)) { err(); d = 'B'; say(' : Day must be supplied.'); }
    else if (testNumval(dd) !== 0) { err(); d = '0'; say(':day must be a number between 1 and 31.'); }
    else {
      dd = numval(dd).abs().trunc().mod(100).toFixed(0).padStart(2, '0');
      if (!(Number(dd) >= 1 && Number(dd) <= 31)) { err(); d = '0'; say(':day must be a number between 1 and 31.'); }
    }
    editedDate = ccyy + mm + dd;
    const mmN = Number(zonedValue(mm));
    const ddN = Number(zonedValue(dd));
    if (![1, 3, 5, 7, 8, 10, 12].includes(mmN) && ddN === 31) {
      err(); say(':Cannot have 31 days in this month.'); return [y, '0', '0'];
    }
    if (mmN === 2 && ddN === 30) { err(); say(':Cannot have 30 days in this month.'); return [y, '0', '0']; }
    if (mmN === 2 && ddN === 29) {
      const div = zonedValue(ccyy.slice(2, 4)) === 0n ? 400n : 4n;
      if (zonedValue(ccyy) % div !== 0n) { err(); say(':Not a leap year.Cannot have 29 days in this month.'); return ['0', '0', '0']; }
    }
    if (!(y === LOW && m === LOW && d === LOW)) return [y, m, d];
    const r = csutldtc(editedDate, 'YYYYMMDD');
    if (Number(r.severity) !== 0) {
      err();
      if (msgOff()) setMsg(cobolString([varName, ' validation error Sev code: ', r.severity, ' Message code: ', r.msgNo], spaces(75)));
    }
    // EDIT-DATE-LE-EXIT falls into SET WS-EDIT-DATE-IS-VALID TO TRUE.
    return [LOW, LOW, LOW];
  };
  const phone = (ph: string): [string, string, string] => {
    const a = ph.slice(1, 4);
    const b = ph.slice(5, 8);
    const c = ph.slice(9, 13);
    const bl = (s: string): boolean => /^ *$/.test(s) || /^\0*$/.test(s);
    // The third test checks NUMA for SPACES (not NUMC) - kept as coded.
    if (bl(a) && bl(b) && (/^ *$/.test(a) || /^\0*$/.test(c))) return [LOW, LOW, LOW];
    const part = (v: string, len: number, label: string, extra?: (v: string) => boolean): string => {
      if (bl(v)) { err(); say(`: ${label} must be supplied.`); return 'B'; }
      if (!isNumeric(v)) { err(); say(`: ${label} must be A ${len} digit number.`); return '0'; }
      if (zonedValue(v) === 0n) { err(); say(`: ${label} cannot be zero`); return '0'; }
      if (extra && !extra(v)) { err(); say(': Not valid North America general purpose area code'); return '0'; }
      return LOW;
    };
    return [
      part(a, 3, 'Area code', (v) => isValidGeneralPurposeCode(v.trim())),
      part(b, 3, 'Prefix code'),
      part(c, 4, 'Line number code'),
    ];
  };

  // ---- 1100-RECEIVE-MAP ---------------------------------------------------------
  const receive = (): void => {
    ctx.receiveMap(map);
    st.nu = blankDetails();
    const inp = (f: string): string | undefined => {
      const v = map.get(f);
      return v === moveAlnum('*', v.length) || /^ *$/.test(v) ? undefined : v;
    };
    const acct = inp('ACCTSID');
    ccAcctId = acct ?? low(11);
    st.nu.acctId = ccAcctId;
    if (isNotFetched(st.action)) return;
    const put = (k: Key, f: string, n: number): void => { st.nu[k] = inp(f) ?? low(n); };
    put('status', 'ACSTTUS', 1);
    const amt = (k: MoneyKey, f: string): void => {
      const v = inp(f);
      if (v === undefined) { rawX.set(k, low(15)); return; }
      rawX.set(k, moveAlnum(v, 15));
      if (testNumvalC(moveAlnum(v, 15)) === 0) st.nu[k] = encodeMoney(numvalC(v));
    };
    amt('creditLimit', 'ACRDLIM');
    amt('cashCreditLimit', 'ACSHLIM');
    amt('currBal', 'ACURBAL');
    amt('cycCredit', 'ACRCYCR');
    amt('cycDebit', 'ACRCYDB');
    const date = (k: Key, fy: string, fm: string, fd: string): void => {
      st.nu[k] = (inp(fy) ?? low(4)) + (inp(fm) ?? low(2)) + (inp(fd) ?? low(2));
    };
    date('openDate', 'OPNYEAR', 'OPNMON', 'OPNDAY');
    date('expDate', 'EXPYEAR', 'EXPMON', 'EXPDAY');
    date('reissueDate', 'RISYEAR', 'RISMON', 'RISDAY');
    put('groupId', 'AADDGRP', 10);
    put('custId', 'ACSTNUM', 9);
    st.nu.ssn = (inp('ACTSSN1') ?? low(3)) + (inp('ACTSSN2') ?? low(2)) + (inp('ACTSSN3') ?? low(4));
    date('dob', 'DOBYEAR', 'DOBMON', 'DOBDAY');
    put('fico', 'ACSTFCO', 3);
    put('fname', 'ACSFNAM', 25);
    put('mname', 'ACSMNAM', 25);
    put('lname', 'ACSLNAM', 25);
    put('addr1', 'ACSADL1', 50);
    put('addr2', 'ACSADL2', 50);
    put('addr3', 'ACSCITY', 50);
    put('state', 'ACSSTTE', 2);
    put('country', 'ACSCTRY', 3);
    put('zip', 'ACSZIPC', 10);
    const ph = (a: string, b: string, c: string): string => ' ' + (inp(a) ?? low(3)) + ' ' + (inp(b) ?? low(3)) + ' ' + (inp(c) ?? low(4)) + '  ';
    st.nu.phone1 = ph('ACSPH1A', 'ACSPH1B', 'ACSPH1C');
    st.nu.phone2 = ph('ACSPH2A', 'ACSPH2B', 'ACSPH2C');
    put('govt', 'ACSGOVT', 20);
    put('eft', 'ACSEFTC', 10);
    put('pri', 'ACSPFLG', 1);
  };

  // ---- 1205-COMPARE-OLD-NEW -------------------------------------------------------
  const compareOldNew = (): void => {
    dataChanged = '0';
    const n = st.nu;
    const o = st.old;
    const acctSame = n.acctId === o.acctId && upperCase(n.status) === upperCase(o.status) &&
      MONEY.every((k) => n[k] === o[k]) && n.openDate === o.openDate && n.expDate === o.expDate &&
      n.reissueDate === o.reissueDate && trimU(n.groupId) === trimU(o.groupId);
    if (!acctSame) { dataChanged = '1'; return; }
    const tk: Key[] = ['custId', 'fname', 'mname', 'lname', 'addr1', 'addr2', 'addr3', 'state', 'country', 'zip'];
    const ph = (s: string): string => s.slice(1, 4) + s.slice(5, 8) + s.slice(9, 13);
    const custSame = tk.every((k) => trimU(n[k]) === trimU(o[k])) && ph(n.phone1) === ph(o.phone1) &&
      ph(n.phone2) === ph(o.phone2) && n.ssn === o.ssn && trimU(n.govt) === trimU(o.govt) && n.dob === o.dob &&
      n.eft === o.eft && trimU(n.pri) === trimU(o.pri) && n.fico === o.fico;
    if (custSame) setMsg(MSG.NO_CHANGES_DETECTED);
    else dataChanged = '1';
  };

  // ---- 1200-EDIT-MAP-INPUTS -------------------------------------------------------
  const editInputs = (): void => {
    inputFlag = '0';
    if (isNotFetched(st.action)) {
      // 1210-EDIT-ACCOUNT
      acctFilter = '0';
      if (isSpacesOrLowValues(ccAcctId)) {
        err();
        acctFilter = ' ';
        if (msgOff()) setMsg('Account number not provided');
        cdemo.setNumber('CDEMO-ACCT-ID', 0);
        st.nu.acctId = '0'.repeat(11);
      } else {
        st.nu.acctId = ccAcctId;
        if (!isNumeric(ccAcctId) || zonedValue(ccAcctId) === 0n) {
          err();
          if (msgOff()) setMsg('Account Number if supplied must be a 11 digit Non-Zero Number');
          cdemo.setNumber('CDEMO-ACCT-ID', 0);
        } else {
          cdemo.setNumber('CDEMO-ACCT-ID', ccAcctId);
          acctFilter = '1';
        }
      }
      const s = serializeDetails(st.old);
      st.old = parseDetails(low(ACCT_DATA_LEN) + s.slice(ACCT_DATA_LEN));
      if (acctFilter === ' ') setMsg('No input received');
      return;
    }
    setInfo(INFO.FOUND_ACCOUNT_DATA);
    foundAcct = '1';
    acctFilter = '1';
    foundCust = '1';
    _custFilter = '1';
    compareOldNew();
    if (dataChanged === '0' || st.action === 'N' || st.action === 'C') {
      allFlagsLow();
      return;
    }
    st.action = 'E';
    const n = st.nu;
    setName('Account Status');
    setFlag('ACCT-STATUS', yesNo(n.status));
    const dateFlags = (prefix: 'OPEN' | 'EXPIRY' | 'REISSUE' | 'DOB', f: [string, string, string]): void => {
      setFlag(`${prefix}-YEAR`, f[0]);
      setFlag(`${prefix}-MONTH`, f[1]);
      setFlag(`${prefix}-DAY`, f[2]);
    };
    setName('Open Date');
    dateFlags('OPEN', editDate(n.openDate));
    setName('Credit Limit');
    setFlag('CRED-LIMIT', signed9v2(rawX.get('creditLimit')!));
    setName('Expiry Date');
    dateFlags('EXPIRY', editDate(n.expDate));
    setName('Cash Credit Limit');
    setFlag('CASH-CREDIT-LIMIT', signed9v2(rawX.get('cashCreditLimit')!));
    setName('Reissue Date');
    dateFlags('REISSUE', editDate(n.reissueDate));
    setName('Current Balance');
    setFlag('CURR-BAL', signed9v2(rawX.get('currBal')!));
    setName('Current Cycle Credit Limit');
    setFlag('CURR-CYC-CREDIT', signed9v2(rawX.get('cycCredit')!));
    setName('Current Cycle Debit Limit');
    setFlag('CURR-CYC-DEBIT', signed9v2(rawX.get('cycDebit')!));
    // 1265-EDIT-US-SSN
    setName('SSN: First 3 chars');
    let f1 = numReqd(n.ssn.slice(0, 3), 3);
    if (f1 === LOW) {
      const p1 = zonedValue(n.ssn.slice(0, 3));
      if (p1 === 0n || p1 === 666n || (p1 >= 900n && p1 <= 999n)) {
        err();
        f1 = '0';
        say(': should not be 000, 666, or between 900 and 999');
      }
    }
    setFlag('SSN1', f1);
    setName('SSN 4th & 5th chars');
    setFlag('SSN2', numReqd(n.ssn.slice(3, 5), 2));
    setName('SSN Last 4 chars');
    setFlag('SSN3', numReqd(n.ssn.slice(5, 9), 4));
    setName('Date of Birth');
    let dob = editDate(n.dob);
    if (dob.every((x) => x === LOW)) {
      // EDIT-DATE-OF-BIRTH: must be strictly before today.
      const now = ctx.now();
      const today = `${now.getFullYear()}${String(now.getMonth() + 1).padStart(2, '0')}${String(now.getDate()).padStart(2, '0')}`;
      if (!(today > editedDate)) {
        err();
        dob = ['0', '0', '0'];
        say(':cannot be in the future ');
      }
    }
    dateFlags('DOB', dob);
    setName('FICO Score');
    let fico = numReqd(n.fico, 3);
    if (fico === LOW) {
      const v = zonedValue(n.fico);
      if (!(v >= 300n && v <= 850n)) { err(); fico = '0'; say(': should be between 300 and 850'); }
    }
    setFlag('FICO', fico);
    setName('First Name');
    setFlag('FIRST-NAME', alphaReqd(n.fname, 25));
    setName('Middle Name');
    setFlag('MIDDLE-NAME', alphaReqd(n.mname, 25, true));
    setName('Last Name');
    setFlag('LAST-NAME', alphaReqd(n.lname, 25));
    setName('Address Line 1');
    setFlag('ADDRESS-LINE-1', mandatory(n.addr1, 50));
    setName('State');
    let stateFlag = alphaReqd(n.state, 2);
    if (stateFlag === LOW && !isValidUsStateCode(n.state)) { err(); stateFlag = '0'; say(': is not a valid state code'); }
    setFlag('STATE', stateFlag);
    setName('Zip');
    setFlag('ZIPCODE', numReqd(n.zip, 5));
    setName('City');
    setFlag('CITY', alphaReqd(n.addr3, 50));
    setName('Country');
    setFlag('COUNTRY', alphaReqd(n.country, 3));
    setName('Phone Number 1');
    const p1 = phone(n.phone1);
    setFlag('PHONE-1A', p1[0]); setFlag('PHONE-1B', p1[1]); setFlag('PHONE-1C', p1[2]);
    setName('Phone Number 2');
    const p2 = phone(n.phone2);
    setFlag('PHONE-2A', p2[0]); setFlag('PHONE-2B', p2[1]); setFlag('PHONE-2C', p2[2]);
    setName('EFT Account Id');
    setFlag('EFT', numReqd(n.eft, 10));
    setName('Primary Card Holder');
    setFlag('PRI-CARDHOLDER', yesNo(n.pri));
    if (flag('STATE') === LOW && flag('ZIPCODE') === LOW && !isValidUsStateZipCombo(n.state + n.zip.slice(0, 2))) {
      err();
      setFlag('STATE', '0');
      setFlag('ZIPCODE', '0');
      if (msgOff()) setMsg('Invalid zip code for state');
    }
    if (!isErr()) st.action = 'N';
  };

  // ---- 9000-READ-ACCT ---------------------------------------------------------------
  const readAcct = (): void => {
    st.old = blankDetails();
    infoMsg = spaces(40);
    st.old.acctId = ccAcctId;
    const key = ccAcctId;
    let r = ctx.track(ctx.files.read({ file: 'CXACAIX', ridfld: key, keylength: 11 }));
    if (r.resp === DFHRESP.NORMAL) {
      const rec = r.record!.toString('latin1');
      xrefCardNum = rec.slice(0, 16);
      cdemo.setNumber('CDEMO-CUST-ID', zonedValue(rec.slice(16, 25)));
      cdemo.setNumber('CDEMO-CARD-NUM', zonedValue(xrefCardNum));
    } else if (r.resp === DFHRESP.NOTFND) {
      err();
      acctFilter = '0';
      if (msgOff()) returnMsg = cobolString(['Account:', key, ' not found in', ' Cross ref file.  Resp:', respText(r.resp), ' Reas:', respText(r.resp2)], returnMsg);
    } else {
      err();
      acctFilter = '0';
      setMsg(fileErrorMessage('READ', 'CXACAIX', r.resp, r.resp2));
    }
    if (acctFilter === '0') return;
    r = ctx.track(ctx.files.read({ file: 'ACCTDAT', ridfld: key, keylength: 11 }));
    if (r.resp === DFHRESP.NORMAL) {
      foundAcct = '1';
      account = r.record!;
    } else if (r.resp === DFHRESP.NOTFND) {
      err();
      acctFilter = '0';
      if (msgOff()) returnMsg = cobolString(['Account:', key, ' not found in', ' Acct Master file.Resp:', respText(r.resp), ' Reas:', respText(r.resp2)], returnMsg);
    } else {
      err();
      acctFilter = '0';
      setMsg(fileErrorMessage('READ', 'ACCTDAT', r.resp, r.resp2));
    }
    // DID-NOT-FIND-ACCT-IN-ACCTDAT / -CUST-IN-CUSTDAT are never set: reads always continue.
    const custKey = cdemo.get('CDEMO-CUST-ID');
    r = ctx.track(ctx.files.read({ file: 'CUSTDAT', ridfld: custKey, keylength: 9 }));
    if (r.resp === DFHRESP.NORMAL) {
      foundCust = '1';
      customer = r.record!;
    } else if (r.resp === DFHRESP.NOTFND) {
      err();
      _custFilter = '0';
      if (msgOff()) returnMsg = cobolString(['CustId:', custKey, ' not found', ' in customer master.Resp: ', respText(r.resp), ' REAS:', respText(r.resp2)], returnMsg);
    } else {
      err();
      _custFilter = '0';
      setMsg(fileErrorMessage('READ', 'CUSTDAT', r.resp, r.resp2));
    }
    // 9500-STORE-FETCHED-DATA
    const a = (account ?? Buffer.alloc(300, 0)).toString('latin1');
    const c = (customer ?? Buffer.alloc(500, 0)).toString('latin1');
    cdemo.setNumber('CDEMO-ACCT-ID', zonedValue(a.slice(0, 11)));
    cdemo.setNumber('CDEMO-CUST-ID', zonedValue(c.slice(0, 9)));
    cdemo.set('CDEMO-CUST-FNAME', c.slice(9, 34));
    cdemo.set('CDEMO-CUST-MNAME', c.slice(34, 59));
    cdemo.set('CDEMO-CUST-LNAME', c.slice(59, 84));
    cdemo.set('CDEMO-ACCT-STATUS', a.slice(11, 12));
    cdemo.setNumber('CDEMO-CARD-NUM', zonedValue(xrefCardNum));
    const o = blankDetails();
    o.acctId = a.slice(0, 11);
    o.status = a.slice(11, 12);
    o.currBal = a.slice(12, 24);
    o.creditLimit = a.slice(24, 36);
    o.cashCreditLimit = a.slice(36, 48);
    o.cycCredit = a.slice(78, 90);
    o.cycDebit = a.slice(90, 102);
    const ymd = (s: string): string => s.slice(0, 4) + s.slice(5, 7) + s.slice(8, 10);
    o.openDate = ymd(a.slice(48, 58));
    o.expDate = ymd(a.slice(58, 68));
    o.reissueDate = ymd(a.slice(68, 78));
    o.groupId = a.slice(112, 122);
    o.custId = c.slice(0, 9);
    o.ssn = c.slice(279, 288);
    o.dob = ymd(c.slice(308, 318));
    o.fico = c.slice(329, 332);
    o.fname = c.slice(9, 34);
    o.mname = c.slice(34, 59);
    o.lname = c.slice(59, 84);
    o.addr1 = c.slice(84, 134);
    o.addr2 = c.slice(134, 184);
    o.addr3 = c.slice(184, 234);
    o.state = c.slice(234, 236);
    o.country = c.slice(236, 239);
    o.zip = c.slice(239, 249);
    o.phone1 = c.slice(249, 264);
    o.phone2 = c.slice(264, 279);
    o.govt = c.slice(288, 308);
    o.eft = c.slice(318, 328);
    o.pri = c.slice(328, 329);
    st.old = o;
  };

  // ---- 9600-WRITE-PROCESSING ----------------------------------------------------------
  const writeProcessing = (): void => {
    const ra = ctx.track(ctx.files.read({ file: 'ACCTDAT', ridfld: ccAcctId, keylength: 11, update: true }));
    if (ra.resp !== DFHRESP.NORMAL) {
      err();
      if (msgOff()) setMsg(MSG.COULD_NOT_LOCK_ACCT);
      return;
    }
    const rc = ctx.track(ctx.files.read({ file: 'CUSTDAT', ridfld: cdemo.get('CDEMO-CUST-ID'), keylength: 9, update: true }));
    if (rc.resp !== DFHRESP.NORMAL) {
      err();
      if (msgOff()) setMsg(MSG.COULD_NOT_LOCK_CUST);
      return;
    }
    const a = ra.record!.toString('latin1');
    const c = rc.record!.toString('latin1');
    const o = st.old;
    // 9700-CHECK-CHANGE-IN-REC
    const acctSame = a.slice(11, 12) === o.status && money(a.slice(12, 24)).eq(money(o.currBal)) &&
      money(a.slice(24, 36)).eq(money(o.creditLimit)) && money(a.slice(36, 48)).eq(money(o.cashCreditLimit)) &&
      money(a.slice(78, 90)).eq(money(o.cycCredit)) && money(a.slice(90, 102)).eq(money(o.cycDebit)) &&
      a.slice(48, 52) === o.openDate.slice(0, 4) && a.slice(53, 55) === o.openDate.slice(4, 6) && a.slice(56, 58) === o.openDate.slice(6, 8) &&
      a.slice(58, 62) === o.expDate.slice(0, 4) && a.slice(63, 65) === o.expDate.slice(4, 6) && a.slice(66, 68) === o.expDate.slice(6, 8) &&
      a.slice(68, 72) === o.reissueDate.slice(0, 4) && a.slice(73, 75) === o.reissueDate.slice(4, 6) && a.slice(76, 78) === o.reissueDate.slice(6, 8) &&
      a.slice(112, 122).toLowerCase() === o.groupId.toLowerCase();
    const U = upperCase;
    const custSame = acctSame && U(c.slice(9, 34)) === U(o.fname) && U(c.slice(34, 59)) === U(o.mname) &&
      U(c.slice(59, 84)) === U(o.lname) && U(c.slice(84, 134)) === U(o.addr1) && U(c.slice(134, 184)) === U(o.addr2) &&
      U(c.slice(184, 234)) === U(o.addr3) && U(c.slice(234, 236)) === U(o.state) && U(c.slice(236, 239)) === U(o.country) &&
      c.slice(239, 249) === o.zip && c.slice(249, 264) === o.phone1 && c.slice(264, 279) === o.phone2 &&
      zonedValue(c.slice(279, 288)) === zonedValue(o.ssn) && U(c.slice(288, 308)) === U(o.govt) &&
      c.slice(308, 312) === o.dob.slice(0, 4) && c.slice(313, 315) === o.dob.slice(4, 6) && c.slice(316, 318) === o.dob.slice(6, 8) &&
      c.slice(318, 328) === o.eft && c.slice(328, 329) === o.pri && zonedValue(c.slice(329, 332)) === zonedValue(o.fico);
    if (!custSame) {
      setMsg(MSG.DATA_WAS_CHANGED);
      return;
    }
    const n = st.nu;
    const digits = (s: string, len: number): string => zonedValue(s).toString().padStart(len, '0').slice(-len);
    const dash = (s: string): string => moveAlnum(`${s.slice(0, 4)}-${s.slice(4, 6)}-${s.slice(6, 8)}`, 10);
    const amount = (k: MoneyKey): string => encodeMoney(money(n[k]));
    // ACCT-UPDATE-RECORD has no ACCT-ADDR-ZIP: GROUP-ID lands in the ZIP slot (FILLER modelled as spaces).
    const acctRec = digits(n.acctId, 11) + moveAlnum(n.status, 1) + amount('currBal') + amount('creditLimit') +
      amount('cashCreditLimit') + dash(n.openDate) + dash(n.expDate) + dash(n.reissueDate) + amount('cycCredit') +
      amount('cycDebit') + moveAlnum(n.groupId, 10) + spaces(188);
    const ph = (p: string): string => moveAlnum(`(${p.slice(1, 4)})${p.slice(5, 8)}-${p.slice(9, 13)}`, 15);
    const custRec = digits(n.custId, 9) + moveAlnum(n.fname, 25) + moveAlnum(n.mname, 25) + moveAlnum(n.lname, 25) +
      moveAlnum(n.addr1, 50) + moveAlnum(n.addr2, 50) + moveAlnum(n.addr3, 50) + moveAlnum(n.state, 2) + moveAlnum(n.country, 3) +
      moveAlnum(n.zip, 10) + ph(n.phone1) + ph(n.phone2) + digits(n.ssn, 9) + moveAlnum(n.govt, 20) + dash(n.dob) +
      moveAlnum(n.eft, 10) + moveAlnum(n.pri, 1) + digits(n.fico, 3) + spaces(168);
    const wa = ctx.track(ctx.files.rewrite({ file: 'ACCTDAT', from: Buffer.from(acctRec, 'latin1') }));
    if (wa.resp !== DFHRESP.NORMAL) {
      setMsg(MSG.UPDATE_FAILED);
      return;
    }
    const wc = ctx.track(ctx.files.rewrite({ file: 'CUSTDAT', from: Buffer.from(custRec, 'latin1') }));
    if (wc.resp !== DFHRESP.NORMAL) {
      setMsg(MSG.UPDATE_FAILED);
      ctx.syncpointRollback();
    }
  };

  // ---- 2000-DECIDE-ACTION --------------------------------------------------------------
  const decideAction = (): void => {
    const a = st.action;
    if (isNotFetched(a) || aid === CCARD_AID.PFK12) {
      if (acctFilter === '1') {
        returnMsg = spaces(75);
        readAcct();
        if (foundCust === '1') st.action = 'S';
      }
    } else if (a === 'S') {
      if (!(isErr() || returnMsg === moveAlnum(MSG.NO_CHANGES_DETECTED, 75))) st.action = 'N';
    } else if (a === 'E') {
      // CONTINUE
    } else if (a === 'N' && aid === CCARD_AID.PFK05) {
      writeProcessing();
      if (returnMsg === moveAlnum(MSG.COULD_NOT_LOCK_ACCT, 75)) st.action = 'L';
      else if (returnMsg === moveAlnum(MSG.UPDATE_FAILED, 75)) st.action = 'F';
      else if (returnMsg === moveAlnum(MSG.DATA_WAS_CHANGED, 75)) st.action = 'S';
      else st.action = 'C';
    } else if (a === 'N') {
      // CONTINUE
    } else if (a === 'C') {
      st.action = 'S';
      if (cdemo.isBlank('CDEMO-FROM-TRANID')) {
        cdemo.setNumber('CDEMO-ACCT-ID', 0);
        cdemo.setNumber('CDEMO-CARD-NUM', 0);
        cdemo.set('CDEMO-ACCT-STATUS', LOW);
      }
    } else {
      abendRoutine(ctx, LIT_THISPGM, '0001', 'UNEXPECTED DATA SCENARIO');
    }
  };

  // ---- 3000-SEND-MAP ---------------------------------------------------------------------
  const sendMapScreen = (): void => {
    map.lowValues();
    populateHeaderInfo(map, { tranid: LIT_THISTRANID, pgmname: LIT_THISPGM, now: ctx.now() });
    const a = st.action;
    const showAmount = (f: string, raw: string): void => { map.set(f, formatEdited(AMOUNT_PIC, money(raw))); };
    const date = (y: string, m: string, d: string, v: string): void => {
      map.setRaw(y, v.slice(0, 4));
      map.setRaw(m, v.slice(4, 6));
      map.setRaw(d, v.slice(6, 8));
    };
    const phoneOut = (pre: string, v: string): void => {
      map.setRaw(`${pre}A`, v.slice(1, 4));
      map.setRaw(`${pre}B`, v.slice(5, 8));
      map.setRaw(`${pre}C`, v.slice(9, 13));
    };
    const custOut = (d: Details, newPhones: boolean): void => {
      map.setRaw('ACSTNUM', d.custId);
      map.setRaw('ACTSSN1', d.ssn.slice(0, 3));
      map.setRaw('ACTSSN2', d.ssn.slice(3, 5));
      map.setRaw('ACTSSN3', d.ssn.slice(5, 9));
      map.setRaw('ACSTFCO', d.fico);
      date('DOBYEAR', 'DOBMON', 'DOBDAY', d.dob);
      map.setRaw('ACSFNAM', d.fname);
      map.setRaw('ACSMNAM', d.mname);
      map.setRaw('ACSLNAM', d.lname);
      map.setRaw('ACSADL1', d.addr1);
      map.setRaw('ACSADL2', d.addr2);
      map.setRaw('ACSCITY', d.addr3);
      map.setRaw('ACSSTTE', d.state);
      map.setRaw('ACSZIPC', d.zip);
      map.setRaw('ACSCTRY', d.country);
      phoneOut('ACSPH1', d.phone1);
      phoneOut('ACSPH2', d.phone2);
      void newPhones;
      map.setRaw('ACSGOVT', d.govt);
      map.setRaw('ACSEFTC', d.eft);
      map.setRaw('ACSPFLG', d.pri);
    };
    // 3200-SETUP-SCREEN-VARS
    if (!cdemo.isPgmEnter) {
      if (zonedValue(ccAcctId) === 0n && acctFilter === '1') map.setRaw('ACCTSID', '');
      else map.setRaw('ACCTSID', ccAcctId);
      const showOriginal = (): void => {
        allFlagsLow();
        setInfo(INFO.PROMPT_FOR_CHANGES);
        const o = st.old;
        if (foundAcct === '1' || foundCust === '1') {
          map.setRaw('ACSTTUS', o.status);
          showAmount('ACURBAL', o.currBal);
          showAmount('ACRDLIM', o.creditLimit);
          showAmount('ACSHLIM', o.cashCreditLimit);
          showAmount('ACRCYCR', o.cycCredit);
          showAmount('ACRCYDB', o.cycDebit);
          date('OPNYEAR', 'OPNMON', 'OPNDAY', o.openDate);
          date('EXPYEAR', 'EXPMON', 'EXPDAY', o.expDate);
          date('RISYEAR', 'RISMON', 'RISDAY', o.reissueDate);
          map.setRaw('AADDGRP', o.groupId);
        }
        if (foundCust === '1') custOut(o, false);
      };
      if (isNotFetched(a) || zonedValue(ccAcctId) === 0n) {
        // 3201-SHOW-INITIAL-VALUES: map already LOW-VALUES.
      } else if (a === 'S') {
        showOriginal();
      } else if (isChangesMade(a)) {
        const n = st.nu;
        map.setRaw('ACSTTUS', n.status);
        const amt = (f: string, k: MoneyKey, fk: FlagKey): void => {
          if (flag(fk) === LOW) showAmount(f, n[k]);
          else map.setRaw(f, rawX.get(k)!);
        };
        amt('ACRDLIM', 'creditLimit', 'CRED-LIMIT');
        amt('ACSHLIM', 'cashCreditLimit', 'CASH-CREDIT-LIMIT');
        amt('ACURBAL', 'currBal', 'CURR-BAL');
        amt('ACRCYCR', 'cycCredit', 'CURR-CYC-CREDIT');
        amt('ACRCYDB', 'cycDebit', 'CURR-CYC-DEBIT');
        date('OPNYEAR', 'OPNMON', 'OPNDAY', n.openDate);
        date('EXPYEAR', 'EXPMON', 'EXPDAY', n.expDate);
        date('RISYEAR', 'RISMON', 'RISDAY', n.reissueDate);
        map.setRaw('AADDGRP', n.groupId);
        custOut(n, true);
      } else {
        showOriginal();
      }
    }
    // 3250-SETUP-INFOMSG
    if (cdemo.isPgmEnter || isNotFetched(a)) setInfo(INFO.PROMPT_FOR_SEARCH_KEYS);
    else if (a === 'S' || a === 'E') setInfo(INFO.PROMPT_FOR_CHANGES);
    else if (a === 'N') setInfo(INFO.PROMPT_FOR_CONFIRMATION);
    else if (a === 'C') setInfo(INFO.CONFIRM_UPDATE_SUCCESS);
    else if (a === 'L' || a === 'F') setInfo(INFO.INFORM_FAILURE);
    else if (isSpacesOrLowValues(infoMsg)) setInfo(INFO.PROMPT_FOR_SEARCH_KEYS);
    map.set('INFOMSG', infoMsg);
    map.set('ERRMSG', returnMsg);
    // 3300-SETUP-SCREEN-ATTRS
    const all = ['ACCTSID', ...FLAG_FIELDS.map(([, m]) => m).filter((m) => m !== 'ACSADL2'), 'ACSADL2', 'AADDGRP', 'ACSTNUM', 'ACSGOVT', 'INFOMSG'];
    for (const f of all) map.setAttr(f, DFHBMSCA.DFHBMPRF);
    const fse = (fs: readonly string[], v: FieldAttribute): void => { for (const f of fs) map.setAttr(f, v); };
    if (isNotFetched(a)) {
      map.setAttr('ACCTSID', DFHBMSCA.DFHBMFSE);
    } else if (a === 'S' || a === 'E') {
      fse(['ACSTTUS', 'ACRDLIM', 'ACSHLIM', 'ACURBAL', 'ACRCYCR', 'ACRCYDB', 'OPNYEAR', 'OPNMON', 'OPNDAY', 'EXPYEAR', 'EXPMON',
        'EXPDAY', 'RISYEAR', 'RISMON', 'RISDAY', 'DOBYEAR', 'DOBMON', 'DOBDAY', 'AADDGRP'], DFHBMSCA.DFHBMFSE);
      map.setAttr('ACSTNUM', DFHBMSCA.DFHBMPRF);
      fse(['ACTSSN1', 'ACTSSN2', 'ACTSSN3', 'ACSTFCO', 'ACSFNAM', 'ACSMNAM', 'ACSLNAM', 'ACSADL1', 'ACSADL2', 'ACSCITY', 'ACSSTTE',
        'ACSZIPC'], DFHBMSCA.DFHBMFSE);
      map.setAttr('ACSCTRY', DFHBMSCA.DFHBMPRF);
      fse(['ACSPH1A', 'ACSPH1B', 'ACSPH1C', 'ACSPH2A', 'ACSPH2B', 'ACSPH2C', 'ACSGOVT', 'ACSEFTC', 'ACSPFLG'], DFHBMSCA.DFHBMFSE);
      map.setAttr('INFOMSG', DFHBMSCA.DFHBMPRF);
    } else if (a === 'N' || a === 'C') {
      // CONTINUE
    } else {
      map.setAttr('ACCTSID', DFHBMSCA.DFHBMFSE);
    }
    const bad = (k: FlagKey): boolean => flag(k) === '0' || (flag(k) === 'B' && k !== 'MIDDLE-NAME');
    if (infoMsg === moveAlnum(INFO.FOUND_ACCOUNT_DATA, 40) || returnMsg === moveAlnum(MSG.NO_CHANGES_DETECTED, 75)) {
      map.setLength('ACSTTUS', -1);
    } else if (acctFilter === '0' || acctFilter === ' ') {
      map.setLength('ACCTSID', -1);
    } else {
      const k = CURSOR_ORDER.find(bad);
      map.setLength(k ? MAP_OF.get(k)! : 'ACCTSID', -1);
    }
    if (cdemo.get('CDEMO-LAST-MAPSET') === LIT_CCLISTMAPSET) map.setColor('ACCTSID', DFHCOLOR.DFHDFCOL);
    if (acctFilter === '0') map.setColor('ACCTSID', DFHCOLOR.DFHRED);
    if (acctFilter === ' ' && cdemo.isPgmReenter) {
      map.set('ACCTSID', '*');
      map.setColor('ACCTSID', DFHCOLOR.DFHRED);
    }
    if (!(isNotFetched(a) || acctFilter === ' ' || acctFilter === '0')) {
      // COPY CSSETATY for each edited field.
      for (const [k, f] of FLAG_FIELDS) {
        const v = flag(k);
        if ((v === '0' || v === 'B') && cdemo.isPgmReenter) {
          map.setColor(f, DFHCOLOR.DFHRED);
          if (v === 'B') map.set(f, '*');
        }
      }
    }
    // 3390-SETUP-INFOMSG-ATTRS
    map.setAttr('INFOMSG', isSpacesOrLowValues(infoMsg) ? DFHBMSCA.DFHBMDAR : DFHBMSCA.DFHBMASB);
    if (isChangesMade(a) && a !== 'C') map.setAttr('FKEY12', DFHBMSCA.DFHBMASB);
    if (infoMsg === moveAlnum(INFO.PROMPT_FOR_CONFIRMATION, 40)) {
      map.setAttr('FKEY05', DFHBMSCA.DFHBMASB);
      map.setAttr('FKEY12', DFHBMSCA.DFHBMASB);
    }
    ctx.sendMap(map, { mapset: LIT_THISMAPSET, cursor: true, erase: true });
  };

  const commonReturn = (): never => ctx.return({ transid: LIT_THISTRANID, commarea: buildWsCommarea(cdemo, serializeState(st)) });

  const a = st.action;
  if (aid === CCARD_AID.PFK03) {
    cdemo.toTranid = cdemo.isBlank('CDEMO-FROM-TRANID') ? LIT_MENUTRANID : cdemo.fromTranid;
    cdemo.toProgram = cdemo.isBlank('CDEMO-FROM-PROGRAM') ? LIT_MENUPGM : cdemo.fromProgram;
    cdemo.fromTranid = LIT_THISTRANID;
    cdemo.fromProgram = LIT_THISPGM;
    cdemo.userType = 'U';
    cdemo.setPgmEnter();
    cdemo.set('CDEMO-LAST-MAPSET', LIT_THISMAPSET);
    cdemo.set('CDEMO-LAST-MAP', LIT_THISMAP);
    ctx.syncpoint();
    ctx.xctl(cdemo.toProgram, cdemo.toBuffer());
  } else if ((isNotFetched(a) && cdemo.isPgmEnter) || (cdemo.fromProgram === LIT_MENUPGM && !cdemo.isPgmReenter)) {
    st = initialState();
    sendMapScreen();
    cdemo.setPgmReenter();
    st.action = LOW;
    commonReturn();
  } else if (a === 'C' || a === 'L' || a === 'F') {
    st = initialState();
    returnMsg = spaces(75);
    infoMsg = spaces(40);
    inputFlag = acctFilter = _custFilter = foundAcct = foundCust = dataChanged = ' ';
    ccAcctId = spaces(11);
    flags = new Map(FLAG_FIELDS.map(([k]) => [k, ' ']));
    cdemo.setNumber('CDEMO-ACCT-ID', 0);
    cdemo.setPgmEnter();
    sendMapScreen();
    cdemo.setPgmReenter();
    st.action = LOW;
    commonReturn();
  } else {
    receive();
    editInputs();
    decideAction();
    sendMapScreen();
    commonReturn();
  }
}

export const COACTUPC: OnlineProgram = { program: LIT_THISPGM, transid: LIT_THISTRANID, run };
