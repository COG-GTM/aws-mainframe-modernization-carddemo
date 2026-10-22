/**
 * COACTVWC - account view (transaction CAVW, map CACTVWA / mapset COACTVW).
 * Reads CXACAIX (card xref by account) -> ACCTDAT -> CUSTDAT and displays them.
 */
import { AccountRecordLayout, CCARD_AID, CardXrefRecordLayout, CustomerRecordLayout, type AccountRecord, type CustomerRecord } from '../../common/copybooks/index.js';
import {
  CarddemoCommarea,
  DFHBMSCA,
  DFHCOLOR,
  DFHRESP,
  SymbolicMap,
  cobolString,
  formatEdited,
  isSpacesOrLowValues,
  moveAlnum,
  populateHeaderInfo,
  storePfKey,
  type CicsContext,
  type OnlineProgram,
} from '../framework/index.js';
import { buildWsCommarea, fileErrorMessage, initializedCommarea, installAbendRoutine, isNumeric, programCommarea, respText, sendPlainText } from './common.js';

const LIT_THISPGM = 'COACTVWC';
const LIT_THISTRANID = 'CAVW';
const LIT_THISMAPSET = 'COACTVW';
const LIT_THISMAP = 'CACTVWA';
const LIT_MENUPGM = 'COMEN01C';
const LIT_MENUTRANID = 'CM00';
const PROG_COMMAREA_LEN = 12;
const AMOUNT_PIC = '+ZZZ,ZZZ,ZZZ.99';

const MSG_PROMPT_FOR_INPUT = 'Enter or update id of account to display';

function run(ctx: CicsContext): void {
  installAbendRoutine(ctx, LIT_THISPGM);
  const map = new SymbolicMap(LIT_THISMAP, LIT_THISMAPSET);
  let returnMsg = moveAlnum('', 75);
  let infoMsg = moveAlnum('', 40);
  let inputError = false;
  /** WS-EDIT-ACCT-FLAG: ' ' blank, '0' not ok, '1' valid. */
  let acctFlag: ' ' | '0' | '1' = ' ';
  let ccAcctId = moveAlnum('', 11);
  let foundAcct = false;
  let foundCust = false;
  let account: AccountRecord | undefined;
  let customer: CustomerRecord | undefined;
  const msgOff = (): boolean => /^ *$/.test(returnMsg);
  const setMsg = (m: string): void => { returnMsg = moveAlnum(m, 75); };

  // 0000-MAIN. WORKING-STORAGE CARDDEMO-COMMAREA is fresh (LOW-VALUES), so the
  // "from menu and not re-enter" test never holds when EIBCALEN > 0.
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
    // 1100-SCREEN-INIT
    map.lowValues();
    populateHeaderInfo(map, { tranid: LIT_THISTRANID, pgmname: LIT_THISPGM, now: ctx.now() });
    // 1200-SETUP-SCREEN-VARS
    if (ctx.eib.eibcalen === 0) {
      infoMsg = moveAlnum(MSG_PROMPT_FOR_INPUT, 40);
    } else {
      if (acctFlag === ' ') map.setRaw('ACCTSID', '');
      else map.set('ACCTSID', ccAcctId);
      if (foundAcct || foundCust) {
        const a = account ?? AccountRecordLayout.parse(Buffer.alloc(AccountRecordLayout.length, 0x30));
        map.set('ACSTTUS', a.acctActiveStatus);
        map.set('ACURBAL', formatEdited(AMOUNT_PIC, a.acctCurrBal));
        map.set('ACRDLIM', formatEdited(AMOUNT_PIC, a.acctCreditLimit));
        map.set('ACSHLIM', formatEdited(AMOUNT_PIC, a.acctCashCreditLimit));
        map.set('ACRCYCR', formatEdited(AMOUNT_PIC, a.acctCurrCycCredit));
        map.set('ACRCYDB', formatEdited(AMOUNT_PIC, a.acctCurrCycDebit));
        map.set('ADTOPEN', a.acctOpenDate);
        map.set('AEXPDT', a.acctExpiraionDate);
        map.set('AREISDT', a.acctReissueDate);
        map.set('AADDGRP', a.acctGroupId);
      }
      if (foundCust && customer) {
        const c = customer;
        const ssn = c.custSsn.toFixed(0).padStart(9, '0');
        map.set('ACSTNUM', c.custId.toFixed(0).padStart(9, '0'));
        map.set('ACSTSSN', `${ssn.slice(0, 3)}-${ssn.slice(3, 5)}-${ssn.slice(5, 9)}`);
        map.set('ACSTFCO', c.custFicoCreditScore.toFixed(0).padStart(3, '0'));
        map.set('ACSTDOB', c.custDobYyyyMmDd);
        map.set('ACSFNAM', c.custFirstName);
        map.set('ACSMNAM', c.custMiddleName);
        map.set('ACSLNAM', c.custLastName);
        map.set('ACSADL1', c.custAddrLine1);
        map.set('ACSADL2', c.custAddrLine2);
        map.set('ACSCITY', c.custAddrLine3);
        map.set('ACSSTTE', c.custAddrStateCd);
        map.set('ACSZIPC', c.custAddrZip);
        map.set('ACSCTRY', c.custAddrCountryCd);
        map.set('ACSPHN1', c.custPhoneNum1);
        map.set('ACSPHN2', c.custPhoneNum2);
        map.set('ACSGOVT', c.custGovtIssuedId);
        map.set('ACSEFTC', c.custEftAccountId);
        map.set('ACSPFLG', c.custPriCardHolderInd);
      }
    }
    if (isSpacesOrLowValues(infoMsg)) infoMsg = moveAlnum(MSG_PROMPT_FOR_INPUT, 40);
    map.set('ERRMSG', returnMsg);
    map.set('INFOMSG', infoMsg);
    // 1300-SETUP-SCREEN-ATTRS
    map.setAttr('ACCTSID', DFHBMSCA.DFHBMFSE);
    map.setLength('ACCTSID', -1);
    map.setColor('ACCTSID', DFHCOLOR.DFHDFCOL);
    if (acctFlag === '0') map.setColor('ACCTSID', DFHCOLOR.DFHRED);
    if (acctFlag === ' ' && cdemo.isPgmReenter) {
      map.set('ACCTSID', '*');
      map.setColor('ACCTSID', DFHCOLOR.DFHRED);
    }
    // MOVE DFHBMDAR TO INFOMSGC moves an attribute byte into the colour slot: modelled as default colour.
    map.setColor('INFOMSG', isSpacesOrLowValues(infoMsg) ? undefined : DFHCOLOR.DFHNEUTR);
    // 1400-SEND-SCREEN
    cdemo.setPgmReenter();
    ctx.sendMap(map, { mapset: LIT_THISMAPSET, cursor: true, erase: true });
  };

  const editAccount = (): void => {
    acctFlag = '0';
    if (isSpacesOrLowValues(ccAcctId)) {
      inputError = true;
      acctFlag = ' ';
      if (msgOff()) setMsg('Account number not provided');
      cdemo.setNumber('CDEMO-ACCT-ID', 0);
      return;
    }
    if (!isNumeric(ccAcctId) || /^0+$/.test(ccAcctId)) {
      inputError = true;
      acctFlag = '0';
      if (msgOff()) setMsg('Account Filter must  be a non-zero 11 digit number');
      cdemo.setNumber('CDEMO-ACCT-ID', 0);
      return;
    }
    cdemo.setNumber('CDEMO-ACCT-ID', ccAcctId);
    acctFlag = '1';
  };

  const processInputs = (): void => {
    ctx.receiveMap(map);
    inputError = false;
    acctFlag = '1';
    const input = map.get('ACCTSID');
    ccAcctId = input === moveAlnum('*', 11) || /^ *$/.test(input) ? '\0'.repeat(11) : input;
    editAccount();
    if ((acctFlag as string) === ' ') setMsg('No input received');
  };

  const readAcct = (): void => {
    infoMsg = moveAlnum('', 40);
    const acctKey = cdemo.get('CDEMO-ACCT-ID');
    // 9200-GETCARDXREF-BYACCT
    let r = ctx.track(ctx.files.read({ file: 'CXACAIX', ridfld: acctKey, keylength: 11 }));
    if (r.resp === DFHRESP.NORMAL) {
      const x = CardXrefRecordLayout.parse(r.record!);
      cdemo.setNumber('CDEMO-CUST-ID', x.xrefCustId.toFixed(0));
      cdemo.setNumber('CDEMO-CARD-NUM', x.xrefCardNum);
    } else if (r.resp === DFHRESP.NOTFND) {
      inputError = true;
      acctFlag = '0';
      if (msgOff()) {
        returnMsg = cobolString(['Account:', acctKey, ' not found in', ' Cross ref file.  Resp:', respText(r.resp), ' Reas:', respText(r.resp2)], returnMsg);
      }
    } else {
      inputError = true;
      acctFlag = '0';
      setMsg(fileErrorMessage('READ', 'CXACAIX', r.resp, r.resp2));
    }
    if (acctFlag === '0') return;
    // 9300-GETACCTDATA-BYACCT
    r = ctx.track(ctx.files.read({ file: 'ACCTDAT', ridfld: acctKey, keylength: 11 }));
    if (r.resp === DFHRESP.NORMAL) {
      foundAcct = true;
      account = AccountRecordLayout.parse(r.record!);
    } else if (r.resp === DFHRESP.NOTFND) {
      inputError = true;
      acctFlag = '0';
      if (msgOff()) {
        returnMsg = cobolString(['Account:', acctKey, ' not found in', ' Acct Master file.Resp:', respText(r.resp), ' Reas:', respText(r.resp2)], returnMsg);
      }
    } else {
      inputError = true;
      acctFlag = '0';
      setMsg(fileErrorMessage('READ', 'ACCTDAT', r.resp, r.resp2));
    }
    // IF DID-NOT-FIND-ACCT-IN-ACCTDAT tests an 88 value that is never moved, so
    // the customer read always follows (bug-for-bug).
    if (returnMsg === moveAlnum('Did not find this account in account master file', 75)) return;
    // 9400-GETCUSTDATA-BYCUST
    const custKey = cdemo.get('CDEMO-CUST-ID');
    r = ctx.track(ctx.files.read({ file: 'CUSTDAT', ridfld: custKey, keylength: 9 }));
    if (r.resp === DFHRESP.NORMAL) {
      foundCust = true;
      customer = CustomerRecordLayout.parse(r.record!);
    } else if (r.resp === DFHRESP.NOTFND) {
      inputError = true;
      if (msgOff()) {
        returnMsg = cobolString(['CustId:', custKey, ' not found', ' in customer master.Resp: ', respText(r.resp), ' REAS:', respText(r.resp2)], returnMsg);
      }
    } else {
      inputError = true;
      setMsg(fileErrorMessage('READ', 'CUSTDAT', r.resp, r.resp2));
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
  } else if (cdemo.isPgmEnter) {
    sendMapScreen();
    commonReturn();
  } else if (cdemo.isPgmReenter) {
    processInputs();
    if (!inputError) readAcct();
    sendMapScreen();
    commonReturn();
  } else {
    sendPlainText(ctx, 'UNEXPECTED DATA SCENARIO');
  }
  commonReturn();
}

export const COACTVWC: OnlineProgram = { program: LIT_THISPGM, transid: LIT_THISTRANID, run };
