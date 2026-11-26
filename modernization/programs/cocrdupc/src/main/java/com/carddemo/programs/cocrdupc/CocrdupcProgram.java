package com.carddemo.programs.cocrdupc;

import static com.carddemo.programs.cocrdupc.Cobol.alnum;
import static com.carddemo.programs.cocrdupc.Cobol.displayIsZero;
import static com.carddemo.programs.cocrdupc.Cobol.displayValue;
import static com.carddemo.programs.cocrdupc.Cobol.eq;
import static com.carddemo.programs.cocrdupc.Cobol.isLowValues;
import static com.carddemo.programs.cocrdupc.Cobol.isNumeric;
import static com.carddemo.programs.cocrdupc.Cobol.isSpaces;
import static com.carddemo.programs.cocrdupc.Cobol.isZeros;
import static com.carddemo.programs.cocrdupc.Cobol.lowValues;
import static com.carddemo.programs.cocrdupc.Cobol.spaces;
import static com.carddemo.programs.cocrdupc.Cobol.upper;
import static com.carddemo.programs.cocrdupc.Cobol.zeros;
import static com.carddemo.programs.cocrdupc.copybook.Dfhbmsca.DFHBMBRY;
import static com.carddemo.programs.cocrdupc.copybook.Dfhbmsca.DFHBMDAR;
import static com.carddemo.programs.cocrdupc.copybook.Dfhbmsca.DFHBMFSE;
import static com.carddemo.programs.cocrdupc.copybook.Dfhbmsca.DFHBMPRF;
import static com.carddemo.programs.cocrdupc.copybook.Dfhbmsca.DFHDFCOL;
import static com.carddemo.programs.cocrdupc.copybook.Dfhbmsca.DFHRED;

import com.carddemo.programs.cocrdupc.CardFileGateway.FileResult;
import com.carddemo.programs.cocrdupc.CardUpdateProgramArea.CardDetails;
import com.carddemo.programs.cocrdupc.CardUpdateProgramArea.ChangeAction;
import com.carddemo.programs.cocrdupc.MiscStorage.EditFlag;
import com.carddemo.programs.cocrdupc.copybook.AbendData;
import com.carddemo.programs.cocrdupc.copybook.CardDemoCommarea;
import com.carddemo.programs.cocrdupc.copybook.CardRecord;
import com.carddemo.programs.cocrdupc.copybook.CcWorkArea;
import com.carddemo.programs.cocrdupc.copybook.CcWorkArea.CcardAid;
import com.carddemo.programs.cocrdupc.copybook.CcrdupaMap;
import com.carddemo.programs.cocrdupc.copybook.CcrdupaMap.Field;
import com.carddemo.programs.cocrdupc.copybook.DateTimeFields;
import com.carddemo.programs.cocrdupc.copybook.ScreenTitles;
import com.carddemo.programs.cocrdupc.copybook.StorePfKey;

import java.time.LocalDateTime;
import java.util.function.Supplier;

/**
 * COCRDUPC working storage and PROCEDURE DIVISION for one task. Paragraph names are kept in the
 * method comments; PERFORM ... THRU ...-EXIT is a method call and GO TO ...-EXIT is a return.
 */
final class CocrdupcProgram {

    // WS-LITERALS
    static final String LIT_THISPGM = "COCRDUPC";
    static final String LIT_THISTRANID = "CCUP";
    static final String LIT_THISMAPSET = "COCRDUP ";
    static final String LIT_THISMAP = "CCRDUPA";
    static final String LIT_CCLISTPGM = "COCRDLIC";
    static final String LIT_CCLISTTRANID = "CCLI";
    static final String LIT_CCLISTMAPSET = "COCRDLI";
    static final String LIT_CCLISTMAP = "CCRDSLA";
    static final String LIT_MENUPGM = "COMEN01C";
    static final String LIT_MENUTRANID = "CM00";
    static final String LIT_MENUMAPSET = "COMEN01";
    static final String LIT_MENUMAP = "COMEN1A";
    static final String LIT_CARDDTLPGM = "COCRDSLC";
    static final String LIT_CARDDTLTRANID = "CCDL";
    static final String LIT_CARDDTLMAPSET = "COCRDSL";
    static final String LIT_CARDDTLMAP = "CCRDSLA";
    static final String LIT_CARDFILENAME = "CARDDAT ";
    static final String LIT_CARDFILENAME_ACCT_PATH = "CARDAIX ";
    static final String LIT_ALL_ALPHA_FROM = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
    static final int WS_COMMAREA_LENGTH = 2000;

    private final CardFileGateway cardFile;
    private final Supplier<LocalDateTime> clock;

    private CcWorkArea cc;
    private MiscStorage ws;
    private CardDemoCommarea commarea = CardDemoCommarea.uninitialized();
    private CardUpdateProgramArea prog = CardUpdateProgramArea.uninitialized();
    private String wsCommarea;
    private final CcrdupaMap map = new CcrdupaMap();
    private final DateTimeFields dateTime = new DateTimeFields();
    private final AbendData abendData = new AbendData();
    private CardRecord cardRecord;
    private CcrdupaScreen sentScreen;

    CocrdupcProgram(CardFileGateway cardFile, Supplier<LocalDateTime> clock) {
        this.cardFile = cardFile;
        this.clock = clock;
    }

    /** EXEC CICS HANDLE ABEND LABEL(ABEND-ROUTINE) around 0000-MAIN. */
    CocrdupcResponse run(CocrdupcRequest request) {
        try {
            return mainPara(request);
        } catch (AbendException e) {
            throw e;
        } catch (RuntimeException e) {
            abendRoutine(e);
            throw new IllegalStateException("unreachable");
        }
    }

    // 0000-MAIN
    private CocrdupcResponse mainPara(CocrdupcRequest request) {
        cc = CcWorkArea.initialized();
        ws = new MiscStorage();
        wsCommarea = spaces(WS_COMMAREA_LENGTH);

        ws.wsTranid = LIT_THISTRANID;
        ws.setReturnMsgOff();

        // Tested before DFHCOMMAREA is copied in, so CDEMO-FROM-PROGRAM is still fresh storage here.
        if (request.eibcalen() == 0
                || (eq(commarea.fromProgram(), LIT_MENUPGM) && !commarea.isPgmReenter())) {
            commarea.initialize();
            prog.initialize();
            commarea.setPgmEnter();
            prog.set(ChangeAction.DETAILS_NOT_FETCHED);
        } else {
            int needed = CardDemoCommarea.LENGTH + CardUpdateProgramArea.LENGTH;
            String dfh = request.commarea();
            if (dfh.length() < needed) {
                dfh = dfh + lowValues(needed - dfh.length());
            }
            commarea = CardDemoCommarea.decode(dfh.substring(0, CardDemoCommarea.LENGTH));
            prog = CardUpdateProgramArea.decode(dfh.substring(CardDemoCommarea.LENGTH, needed));
        }

        StorePfKey.yyyyStorePfkey(request.eibaid(), cc);

        ws.setPfkInvalid();
        if (cc.is(CcardAid.ENTER)
                || cc.is(CcardAid.PFK03)
                || (cc.is(CcardAid.PFK05) && prog.isChangesOkNotConfirmed())
                || (cc.is(CcardAid.PFK12) && !prog.isDetailsNotFetched())) {
            ws.setPfkValid();
        }
        if (ws.isPfkInvalid()) {
            cc.setCcardAid(CcardAid.ENTER);
        }

        if (cc.is(CcardAid.PFK03)
                || (prog.isChangesOkayedAndDone() && eq(commarea.lastMapset(), LIT_CCLISTMAPSET))
                || (prog.isChangesFailed() && eq(commarea.lastMapset(), LIT_CCLISTMAPSET))) {
            // USER PRESSES PF03 TO EXIT OR IS DONE WITH UPDATE: XCTL TO CALLER OR MAIN MENU
            cc.setCcardAid(CcardAid.PFK03);
            if (isLowValues(commarea.fromTranid()) || isSpaces(commarea.fromTranid())) {
                commarea.setToTranid(LIT_MENUTRANID);
            } else {
                commarea.setToTranid(commarea.fromTranid());
            }
            if (isLowValues(commarea.fromProgram()) || isSpaces(commarea.fromProgram())) {
                commarea.setToProgram(LIT_MENUPGM);
            } else {
                commarea.setToProgram(commarea.fromProgram());
            }
            commarea.setFromTranid(LIT_THISTRANID);
            commarea.setFromProgram(LIT_THISPGM);
            if (eq(commarea.lastMapset(), LIT_CCLISTMAPSET)) {
                commarea.setAcctId(0);
                commarea.setCardNum(0);
            }
            commarea.setUsrtypUser();
            commarea.setPgmEnter();
            commarea.setLastMapset(LIT_THISMAPSET);
            commarea.setLastMap(LIT_THISMAP);
            cardFile.syncpoint();
            return new CocrdupcResponse(CocrdupcResponse.Kind.XCTL, null, commarea.toProgram(),
                    commarea.encode(), commarea.copy(), CardUpdateProgramArea.decode(prog.encode()),
                    null, true);
        }
        if ((commarea.isPgmEnter() && eq(commarea.fromProgram(), LIT_CCLISTPGM))
                || (cc.is(CcardAid.PFK12) && eq(commarea.fromProgram(), LIT_CCLISTPGM))) {
            // USER CAME FROM CREDIT CARD LIST SCREEN: FETCH THE CARD FOR UPDATE
            commarea.setPgmReenter();
            ws.setInputOk();
            ws.wsEditAcctFlag = EditFlag.IS_VALID.value;
            ws.wsEditCardFlag = EditFlag.IS_VALID.value;
            cc.setCcAcctIdN(commarea.acctId());
            cc.setCcCardNumN(commarea.cardNum());
            readData9000();
            prog.set(ChangeAction.SHOW_DETAILS);
            sendMap3000();
            return commonReturn();
        }
        if ((prog.isDetailsNotFetched() && commarea.isPgmEnter())
                || (eq(commarea.fromProgram(), LIT_MENUPGM) && !commarea.isPgmReenter())) {
            // FRESH ENTRY INTO PROGRAM: ASK FOR THE SEARCH KEYS
            prog.initialize();
            sendMap3000();
            commarea.setPgmReenter();
            prog.set(ChangeAction.DETAILS_NOT_FETCHED);
            return commonReturn();
        }
        if (prog.isChangesOkayedAndDone() || prog.isChangesFailed()) {
            // CHANGES DONE (OR FAILED): RESET AND ASK FOR FRESH SEARCH CRITERIA
            prog.initialize();
            ws = new MiscStorage();
            commarea.setAcctId(0);
            commarea.setCardNum(0);
            commarea.setPgmEnter();
            sendMap3000();
            commarea.setPgmReenter();
            prog.set(ChangeAction.DETAILS_NOT_FETCHED);
            return commonReturn();
        }
        // CARD DATA PRESENTED: CHECK INPUTS, DECIDE, PRESENT NEXT STEP
        processInputs1000(request.mapInput());
        decideAction2000();
        sendMap3000();
        return commonReturn();
    }

    // COMMON-RETURN
    private CocrdupcResponse commonReturn() {
        cc.setCcardErrorMsg(ws.wsReturnMsg);
        wsCommarea = alnum(commarea.encode(), WS_COMMAREA_LENGTH);
        int start = CardDemoCommarea.LENGTH;
        wsCommarea = wsCommarea.substring(0, start) + prog.encode()
                + wsCommarea.substring(start + CardUpdateProgramArea.LENGTH);
        return new CocrdupcResponse(CocrdupcResponse.Kind.RETURN_TRANSID, LIT_THISTRANID, null,
                wsCommarea, commarea.copy(), CardUpdateProgramArea.decode(prog.encode()), sentScreen,
                false);
    }

    // 1000-PROCESS-INPUTS
    private void processInputs1000(CcrdupaInput input) {
        receiveMap1100(input);
        editMapInputs1200();
        cc.setCcardErrorMsg(ws.wsReturnMsg);
        cc.setCcardNextProg(LIT_THISPGM);
        cc.setCcardNextMapset(LIT_THISMAPSET);
        cc.setCcardNextMap(LIT_THISMAP);
    }

    // 1100-RECEIVE-MAP
    private void receiveMap1100(CcrdupaInput input) {
        CcrdupaInput in = input == null ? CcrdupaInput.none() : input;
        String acctsidi = leftInput(in.acctsid(), Field.ACCTSID);
        String cardsidi = leftInput(in.cardsid(), Field.CARDSID);
        String crdnamei = leftInput(in.crdname(), Field.CRDNAME);
        String crdstcdi = leftInput(in.crdstcd(), Field.CRDSTCD);
        String expmoni = rightInput(in.expmon(), Field.EXPMON);
        String expyeari = rightInput(in.expyear(), Field.EXPYEAR);
        String expdayi = rightInput(in.expday(), Field.EXPDAY);

        CardDetails nw = prog.newDetails();
        nw.initialize();
        if (eq(acctsidi, "*") || isSpaces(acctsidi)) {
            cc.setCcAcctId(lowValues(11));
            nw.setAcctId(lowValues(11));
        } else {
            cc.setCcAcctId(acctsidi);
            nw.setAcctId(acctsidi);
        }
        if (eq(cardsidi, "*") || isSpaces(cardsidi)) {
            cc.setCcCardNum(lowValues(16));
            nw.setCardId(lowValues(16));
        } else {
            cc.setCcCardNum(cardsidi);
            nw.setCardId(cardsidi);
        }
        if (eq(crdnamei, "*") || isSpaces(crdnamei)) {
            nw.setCrdName(lowValues(50));
        } else {
            nw.setCrdName(crdnamei);
        }
        if (eq(crdstcdi, "*") || isSpaces(crdstcdi)) {
            nw.setCrdStCd(lowValues(1));
        } else {
            nw.setCrdStCd(crdstcdi);
        }
        nw.setExpDay(expdayi);
        if (eq(expmoni, "*") || isSpaces(expmoni)) {
            nw.setExpMon(lowValues(2));
        } else {
            nw.setExpMon(expmoni);
        }
        if (eq(expyeari, "*") || isSpaces(expyeari)) {
            nw.setExpYear(lowValues(4));
        } else {
            nw.setExpYear(expyeari);
        }
    }

    private String leftInput(String v, Field f) {
        String data = v == null ? lowValues(f.length()) : alnum(v, f.length());
        map.get(f).setData(data);
        return data;
    }

    private String rightInput(String v, Field f) {
        String data = v == null ? lowValues(f.length()) : Cobol.rightJustify(v, f.length());
        map.get(f).setData(data);
        return data;
    }

    // 1200-EDIT-MAP-INPUTS
    private void editMapInputs1200() {
        ws.setInputOk();
        if (prog.isDetailsNotFetched()) {
            // VALIDATE THE SEARCH KEYS
            editAccount1210();
            editCard1220();
            prog.newDetails().setCardData(lowValues(59));
            if (ws.wsEditAcctFlag == EditFlag.BLANK.value && ws.wsEditCardFlag == EditFlag.BLANK.value) {
                ws.set(ReturnMessage.NO_SEARCH_CRITERIA_RECEIVED);
            }
            return;
        }
        // SEARCH KEYS ALREADY VALIDATED AND DATA FETCHED
        ws.set(InfoMessage.FOUND_CARDS_FOR_ACCOUNT);
        ws.wsEditAcctFlag = EditFlag.IS_VALID.value;
        ws.wsEditCardFlag = EditFlag.IS_VALID.value;
        CardDetails old = prog.oldDetails();
        commarea.setAcctId(displayValue(old.acctId()));
        commarea.setCardNum(displayValue(old.cardId()));
        // CARD-EMBOSSED-NAME / CARD-ACTIVE-STATUS are CARD-RECORD fields; CARD-EXPIRY-* overlay
        // CARD-EXPIRAION-DATE-X in CICS-OUTPUT-EDIT-VARS
        CardRecord base = cardRecord == null ? CardRecord.parse(lowValues(CardRecord.LENGTH)) : cardRecord;
        cardRecord = new CardRecord(base.cardNum(), base.cardAcctId(), base.cardCvvCd(), old.crdName(),
                base.cardExpiraionDate(), old.crdStCd(), base.filler());
        ws.cardExpiraionDateX = old.expYear() + ws.cardExpiraionDateX.charAt(4) + old.expMon()
                + ws.cardExpiraionDateX.charAt(7) + old.expDay();

        if (upper(prog.newDetails().cardData()).equals(upper(old.cardData()))) {
            ws.set(ReturnMessage.NO_CHANGES_DETECTED);
        }
        if (ws.is(ReturnMessage.NO_CHANGES_DETECTED)
                || prog.isChangesOkNotConfirmed()
                || prog.isChangesOkayedAndDone()) {
            ws.wsEditCardnameFlag = EditFlag.IS_VALID.value;
            ws.wsEditCardstatusFlag = EditFlag.IS_VALID.value;
            ws.wsEditCardexpmonFlag = EditFlag.IS_VALID.value;
            ws.wsEditCardexpyearFlag = EditFlag.IS_VALID.value;
            return;
        }
        prog.set(ChangeAction.CHANGES_NOT_OK);
        editName1230();
        editCardStatus1240();
        editExpiryMon1250();
        editExpiryYear1260();
        if (!ws.isInputError()) {
            prog.set(ChangeAction.CHANGES_OK_NOT_CONFIRMED);
        }
    }

    // 1210-EDIT-ACCOUNT
    private void editAccount1210() {
        ws.wsEditAcctFlag = EditFlag.NOT_OK.value;
        String acct = cc.ccAcctId();
        if (isLowValues(acct) || isSpaces(acct) || displayIsZero(acct)) {
            ws.setInputError();
            ws.wsEditAcctFlag = EditFlag.BLANK.value;
            if (ws.isReturnMsgOff()) {
                ws.set(ReturnMessage.WS_PROMPT_FOR_ACCT);
            }
            commarea.setAcctId(0);
            prog.newDetails().setAcctId(lowValues(11));
            return;
        }
        if (!isNumeric(acct)) {
            ws.setInputError();
            ws.wsEditAcctFlag = EditFlag.NOT_OK.value;
            if (ws.isReturnMsgOff()) {
                ws.set(ReturnMessage.ACCOUNT_FILTER_NOT_NUMERIC);
            }
            commarea.setAcctId(0);
            prog.newDetails().setAcctId(lowValues(11));
        } else {
            commarea.setAcctId(displayValue(acct));
            prog.newDetails().setAcctId(acct);
            ws.wsEditAcctFlag = EditFlag.IS_VALID.value;
        }
    }

    // 1220-EDIT-CARD
    private void editCard1220() {
        ws.wsEditCardFlag = EditFlag.NOT_OK.value;
        String card = cc.ccCardNum();
        if (isLowValues(card) || isSpaces(card) || displayIsZero(card)) {
            ws.setInputError();
            ws.wsEditCardFlag = EditFlag.BLANK.value;
            if (ws.isReturnMsgOff()) {
                ws.set(ReturnMessage.WS_PROMPT_FOR_CARD);
            }
            commarea.setCardNum(0);
            prog.newDetails().setCardId(zeros(16));
            return;
        }
        if (!isNumeric(card)) {
            ws.setInputError();
            ws.wsEditCardFlag = EditFlag.NOT_OK.value;
            if (ws.isReturnMsgOff()) {
                ws.set(ReturnMessage.CARD_FILTER_NOT_NUMERIC);
            }
            commarea.setCardNum(0);
            prog.newDetails().setCardId(lowValues(16));
        } else {
            commarea.setCardNum(cc.ccCardNumN());
            prog.newDetails().setCardId(card);
            ws.wsEditCardFlag = EditFlag.IS_VALID.value;
        }
    }

    // 1230-EDIT-NAME
    private void editName1230() {
        ws.wsEditCardnameFlag = EditFlag.NOT_OK.value;
        String name = prog.newDetails().crdName();
        if (isLowValues(name) || isSpaces(name) || isZeros(name)) {
            ws.setInputError();
            ws.wsEditCardnameFlag = EditFlag.BLANK.value;
            if (ws.isReturnMsgOff()) {
                ws.set(ReturnMessage.WS_PROMPT_FOR_NAME);
            }
            return;
        }
        // INSPECT CARD-NAME-CHECK CONVERTING LIT-ALL-ALPHA-FROM TO LIT-ALL-SPACES-TO
        StringBuilder check = new StringBuilder(name.length());
        for (int i = 0; i < name.length(); i++) {
            char c = name.charAt(i);
            check.append(LIT_ALL_ALPHA_FROM.indexOf(c) >= 0 ? ' ' : c);
        }
        ws.cardNameCheck = check.toString();
        if (!isSpaces(ws.cardNameCheck)) {
            ws.setInputError();
            ws.wsEditCardnameFlag = EditFlag.NOT_OK.value;
            if (ws.isReturnMsgOff()) {
                ws.set(ReturnMessage.WS_NAME_MUST_BE_ALPHA);
            }
            return;
        }
        ws.wsEditCardnameFlag = EditFlag.IS_VALID.value;
    }

    // 1240-EDIT-CARDSTATUS
    private void editCardStatus1240() {
        ws.wsEditCardstatusFlag = EditFlag.NOT_OK.value;
        String st = prog.newDetails().crdStCd();
        if (isLowValues(st) || isSpaces(st) || isZeros(st)) {
            ws.setInputError();
            ws.wsEditCardstatusFlag = EditFlag.BLANK.value;
            if (ws.isReturnMsgOff()) {
                ws.set(ReturnMessage.CARD_STATUS_MUST_BE_YES_NO);
            }
            return;
        }
        ws.flgYesNoCheck = st.charAt(0);
        if (ws.isFlgYesNoValid()) {
            ws.wsEditCardstatusFlag = EditFlag.IS_VALID.value;
        } else {
            ws.setInputError();
            ws.wsEditCardstatusFlag = EditFlag.NOT_OK.value;
            if (ws.isReturnMsgOff()) {
                ws.set(ReturnMessage.CARD_STATUS_MUST_BE_YES_NO);
            }
        }
    }

    // 1250-EDIT-EXPIRY-MON
    private void editExpiryMon1250() {
        ws.wsEditCardexpmonFlag = EditFlag.NOT_OK.value;
        String mon = prog.newDetails().expMon();
        if (isLowValues(mon) || isSpaces(mon) || isZeros(mon)) {
            ws.setInputError();
            ws.wsEditCardexpmonFlag = EditFlag.BLANK.value;
            if (ws.isReturnMsgOff()) {
                ws.set(ReturnMessage.CARD_EXPIRY_MONTH_NOT_VALID);
            }
            return;
        }
        ws.cardMonthCheck = mon;
        if (ws.isValidMonth()) {
            ws.wsEditCardexpmonFlag = EditFlag.IS_VALID.value;
        } else {
            ws.setInputError();
            ws.wsEditCardexpmonFlag = EditFlag.NOT_OK.value;
            if (ws.isReturnMsgOff()) {
                ws.set(ReturnMessage.CARD_EXPIRY_MONTH_NOT_VALID);
            }
        }
    }

    // 1260-EDIT-EXPIRY-YEAR
    private void editExpiryYear1260() {
        String year = prog.newDetails().expYear();
        if (isLowValues(year) || isSpaces(year) || isZeros(year)) {
            ws.setInputError();
            ws.wsEditCardexpyearFlag = EditFlag.BLANK.value;
            if (ws.isReturnMsgOff()) {
                ws.set(ReturnMessage.CARD_EXPIRY_YEAR_NOT_VALID);
            }
            return;
        }
        ws.wsEditCardexpyearFlag = EditFlag.NOT_OK.value;
        ws.cardYearCheck = year;
        if (ws.isValidYear()) {
            ws.wsEditCardexpyearFlag = EditFlag.IS_VALID.value;
        } else {
            ws.setInputError();
            ws.wsEditCardexpyearFlag = EditFlag.NOT_OK.value;
            if (ws.isReturnMsgOff()) {
                ws.set(ReturnMessage.CARD_EXPIRY_YEAR_NOT_VALID);
            }
        }
    }

    // 2000-DECIDE-ACTION
    private void decideAction2000() {
        if (prog.isDetailsNotFetched() || cc.is(CcardAid.PFK12)) {
            // NO DETAILS SHOWN, OR CHANGES MADE BUT USER CANCELS: (RE)FETCH
            if (ws.wsEditAcctFlag == EditFlag.IS_VALID.value
                    && ws.wsEditCardFlag == EditFlag.IS_VALID.value) {
                readData9000();
                if (ws.is(InfoMessage.FOUND_CARDS_FOR_ACCOUNT)) {
                    prog.set(ChangeAction.SHOW_DETAILS);
                }
            }
        } else if (prog.isShowDetails()) {
            if (!(ws.isInputError() || ws.is(ReturnMessage.NO_CHANGES_DETECTED))) {
                prog.set(ChangeAction.CHANGES_OK_NOT_CONFIRMED);
            }
        } else if (prog.isChangesNotOk()) {
            // DETAILS SHOWN BUT INPUT EDIT ERRORS FOUND
        } else if (prog.isChangesOkNotConfirmed() && cc.is(CcardAid.PFK05)) {
            // CONFIRMATION GIVEN: SAVE THE CHANGES
            writeProcessing9200();
            if (ws.is(ReturnMessage.COULD_NOT_LOCK_FOR_UPDATE)) {
                prog.set(ChangeAction.CHANGES_OKAYED_LOCK_ERROR);
            } else if (ws.is(ReturnMessage.LOCKED_BUT_UPDATE_FAILED)) {
                prog.set(ChangeAction.CHANGES_OKAYED_BUT_FAILED);
            } else if (ws.is(ReturnMessage.DATA_WAS_CHANGED_BEFORE_UPDATE)) {
                prog.set(ChangeAction.SHOW_DETAILS);
            } else {
                prog.set(ChangeAction.CHANGES_OKAYED_AND_DONE);
            }
        } else if (prog.isChangesOkNotConfirmed()) {
            // CONFIRMATION NOT GIVEN: SHOW DETAILS AGAIN
        } else if (prog.isChangesOkayedAndDone()) {
            prog.set(ChangeAction.SHOW_DETAILS);
            if (isLowValues(commarea.fromTranid()) || isSpaces(commarea.fromTranid())) {
                commarea.setAcctId(0);
                commarea.setCardNum(0);
                commarea.setAcctStatus(lowValues(1));
            }
        } else {
            abendData.setAbendCulprit(LIT_THISPGM);
            abendData.setAbendCode("0001");
            abendData.setAbendReason(spaces(50));
            abendData.setAbendMsg("UNEXPECTED DATA SCENARIO");
            abendRoutine(null);
        }
    }

    // 3000-SEND-MAP
    private void sendMap3000() {
        screenInit3100();
        setupScreenVars3200();
        setupInfoMsg3250();
        setupScreenAttrs3300();
        sendScreen3400();
    }

    // 3100-SCREEN-INIT
    private void screenInit3100() {
        map.clearToLowValues();
        dateTime.setCurrentDate(clock.get());
        map.get(Field.TITLE01).setData(ScreenTitles.CCDA_TITLE01);
        map.get(Field.TITLE02).setData(ScreenTitles.CCDA_TITLE02);
        map.get(Field.TRNNAME).setData(LIT_THISTRANID);
        map.get(Field.PGMNAME).setData(LIT_THISPGM);
        map.get(Field.CURDATE).setData(dateTime.wsCurdateMmDdYy());
        map.get(Field.CURTIME).setData(dateTime.wsCurtimeHhMmSs());
    }

    // 3200-SETUP-SCREEN-VARS
    private void setupScreenVars3200() {
        if (commarea.isPgmEnter()) {
            return;
        }
        map.get(Field.ACCTSID).setData(cc.ccAcctIdN() == 0 ? lowValues(11) : cc.ccAcctId());
        map.get(Field.CARDSID).setData(cc.ccCardNumN() == 0 ? lowValues(16) : cc.ccCardNum());
        CardDetails old = prog.oldDetails();
        CardDetails nw = prog.newDetails();
        if (prog.isDetailsNotFetched()) {
            map.get(Field.CRDNAME).setData(lowValues(50));
            map.get(Field.CRDSTCD).setData(lowValues(1));
            map.get(Field.EXPDAY).setData(lowValues(2));
            map.get(Field.EXPMON).setData(lowValues(2));
            map.get(Field.EXPYEAR).setData(lowValues(4));
        } else if (prog.isShowDetails()) {
            map.get(Field.CRDNAME).setData(old.crdName());
            map.get(Field.CRDSTCD).setData(old.crdStCd());
            map.get(Field.EXPDAY).setData(old.expDay());
            map.get(Field.EXPMON).setData(old.expMon());
            map.get(Field.EXPYEAR).setData(old.expYear());
        } else if (prog.isChangesMade()) {
            map.get(Field.CRDNAME).setData(nw.crdName());
            map.get(Field.CRDSTCD).setData(nw.crdStCd());
            map.get(Field.EXPMON).setData(nw.expMon());
            map.get(Field.EXPYEAR).setData(nw.expYear());
            map.get(Field.EXPDAY).setData(old.expDay());
        } else {
            map.get(Field.CRDNAME).setData(old.crdName());
            map.get(Field.CRDSTCD).setData(old.crdStCd());
            map.get(Field.EXPDAY).setData(old.expDay());
            map.get(Field.EXPMON).setData(old.expMon());
            map.get(Field.EXPYEAR).setData(old.expYear());
        }
    }

    // 3250-SETUP-INFOMSG
    private void setupInfoMsg3250() {
        if (commarea.isPgmEnter()) {
            ws.set(InfoMessage.PROMPT_FOR_SEARCH_KEYS);
        } else if (prog.isDetailsNotFetched()) {
            ws.set(InfoMessage.PROMPT_FOR_SEARCH_KEYS);
        } else if (prog.isShowDetails()) {
            ws.set(InfoMessage.FOUND_CARDS_FOR_ACCOUNT);
        } else if (prog.isChangesNotOk()) {
            ws.set(InfoMessage.PROMPT_FOR_CHANGES);
        } else if (prog.isChangesOkNotConfirmed()) {
            ws.set(InfoMessage.PROMPT_FOR_CONFIRMATION);
        } else if (prog.isChangesOkayedAndDone()) {
            ws.set(InfoMessage.CONFIRM_UPDATE_SUCCESS);
        } else if (prog.isChangesOkayedLockError()) {
            ws.set(InfoMessage.INFORM_FAILURE);
        } else if (prog.isChangesOkayedButFailed()) {
            ws.set(InfoMessage.INFORM_FAILURE);
        } else if (ws.isNoInfoMessage()) {
            ws.set(InfoMessage.PROMPT_FOR_SEARCH_KEYS);
        }
        map.get(Field.INFOMSG).setData(ws.wsInfoMsg);
        map.get(Field.ERRMSG).setData(ws.wsReturnMsg);
    }

    // 3300-SETUP-SCREEN-ATTRS
    private void setupScreenAttrs3300() {
        // PROTECT OR UNPROTECT BASED ON CONTEXT (EXPDAY attribute is commented out in the source)
        if (prog.isDetailsNotFetched()) {
            attrs(DFHBMFSE, Field.ACCTSID, Field.CARDSID);
            attrs(DFHBMPRF, Field.CRDNAME, Field.CRDSTCD, Field.EXPMON, Field.EXPYEAR);
        } else if (prog.isShowDetails() || prog.isChangesNotOk()) {
            attrs(DFHBMPRF, Field.ACCTSID, Field.CARDSID);
            attrs(DFHBMFSE, Field.CRDNAME, Field.CRDSTCD, Field.EXPMON, Field.EXPYEAR);
        } else if (prog.isChangesOkNotConfirmed() || prog.isChangesOkayedAndDone()) {
            attrs(DFHBMPRF, Field.ACCTSID, Field.CARDSID, Field.CRDNAME, Field.CRDSTCD,
                    Field.EXPMON, Field.EXPYEAR);
        } else {
            attrs(DFHBMFSE, Field.ACCTSID, Field.CARDSID);
            attrs(DFHBMPRF, Field.CRDNAME, Field.CRDSTCD, Field.EXPMON, Field.EXPYEAR);
        }

        // POSITION CURSOR
        if (ws.is(InfoMessage.FOUND_CARDS_FOR_ACCOUNT) || ws.is(ReturnMessage.NO_CHANGES_DETECTED)) {
            cursor(Field.CRDNAME);
        } else if (notOkOrBlank(ws.wsEditAcctFlag)) {
            cursor(Field.ACCTSID);
        } else if (notOkOrBlank(ws.wsEditCardFlag)) {
            cursor(Field.CARDSID);
        } else if (notOkOrBlank(ws.wsEditCardnameFlag)) {
            cursor(Field.CRDNAME);
        } else if (notOkOrBlank(ws.wsEditCardstatusFlag)) {
            cursor(Field.CRDSTCD);
        } else if (notOkOrBlank(ws.wsEditCardexpmonFlag)) {
            cursor(Field.EXPMON);
        } else if (notOkOrBlank(ws.wsEditCardexpyearFlag)) {
            cursor(Field.EXPYEAR);
        } else {
            cursor(Field.ACCTSID);
        }

        // SETUP COLOR
        if (eq(commarea.lastMapset(), LIT_CCLISTMAPSET)) {
            map.get(Field.ACCTSID).setColor(DFHDFCOL);
            map.get(Field.CARDSID).setColor(DFHDFCOL);
        }
        if (ws.wsEditAcctFlag == EditFlag.NOT_OK.value) {
            map.get(Field.ACCTSID).setColor(DFHRED);
        }
        if (ws.wsEditAcctFlag == EditFlag.BLANK.value && commarea.isPgmReenter()) {
            starRed(Field.ACCTSID);
        }
        if (ws.wsEditCardFlag == EditFlag.NOT_OK.value) {
            map.get(Field.CARDSID).setColor(DFHRED);
        }
        if (ws.wsEditCardFlag == EditFlag.BLANK.value && commarea.isPgmReenter()) {
            starRed(Field.CARDSID);
        }
        editColor(ws.wsEditCardnameFlag, Field.CRDNAME);
        editColor(ws.wsEditCardstatusFlag, Field.CRDSTCD);
        map.get(Field.EXPDAY).setColor(DFHBMDAR);
        editColor(ws.wsEditCardexpmonFlag, Field.EXPMON);
        editColor(ws.wsEditCardexpyearFlag, Field.EXPYEAR);
        map.get(Field.INFOMSG).setAttribute(ws.isNoInfoMessage() ? DFHBMDAR : DFHBMBRY);
        if (ws.is(InfoMessage.PROMPT_FOR_CONFIRMATION)) {
            map.get(Field.FKEYSC).setAttribute(DFHBMBRY);
        }
    }

    private void attrs(int attribute, Field... fields) {
        for (Field f : fields) {
            map.get(f).setAttribute(attribute);
        }
    }

    private void cursor(Field f) {
        map.get(f).setLength(-1);
    }

    private static boolean notOkOrBlank(char flag) {
        return flag == EditFlag.NOT_OK.value || flag == EditFlag.BLANK.value;
    }

    private void starRed(Field f) {
        map.get(f).setData("*");
        map.get(f).setColor(DFHRED);
    }

    private void editColor(char flag, Field f) {
        if (flag == EditFlag.NOT_OK.value && prog.isChangesNotOk()) {
            map.get(f).setColor(DFHRED);
        }
        if (flag == EditFlag.BLANK.value && prog.isChangesNotOk()) {
            starRed(f);
        }
    }

    // 3400-SEND-SCREEN
    private void sendScreen3400() {
        cc.setCcardNextMapset(LIT_THISMAPSET);
        cc.setCcardNextMap(LIT_THISMAP);
        sentScreen = CcrdupaScreen.of(cc.ccardNextMapset(), cc.ccardNextMap(), map);
    }

    // 9000-READ-DATA
    private void readData9000() {
        CardDetails old = prog.oldDetails();
        old.initialize();
        old.setAcctId(cc.ccAcctId());
        old.setCardId(cc.ccCardNum());
        getCardByAcctCard9100();
        if (ws.is(InfoMessage.FOUND_CARDS_FOR_ACCOUNT)) {
            old.setCvvCd(cardRecord.cardCvvCdDisplay());
            cardRecord = withEmbossedName(cardRecord, upper(cardRecord.cardEmbossedName()));
            old.setCrdName(cardRecord.cardEmbossedName());
            old.setExpYear(cardRecord.cardExpiraionDate().substring(0, 4));
            old.setExpMon(cardRecord.cardExpiraionDate().substring(5, 7));
            old.setExpDay(cardRecord.cardExpiraionDate().substring(8, 10));
            old.setCrdStCd(cardRecord.cardActiveStatus());
        }
    }

    // 9100-GETCARD-BYACCTCARD
    private void getCardByAcctCard9100() {
        ws.wsCardRidCardnum = cc.ccCardNum();
        FileResult r = cardFile.read(ws.wsCardRidCardnum);
        ws.wsRespCd = r.resp().code();
        ws.wsReasCd = r.resp2();
        if (r.resp() == CicsResp.NORMAL) {
            cardRecord = r.record();
            ws.set(InfoMessage.FOUND_CARDS_FOR_ACCOUNT);
        } else if (r.resp() == CicsResp.NOTFND) {
            ws.setInputError();
            ws.wsEditAcctFlag = EditFlag.NOT_OK.value;
            ws.wsEditCardFlag = EditFlag.NOT_OK.value;
            if (ws.isReturnMsgOff()) {
                ws.set(ReturnMessage.DID_NOT_FIND_ACCTCARD_COMBO);
            }
        } else {
            ws.setInputError();
            if (ws.isReturnMsgOff()) {
                ws.wsEditAcctFlag = EditFlag.NOT_OK.value;
            }
            ws.errorOpname = alnum("READ", 8);
            ws.errorFile = alnum(LIT_CARDFILENAME, 9);
            ws.errorResp = MiscStorage.respToAlnum(ws.wsRespCd);
            ws.errorResp2 = MiscStorage.respToAlnum(ws.wsReasCd);
            ws.setReturnMsg(ws.wsFileErrorMessage());
        }
    }

    // 9200-WRITE-PROCESSING
    private void writeProcessing9200() {
        ws.wsCardRidCardnum = cc.ccCardNum();
        FileResult r = cardFile.readForUpdate(ws.wsCardRidCardnum);
        ws.wsRespCd = r.resp().code();
        ws.wsReasCd = r.resp2();
        if (r.resp() != CicsResp.NORMAL) {
            // Could we lock the record?
            ws.setInputError();
            if (ws.isReturnMsgOff()) {
                ws.set(ReturnMessage.COULD_NOT_LOCK_FOR_UPDATE);
            }
            return;
        }
        cardRecord = r.record();
        // Did someone change the record while we were out?
        checkChangeInRec9300();
        if (ws.is(ReturnMessage.DATA_WAS_CHANGED_BEFORE_UPDATE)) {
            return;
        }
        // Prepare the update
        CardUpdateProgramArea.CardUpdateRecord upd = prog.cardUpdateRecord();
        CardDetails nw = prog.newDetails();
        upd.initialize();
        upd.setNum(nw.cardId());
        upd.setAcctId(cc.ccAcctIdN());
        ws.cardCvvCdX = nw.cvvCd();
        upd.setCvvCd(displayValue(ws.cardCvvCdX));
        upd.setEmbossedName(nw.crdName());
        upd.setExpiraionDate(nw.expYear() + "-" + nw.expMon() + "-" + nw.expDay());
        upd.setActiveStatus(nw.crdStCd());
        FileResult w = cardFile.rewrite(upd.toCardRecord());
        ws.wsRespCd = w.resp().code();
        ws.wsReasCd = w.resp2();
        // Did the update succeed?
        if (w.resp() != CicsResp.NORMAL) {
            ws.set(ReturnMessage.LOCKED_BUT_UPDATE_FAILED);
        }
    }

    // 9300-CHECK-CHANGE-IN-REC
    private void checkChangeInRec9300() {
        cardRecord = withEmbossedName(cardRecord, upper(cardRecord.cardEmbossedName()));
        CardDetails old = prog.oldDetails();
        String date = cardRecord.cardExpiraionDate();
        if (eq(cardRecord.cardCvvCdDisplay(), old.cvvCd())
                && eq(cardRecord.cardEmbossedName(), old.crdName())
                && eq(date.substring(0, 4), old.expYear())
                && eq(date.substring(5, 7), old.expMon())
                && eq(date.substring(8, 10), old.expDay())
                && eq(cardRecord.cardActiveStatus(), old.crdStCd())) {
            return;
        }
        ws.set(ReturnMessage.DATA_WAS_CHANGED_BEFORE_UPDATE);
        old.setCvvCd(cardRecord.cardCvvCdDisplay());
        old.setCrdName(cardRecord.cardEmbossedName());
        old.setExpYear(date.substring(0, 4));
        old.setExpMon(date.substring(5, 7));
        old.setExpDay(date.substring(8, 10));
        old.setCrdStCd(cardRecord.cardActiveStatus());
    }

    private static CardRecord withEmbossedName(CardRecord r, String name) {
        return new CardRecord(r.cardNum(), r.cardAcctId(), r.cardCvvCd(), name,
                r.cardExpiraionDate(), r.cardActiveStatus(), r.filler());
    }

    // ABEND-ROUTINE
    private void abendRoutine(Throwable cause) {
        if (isLowValues(abendData.abendMsg())) {
            abendData.setAbendMsg("UNEXPECTED ABEND OCCURRED.");
        }
        abendData.setAbendCulprit(LIT_THISPGM);
        throw new AbendException("9999", abendData.copy(), cause);
    }
}
