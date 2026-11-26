package com.carddemo.programs.cotrtlic;

import static com.carddemo.programs.cotrtlic.MiscStorage.*;

import com.carddemo.programs.cotrtlic.copybook.CardDemoCommarea;
import com.carddemo.programs.cotrtlic.copybook.CcWorkArea;
import com.carddemo.programs.cotrtlic.copybook.CcWorkArea.CcardAid;
import com.carddemo.programs.cotrtlic.copybook.CotrtliMap;
import com.carddemo.programs.cotrtlic.copybook.DateTimeWork;
import com.carddemo.programs.cotrtlic.copybook.DclTransactionType;
import com.carddemo.programs.cotrtlic.copybook.Dfhbmsca;
import com.carddemo.programs.cotrtlic.copybook.PfKeyStore;
import com.carddemo.programs.cotrtlic.copybook.ScreenTitles;
import com.carddemo.programs.cotrtlic.copybook.Sqlca;
import com.carddemo.programs.cotrtlic.gateway.CicsSyncpoint;
import com.carddemo.programs.cotrtlic.gateway.Dsntiac;
import com.carddemo.programs.cotrtlic.gateway.SqlResult;
import com.carddemo.programs.cotrtlic.gateway.TransactionTypeDb2Gateway;
import com.carddemo.programs.cotrtlic.gateway.TypeFilter;
import com.carddemo.programs.cotrtlic.support.Cob;
import java.time.LocalDateTime;

/** One execution (CICS task) of COTRTLIC. Working storage is fresh per task, as in CICS. */
final class CotrtlicTask {

    // 01 WS-CONSTANTS
    static final String LIT_THISPGM = "COTRTLIC";
    static final String LIT_THISTRANID = "CTLI";
    static final String LIT_THISMAPSET = "COTRTLI";
    static final String LIT_THISMAP = "CTRTLIA";
    static final String LIT_ADMINPGM = "COADM01C";
    static final String LIT_ADMINTRANID = "CA00";
    static final String LIT_ADMINMAPSET = "COADM01";
    static final String LIT_ADDTPGM = "COTRTUPC";
    static final String LIT_ADDTTRANID = "CTTU";
    static final String LIT_ADDTMAPSET = "COTRTUP";
    static final String LIT_ADDTMAP = "CTRTUPA";
    static final String LIT_DSNTIAC = "DSNTIAC";
    static final String LIT_ASTERISK = "*      ";
    static final String LIT_TRANTYPE_TABLE = Cob.move("TRANSACTION_TYPE ", 30);
    static final char LIT_DELETE_FLAG = 'D';
    static final char LIT_UPDATE_FLAG = 'U';
    static final int WS_MAX_SCREEN_LINES = 7;
    static final String LIT_ALL_ALPHANUM_FROM_X =
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ" + "abcdefghijklmnopqrstuvwxyz" + "0123456789";
    static final String LIT_ALPHANUM_SPACES_TO = Cob.spaces(62);

    private static final class ProgramExit extends RuntimeException {
        private final transient CicsResponse response;

        ProgramExit(CicsResponse response) {
            super(null, null, false, false);
            this.response = response;
        }
    }

    private final TransactionTypeDb2Gateway db;
    private final Dsntiac dsntiac;
    private final CicsSyncpoint cics;
    private final LocalDateTime now;

    private final MiscStorage ws = new MiscStorage();
    private final CcWorkArea work = new CcWorkArea();
    private final DateTimeWork dateTime = new DateTimeWork();
    private final Sqlca sqlca = new Sqlca();
    private final CotrtliMap map = new CotrtliMap();
    private CardDemoCommarea cdemo = new CardDemoCommarea();
    private ThisProgCommarea ca = new ThisProgCommarea();
    private int eibcalen;
    private CotrtliMap sentMap;

    CotrtlicTask(TransactionTypeDb2Gateway db, Dsntiac dsntiac, CicsSyncpoint cics, LocalDateTime now) {
        this.db = db;
        this.dsntiac = dsntiac;
        this.cics = cics;
        this.now = now;
    }

    CicsResponse run(CicsRequest request) {
        try {
            main0000(request);
            throw new IllegalStateException("COTRTLIC ended without a CICS RETURN/XCTL");
        } catch (ProgramExit exit) {
            return exit.response;
        }
    }

    private boolean aid(CcardAid a) {
        return work.is(a);
    }

    private boolean fromThisProgram() {
        return Cob.eq(cdemo.fromProgram, LIT_THISPGM);
    }

    // 0000-MAIN
    private void main0000(CicsRequest req) {
        work.initialize();
        ws.initialize();
        ws.tranid = LIT_THISTRANID;
        ws.returnMsg = Cob.spaces(75);

        eibcalen = req.commarea() == null ? 0 : ProgramCommarea.LENGTH;
        if (eibcalen == 0) {
            cdemo.initialize();
            ca.initialize();
            cdemo.fromTranid = LIT_THISTRANID;
            cdemo.fromProgram = LIT_THISPGM;
            cdemo.setUsrtypAdmin();
            cdemo.setPgmEnter();
            cdemo.lastMap = LIT_THISMAP;
            cdemo.lastMapset = LIT_THISMAPSET;
            ca.setFirstPage();
            ca.setLastPageNotShown();
        } else {
            cdemo = req.commarea().cardDemo().copy();
            ca = req.commarea().thisProg().copy();
        }

        PfKeyStore.storePfKey(req.eibaid(), work);

        if ((cdemo.isPgmEnter() && !fromThisProgram())
                || (aid(CcardAid.PFK03) && Cob.eq(cdemo.fromTranid, LIT_ADDTTRANID))) {
            ca.initialize();
            cdemo.setPgmEnter();
            work.set(CcardAid.ENTER);
            cdemo.lastMap = LIT_THISMAP;
            ca.setFirstPage();
            ca.setLastPageNotShown();
        }

        if (eibcalen > 0 && fromThisProgram()) {
            receiveMap1000(req.terminalMap());
        }

        ws.pfkFlag = '1';
        if (aid(CcardAid.ENTER) || aid(CcardAid.PFK02) || aid(CcardAid.PFK03) || aid(CcardAid.PFK07)
                || aid(CcardAid.PFK08)
                || (aid(CcardAid.PFK10) && ca.isDeleteRequested())
                || (aid(CcardAid.PFK10) && ca.isUpdateRequested())) {
            ws.pfkFlag = '0';
        }
        if (ws.pfkFlag == '1') {
            work.set(CcardAid.ENTER);
        }

        if (aid(CcardAid.PFK03)) {
            if (Cob.isLowValues(cdemo.fromTranid) || Cob.isSpaces(cdemo.fromTranid)
                    || Cob.eq(cdemo.fromTranid, LIT_THISTRANID)) {
                cdemo.toTranid = LIT_ADMINTRANID;
            } else {
                cdemo.toTranid = cdemo.fromTranid;
            }
            if (Cob.isLowValues(cdemo.fromProgram) || Cob.isSpaces(cdemo.fromProgram) || fromThisProgram()) {
                cdemo.toProgram = LIT_ADMINPGM;
            } else {
                cdemo.toProgram = cdemo.fromProgram;
            }
            cdemo.fromTranid = LIT_THISTRANID;
            cdemo.fromProgram = LIT_THISPGM;
            cdemo.setUsrtypAdmin();
            cdemo.setPgmEnter();
            cdemo.lastMapset = LIT_THISMAPSET;
            cdemo.lastMap = LIT_THISMAP;
            cics.syncpoint();
            throw new ProgramExit(new CicsResponse.Xctl(cdemo.toProgram, cdemo.copy()));
        }

        if (aid(CcardAid.PFK02) && fromThisProgram()) {
            cdemo.fromTranid = LIT_THISTRANID;
            cdemo.fromProgram = LIT_THISPGM;
            cdemo.setUsrtypUser();
            cdemo.setPgmEnter();
            cdemo.lastMapset = LIT_THISMAPSET;
            cdemo.lastMap = LIT_THISMAP;
            cdemo.toProgram = LIT_ADDTPGM;
            work.ccardNextMapset = LIT_ADDTMAPSET;
            work.ccardNextMap = LIT_ADDTMAP;
            ws.setReturnMsg(EXIT_MESSAGE);
            cdemo.setPgmEnter();
            throw new ProgramExit(new CicsResponse.Xctl(LIT_ADDTPGM, cdemo.copy()));
        }

        if (!aid(CcardAid.PFK08)) {
            ca.setLastPageNotShown();
        }

        if (aid(CcardAid.PFK10)) {
            if ((ca.isDeleteRequested() || ca.isUpdateRequested())
                    && ws.typefilterChanged == Cob.LOW_VALUE
                    && ws.descfilterChanged == Cob.LOW_VALUE
                    && ws.rowSelectionChanged == Cob.LOW_VALUE) {
                // CONTINUE
            } else {
                work.set(CcardAid.ENTER);
            }
        }

        primingQuery9998();
        if (ws.db2.isDb2Error()) {
            sendLongText();
        }

        if (ws.isInputError()) {
            inputErrorTargets();
            ws.startKey = ca.caFirstTrCode;
            if (ws.editTypeFlag != '0' && ws.editDescFlag != '0') {
                readForward8000();
            }
            sendMap2000();
            commonReturn();
        } else if (aid(CcardAid.PFK07) && ca.isFirstPage()) {
            ws.startKey = ca.caFirstTrCode;
            readForward8000();
            sendMap2000();
            commonReturn();
        } else if (aid(CcardAid.PFK03) || (cdemo.isPgmReenter() && !fromThisProgram())) {
            cdemo.initialize();
            ca.initialize();
            ws.initialize();
            cdemo.fromTranid = LIT_THISTRANID;
            cdemo.fromProgram = LIT_THISPGM;
            cdemo.lastMap = LIT_THISMAP;
            cdemo.lastMapset = LIT_THISMAPSET;
            cdemo.setUsrtypAdmin();
            cdemo.setPgmEnter();
            ca.setFirstPage();
            ca.setLastPageNotShown();
            ws.startKey = ca.caFirstTrCode;
            readForward8000();
            sendMap2000();
            commonReturn();
        } else if (aid(CcardAid.PFK08) && ca.isNextPageExists()) {
            ws.startKey = ca.caLastTrCode;
            ca.caScreenNum = Cob.truncUnsigned(ca.caScreenNum + 1L, 1);
            readForward8000();
            java.util.Arrays.fill(ws.editSelect, ' ');
            sendMap2000();
            commonReturn();
        } else if (aid(CcardAid.PFK07) && !ca.isFirstPage()) {
            ws.startKey = ca.caFirstTrCode;
            ca.caScreenNum = Cob.truncUnsigned(ca.caScreenNum - 1L, 1);
            readBackwards8100();
            java.util.Arrays.fill(ws.editSelect, ' ');
            sendMap2000();
            commonReturn();
        } else if (aid(CcardAid.ENTER) && ws.deletesRequested > 0 && fromThisProgram()) {
            ws.startKey = ca.caFirstTrCode;
            if (ws.editTypeFlag != '0' && ws.editDescFlag != '0') {
                readForward8000();
            }
            sendMap2000();
            commonReturn();
        } else if (aid(CcardAid.PFK10) && ws.deletesRequested > 0 && fromThisProgram()) {
            deleteRecord9300();
            ws.deleteStatus = ca.isDeleteSucceeded() ? 'Y' : Cob.LOW_VALUE;
            sendMap2000();
            if (ws.deleteStatus == 'Y') {
                cdemo.initialize();
                ca.initialize();
                ws.initialize();
                cdemo.setPgmEnter();
                ca.setFirstPage();
                ca.setLastPageNotShown();
            }
            commonReturn();
        } else if (aid(CcardAid.ENTER) && ws.updatesRequested > 0 && fromThisProgram()) {
            ws.startKey = ca.caFirstTrCode;
            if (ws.editTypeFlag != '0' && ws.editDescFlag != '0') {
                readForward8000();
            }
            sendMap2000();
            commonReturn();
        } else if (aid(CcardAid.PFK10) && ws.updatesRequested > 0 && fromThisProgram()) {
            updateRecord9200();
            if (ca.isUpdateSucceeded()) {
                ws.updateStatus = 'Y';
            }
            ws.startKey = ca.caFirstTrCode;
            readForward8000();
            sendMap2000();
            // no GO TO: falls out of the EVALUATE
        } else {
            ws.startKey = ca.caFirstTrCode;
            readForward8000();
            sendMap2000();
            commonReturn();
        }

        if (ws.isInputError()) {
            inputErrorTargets();
            commonReturn();
        }
        work.ccardNextProg = LIT_THISPGM;
        commonReturn();
    }

    /** The MOVEs shared by the INPUT-ERROR branches of 0000-MAIN. */
    private void inputErrorTargets() {
        work.ccardErrorMsg = ws.returnMsg;
        cdemo.fromProgram = LIT_THISPGM;
        cdemo.lastMapset = LIT_THISMAPSET;
        cdemo.lastMap = LIT_THISMAP;
        work.ccardNextProg = LIT_THISPGM;
        work.ccardNextMapset = LIT_THISMAPSET;
        work.ccardNextMap = LIT_THISMAP;
    }

    // COMMON-RETURN
    private void commonReturn() {
        cdemo.fromTranid = LIT_THISTRANID;
        cdemo.fromProgram = LIT_THISPGM;
        cdemo.lastMapset = LIT_THISMAPSET;
        cdemo.lastMap = LIT_THISMAP;
        ProgramCommarea out = new ProgramCommarea(cdemo.copy(), ca.copy());
        throw new ProgramExit(new CicsResponse.SendMapAndReturn(sentMap, LIT_THISMAP, LIT_THISMAPSET,
                LIT_THISTRANID, out));
    }

    // 1000-RECEIVE-MAP
    private void receiveMap1000(CotrtliMap terminal) {
        receiveScreen1100(terminal);
        editInputs1200();
    }

    // 1100-RECEIVE-SCREEN
    private void receiveScreen1100(CotrtliMap terminal) {
        map.receiveFrom(terminal == null ? new CotrtliMap() : terminal);
        ws.inTypeCd = map.trtype.data();
        ws.inTypeDesc = map.trdesc.data();
        for (int i = 1; i <= WS_MAX_SCREEN_LINES; i++) {
            CotrtliMap.Row row = map.row(i);
            ws.editSelect[i - 1] = row.trtsel.data().charAt(0);
            ws.rowTrCodeIn[i - 1] = row.trttyp.data();
            ws.rowTrDescIn[i - 1] = Cob.lowValues(50);
            String typed = row.trtypd.data();
            if (Cob.eq(typed, LIT_ASTERISK) || Cob.isSpaces(typed)) {
                continue;
            }
            ws.rowTrDescIn[i - 1] = Cob.move(Cob.trim(typed), 50);
        }
    }

    // 1200-EDIT-INPUTS
    private void editInputs1200() {
        ws.inputFlag = '0';
        ws.protectSelectRows = '0';
        editArray1210();
        editDesc1230();
        editTypecd1220();
        crossEdits1290();
    }

    // 1210-EDIT-ARRAY
    private void editArray1210() {
        ws.actionsRequested = 0;
        ws.noActionsSelected = 0;
        ws.deletesRequested = 0;
        ws.updatesRequested = 0;
        ws.validActionsSelected = 0;
        if (ws.typefilterChanged == 'Y' || ws.descfilterChanged == 'Y') {
            java.util.Arrays.fill(ws.editSelect, ' ');
            return;
        }
        String flags = new String(ws.editSelect);
        ws.noActionsSelected = Cob.tally(flags, ' ', Cob.LOW_VALUE);
        ws.deletesRequested = Cob.tally(flags, LIT_DELETE_FLAG);
        ws.updatesRequested = Cob.tally(flags, LIT_UPDATE_FLAG);
        ws.actionsRequested = Cob.truncSigned(WS_MAX_SCREEN_LINES - ws.noActionsSelected, 4);
        ws.validActionsSelected = Cob.truncSigned(ws.deletesRequested + ws.updatesRequested, 4);
        ws.iSelected = 0;
        ws.badSelectionAction = Cob.LOW_VALUE;
        for (ws.i = WS_MAX_SCREEN_LINES; ws.i != 0; ws.i--) {
            char sel = ws.editSelect[ws.i - 1];
            if (sel == 'D' || sel == 'U') {
                ws.iSelected = ws.i;
                if (ws.isMoreThan1Action()) {
                    ws.rowTrtselectError[ws.i - 1] = '1';
                    ws.badSelectionAction = 'Y';
                }
                if (sel == 'U') {
                    editArrayDesc1211();
                }
            } else if (sel == ' ' || sel == Cob.LOW_VALUE) {
                // CONTINUE
            } else {
                ws.inputFlag = '1';
                ws.rowTrtselectError[ws.i - 1] = '1';
                ws.badSelectionAction = 'Y';
                ws.setReturnMsg(MESG_INVALID_ACTION_CODE);
            }
        }
        if (ws.iSelected == ca.caRowSelected) {
            ws.rowSelectionChanged = Cob.LOW_VALUE;
        } else {
            ws.rowSelectionChanged = 'Y';
            ca.caRowSelected = ws.iSelected;
        }
        if (ws.isMoreThan1Action()) {
            ws.inputFlag = '1';
            ws.setReturnMsg(MESG_MORE_THAN_1_ACTION);
        }
    }

    // 1211-EDIT-ARRAY-DESC
    private void editArrayDesc1211() {
        int idx = ws.i - 1;
        ws.datachangedFlag = '0';
        String in = upperCase(Cob.trim(ws.rowTrDescIn[idx]));
        String db = upperCase(Cob.trim(ca.caRowTrDescOut[idx]));
        if (in.equals(db)) {
            ws.setReturnMsg(MESG_NO_CHANGES_DETECTED);
            return;
        }
        ws.datachangedFlag = '1';
        ws.arrayDescriptionFlgs = '0';
        ws.editVariableName = Cob.move("Transaction Desc", 25);
        ws.editAlphanumOnly = Cob.move(ws.rowTrDescIn[idx], 256);
        ws.editAlphanumLength = 50;
        editAlphanumReqd1240();
        ws.arrayDescriptionFlgs = ws.editAlphanumOnlyFlags;
    }

    /** FUNCTION UPPER-CASE: only a-z are folded. */
    private static String upperCase(String s) {
        StringBuilder sb = new StringBuilder(s.length());
        for (char c : s.toCharArray()) {
            sb.append(c >= 'a' && c <= 'z' ? (char) (c - 32) : c);
        }
        return sb.toString();
    }

    // 1220-EDIT-TYPECD (+ 1220-EDIT-TYPECD-EXIT, which carries logic)
    private void editTypecd1220() {
        ws.editTypeFlag = ' ';
        if (Cob.isLowValues(ws.inTypeCd) || Cob.isSpaces(ws.inTypeCd) || ws.inTypeCd.equals("00")) {
            ws.editTypeFlag = ' ';
            ws.typeCdFilter = "00";
        } else if (!Cob.isNumeric(ws.inTypeCd)) {
            ws.inputFlag = '1';
            ws.editTypeFlag = '0';
            ws.protectSelectRows = '1';
            ws.setReturnMsg("TYPE CODE FILTER,IF SUPPLIED MUST BE A 2 DIGIT NUMBER");
        } else {
            ws.typeCdFilter = ws.inTypeCd;
            ws.editTypeFlag = '1';
        }
        // 1220-EDIT-TYPECD-EXIT
        if (Cob.eq(ws.inTypeCd, ca.caTypeCd)
                || (ws.editTypeFlag == ' '
                        && (ca.caTypeCd.equals("00") || Cob.isLowValues(ca.caTypeCd) || Cob.isSpaces(ca.caTypeCd)))) {
            ws.typefilterChanged = Cob.LOW_VALUE;
        } else {
            ca.initializePagingVariables();
            ca.caTypeCd = ws.inTypeCd;
            ws.typefilterChanged = 'Y';
        }
    }

    // 1230-EDIT-DESC (+ 1230-EDIT-DESC-EXIT, which carries logic)
    private void editDesc1230() {
        ws.editDescFlag = ' ';
        if (Cob.isLowValues(ws.inTypeDesc) || Cob.isSpaces(ws.inTypeDesc)) {
            ws.editDescFlag = ' ';
        } else {
            ws.editDescFlag = '1';
            ws.typeDescFilter = Cob.stringInto(ws.typeDescFilter, "%", Cob.trim(ws.inTypeDesc), "%");
        }
        // 1230-EDIT-DESC-EXIT
        if (Cob.eq(ws.inTypeDesc, ca.caTypeDesc)
                || (ws.editDescFlag == ' ' && (Cob.isLowValues(ca.caTypeDesc) || Cob.isSpaces(ca.caTypeDesc)))) {
            ws.descfilterChanged = Cob.LOW_VALUE;
        } else {
            ca.initializePagingVariables();
            ca.caTypeDesc = ws.inTypeDesc;
            ws.descfilterChanged = 'Y';
        }
    }

    // 1240-EDIT-ALPHANUM-REQD
    private void editAlphanumReqd1240() {
        ws.editAlphanumOnlyFlags = '0';
        int len = ws.editAlphanumLength;
        String field = ws.editAlphanumOnly.substring(0, len);
        if (Cob.isLowValues(field) || Cob.isSpaces(field) || Cob.trim(field).isEmpty()) {
            ws.inputFlag = '1';
            ws.editAlphanumOnlyFlags = 'B';
            if (ws.isReturnMsgOff()) {
                ws.returnMsg = Cob.stringInto(ws.returnMsg, Cob.trim(ws.editVariableName), " must be supplied.");
            }
            return;
        }
        String converted = Cob.convert(field, LIT_ALL_ALPHANUM_FROM_X, LIT_ALPHANUM_SPACES_TO);
        ws.editAlphanumOnly = converted + ws.editAlphanumOnly.substring(len);
        if (Cob.trim(converted).isEmpty()) {
            // CONTINUE
        } else {
            ws.inputFlag = '1';
            ws.editAlphanumOnlyFlags = '0';
            if (ws.isReturnMsgOff()) {
                ws.returnMsg = Cob.stringInto(ws.returnMsg, Cob.trim(ws.editVariableName),
                        " can have numbers or alphabets only.");
            }
            return;
        }
        ws.editAlphanumOnlyFlags = Cob.LOW_VALUE;
    }

    // 1290-CROSS-EDITS
    private void crossEdits1290() {
        if (!(ws.editTypeFlag == '1' || ws.editDescFlag == '1')) {
            return;
        }
        checkFilters9100();
        if (ws.recordsCount == 0) {
            ws.inputFlag = '1';
            if (ws.editTypeFlag == '1') {
                ws.editTypeFlag = '0';
            }
            if (ws.editDescFlag == '1') {
                ws.editDescFlag = '0';
            }
            ws.protectSelectRows = '1';
            ws.setReturnMsg("No Records found for these filter conditions");
        }
    }

    // 2000-SEND-MAP
    private void sendMap2000() {
        screenInit2100();
        setupArrayAttribs2200();
        screenArrayInit2300();
        setupScreenAttrs2400();
        setupMessage2500();
        sendScreen2600();
    }

    // 2100-SCREEN-INIT
    private void screenInit2100() {
        map.clear();
        dateTime.setCurrentDate(now);
        map.title01.setData(ScreenTitles.CCDA_TITLE01);
        map.title02.setData(ScreenTitles.CCDA_TITLE02);
        map.trnname.setData(LIT_THISTRANID);
        map.pgmname.setData(LIT_THISPGM);
        dateTime.setCurrentDate(now);
        dateTime.curdateMm = dateTime.curdateMonth;
        dateTime.curdateDd = dateTime.curdateDay;
        dateTime.curdateYy = Integer.parseInt(Cob.zeroPad(dateTime.curdateYear, 4).substring(2, 4));
        map.curdate.setData(dateTime.curdateMmDdYy());
        dateTime.curtimeHh = dateTime.curtimeHours;
        dateTime.curtimeMm = dateTime.curtimeMinute;
        dateTime.curtimeSs = dateTime.curtimeSecond;
        map.curtime.setData(dateTime.curtimeHhMmSs());
        map.pageno.setData(Cob.zeroPad(ca.caScreenNum, 1));
        ws.infoMsg = Cob.spaces(45);
        map.infomsg.setData(ws.infoMsg);
        map.infomsg.color = Dfhbmsca.DFHBMDAR;
    }

    // 2200-SETUP-ARRAY-ATTRIBS
    private void setupArrayAttribs2200() {
        for (int i = WS_MAX_SCREEN_LINES; i != 0; i--) {
            CotrtliMap.Row row = map.row(i);
            char sel = ws.editSelect[i - 1];
            row.trtypd.attribute = Dfhbmsca.DFHBMPRF;
            if (ca.eachRowOutIsLowValues(i) || ws.protectSelectRows == '1') {
                row.trtsel.attribute = Dfhbmsca.DFHBMPRO;
            } else {
                if (ws.rowTrtselectError[i - 1] == '1') {
                    row.trtsel.color = Dfhbmsca.DFHRED;
                    row.trtsel.length = -1;
                }
                if (sel == 'D' && ws.isOnly1ValidAction() && ws.isBadActionsSelectedNo()) {
                    row.trttyp.color = Dfhbmsca.DFHNEUTR;
                    row.trtypd.color = Dfhbmsca.DFHNEUTR;
                    row.trtsel.length = -1;
                }
                if (sel == 'U' && ws.isOnly1ValidAction() && ws.isBadActionsSelectedNo()) {
                    row.trttyp.color = Dfhbmsca.DFHNEUTR;
                    if (ws.updateStatus == 'Y') {
                        row.trtsel.length = -1;
                        row.trtypd.color = Dfhbmsca.DFHNEUTR;
                    } else {
                        row.trtypd.length = -1;
                        row.trtypd.attribute = Dfhbmsca.DFHBMFSE;
                        if (!ws.isRowDescriptionIsValid()) {
                            row.trtypd.color = Dfhbmsca.DFHRED;
                        }
                    }
                }
                row.trtsel.attribute = Dfhbmsca.DFHBMFSE;
            }
        }
    }

    // 2300-SCREEN-ARRAY-INIT
    private void screenArrayInit2300() {
        for (int i = 1; i <= WS_MAX_SCREEN_LINES; i++) {
            if (ca.eachRowOutIsLowValues(i)) {
                continue;
            }
            CotrtliMap.Row row = map.row(i);
            if (ws.editSelect[i - 1] == 'D' && ws.isOnly1ValidAction() && ws.isBadActionsSelectedNo()) {
                if (ws.deleteStatus == 'Y') {
                    ws.editSelect[i - 1] = ' ';
                } else {
                    ca.caDeleteFlag = 'Y';
                }
            }
            row.trttyp.setData(ca.caRowTrCodeOut[i - 1]);
            if (ws.editSelect[i - 1] == 'U' && ws.isOnly1ValidAction() && ws.isBadActionsSelectedNo()) {
                if (ws.updateStatus == 'Y') {
                    ws.editSelect[i - 1] = ' ';
                } else {
                    ca.caUpdateFlag = 'Y';
                }
                if (ws.datachangedFlag == '1') {
                    if (ws.arrayDescriptionFlgs == 'B') {
                        row.trtypd.setData(LIT_ASTERISK);
                    } else {
                        row.trtypd.setData(ws.rowTrDescIn[i - 1]);
                    }
                } else {
                    row.trtypd.setData(ca.caRowTrDescOut[i - 1]);
                }
            } else {
                row.trtypd.setData(ca.caRowTrDescOut[i - 1]);
            }
            row.trtsel.setData(String.valueOf(ws.editSelect[i - 1]));
        }
    }

    // 2400-SETUP-SCREEN-ATTRS
    private void setupScreenAttrs2400() {
        if (eibcalen == 0 || (cdemo.isPgmEnter() && Cob.eq(cdemo.fromProgram, LIT_ADMINPGM))) {
            // CONTINUE
        } else {
            if (ws.actionsRequested > 0) {
                map.trtype.setData(ws.inTypeCd);
                map.trtype.attribute = Dfhbmsca.DFHBMASF;
                map.trtype.color = Dfhbmsca.DFHBLUE;
            } else if (ws.editTypeFlag == '1' || ws.editTypeFlag == '0') {
                map.trtype.setData(ws.inTypeCd);
                map.trtype.attribute = Dfhbmsca.DFHBMFSE;
            } else if (Cob.eq(ws.inTypeCd, "0")) {
                map.trtype.setRaw(Cob.lowValues(2));
            } else {
                map.trtype.setRaw(Cob.lowValues(2));
                map.trtype.attribute = Dfhbmsca.DFHBMFSE;
            }
            if (ws.actionsRequested > 0) {
                map.trdesc.setData(ws.inTypeDesc);
                map.trdesc.attribute = Dfhbmsca.DFHBMASF;
                map.trdesc.color = Dfhbmsca.DFHBLUE;
            } else if (ws.editDescFlag == '1' || ws.editDescFlag == '0') {
                map.trdesc.setData(ws.inTypeDesc);
                map.trdesc.attribute = Dfhbmsca.DFHBMFSE;
            } else {
                map.trdesc.attribute = Dfhbmsca.DFHBMFSE;
            }
        }
        if (ws.editTypeFlag == '0') {
            map.trtype.color = Dfhbmsca.DFHRED;
            map.trtype.length = -1;
        }
        if (ws.editDescFlag == '0') {
            map.trdesc.color = Dfhbmsca.DFHRED;
            map.trdesc.length = -1;
        }
        if (ws.isInputOk()) {
            if (ws.actionsRequested > 0 && !aid(CcardAid.PFK07) && !aid(CcardAid.PFK08)) {
                // CONTINUE
            } else {
                map.trtype.length = -1;
            }
        }
    }

    // 2500-SETUP-MESSAGE
    private void setupMessage2500() {
        if (ws.deleteStatus == 'Y') {
            ws.setInfoMsg(INFORM_DELETE_SUCCESS);
        } else if (ws.updateStatus == 'Y') {
            ws.setInfoMsg(INFORM_UPDATE_SUCCESS);
        } else if (ws.editTypeFlag == '0' || ws.editDescFlag == '0') {
            // CONTINUE
        } else if (aid(CcardAid.ENTER) && ws.deletesRequested > 0 && ws.isOnly1Action() && ws.isOnly1ValidAction()) {
            if (ws.isNoInfoMessage() && ws.typefilterChanged == Cob.LOW_VALUE
                    && ws.descfilterChanged == Cob.LOW_VALUE) {
                ws.setInfoMsg(INFORM_DELETE);
            }
        } else if (aid(CcardAid.ENTER) && ws.updatesRequested > 0 && ws.isOnly1Action() && ws.isOnly1ValidAction()) {
            if (ws.isNoInfoMessage() && ws.typefilterChanged == Cob.LOW_VALUE
                    && ws.descfilterChanged == Cob.LOW_VALUE) {
                ws.setInfoMsg(INFORM_UPDATE);
            }
        } else if (aid(CcardAid.PFK07) && ca.isFirstPage()) {
            ws.setReturnMsg("No previous pages to display");
        } else if (aid(CcardAid.PFK08) && ca.isNextPageNotExists() && ca.isLastPageShown()) {
            ws.setReturnMsg("No more pages to display");
        } else if (aid(CcardAid.PFK08) && ca.isNextPageNotExists()) {
            if (ws.isNoInfoMessage()) {
                ws.setInfoMsg(INFORM_REC_ACTIONS);
            }
            if (ca.isLastPageNotShown() && ca.isNextPageNotExists()) {
                ca.setLastPageShown();
            }
        } else if (ws.isNoInfoMessage() || ca.isNextPageExists()) {
            ws.setInfoMsg(INFORM_REC_ACTIONS);
        } else {
            ws.infoMsg = Cob.spaces(45);
        }
        map.errmsg.setData(ws.returnMsg);
        ws.stringLen = Cob.truncUnsigned(Cob.trim(ws.infoMsg).length(), 3);
        ws.stringMid = Cob.truncUnsigned((ws.infoMsg.length() - ws.stringLen) / 2 + 1, 3);
        if (ws.stringLen > 0) {
            ws.stringOut = Cob.overlay(ws.stringOut, ws.stringMid - 1, ws.infoMsg.substring(0, ws.stringLen));
        }
        if (!ws.isNoInfoMessage() && !ws.returnMsg.equals(Cob.move(MESG_NO_RECORDS_FOUND, 75))) {
            map.infomsg.setData(ws.stringOut);
            map.infomsg.color = Dfhbmsca.DFHNEUTR;
        }
    }

    // 2600-SEND-SCREEN
    private void sendScreen2600() {
        sentMap = map.copy();
    }

    private TypeFilter filter() {
        return new TypeFilter(ws.startKey, ws.editTypeFlag, ws.typeCdFilter, ws.editDescFlag, ws.typeDescFilter);
    }

    private <T> T sql(SqlResult<T> r) {
        sqlca.set(r.sqlcode(), r.sqlerrmc(), null);
        ws.db2.setDispSqlcode(r.sqlcode());
        return r.value();
    }

    // 8000-READ-FORWARD
    private void readForward8000() {
        ca.clearAllRowsOut();
        openForwardCursor9400();
        if (ws.db2.isDb2Error()) {
            return;
        }
        ws.rowNumber = 0;
        ca.caNextPageInd = 'Y';
        ws.recordsToProcessFlag = '1';
        while (ws.recordsToProcessFlag != '0') {
            DclTransactionType dcl = DclTransactionType.initialized();
            DclTransactionType fetched = sql(db.fetchForward());
            if (sqlca.sqlcode == 0) {
                dcl = fetched;
                ws.rowNumber++;
                ca.caRowTrCodeOut[ws.rowNumber - 1] = dcl.trType();
                ca.caRowTrDescOut[ws.rowNumber - 1] = dcl.descriptionText();
                if (ws.rowNumber == 1) {
                    ca.caFirstTrCode = dcl.trType();
                    if (ca.caScreenNum == 0) {
                        ca.caScreenNum = Cob.truncUnsigned(ca.caScreenNum + 1L, 1);
                    }
                }
                if (ws.rowNumber == WS_MAX_SCREEN_LINES) {
                    ws.recordsToProcessFlag = '0';
                    ca.caLastTrCode = dcl.trType();
                    DclTransactionType lookAhead = sql(db.fetchForward());
                    if (sqlca.sqlcode == 0) {
                        ca.caNextPageInd = 'Y';
                        ca.caLastTrCode = lookAhead.trType();
                    } else if (sqlca.sqlcode == 100) {
                        ca.caNextPageInd = Cob.LOW_VALUE;
                        if (ws.isReturnMsgOff() && aid(CcardAid.PFK08)) {
                            ws.setReturnMsg(MESG_NO_MORE_RECORDS);
                        }
                    } else {
                        ws.recordsToProcessFlag = '0';
                        if (ws.isReturnMsgOff()) {
                            ws.db2.db2CurrentAction = Cob.move("C-TR-TYPE-FORWARD fetch", 72);
                            formatDb2Message9999();
                        }
                    }
                }
            } else if (sqlca.sqlcode == 100) {
                ws.recordsToProcessFlag = '0';
                ca.caNextPageInd = Cob.LOW_VALUE;
                ca.caLastTrCode = dcl.trType();
                if (ws.isReturnMsgOff() && aid(CcardAid.PFK08)) {
                    ws.setReturnMsg(MESG_NO_MORE_RECORDS);
                }
                if (ca.caScreenNum == 1 && ws.rowNumber == 0) {
                    ws.setReturnMsg(MESG_NO_RECORDS_FOUND);
                }
            } else {
                ws.recordsToProcessFlag = '0';
                ws.db2.setDb2Error();
                if (ws.isReturnMsgOff()) {
                    ws.db2.db2CurrentAction = Cob.move("C-TR-TYPE-FORWARD close", 72);
                    formatDb2Message9999();
                }
            }
        }
        closeForwardCursor9450();
    }

    // 8100-READ-BACKWARDS
    private void readBackwards8100() {
        ca.clearAllRowsOut();
        ca.caLastTrCode = ca.caFirstTrCode;
        ws.rowNumber = WS_MAX_SCREEN_LINES;
        ca.caNextPageInd = 'Y';
        ws.recordsToProcessFlag = '1';
        openBackwardCursor9500();
        while (ws.recordsToProcessFlag != '0') {
            DclTransactionType fetched = sql(db.fetchBackward());
            if (sqlca.sqlcode == 0) {
                ca.caRowTrCodeOut[ws.rowNumber - 1] = fetched.trType();
                ca.caRowTrDescOut[ws.rowNumber - 1] = fetched.descriptionText();
                ws.rowNumber--;
                if (ws.rowNumber == 0) {
                    ws.recordsToProcessFlag = '0';
                    ca.caFirstTrCode = fetched.trType();
                }
            } else {
                ws.recordsToProcessFlag = '0';
                ws.db2.setDb2Error();
                if (ws.isReturnMsgOff()) {
                    ws.db2.db2CurrentAction = Cob.move("Error on fetch Cursor C-TR-TYPE-BACKWARD", 72);
                    formatDb2Message9999();
                }
            }
        }
        // 8100-READ-BACKWARDS-EXIT
        closeBackCursor9550();
    }

    // 9100-CHECK-FILTERS
    private void checkFilters9100() {
        Integer count = sql(db.countByFilters(filter()));
        if (sqlca.sqlcode == 0) {
            ws.recordsCount = Cob.truncSigned(count, 4);
        } else {
            ws.inputFlag = '1';
            if (ws.isReturnMsgOff()) {
                ws.db2.db2CurrentAction = Cob.move("Error reading TRANSACTION_TYPE table ", 72);
                formatDb2Message9999();
            }
        }
    }

    // 9200-UPDATE-RECORD
    private void updateRecord9200() {
        int idx = ws.iSelected - 1;
        String text = Cob.move(Cob.trim(ws.rowTrDescIn[idx]), 50);
        int len = ws.rowTrDescIn[idx].length();
        DclTransactionType host = new DclTransactionType(ws.rowTrCodeIn[idx], len, text);
        sql(db.updateDescription(host));
        int code = sqlca.sqlcode;
        if (code == 0) {
            cics.syncpoint();
            ca.caUpdateFlag = Cob.LOW_VALUE;
            if (ws.isNoInfoMessage()) {
                ws.setInfoMsg(INFORM_UPDATE_SUCCESS);
            }
        } else if (code == 100) {
            ca.caUpdateFlag = 'Y';
            if (ws.isReturnMsgOff()) {
                ws.db2.db2CurrentAction = Cob.move("Record not found. Deleted by others ? ", 72);
                formatDb2Message9999();
            }
        } else if (code == -911) {
            ca.caUpdateFlag = 'Y';
            ws.inputFlag = '1';
            if (ws.isReturnMsgOff()) {
                ws.db2.db2CurrentAction = Cob.move("Deadlock. Someone else updating ?", 72);
                formatDb2Message9999();
            }
        } else if (code < 0) {
            ca.caUpdateFlag = 'Y';
            if (ws.isReturnMsgOff()) {
                ws.db2.db2CurrentAction = Cob.move("Update failed with", 72);
                formatDb2Message9999();
            }
        }
    }

    // 9300-DELETE-RECORD
    private void deleteRecord9300() {
        sql(db.deleteByType(ws.rowTrCodeIn[ws.iSelected - 1]));
        int code = sqlca.sqlcode;
        if (code == 0) {
            cics.syncpoint();
            ca.caDeleteFlag = Cob.LOW_VALUE;
            if (ws.isNoInfoMessage()) {
                ws.setInfoMsg(INFORM_DELETE_SUCCESS);
            }
        } else if (code == -532) {
            ca.caDeleteFlag = 'Y';
            if (ws.isReturnMsgOff()) {
                ws.db2.db2CurrentAction = Cob.move("Please delete associated child records first:", 72);
                formatDb2Message9999();
            }
        } else {
            if (ws.isReturnMsgOff()) {
                ws.db2.db2CurrentAction = Cob.move("Delete failed with message:", 72);
                formatDb2Message9999();
            }
        }
    }

    // 9400-OPEN-FORWARD-CURSOR
    private void openForwardCursor9400() {
        cursorStatement(db.openForwardCursor(filter()), "C-TR-TYPE-FORWARD Open");
    }

    // 9450-CLOSE-FORWARD-CURSOR
    private void closeForwardCursor9450() {
        cursorStatement(db.closeForwardCursor(), "C-TR-TYPE-FORWARD close");
    }

    // 9500-OPEN-BACKWARD-CURSOR
    private void openBackwardCursor9500() {
        cursorStatement(db.openBackwardCursor(filter()), "C-TR-TYPE-BACKWARD Open");
    }

    // 9550-CLOSE-BACK-CURSOR
    private void closeBackCursor9550() {
        cursorStatement(db.closeBackwardCursor(), "C-TR-TYPE-BACKWARD close");
    }

    private void cursorStatement(SqlResult<Void> result, String action) {
        sql(result);
        if (sqlca.sqlcode != 0) {
            ws.db2.setDb2Error();
            if (ws.isReturnMsgOff()) {
                ws.db2.db2CurrentAction = Cob.move(action, 72);
                formatDb2Message9999();
            }
        }
    }

    // 9998-PRIMING-QUERY (CSDB2RPY)
    private void primingQuery9998() {
        Integer one = sql(db.primingQuery());
        if (sqlca.sqlcode == 0) {
            ws.db2.dummyDb2Int = java.math.BigDecimal.valueOf(one == null ? 0 : one);
        } else {
            ws.db2.setDb2Error();
            ws.db2.db2CurrentAction = Cob.move("Db2 access failure. ", 72);
            formatDb2Message9999();
        }
    }

    // 9999-FORMAT-DB2-MESSAGE (CSDB2RPY)
    private void formatDb2Message9999() {
        ws.db2.dsntiacLrecl = 72;
        Dsntiac.Result r = dsntiac.format(sqlca, ws.db2.dsntiacMesgLen, ws.db2.dsntiacLrecl);
        if (r.text() != null) {
            ws.db2.dsntiacFmtdText = Cob.move(r.text(), ws.db2.dsntiacFmtdText.length());
        }
        ws.db2.setDsntiacErrCd(r.returnCode());
        if (Cob.truncUnsigned(r.returnCode(), 2) == 0) {
            // CONTINUE
        } else {
            ws.db2.dsntiacErrMsg = Cob.move("DSNTIAC CD: ", 10);
            ws.db2.dsntiacFmtdText = Cob.move(ws.db2.dsntiacError(), ws.db2.dsntiacFmtdText.length());
        }
        ws.longMsg = Cob.stringInto(ws.longMsg, Cob.trim(ws.db2.db2CurrentAction), " SQLCODE:",
                ws.db2.dispSqlcode, " ", ws.db2.dsntiacFmtdText);
        ws.returnMsg = Cob.move(ws.longMsg, 75);
    }

    // SEND-PLAIN-TEXT (present in the source, never PERFORMed)
    @SuppressWarnings("unused")
    private void sendPlainText() {
        throw new ProgramExit(new CicsResponse.SendTextAndReturn(ws.returnMsg));
    }

    // SEND-LONG-TEXT
    private void sendLongText() {
        throw new ProgramExit(new CicsResponse.SendTextAndReturn(ws.longMsg));
    }
}
