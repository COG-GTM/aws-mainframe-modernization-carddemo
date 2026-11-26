package com.carddemo.programs.cotrtupc;

import com.carddemo.programs.cotrtupc.WsMiscStorage.InfoMsg;
import com.carddemo.programs.cotrtupc.WsMiscStorage.ReturnMsg;
import com.carddemo.programs.cotrtupc.copybook.AbendData;
import com.carddemo.programs.cotrtupc.copybook.CarddemoCommarea;
import com.carddemo.programs.cotrtupc.copybook.CcWorkArea;
import com.carddemo.programs.cotrtupc.copybook.CcWorkArea.CcardAid;
import com.carddemo.programs.cotrtupc.copybook.CtrtupaMap;
import com.carddemo.programs.cotrtupc.copybook.DateTimeArea;
import com.carddemo.programs.cotrtupc.copybook.DclTransactionType;
import com.carddemo.programs.cotrtupc.copybook.Dfhaid;
import com.carddemo.programs.cotrtupc.copybook.Dfhbmsca;
import com.carddemo.programs.cotrtupc.copybook.ScreenTitles;
import com.carddemo.programs.cotrtupc.copybook.Sqlca;
import com.carddemo.programs.cotrtupc.gateway.CicsSyncpoint;
import com.carddemo.programs.cotrtupc.gateway.TransactionTypeGateway;
import com.carddemo.programs.cotrtupc.support.CobolText;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.Objects;

import static com.carddemo.programs.cotrtupc.support.CobolText.eq;
import static com.carddemo.programs.cotrtupc.support.CobolText.isLowValues;
import static com.carddemo.programs.cotrtupc.support.CobolText.isSpaces;
import static com.carddemo.programs.cotrtupc.support.CobolText.trim;
import static com.carddemo.programs.cotrtupc.support.CobolText.upper;

/**
 * COTRTUPC - Transaction Type maintenance (CICS transaction CTTU, map CTRTUPA of mapset COTRTUP,
 * DB2 table CARDDEMO.TRANSACTION_TYPE). Business logic of the COBOL program as a plain service: one
 * {@link #execute} call is one pseudo-conversational task.
 */
public final class Cotrtupc {

    public static final String LIT_THISPGM = "COTRTUPC";
    public static final String LIT_THISTRANID = "CTTU";
    public static final String LIT_THISMAPSET = "COTRTUP ";
    public static final String LIT_THISMAP = "CTRTUPA";
    public static final String LIT_ADMINPGM = "COADM01C";
    public static final String LIT_ADMINTRANID = "CA00";
    public static final String LIT_ADMINMAPSET = "COADM01";
    public static final String LIT_ADMINMAP = "COADM1A";
    public static final String LIT_LISTTPGM = "COTRTLIC";
    public static final String LIT_LISTTTRANID = "CTLI";
    public static final String LIT_LISTTMAPSET = "COTRTLI";
    public static final String LIT_LISTTMAP = "CTRTLIA";
    static final String LIT_ALL_ALPHANUM_FROM_X =
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

    /** Length of WS-COMMAREA passed on EXEC CICS RETURN. */
    public static final int WS_COMMAREA_LENGTH = 2000;

    private final TransactionTypeGateway db;
    private final CicsSyncpoint cics;
    private final Clock clock;

    public Cotrtupc(TransactionTypeGateway db, CicsSyncpoint cics, Clock clock) {
        this.db = Objects.requireNonNull(db);
        this.cics = Objects.requireNonNull(cics);
        this.clock = Objects.requireNonNull(clock);
    }

    public TranTypeMaintResponse execute(TranTypeMaintRequest request) {
        return new Task(request).run();
    }

    /** Terminates the task (GO TO COMMON-RETURN / XCTL) - internal control flow only. */
    private static final class TaskEnd extends RuntimeException {
        final transient TranTypeMaintResponse response;

        TaskEnd(TranTypeMaintResponse response) {
            super(null, null, false, false);
            this.response = response;
        }
    }

    /** WORKING-STORAGE of one task plus the PROCEDURE DIVISION paragraphs. */
    private final class Task {
        private final TranTypeMaintRequest req;
        private final CcWorkArea cc = new CcWorkArea();
        private final WsMiscStorage ws = new WsMiscStorage();
        private final CarddemoCommarea carddemo = new CarddemoCommarea();
        private final TtupProgCommarea ttup = new TtupProgCommarea();
        private final CtrtupaMap map = new CtrtupaMap();
        private final DateTimeArea dt = new DateTimeArea();
        private final AbendData abend = new AbendData();
        private final DclTransactionType dcl = new DclTransactionType();
        private Sqlca sqlca = new Sqlca();
        private String wsCommarea = CobolText.lowValues(WS_COMMAREA_LENGTH);

        Task(TranTypeMaintRequest req) {
            this.req = req;
            ws.initialize();
            ttup.changeAction(CobolText.LOW_VALUE);
        }

        TranTypeMaintResponse run() {
            try {
                main0000();
                return commonReturn();
            } catch (TaskEnd end) {
                return end.response;
            } catch (CicsAbendException e) {
                cics.syncpointRollback();
                throw e;
            } catch (RuntimeException e) {
                // EXEC CICS HANDLE ABEND LABEL(ABEND-ROUTINE)
                CicsAbendException abended = abendRoutine(e);
                cics.syncpointRollback();
                throw abended;
            }
        }

        // 0000-MAIN
        private void main0000() {
            cc.initialize();
            ws.initialize();
            wsCommarea = CobolText.spaces(WS_COMMAREA_LENGTH);
            ws.wsTranid = LIT_THISTRANID;
            ws.setReturnMsgOff();

            // CARDDEMO-COMMAREA is tested before DFHCOMMAREA is moved in (fresh storage, LOW-VALUES).
            if (req.eibcalen() == 0
                    || (eq(carddemo.fromProgram(), LIT_ADMINPGM) && !carddemo.isPgmReenter())
                    || (eq(carddemo.fromProgram(), LIT_LISTTPGM) && !carddemo.isPgmReenter())) {
                carddemo.initialize();
                ttup.initialize();
                carddemo.setPgmEnter();
                ttup.setDetailsNotFetched();
            } else {
                String dfh = req.commarea();
                carddemo.fromFixed(slice(dfh, 0, CarddemoCommarea.LENGTH));
                ttup.fromFixed(slice(dfh, CarddemoCommarea.LENGTH, TtupProgCommarea.LENGTH));
            }

            yyyyStorePfkey();
            ws.setPfkInvalid();
            checkPfkeys0001();

            if ((cc.is(CcardAid.PFK12)
                    && (ttup.isShowDetails() || ttup.isCreateNewRecord() || ttup.isDetailsNotFound()))
                    || ttup.isChangesOkayedAndDone()
                    || ttup.isChangesFailed()
                    || (ttup.isChangesBackedOut()
                        && (isLowValues(ttup.oldDetails()) || isSpaces(ttup.oldDetails())))
                    || ttup.isDeleteDone()
                    || ttup.isDeleteFailed()) {
                carddemo.setPgmEnter();
                ttup.setDetailsNotFetched();
            }

            if (cc.is(CcardAid.PFK03)) {
                if (isLowValues(carddemo.fromTranid()) || isSpaces(carddemo.fromTranid())) {
                    carddemo.toTranid(LIT_ADMINTRANID);
                } else {
                    carddemo.toTranid(carddemo.fromTranid());
                }
                if (isLowValues(carddemo.fromProgram()) || isSpaces(carddemo.fromProgram())) {
                    carddemo.toProgram(LIT_ADMINPGM);
                } else {
                    carddemo.toProgram(carddemo.fromProgram());
                }
                carddemo.fromTranid(LIT_THISTRANID);
                carddemo.fromProgram(LIT_THISPGM);
                carddemo.setUsrtypAdmin();
                carddemo.setPgmEnter();
                carddemo.lastMapset(LIT_THISMAPSET);
                carddemo.lastMap(LIT_THISMAP);
                cics.syncpoint();
                throw new TaskEnd(TranTypeMaintResponse.xctl(carddemo.toProgram(), carddemo.toFixed()));
            } else if ((!carddemo.isPgmReenter() && eq(carddemo.fromProgram(), LIT_ADMINPGM))
                    || (!carddemo.isPgmReenter() && eq(carddemo.fromProgram(), LIT_LISTTPGM))
                    || (carddemo.isPgmEnter() && ttup.isDetailsNotFetched())) {
                ttup.initialize();
                ws.initialize();
                carddemo.acctId(0);
                sendMap3000();
                carddemo.setPgmReenter();
                ttup.setDetailsNotFetched();
            } else if (cc.is(CcardAid.PFK04) && ttup.isConfirmDelete()) {
                ttup.setStartDelete();
                deleteProcessing9800();
                sendMap3000();
            } else if (cc.is(CcardAid.PFK04) && ttup.isShowDetails()) {
                ttup.setConfirmDelete();
                sendMap3000();
            } else if (cc.is(CcardAid.PFK05) && ttup.isDetailsNotFound()) {
                ttup.setCreateNewRecord();
                sendMap3000();
            } else if (cc.is(CcardAid.PFK05) && ttup.isChangesOkNotConfirmed()) {
                writeProcessing9600();
                sendMap3000();
            } else if (cc.is(CcardAid.PFK12)
                    && (ttup.isChangesOkNotConfirmed() || ttup.isConfirmDelete() || ttup.isShowDetails())) {
                ws.setFoundTrantypeInTable();
                decideAction2000();
                sendMap3000();
            } else if (ws.is(ReturnMsg.WS_INVALID_KEY_PRESSED)) {
                sendMap3000();
            } else {
                processInputs1000();
                decideAction2000();
                sendMap3000();
            }
        }

        // COMMON-RETURN
        private TranTypeMaintResponse commonReturn() {
            cc.errorMsg(ws.wsReturnMsg);
            String rest = wsCommarea.substring(CarddemoCommarea.LENGTH + TtupProgCommarea.LENGTH);
            wsCommarea = carddemo.toFixed() + ttup.toFixed() + rest;
            return TranTypeMaintResponse.returned(LIT_THISTRANID, wsCommarea, map.copy());
        }

        // 0001-CHECK-PFKEYS
        private void checkPfkeys0001() {
            if (cc.is(CcardAid.PFK03)
                    || (cc.is(CcardAid.ENTER) && !ttup.isConfirmDelete())
                    || (cc.is(CcardAid.PFK04) && (ttup.isShowDetails() || ttup.isConfirmDelete()))
                    || (cc.is(CcardAid.PFK05) && (ttup.isChangesOkNotConfirmed()
                        || ttup.isDetailsNotFound() || ttup.isDeleteInProgress()))
                    || (cc.is(CcardAid.PFK12) && (ttup.isChangesOkNotConfirmed() || ttup.isShowDetails()
                        || ttup.isDetailsNotFound() || ttup.isConfirmDelete() || ttup.isCreateNewRecord()))) {
                ws.setPfkValid();
            } else {
                ws.setPfkInvalid();
                if (ws.isReturnMsgOff()) {
                    ws.setReturn(ReturnMsg.WS_INVALID_KEY_PRESSED);
                }
            }
        }

        // 1000-PROCESS-INPUTS
        private void processInputs1000() {
            receiveMap1100();
            storeMapInNew1150();
            editMapInputs1200();
            cc.errorMsg(ws.wsReturnMsg);
            cc.nextProg(LIT_THISPGM);
            cc.nextMapset(LIT_THISMAPSET);
            cc.nextMap(LIT_THISMAP);
        }

        // 1100-RECEIVE-MAP
        private void receiveMap1100() {
            boolean any = false;
            if (req.trtypcdI() != null) {
                map.trtypcd.move(req.trtypcdI());
                map.trtypcd.length = (short) Math.min(req.trtypcdI().length(), map.trtypcd.size);
                any = true;
            }
            if (req.trtydscI() != null) {
                map.trtydsc.move(req.trtydscI());
                map.trtydsc.length = (short) Math.min(req.trtydscI().length(), map.trtydsc.size);
                any = true;
            }
            ws.wsRespCd = any ? 0 : 36; // DFHRESP(MAPFAIL)
            ws.wsReasCd = 0;
        }

        // 1150-STORE-MAP-IN-NEW
        private void storeMapInNew1150() {
            String typI = map.trtypcd.data;
            String dscI = map.trtydsc.data;
            if (ttup.isDetailsNotFound() && !cc.is(CcardAid.PFK05) && eq(trim(typI), ttup.newType())) {
                return;
            }
            ttup.initializeNewDetails();
            if (eq(typI, "*") || isSpaces(typI)) {
                ttup.newTypeLowValues();
            } else {
                ttup.newType(trim(typI));
            }
            if (eq(dscI, "*") || isSpaces(dscI)) {
                ttup.newDescLowValues();
            } else {
                ttup.newDesc(trim(dscI));
            }
        }

        // 1200-EDIT-MAP-INPUTS
        private void editMapInputs1200() {
            ws.setInputOk();
            if (ttup.isDetailsNotFound() && eq(trim(map.trtypcd.data), ttup.newType())) {
                if (!cc.is(CcardAid.PFK05)) {
                    ttup.setDetailsNotFetched();
                }
                ws.setTranfilterIsValid();
                return;
            }
            if (!(ttup.isCreateNewRecord() || ttup.isChangesOkNotConfirmed())) {
                editTrantype1210();
                if (ws.isTranfilterBlank()) {
                    if (ws.isReturnMsgOff()) {
                        ws.setReturn(ReturnMsg.NO_SEARCH_CRITERIA_RECEIVED);
                    }
                    ttup.setDetailsNotFetched();
                    return;
                }
                if (ws.isTranfilterNotOk()) {
                    ttup.setInvalidSearchKeys();
                    ttup.setDetailsNotFetched();
                    return;
                }
                if (ttup.isDetailsNotFetched()) {
                    return;
                }
            }
            ws.setTranfilterIsValid();
            compareOldNew1205();
            if (ws.isNoChangesFound() || ttup.isChangesOkNotConfirmed() || ttup.isChangesOkayedAndDone()) {
                ws.wsEditDescFlags = CobolText.LOW_VALUE;
                return;
            }
            ttup.setChangesNotOk();
            ws.wsEditVariableName = CobolText.pic("Transaction Desc", 25);
            ws.wsEditAlphanumOnly = CobolText.pic(ttup.newDesc(), 256);
            ws.wsEditAlphanumLength = 50;
            editAlphanumReqd1230();
            ws.wsEditDescFlags = ws.wsEditAlphanumOnlyFlags;
            if (!ws.isInputError()) {
                ttup.setChangesOkNotConfirmed();
            }
        }

        // 1205-COMPARE-OLD-NEW
        private void compareOldNew1205() {
            ws.setNoChangesFound();
            String newDesc = trim(ttup.newDesc());
            String oldDesc = trim(ttup.oldDesc());
            if (eq(upper(ttup.newType()), upper(ttup.oldType()))
                    && eq(upper(newDesc), upper(oldDesc))
                    && newDesc.length() == oldDesc.length()) {
                if (ws.isReturnMsgOff()) {
                    ws.setReturn(ReturnMsg.NO_CHANGES_DETECTED);
                }
            } else if (ws.isReturnMsgOff()) {
                ws.setChangeHasOccurred();
            }
        }

        // 1210-EDIT-TRANTYPE
        private void editTrantype1210() {
            ws.setTranfilterNotOk();
            ws.wsEditVariableName = CobolText.pic("Tran Type code", 25);
            ws.wsEditAlphanumOnly = CobolText.pic(ttup.newType(), 256);
            ws.wsEditAlphanumLength = 2;
            editNumReqd1245();
            ws.wsEditTtypFlag = ws.wsEditAlphanumOnlyFlags;
            if (ws.isTranfilterIsValid()) {
                ws.wsEditNumeric2 = CobolText.toUnsignedDisplay(CobolText.numval(ttup.newType()), 2);
                ws.wsEditAlphanumeric2 = ws.wsEditNumeric2.replace(' ', '0');
                ttup.newType(ws.wsEditAlphanumeric2);
            }
        }

        private String editField() {
            return ws.wsEditAlphanumOnly.substring(0, ws.wsEditAlphanumLength);
        }

        private boolean editFieldBlank() {
            String f = editField();
            return isLowValues(f) || isSpaces(f) || trim(f).isEmpty();
        }

        private void editMessage(String suffix) {
            if (ws.isReturnMsgOff()) {
                ws.wsReturnMsg = CobolText.stringInto(ws.wsReturnMsg, trim(ws.wsEditVariableName), suffix);
            }
        }

        // 1230-EDIT-ALPHANUM-REQD
        private void editAlphanumReqd1230() {
            ws.setAlphanumNotOk();
            if (editFieldBlank()) {
                ws.setInputError();
                ws.setAlphanumBlank();
                editMessage(" must be supplied.");
                return;
            }
            StringBuilder converted = new StringBuilder(editField());
            for (int i = 0; i < converted.length(); i++) {
                if (LIT_ALL_ALPHANUM_FROM_X.indexOf(converted.charAt(i)) >= 0) {
                    converted.setCharAt(i, ' ');
                }
            }
            ws.wsEditAlphanumOnly = converted + ws.wsEditAlphanumOnly.substring(ws.wsEditAlphanumLength);
            if (!trim(converted.toString()).isEmpty()) {
                ws.setInputError();
                ws.setAlphanumNotOk();
                editMessage(" can have numbers or alphabets only.");
                return;
            }
            ws.setAlphanumIsValid();
        }

        // 1245-EDIT-NUM-REQD
        private void editNumReqd1245() {
            ws.setAlphanumNotOk();
            if (editFieldBlank()) {
                ws.setInputError();
                ws.setAlphanumBlank();
                editMessage(" must be supplied.");
                return;
            }
            if (CobolText.testNumval(editField()) != 0) {
                ws.setInputError();
                ws.setAlphanumNotOk();
                editMessage(" must be numeric.");
                return;
            }
            if (CobolText.numval(editField()).compareTo(BigDecimal.ZERO) == 0) {
                ws.setInputError();
                ws.setAlphanumNotOk();
                editMessage(" must not be zero.");
                return;
            }
            ws.setAlphanumIsValid();
        }

        // 2000-DECIDE-ACTION
        private void decideAction2000() {
            if (ttup.isDetailsNotFetched() || cc.is(CcardAid.PFK12)) {
                if (ws.isTranfilterIsValid()) {
                    ws.setReturnMsgOff();
                    readTrantype9000();
                    if (ws.isFoundTrantypeInTable()) {
                        ttup.setShowDetails();
                    } else {
                        ttup.setDetailsNotFound();
                    }
                } else if (ttup.isConfirmDelete()) {
                    ws.setReturn(ReturnMsg.WS_DELETE_WAS_CANCELLED);
                    ttup.setDetailsNotFetched();
                } else if (ttup.isChangesOkNotConfirmed()) {
                    ws.setReturn(ReturnMsg.WS_UPDATE_WAS_CANCELLED);
                    ttup.setChangesBackedOut();
                } else {
                    ttup.setDetailsNotFetched();
                }
            } else if (ttup.isConfirmDelete() && cc.is(CcardAid.PFK12)) {
                ttup.setConfirmDelete();
            } else if (ttup.isShowDetails()) {
                if (!(ws.isInputError() || ws.is(ReturnMsg.NO_CHANGES_DETECTED) || ws.is(ReturnMsg.WS_INVALID_KEY))) {
                    ttup.setChangesOkNotConfirmed();
                }
            } else if (ttup.isChangesNotOk()) {
                // CONTINUE
            } else if (ttup.isChangesBackedOut()) {
                ttup.setChangesNotOk();
            } else if (ttup.isInvalidSearchKeys()) {
                // CONTINUE
            } else if (cc.is(CcardAid.PFK05) && ttup.isDetailsNotFound()) {
                ttup.setCreateNewRecord();
            } else if (ttup.isChangesOkNotConfirmed()) {
                // CONTINUE
            } else if (ttup.isChangesOkayedAndDone()) {
                ttup.setShowDetails();
                if (isLowValues(carddemo.fromTranid()) || isSpaces(carddemo.fromTranid())) {
                    carddemo.acctId(0);
                    carddemo.cardNum(0);
                    carddemo.acctStatusLowValues();
                }
            } else {
                abend.abendCulprit(LIT_THISPGM);
                abend.abendCode("0001");
                abend.abendReason("");
                abend.abendMsg("UNEXPECTED DATA SCENARIO");
                throw abendRoutine(null);
            }
        }

        // 3000-SEND-MAP
        private void sendMap3000() {
            screenInit3100();
            setupScreenVars3200();
            setupInfomsg3250();
            setupScreenAttrs3300();
            setupInfomsgAttrs3390();
            setupPfkeyAttrs3391();
            sendScreen3400();
        }

        // 3100-SCREEN-INIT
        private void screenInit3100() {
            map.lowValues();
            LocalDateTime now = LocalDateTime.now(clock);
            dt.moveCurrentDate(now);
            map.title01.move(ScreenTitles.CCDA_TITLE01);
            map.title02.move(ScreenTitles.CCDA_TITLE02);
            map.trnname.move(LIT_THISTRANID);
            map.pgmname.move(LIT_THISPGM);
            dt.moveCurrentDate(now);
            dt.wsCurdateMm = dt.wsCurdateMonth;
            dt.wsCurdateDd = dt.wsCurdateDay;
            dt.wsCurdateYy = dt.wsCurdateYear.substring(2, 4);
            map.curdate.move(dt.wsCurdateMmDdYy());
            dt.wsCurtimeHh = dt.wsCurtimeHours;
            dt.wsCurtimeMm = dt.wsCurtimeMinute;
            dt.wsCurtimeSs = dt.wsCurtimeSecond;
            map.curtime.move(dt.wsCurtimeHhMmSs());
        }

        // 3200-SETUP-SCREEN-VARS
        private void setupScreenVars3200() {
            if (carddemo.isPgmEnter()) {
                return;
            }
            if (ttup.isDetailsNotFetched()) {
                showInitialValues3201();
            } else if (ttup.isShowDetails() || ttup.isConfirmDelete() || ttup.isDeleteFailed()
                    || ttup.isDeleteDone() || ttup.isChangesBackedOut()) {
                ttup.initializeNewDetails();
                showOriginalValues3202();
            } else if (ttup.isChangesMade() || ttup.isChangesNotOk() || ttup.isDetailsNotFound()
                    || ttup.isInvalidSearchKeys() || ttup.isCreateNewRecord() || ttup.isChangesOkayedAndDone()) {
                showUpdatedValues3203();
            } else {
                ttup.initializeNewDetails();
                showOriginalValues3202();
            }
        }

        // 3201-SHOW-INITIAL-VALUES
        private void showInitialValues3201() {
            map.trtypcd.moveLowValues();
        }

        // 3202-SHOW-ORIGINAL-VALUES
        private void showOriginalValues3202() {
            ws.wsEditDescFlags = CobolText.LOW_VALUE;
            map.trtypcd.move(ttup.oldType());
            map.trtydsc.move(ttup.oldDesc());
        }

        // 3203-SHOW-UPDATED-VALUES
        private void showUpdatedValues3203() {
            map.trtypcd.move(ttup.newType());
            map.trtydsc.move(ttup.newDesc());
        }

        private boolean oldTypeBlank() {
            return isLowValues(ttup.oldType()) || isSpaces(ttup.oldType());
        }

        // 3250-SETUP-INFOMSG
        private void setupInfomsg3250() {
            if (carddemo.isPgmEnter()) {
                ws.setInfo(InfoMsg.PROMPT_FOR_SEARCH_KEYS);
            } else if (ttup.isDetailsNotFetched() || ttup.isInvalidSearchKeys()) {
                ws.setInfo(InfoMsg.PROMPT_FOR_SEARCH_KEYS);
            } else if (ttup.isDetailsNotFound()) {
                ws.setInfo(InfoMsg.PROMPT_CREATE_NEW_RECORD);
            } else if (ttup.isShowDetails() || (ttup.isChangesBackedOut() && oldTypeBlank())) {
                ws.setInfo(InfoMsg.PROMPT_FOR_SEARCH_KEYS);
            } else if (ttup.isChangesBackedOut() || ttup.isChangesNotOk()) {
                ws.setInfo(InfoMsg.PROMPT_FOR_CHANGES);
            } else if (ttup.isConfirmDelete()) {
                ws.setInfo(InfoMsg.PROMPT_DELETE_CONFIRM);
            } else if (ttup.isDeleteFailed()) {
                ws.setInfo(InfoMsg.INFORM_FAILURE);
            } else if (ttup.isDeleteDone()) {
                ws.setInfo(InfoMsg.CONFIRM_DELETE_SUCCESS);
            } else if (ttup.isCreateNewRecord()) {
                ws.setInfo(InfoMsg.PROMPT_FOR_NEWDATA);
            } else if (ttup.isChangesOkNotConfirmed()) {
                ws.setInfo(InfoMsg.PROMPT_FOR_CONFIRMATION);
            } else if (ttup.isChangesOkayedAndDone()) {
                ws.setInfo(InfoMsg.CONFIRM_UPDATE_SUCCESS);
            } else if (ttup.isChangesOkayedLockError()) {
                ws.setInfo(InfoMsg.INFORM_FAILURE);
            } else if (ttup.isChangesOkayedButFailed()) {
                ws.setInfo(InfoMsg.INFORM_FAILURE);
            } else if (ws.isNoInfoMessage()) {
                ws.setInfo(InfoMsg.PROMPT_FOR_SEARCH_KEYS);
            }
            ws.wsStringLen = trim(ws.wsInfoMsg).length();
            ws.wsStringMid = (ws.wsInfoMsg.length() - ws.wsStringLen) / 2 + 1;
            StringBuilder out = new StringBuilder(ws.wsStringOut);
            out.replace(ws.wsStringMid - 1, ws.wsStringMid - 1 + ws.wsStringLen,
                    ws.wsInfoMsg.substring(0, ws.wsStringLen));
            ws.wsStringOut = out.toString();
            map.infomsg.move(ws.wsStringOut);
            map.errmsg.move(ws.wsReturnMsg);
        }

        // 3300-SETUP-SCREEN-ATTRS
        private void setupScreenAttrs3300() {
            protectAllAttrs3310();
            if (ttup.isDetailsNotFetched() || ttup.isInvalidSearchKeys() || ttup.isDetailsNotFound()
                    || (ttup.isChangesBackedOut() && oldTypeBlank())) {
                map.trtypcd.attribute = Dfhbmsca.DFHBMFSE;
            } else if (ttup.isShowDetails() || ttup.isChangesNotOk() || ttup.isCreateNewRecord()
                    || ttup.isChangesBackedOut()) {
                unprotectFewAttrs3320();
            } else if (ttup.isChangesOkNotConfirmed() || ttup.isChangesOkayedAndDone()
                    || ttup.isDeleteInProgress()) {
                // CONTINUE
            } else {
                map.trtypcd.attribute = Dfhbmsca.DFHBMFSE;
            }

            if (ttup.isDetailsNotFetched() || ttup.isDetailsNotFound() || ttup.isInvalidSearchKeys()
                    || ws.isTranfilterNotOk() || ws.isTranfilterBlank() || ttup.isChangesOkayedAndDone()
                    || (ttup.isChangesBackedOut() && oldTypeBlank())) {
                map.trtypcd.length = -1;
            } else if (ttup.isCreateNewRecord() || ws.is(ReturnMsg.NO_CHANGES_DETECTED)
                    || ws.isDescriptionNotOk() || ws.isDescriptionBlank() || ttup.isChangesMade()
                    || ttup.isChangesBackedOut() || ttup.isShowDetails()) {
                map.trtydsc.length = -1;
            } else {
                map.trtypcd.length = -1;
            }

            if (ws.isTranfilterNotOk() || ttup.isDeleteFailed()) {
                map.trtypcd.color = Dfhbmsca.DFHRED;
            }
            if (ws.isTranfilterBlank() && carddemo.isPgmReenter()) {
                map.trtypcd.move("*");
                map.trtypcd.color = Dfhbmsca.DFHRED;
            }
            if (ttup.isDetailsNotFetched() || ttup.isDetailsNotFound() || ttup.isInvalidSearchKeys()
                    || ws.isTranfilterBlank() || ws.isTranfilterNotOk()) {
                return;
            }
            csSetAtyDescription();
        }

        // COPY CSSETATY REPLACING (TESTVAR1) BY DESCRIPTION, (SCRNVAR2) BY TRTYDSC, (MAPNAME3) BY CTRTUPA
        private void csSetAtyDescription() {
            if ((ws.isDescriptionNotOk() || ws.isDescriptionBlank()) && carddemo.isPgmReenter()) {
                map.trtydsc.color = Dfhbmsca.DFHRED;
                if (ws.isDescriptionBlank()) {
                    map.trtydsc.move("*");
                }
            }
        }

        // 3310-PROTECT-ALL-ATTRS
        private void protectAllAttrs3310() {
            map.trtypcd.attribute = Dfhbmsca.DFHBMPRF;
            map.trtydsc.attribute = Dfhbmsca.DFHBMPRF;
            map.infomsg.attribute = Dfhbmsca.DFHBMPRF;
        }

        // 3320-UNPROTECT-FEW-ATTRS
        private void unprotectFewAttrs3320() {
            map.trtydsc.attribute = Dfhbmsca.DFHBMFSE;
            map.infomsg.attribute = Dfhbmsca.DFHBMPRF;
        }

        // 3390-SETUP-INFOMSG-ATTRS
        private void setupInfomsgAttrs3390() {
            map.infomsg.attribute = ws.isNoInfoMessage() ? Dfhbmsca.DFHBMDAR : Dfhbmsca.DFHBMASB;
        }

        // 3391-SETUP-PFKEY-ATTRS
        private void setupPfkeyAttrs3391() {
            map.fkeys.attribute = ttup.isConfirmDelete() ? Dfhbmsca.DFHBMDAR : Dfhbmsca.DFHBMASB;
            if (ttup.isShowDetails() || ttup.isConfirmDelete()) {
                map.fkey04.attribute = Dfhbmsca.DFHBMASB;
            }
            if (ttup.isChangesOkNotConfirmed() || ttup.isDetailsNotFound()) {
                map.fkey05.attribute = Dfhbmsca.DFHBMASB;
            }
            if (ttup.isChangesOkNotConfirmed() || ttup.isShowDetails() || ttup.isDetailsNotFound()
                    || ttup.isConfirmDelete() || ttup.isCreateNewRecord()) {
                map.fkey12.attribute = Dfhbmsca.DFHBMASB;
            }
        }

        // 3400-SEND-SCREEN
        private void sendScreen3400() {
            cc.nextMapset(LIT_THISMAPSET);
            cc.nextMap(LIT_THISMAP);
            ws.wsRespCd = 0;
        }

        // 9000-READ-TRANTYPE
        private void readTrantype9000() {
            ttup.initializeOldDetails();
            ws.setNoInfoMessage();
            getTransactionType9100();
            if (ws.isTranfilterNotOk()) {
                return;
            }
            storeFetchedData9500();
        }

        // 9100-GET-TRANSACTION-TYPE
        private void getTransactionType9100() {
            dcl.trType(ttup.newType());
            sqlca = db.select(dcl);
            ws.wsDispSqlcode = CobolText.editMinus4Z9(sqlca.sqlcode);
            if (sqlca.sqlcode == 0) {
                ws.setFoundTrantypeInTable();
            } else if (sqlca.sqlcode == 100) {
                ws.setInputError();
                ws.setTranfilterNotOk();
                if (ws.isReturnMsgOff()) {
                    ws.setReturn(ReturnMsg.WS_RECORD_NOT_FOUND);
                }
            } else if (sqlca.sqlcode < 0) {
                ws.setInputError();
                ws.setTranfilterNotOk();
                if (ws.isReturnMsgOff()) {
                    ws.wsReturnMsg = CobolText.stringInto(ws.wsReturnMsg, "Error accessing:",
                            " TRANSACTION_TYPE table. SQLCODE:", ws.wsDispSqlcode, ":", sqlca.sqlerrmGroup());
                }
            }
        }

        // 9500-STORE-FETCHED-DATA
        private void storeFetchedData9500() {
            ttup.initializeOldDetails();
            ttup.oldType(dcl.trType);
            ttup.oldDesc(dcl.varcharValue());
        }

        // 9600-WRITE-PROCESSING
        private void writeProcessing9600() {
            dcl.trType(ttup.newType());
            dcl.trDescriptionText(trim(ttup.newDesc()));
            dcl.trDescriptionLen = (short) ttup.newDesc().length();
            sqlca = db.update(dcl);
            ws.wsDispSqlcode = CobolText.editMinus4Z9(sqlca.sqlcode);
            if (sqlca.sqlcode == 0) {
                cics.syncpoint();
            } else if (sqlca.sqlcode == 100) {
                insertRecord9700();
            } else if (sqlca.sqlcode == -911) {
                ws.setInputError();
                if (ws.isReturnMsgOff()) {
                    ws.setReturn(ReturnMsg.COULD_NOT_LOCK_REC_FOR_UPDATE);
                }
            } else if (sqlca.sqlcode < 0) {
                ws.setReturn(ReturnMsg.TABLE_UPDATE_FAILED);
                ws.wsReturnMsg = CobolText.stringInto(ws.wsReturnMsg, "Error updating:",
                        " TRANSACTION_TYPE Table. SQLCODE:", ws.wsDispSqlcode, ":", sqlca.sqlerrmGroup());
            }
            if (ws.is(ReturnMsg.COULD_NOT_LOCK_REC_FOR_UPDATE)) {
                ttup.setChangesOkayedLockError();
            } else if (ws.is(ReturnMsg.TABLE_UPDATE_FAILED)) {
                ttup.setChangesOkayedButFailed();
            } else if (ws.is(ReturnMsg.DATA_WAS_CHANGED_BEFORE_UPDATE)) {
                ttup.setShowDetails();
            } else {
                ttup.setChangesOkayedAndDone();
            }
        }

        // 9700-INSERT-RECORD
        private void insertRecord9700() {
            sqlca = db.insert(dcl);
            if (sqlca.sqlcode == 0) {
                cics.syncpoint();
            } else {
                ws.setReturn(ReturnMsg.TABLE_UPDATE_FAILED);
                ws.wsReturnMsg = CobolText.stringInto(ws.wsReturnMsg, "Error inserting record into:",
                        " TRANSACTION_TYPE Table. SQLCODE:", ws.wsDispSqlcode, ":", sqlca.sqlerrmGroup());
            }
        }

        // 9800-DELETE-PROCESSING
        private void deleteProcessing9800() {
            dcl.trType(ttup.oldType());
            sqlca = db.delete(dcl);
            ws.wsDispSqlcode = CobolText.editMinus4Z9(sqlca.sqlcode);
            if (sqlca.sqlcode == 0) {
                ttup.setDeleteDone();
                cics.syncpoint();
            } else if (sqlca.sqlcode == -532) {
                ws.setReturn(ReturnMsg.RECORD_DELETE_FAILED);
                ws.wsReturnMsg = CobolText.stringInto(ws.wsReturnMsg, "Please delete associated child records first:",
                        "SQLCODE :", ws.wsDispSqlcode, ":", sqlca.sqlerrmGroup(), sqlca.sqlerrmGroup());
            } else {
                ws.setReturn(ReturnMsg.RECORD_DELETE_FAILED);
                ttup.setDeleteFailed();
                ws.wsReturnMsg = CobolText.stringInto(ws.wsReturnMsg, "Delete failed with message:",
                        "SQLCODE :", ws.wsDispSqlcode, ":", sqlca.sqlerrmGroup());
            }
        }

        // YYYY-STORE-PFKEY (COPY CSSTRPFY)
        private void yyyyStorePfkey() {
            Dfhaid aid = req.aid();
            if (aid == null) {
                return;
            }
            CcardAid mapped = switch (aid) {
                case DFHENTER -> CcardAid.ENTER;
                case DFHCLEAR -> CcardAid.CLEAR;
                case DFHPA1 -> CcardAid.PA1;
                case DFHPA2 -> CcardAid.PA2;
                case DFHPF1, DFHPF13 -> CcardAid.PFK01;
                case DFHPF2, DFHPF14 -> CcardAid.PFK02;
                case DFHPF3, DFHPF15 -> CcardAid.PFK03;
                case DFHPF4, DFHPF16 -> CcardAid.PFK04;
                case DFHPF5, DFHPF17 -> CcardAid.PFK05;
                case DFHPF6, DFHPF18 -> CcardAid.PFK06;
                case DFHPF7, DFHPF19 -> CcardAid.PFK07;
                case DFHPF8, DFHPF20 -> CcardAid.PFK08;
                case DFHPF9, DFHPF21 -> CcardAid.PFK09;
                case DFHPF10, DFHPF22 -> CcardAid.PFK10;
                case DFHPF11, DFHPF23 -> CcardAid.PFK11;
                case DFHPF12, DFHPF24 -> CcardAid.PFK12;
                default -> null;
            };
            if (mapped != null) {
                cc.setAid(mapped);
            }
        }

        // ABEND-ROUTINE
        private CicsAbendException abendRoutine(Throwable cause) {
            if (abend.isAbendMsgLowValues()) {
                abend.abendMsg("UNEXPECTED ABEND OCCURRED.");
            }
            abend.abendCulprit(LIT_THISPGM);
            abend.abendCode("9999");
            // EXEC CICS SEND FROM(ABEND-DATA) / HANDLE ABEND CANCEL / ABEND ABCODE(ABEND-CODE)
            return new CicsAbendException(abend.copy(), cause);
        }
    }

    private static String slice(String s, int from, int len) {
        if (from >= s.length()) {
            return "";
        }
        return s.substring(from, Math.min(s.length(), from + len));
    }
}
