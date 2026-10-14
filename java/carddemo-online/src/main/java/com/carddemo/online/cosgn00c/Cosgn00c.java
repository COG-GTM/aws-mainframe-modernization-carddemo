package com.carddemo.online.cosgn00c;

import com.carddemo.common.copybook.CardDemoCommarea;
import com.carddemo.common.copybook.CommonMessages;
import com.carddemo.common.copybook.DateTimeFields;
import com.carddemo.common.copybook.ScreenTitles;
import com.carddemo.common.copybook.UserSecurityRecord;
import com.carddemo.runtime.bms.SymbolicMap;
import com.carddemo.runtime.cics.CicsContext;
import com.carddemo.runtime.cics.CicsContext.SendMapOption;
import com.carddemo.runtime.cics.CicsProgram;
import com.carddemo.runtime.cics.CicsResp;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.cobol.RecordLayout;
import com.carddemo.runtime.vsam.FileResult;
import java.util.Optional;
import org.springframework.stereotype.Component;

/**
 * COSGN00C - CardDemo sign-on screen (transaction CC00, mapset COSGN00 / map COSGN0A).
 * Each method is the COBOL paragraph of the same name; line numbers refer to app/cbl/COSGN00C.cbl.
 */
@Component
public class Cosgn00c implements CicsProgram {

    public static final String PROGRAM = "COSGN00C";
    public static final String TRANSID = "CC00";
    public static final String MAPSET = "COSGN00";
    public static final String MAP = "COSGN0A";
    public static final String ADMIN_MENU_PROGRAM = "COADM01C";
    public static final String USER_MENU_PROGRAM = "COMEN01C";

    static final String MSG_ENTER_USER_ID = "Please enter User ID ...";
    static final String MSG_ENTER_PASSWORD = "Please enter Password ...";
    static final String MSG_WRONG_PASSWORD = "Wrong Password. Try again ...";
    static final String MSG_USER_NOT_FOUND = "User not found. Try again ...";
    static final String MSG_UNABLE_TO_VERIFY = "Unable to verify the User ...";

    @Override
    public String programName() {
        return PROGRAM;
    }

    @Override
    public Optional<String> transactionId() {
        return Optional.of(TRANSID);
    }

    @Override
    public Optional<RecordLayout> commareaLayout() {
        return Optional.of(CardDemoCommarea.LAYOUT);
    }

    @Override
    public void execute(CicsContext ctx) {
        new Task(ctx).mainPara();
    }

    /** One task's WORKING-STORAGE (lines 35-58) plus the procedure division. */
    static final class Task {
        private final CicsContext ctx;

        // WS-VARIABLES (lines 35-46)
        final String wsPgmname = PicX.of(PROGRAM, 8);
        final String wsTranid = PicX.of(TRANSID, 4);
        String wsMessage = PicX.spaces(80);
        final String wsUsrsecFile = PicX.of("USRSEC", 8);
        String wsErrFlg = "N";
        int wsRespCd;
        int wsReasCd;
        String wsUserId = PicX.lowValues(8);
        String wsUserPwd = PicX.lowValues(8);

        // COPY COCOM01Y / COSGN00 / CSUSR01Y (no VALUE clauses: LOW-VALUES)
        final CardDemoCommarea commarea = new CardDemoCommarea();
        final SymbolicMap cosgn0a = SymbolicMap.of(MAPSET, MAP);
        final UserSecurityRecord secUserData = new UserSecurityRecord();

        Task(CicsContext ctx) {
            this.ctx = ctx;
        }

        private boolean errFlgOn() {
            return "Y".equals(wsErrFlg);
        }

        private void setMessage(String text) {
            wsMessage = PicX.of(text, 80);
        }

        /** MAIN-PARA (lines 73-102). */
        void mainPara() {
            wsErrFlg = "N";
            wsMessage = PicX.spaces(80);
            cosgn0a.setData("ERRMSG", "");

            if (ctx.eib().eibcalen() == 0) {
                cosgn0a.lowValues();
                cosgn0a.setCursor("USERID");
                sendSignonScreen();
            } else {
                switch (ctx.eib().eibaid()) {
                    case ENTER -> processEnterKey();
                    case PF3 -> {
                        wsMessage = PicX.of(CommonMessages.CCDA_MSG_THANK_YOU, 80);
                        sendPlainText();
                    }
                    default -> {
                        wsErrFlg = "Y";
                        wsMessage = PicX.of(CommonMessages.CCDA_MSG_INVALID_KEY, 80);
                        sendSignonScreen();
                    }
                }
            }

            ctx.returnTransid(wsTranid, commarea);
        }

        /** PROCESS-ENTER-KEY (lines 108-140). */
        void processEnterKey() {
            CicsResp resp = ctx.receiveMap(cosgn0a);
            wsRespCd = resp.code();
            wsReasCd = ctx.eib().eibresp2();

            if (PicX.isSpacesOrLowValues(cosgn0a.data("USERID"))) {
                wsErrFlg = "Y";
                setMessage(MSG_ENTER_USER_ID);
                cosgn0a.setCursor("USERID");
                sendSignonScreen();
            } else if (PicX.isSpacesOrLowValues(cosgn0a.data("PASSWD"))) {
                wsErrFlg = "Y";
                setMessage(MSG_ENTER_PASSWORD);
                cosgn0a.setCursor("PASSWD");
                sendSignonScreen();
            }

            String upperUserId = PicX.upperCase(cosgn0a.data("USERID"));
            wsUserId = PicX.of(upperUserId, 8);
            commarea.setUserId(upperUserId);
            wsUserPwd = PicX.of(PicX.upperCase(cosgn0a.data("PASSWD")), 8);

            if (!errFlgOn()) {
                readUserSecFile();
            }
        }

        /** SEND-SIGNON-SCREEN (lines 145-157). */
        void sendSignonScreen() {
            populateHeaderInfo();
            cosgn0a.setData("ERRMSG", wsMessage);
            ctx.sendMap(cosgn0a, SendMapOption.ERASE, SendMapOption.CURSOR);
        }

        /** SEND-PLAIN-TEXT (lines 162-172). */
        void sendPlainText() {
            ctx.sendText(wsMessage, wsMessage.length(), true, true);
            ctx.returnToCics();
        }

        /** POPULATE-HEADER-INFO (lines 177-204). */
        void populateHeaderInfo() {
            DateTimeFields dt = DateTimeFields.fromCurrentDate(ctx.currentDate());
            cosgn0a.setData("TITLE01", ScreenTitles.CCDA_TITLE01);
            cosgn0a.setData("TITLE02", ScreenTitles.CCDA_TITLE02);
            cosgn0a.setData("TRNNAME", wsTranid);
            cosgn0a.setData("PGMNAME", wsPgmname);
            cosgn0a.setData("CURDATE", dt.curdateMmDdYy());
            cosgn0a.setData("CURTIME", dt.curtimeHhMmSs());
            cosgn0a.setData("APPLID", ctx.assignApplid());
            // ASSIGN SYSID returns 4 bytes; SYSIDO is X(8), so only its first 4 bytes are overwritten.
            cosgn0a.setDataPrefix("SYSID", ctx.assignSysid());
        }

        /** READ-USER-SEC-FILE (lines 209-257). */
        void readUserSecFile() {
            FileResult result = ctx.read(wsUsrsecFile, wsUserId, UserSecurityRecord.LENGTH);
            wsRespCd = result.resp().code();
            wsReasCd = result.resp2();
            if (result.data() != null && (result.isNormal() || result.resp() == CicsResp.LENGERR)) {
                secUserData.setBytes(result.data());
            }

            if (wsRespCd == 0) {
                if (secUserData.password().equals(wsUserPwd)) {
                    commarea.setFromTranid(wsTranid);
                    commarea.setFromProgram(wsPgmname);
                    commarea.setUserId(wsUserId);
                    commarea.setUserType(secUserData.userType());
                    commarea.setPgmContext(0);

                    if (commarea.isAdmin()) {
                        ctx.xctl(ADMIN_MENU_PROGRAM, commarea);
                    } else {
                        ctx.xctl(USER_MENU_PROGRAM, commarea);
                    }
                } else {
                    setMessage(MSG_WRONG_PASSWORD);
                    cosgn0a.setCursor("PASSWD");
                    sendSignonScreen();
                }
            } else if (wsRespCd == 13) {
                wsErrFlg = "Y";
                setMessage(MSG_USER_NOT_FOUND);
                cosgn0a.setCursor("USERID");
                sendSignonScreen();
            } else {
                wsErrFlg = "Y";
                setMessage(MSG_UNABLE_TO_VERIFY);
                cosgn0a.setCursor("USERID");
                sendSignonScreen();
            }
        }
    }
}
