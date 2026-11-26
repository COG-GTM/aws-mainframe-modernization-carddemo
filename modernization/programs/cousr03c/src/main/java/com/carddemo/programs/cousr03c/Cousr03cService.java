package com.carddemo.programs.cousr03c;

import com.carddemo.programs.cousr03c.copybook.CommonMessages;
import com.carddemo.programs.cousr03c.copybook.Cousr03Map;
import com.carddemo.programs.cousr03c.copybook.Cousr03Map.Field;
import com.carddemo.programs.cousr03c.copybook.Dfhaid;
import com.carddemo.programs.cousr03c.copybook.Dfhbmsca;
import com.carddemo.programs.cousr03c.copybook.FixedRecord;
import com.carddemo.programs.cousr03c.copybook.ScreenTitle;
import com.carddemo.programs.cousr03c.copybook.SecUserData;
import com.carddemo.programs.cousr03c.copybook.WsDateTime;
import com.carddemo.programs.cousr03c.dto.Cousr03cRequest;
import com.carddemo.programs.cousr03c.dto.Cousr03cResult;
import com.carddemo.programs.cousr03c.dto.ScreenInput;
import com.carddemo.programs.cousr03c.dto.UserDeleteScreen;
import com.carddemo.programs.cousr03c.gateway.CicsResp;
import com.carddemo.programs.cousr03c.gateway.UserSecurityFile;

import java.time.Clock;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

/** COUSR03C (transaction CU03): delete a user from the USRSEC file. */
public final class Cousr03cService {

    public static final String WS_PGMNAME = "COUSR03C";
    public static final String WS_TRANID = "CU03";
    public static final String WS_USRSEC_FILE = "USRSEC  ";

    public static final String MSG_USER_ID_EMPTY = "User ID can NOT be empty...";
    public static final String MSG_PRESS_PF5 = "Press PF5 key to delete this user ...";
    public static final String MSG_USER_NOT_FOUND = "User ID NOT found...";
    public static final String MSG_UNABLE_TO_LOOKUP = "Unable to lookup User...";
    public static final String MSG_UNABLE_TO_UPDATE = "Unable to Update User...";

    private final UserSecurityFile usrsec;
    private final Clock clock;
    private final Consumer<String> display;

    public Cousr03cService(UserSecurityFile usrsec, Clock clock) {
        this(usrsec, clock, line -> System.out.println(line));
    }

    public Cousr03cService(UserSecurityFile usrsec, Clock clock, Consumer<String> display) {
        this.usrsec = usrsec;
        this.clock = clock;
        this.display = display;
    }

    public Cousr03cResult execute(Cousr03cRequest request) {
        try {
            return new Task(request).mainPara();
        } finally {
            usrsec.endOfTask();
        }
    }

    /** WORKING-STORAGE and procedure division for one CICS task. */
    private final class Task {
        private final Cousr03cRequest request;
        // WS-VARIABLES
        private String wsMessage = " ".repeat(80);        // X(80)
        private char wsErrFlg = 'N';                        // 88 ERR-FLG-ON 'Y' / ERR-FLG-OFF 'N'
        private int wsRespCd;                               // S9(09) COMP
        private int wsReasCd;                               // S9(09) COMP
        private char wsUsrModified = 'N';                   // 88 USR-MODIFIED-YES 'Y' / -NO 'N'
        private Cousr03cCommarea commarea = new Cousr03cCommarea();
        private final Cousr03Map map = new Cousr03Map();
        private final WsDateTime wsDateTime = new WsDateTime();
        private final SecUserData secUserData = new SecUserData();

        private final List<UserDeleteScreen> screens = new ArrayList<>();
        private final List<String> displayLines = new ArrayList<>();
        private String xctlProgram;

        Task(Cousr03cRequest request) {
            this.request = request;
        }

        private boolean errFlgOn() {
            return wsErrFlg == 'Y';
        }

        private void setMessage(String text) {
            wsMessage = fit(text, 80);
        }

        // MAIN-PARA
        Cousr03cResult mainPara() {
            wsErrFlg = 'N';
            wsUsrModified = 'N';
            setMessage("");
            map.setO(Field.ERRMSG, "");

            if (request.eibcalen() == 0) {
                commarea.setToProgram("COSGN00C");
                returnToPrevScreen();
            } else {
                commarea = Cousr03cCommarea.fromDfhcommarea(request.dfhcommarea());
                if (!commarea.isPgmReenter()) {
                    commarea.setPgmContext(Cousr03cCommarea.PgmContext.REENTER);
                    map.fill(FixedRecord.LOW_VALUE);
                    map.setL(Field.USRIDIN, -1);
                    String selected = commarea.cu03UsrSelected();
                    if (!isAll(selected, ' ') && !isAll(selected, '\0')) {
                        map.setI(Field.USRIDIN, selected);
                        processEnterKey();
                    }
                    sendUsrdelScreen();
                } else {
                    receiveUsrdelScreen();
                    Dfhaid aid = request.eibaid() == null ? Dfhaid.DFHNULL : request.eibaid();
                    switch (aid) {
                        case DFHENTER -> processEnterKey();
                        case DFHPF3 -> {
                            if (FixedRecord.isSpacesOrLowValues(commarea.fromProgram())) {
                                commarea.setToProgram("COADM01C");
                            } else {
                                commarea.setToProgram(commarea.fromProgram());
                            }
                            returnToPrevScreen();
                        }
                        case DFHPF4 -> clearCurrentScreen();
                        case DFHPF5 -> deleteUserInfo();
                        case DFHPF12 -> {
                            commarea.setToProgram("COADM01C");
                            returnToPrevScreen();
                        }
                        default -> {
                            wsErrFlg = 'Y';
                            setMessage(CommonMessages.CCDA_MSG_INVALID_KEY);
                            sendUsrdelScreen();
                        }
                    }
                }
            }

            if (xctlProgram != null) {
                return new Cousr03cResult(Cousr03cResult.Action.XCTL, null, xctlProgram, commarea,
                        List.copyOf(screens), List.copyOf(displayLines));
            }
            // EXEC CICS RETURN TRANSID(WS-TRANID) COMMAREA(CARDDEMO-COMMAREA)
            return new Cousr03cResult(Cousr03cResult.Action.RETURN_TRANSID, WS_TRANID, null, commarea,
                    List.copyOf(screens), List.copyOf(displayLines));
        }

        // PROCESS-ENTER-KEY
        private void processEnterKey() {
            if (FixedRecord.isSpacesOrLowValues(map.i(Field.USRIDIN))) {
                wsErrFlg = 'Y';
                setMessage(MSG_USER_ID_EMPTY);
                map.setL(Field.USRIDIN, -1);
                sendUsrdelScreen();
            } else {
                map.setL(Field.USRIDIN, -1);
            }

            if (!errFlgOn()) {
                map.setI(Field.FNAME, "");
                map.setI(Field.LNAME, "");
                map.setI(Field.USRTYPE, "");
                secUserData.setUsrId(map.i(Field.USRIDIN));
                readUserSecFile();
            }

            if (!errFlgOn()) {
                map.setI(Field.FNAME, secUserData.usrFname());
                map.setI(Field.LNAME, secUserData.usrLname());
                map.setI(Field.USRTYPE, secUserData.usrType());
                sendUsrdelScreen();
            }
        }

        // DELETE-USER-INFO
        private void deleteUserInfo() {
            if (FixedRecord.isSpacesOrLowValues(map.i(Field.USRIDIN))) {
                wsErrFlg = 'Y';
                setMessage(MSG_USER_ID_EMPTY);
                map.setL(Field.USRIDIN, -1);
                sendUsrdelScreen();
            } else {
                map.setL(Field.USRIDIN, -1);
            }

            if (!errFlgOn()) {
                secUserData.setUsrId(map.i(Field.USRIDIN));
                readUserSecFile();
                deleteUserSecFile();
            }
        }

        // RETURN-TO-PREV-SCREEN
        private void returnToPrevScreen() {
            if (FixedRecord.isSpacesOrLowValues(commarea.toProgram())) {
                commarea.setToProgram("COSGN00C");
            }
            commarea.setFromTranid(WS_TRANID);
            commarea.setFromProgram(WS_PGMNAME);
            commarea.setPgmContext(0);
            // EXEC CICS XCTL PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA)
            xctlProgram = commarea.toProgram();
        }

        // SEND-USRDEL-SCREEN
        private void sendUsrdelScreen() {
            populateHeaderInfo();
            map.setO(Field.ERRMSG, wsMessage);
            screens.add(snapshot());
        }

        // RECEIVE-USRDEL-SCREEN
        private void receiveUsrdelScreen() {
            ScreenInput in = request.mapInput();
            if (in == null) {
                wsRespCd = CicsResp.MAPFAIL;
                wsReasCd = 0;
                return;
            }
            receiveField(Field.USRIDIN, in.usrIdIn());
            receiveField(Field.FNAME, in.fname());
            receiveField(Field.LNAME, in.lname());
            receiveField(Field.USRTYPE, in.usrType());
            wsRespCd = CicsResp.NORMAL;
            wsReasCd = 0;
        }

        private void receiveField(Field f, String value) {
            if (value == null) {
                return;
            }
            String v = value.length() > f.size ? value.substring(0, f.size) : value;
            map.setL(f, v.length());
            map.setF(f, FixedRecord.LOW_VALUE);
            map.setI(f, v);
        }

        // POPULATE-HEADER-INFO
        private void populateHeaderInfo() {
            wsDateTime.moveCurrentDate(LocalDateTime.now(clock));

            map.setO(Field.TITLE01, ScreenTitle.CCDA_TITLE01);
            map.setO(Field.TITLE02, ScreenTitle.CCDA_TITLE02);
            map.setO(Field.TRNNAME, WS_TRANID);
            map.setO(Field.PGMNAME, WS_PGMNAME);

            wsDateTime.setCurdateMm(wsDateTime.curdateMonth());
            wsDateTime.setCurdateDd(wsDateTime.curdateDay());
            wsDateTime.setCurdateYy(wsDateTime.curdateYear() % 100);   // WS-CURDATE-YEAR(3:2)
            map.setO(Field.CURDATE, wsDateTime.curdateMmDdYy());

            wsDateTime.setCurtimeHh(wsDateTime.curtimeHours());
            wsDateTime.setCurtimeMm(wsDateTime.curtimeMinute());
            wsDateTime.setCurtimeSs(wsDateTime.curtimeSecond());
            map.setO(Field.CURTIME, wsDateTime.curtimeHhMmSs());
        }

        // READ-USER-SEC-FILE
        private void readUserSecFile() {
            UserSecurityFile.ReadResult rr = usrsec.readForUpdate(secUserData.usrId());
            wsRespCd = rr.resp().resp();
            wsReasCd = rr.resp().resp2();
            if (rr.resp().isNormal() && rr.record() != null) {
                secUserData.moveFrom(rr.record().toBytes());
            }

            switch (wsRespCd) {
                case CicsResp.NORMAL -> {
                    setMessage(MSG_PRESS_PF5);
                    map.setC(Field.ERRMSG, Dfhbmsca.DFHNEUTR);
                    sendUsrdelScreen();
                }
                case CicsResp.NOTFND -> {
                    wsErrFlg = 'Y';
                    setMessage(MSG_USER_NOT_FOUND);
                    map.setL(Field.USRIDIN, -1);
                    sendUsrdelScreen();
                }
                default -> {
                    displayResp();
                    wsErrFlg = 'Y';
                    setMessage(MSG_UNABLE_TO_LOOKUP);
                    map.setL(Field.FNAME, -1);
                    sendUsrdelScreen();
                }
            }
        }

        // DELETE-USER-SEC-FILE
        private void deleteUserSecFile() {
            CicsResp resp = usrsec.deleteHeldRecord();
            wsRespCd = resp.resp();
            wsReasCd = resp.resp2();

            switch (wsRespCd) {
                case CicsResp.NORMAL -> {
                    initializeAllFields();
                    setMessage("");
                    map.setC(Field.ERRMSG, Dfhbmsca.DFHGREEN);
                    // STRING 'User ' SEC-USR-ID DELIMITED BY SPACE ' has been deleted ...' INTO WS-MESSAGE
                    String id = secUserData.usrId();
                    int sp = id.indexOf(' ');
                    String built = "User " + (sp < 0 ? id : id.substring(0, sp)) + " has been deleted ...";
                    wsMessage = built + wsMessage.substring(Math.min(built.length(), 80));
                    sendUsrdelScreen();
                }
                case CicsResp.NOTFND -> {
                    wsErrFlg = 'Y';
                    setMessage(MSG_USER_NOT_FOUND);
                    map.setL(Field.USRIDIN, -1);
                    sendUsrdelScreen();
                }
                default -> {
                    displayResp();
                    wsErrFlg = 'Y';
                    setMessage(MSG_UNABLE_TO_UPDATE);
                    map.setL(Field.FNAME, -1);
                    sendUsrdelScreen();
                }
            }
        }

        // CLEAR-CURRENT-SCREEN
        private void clearCurrentScreen() {
            initializeAllFields();
            sendUsrdelScreen();
        }

        // INITIALIZE-ALL-FIELDS
        private void initializeAllFields() {
            map.setL(Field.USRIDIN, -1);
            map.setI(Field.USRIDIN, "");
            map.setI(Field.FNAME, "");
            map.setI(Field.LNAME, "");
            map.setI(Field.USRTYPE, "");
            setMessage("");
        }

        /** DISPLAY 'RESP:' WS-RESP-CD 'REAS:' WS-REAS-CD (signed numeric shown with a leading sign). */
        private void displayResp() {
            String line = "RESP:" + signed9(wsRespCd) + "REAS:" + signed9(wsReasCd);
            displayLines.add(line);
            display.accept(line);
        }

        private UserDeleteScreen snapshot() {
            return new UserDeleteScreen(
                    map.o(Field.TRNNAME), map.o(Field.TITLE01), map.o(Field.CURDATE), map.o(Field.PGMNAME),
                    map.o(Field.TITLE02), map.o(Field.CURTIME), map.o(Field.USRIDIN), map.o(Field.FNAME),
                    map.o(Field.LNAME), map.o(Field.USRTYPE), map.o(Field.ERRMSG),
                    Dfhbmsca.colorName(map.c(Field.ERRMSG)), cursorField());
        }

        /** SEND MAP ... CURSOR: symbolic cursor goes to the first field whose length is -1, else the IC field. */
        private Field cursorField() {
            for (Field f : Field.values()) {
                if (map.l(f) == -1) {
                    return f;
                }
            }
            return Field.USRIDIN;
        }
    }

    private static boolean isAll(String s, char c) {
        return s.chars().allMatch(x -> x == c);
    }

    private static String fit(String s, int len) {
        return s.length() >= len ? s.substring(0, len) : s + " ".repeat(len - s.length());
    }

    private static String signed9(int v) {
        return (v < 0 ? "-" : "+") + String.format("%09d", Math.abs((long) v) % 1_000_000_000L);
    }
}
