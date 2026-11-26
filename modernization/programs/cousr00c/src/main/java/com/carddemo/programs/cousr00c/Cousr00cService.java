package com.carddemo.programs.cousr00c;

import com.carddemo.programs.cousr00c.copybook.CardDemoCommarea;
import com.carddemo.programs.cousr00c.copybook.CommonMessages;
import com.carddemo.programs.cousr00c.copybook.Cousr00Map;
import com.carddemo.programs.cousr00c.copybook.DateTimeFields;
import com.carddemo.programs.cousr00c.copybook.Dfhaid;
import com.carddemo.programs.cousr00c.copybook.ScreenTitle;
import com.carddemo.programs.cousr00c.copybook.SecUserData;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * COUSR00C - CardDemo "List Users" (transaction CU00), admin-only browse of USRSEC, 10 users per
 * page, with U (update -> COUSR02C) / D (delete -> COUSR03C) selection.
 *
 * <p>Each {@link #execute} call is one pseudo-conversational task: working storage is fresh,
 * state is carried only in the commarea.
 */
public class Cousr00cService {

    public static final String WS_PGMNAME = "COUSR00C";
    public static final String WS_TRANID = "CU00";

    public static final String MSG_INVALID_SELECTION = "Invalid selection. Valid values are U and D";
    public static final String MSG_ALREADY_TOP = "You are already at the top of the page...";
    public static final String MSG_ALREADY_BOTTOM = "You are already at the bottom of the page...";
    public static final String MSG_AT_TOP = "You are at the top of the page...";
    public static final String MSG_UNABLE_TO_LOOKUP = "Unable to lookup User...";
    public static final String MSG_REACHED_BOTTOM = "You have reached the bottom of the page...";
    public static final String MSG_REACHED_TOP = "You have reached the top of the page...";

    private final UserSecurityFile usrsec;
    private final Clock clock;

    public Cousr00cService(UserSecurityFile usrsec, Clock clock) {
        this.usrsec = usrsec;
        this.clock = clock;
    }

    public Cousr00cResponse execute(Cousr00cRequest request) {
        return new Task(request).mainPara();
    }

    /** XCTL: control never returns to this program. */
    private static final class Xctl extends RuntimeException {
        private final String program;

        Xctl(String program) {
            super(null, null, false, false);
            this.program = program;
        }
    }

    /** Working storage + procedure division of one task. */
    private final class Task {
        private final Cousr00cRequest request;
        private final Dfhaid eibaid;

        // WS-VARIABLES
        private String wsMessage = Cobol.spaces(80);   // PIC X(80)
        private boolean errFlg;                       // WS-ERR-FLG      88 ERR-FLG-ON/OFF
        private boolean userSecEof;                   // WS-USER-SEC-EOF 88 USER-SEC-EOF/NOT-EOF
        private boolean sendErase = true;             // WS-SEND-ERASE-FLG 88 SEND-ERASE-YES/NO
        private int wsIdx;                            // PIC S9(04) COMP

        private Cousr00cCommarea commarea = new Cousr00cCommarea();
        private final Cousr00Map map = new Cousr00Map();
        private String secUsrId = Cobol.lowValues(SecUserData.KEY_LENGTH);
        private SecUserData secUserData;

        private final List<ScreenSend> sends = new ArrayList<>();
        private final List<String> displays = new ArrayList<>();

        Task(Cousr00cRequest request) {
            this.request = request;
            this.eibaid = request.eibaid() == null ? Dfhaid.DFHENTER : request.eibaid();
        }

        // MAIN-PARA
        Cousr00cResponse mainPara() {
            try {
                errFlg = false;
                userSecEof = false;
                commarea.cu00().setNextPageNo();
                sendErase = true;
                wsMessage = Cobol.spaces(80);
                map.errmsg.move(Cobol.spaces(78));
                map.usridin.setL(-1);
                if (request.eibcalen() == 0) {
                    commarea.general().setToProgram("COSGN00C");
                    returnToPrevScreen();
                } else {
                    commarea = Cousr00cCommarea.fromFixedWidth(request.commarea());
                    if (!commarea.general().isPgmReenter()) {
                        commarea.general().setPgmContext(CardDemoCommarea.PgmContext.REENTER);
                        map.lowValues();
                        processEnterKey();
                        sendUsrlstScreen();
                    } else {
                        receiveUsrlstScreen();
                        switch (eibaid) {
                            case DFHENTER -> processEnterKey();
                            case DFHPF3 -> {
                                commarea.general().setToProgram("COADM01C");
                                returnToPrevScreen();
                            }
                            case DFHPF7 -> processPf7Key();
                            case DFHPF8 -> processPf8Key();
                            default -> {
                                errFlg = true;
                                map.usridin.setL(-1);
                                wsMessage = Cobol.alnum(CommonMessages.CCDA_MSG_INVALID_KEY, 80);
                                sendUsrlstScreen();
                            }
                        }
                    }
                }
                // EXEC CICS RETURN TRANSID(WS-TRANID) COMMAREA(CARDDEMO-COMMAREA)
                return new Cousr00cResponse(Cousr00cResponse.Kind.RETURN, WS_TRANID, commarea, sends, displays);
            } catch (Xctl x) {
                return new Cousr00cResponse(Cousr00cResponse.Kind.XCTL, x.program, commarea, sends, displays);
            }
        }

        // PROCESS-ENTER-KEY
        void processEnterKey() {
            int selected = -1;
            for (int i = 0; i < Cousr00Map.ROWS; i++) {
                if (!Cobol.isSpacesOrLowValues(map.sel[i].data())) {
                    selected = i;
                    break;
                }
            }
            if (selected >= 0) {
                commarea.cu00().setUsrSelFlg(map.sel[selected].data().charAt(0));
                commarea.cu00().setUsrSelected(map.usrid[selected].data());
            } else {
                commarea.cu00().setUsrSelFlg(' ');
                commarea.cu00().setUsrSelected(Cobol.spaces(8));
            }
            String selFlg = String.valueOf(commarea.cu00().getUsrSelFlg());
            if (!Cobol.isSpacesOrLowValues(selFlg) && !Cobol.isSpacesOrLowValues(commarea.cu00().getUsrSelected())) {
                switch (commarea.cu00().getUsrSelFlg()) {
                    case 'U', 'u' -> xctlTo("COUSR02C");
                    case 'D', 'd' -> xctlTo("COUSR03C");
                    default -> {
                        wsMessage = Cobol.alnum(MSG_INVALID_SELECTION, 80);
                        map.usridin.setL(-1);
                    }
                }
            }
            if (Cobol.isSpacesOrLowValues(map.usridin.data())) {
                secUsrId = Cobol.lowValues(8);
            } else {
                secUsrId = Cobol.alnum(map.usridin.data(), 8);
            }
            map.usridin.setL(-1);
            commarea.cu00().setPageNum(0);
            processPageForward();
            if (!errFlg) {
                map.usridin.move(" ");
            }
        }

        private void xctlTo(String program) {
            commarea.general().setToProgram(program);
            commarea.general().setFromTranid(WS_TRANID);
            commarea.general().setFromProgram(WS_PGMNAME);
            commarea.general().setPgmContext(0);
            throw new Xctl(commarea.general().getToProgram());
        }

        // PROCESS-PF7-KEY
        void processPf7Key() {
            String first = commarea.cu00().getUsridFirst();
            secUsrId = Cobol.isSpacesOrLowValues(first) ? Cobol.lowValues(8) : first;
            commarea.cu00().setNextPageYes();
            map.usridin.setL(-1);
            if (commarea.cu00().getPageNum() > 1) {
                processPageBackward();
            } else {
                wsMessage = Cobol.alnum(MSG_ALREADY_TOP, 80);
                sendErase = false;
                sendUsrlstScreen();
            }
        }

        // PROCESS-PF8-KEY
        void processPf8Key() {
            String last = commarea.cu00().getUsridLast();
            secUsrId = Cobol.isSpacesOrLowValues(last) ? Cobol.highValues(8) : last;
            map.usridin.setL(-1);
            if (commarea.cu00().isNextPageYes()) {
                processPageForward();
            } else {
                wsMessage = Cobol.alnum(MSG_ALREADY_BOTTOM, 80);
                sendErase = false;
                sendUsrlstScreen();
            }
        }

        // PROCESS-PAGE-FORWARD
        void processPageForward() {
            startbrUserSecFile();
            if (!errFlg) {
                if (eibaid != Dfhaid.DFHENTER && eibaid != Dfhaid.DFHPF7 && eibaid != Dfhaid.DFHPF3) {
                    readnextUserSecFile();
                }
                if (!userSecEof && !errFlg) {
                    for (wsIdx = 1; wsIdx <= 10; wsIdx++) {
                        initializeUserData();
                    }
                }
                wsIdx = 1;
                while (!(wsIdx >= 11 || userSecEof || errFlg)) {
                    readnextUserSecFile();
                    if (!userSecEof && !errFlg) {
                        populateUserData();
                        wsIdx++;
                    }
                }
                if (!userSecEof && !errFlg) {
                    commarea.cu00().setPageNum(commarea.cu00().getPageNum() + 1);
                    readnextUserSecFile();
                    if (!userSecEof && !errFlg) {
                        commarea.cu00().setNextPageYes();
                    } else {
                        commarea.cu00().setNextPageNo();
                    }
                } else {
                    commarea.cu00().setNextPageNo();
                    if (wsIdx > 1) {
                        commarea.cu00().setPageNum(commarea.cu00().getPageNum() + 1);
                    }
                }
                endbrUserSecFile();
                map.pagenum.move(Cobol.numericDisplay(commarea.cu00().getPageNum(), 8));
                map.usridin.move(" ");
                sendUsrlstScreen();
            }
        }

        // PROCESS-PAGE-BACKWARD
        void processPageBackward() {
            startbrUserSecFile();
            if (!errFlg) {
                if (eibaid != Dfhaid.DFHENTER && eibaid != Dfhaid.DFHPF8) {
                    readprevUserSecFile();
                }
                if (!userSecEof && !errFlg) {
                    for (wsIdx = 1; wsIdx <= 10; wsIdx++) {
                        initializeUserData();
                    }
                }
                wsIdx = 10;
                while (!(wsIdx <= 0 || userSecEof || errFlg)) {
                    readprevUserSecFile();
                    if (!userSecEof && !errFlg) {
                        populateUserData();
                        wsIdx--;
                    }
                }
                if (!userSecEof && !errFlg) {
                    readprevUserSecFile();
                    if (commarea.cu00().isNextPageYes()) {
                        if (!userSecEof && !errFlg && commarea.cu00().getPageNum() > 1) {
                            commarea.cu00().setPageNum(commarea.cu00().getPageNum() - 1);
                        } else {
                            commarea.cu00().setPageNum(1);
                        }
                    }
                }
                endbrUserSecFile();
                map.pagenum.move(Cobol.numericDisplay(commarea.cu00().getPageNum(), 8));
                sendUsrlstScreen();
            }
        }

        // POPULATE-USER-DATA
        void populateUserData() {
            if (wsIdx < 1 || wsIdx > 10) {
                return;
            }
            int i = wsIdx - 1;
            map.usrid[i].move(secUserData.secUsrId());
            map.fname[i].move(secUserData.secUsrFname());
            map.lname[i].move(secUserData.secUsrLname());
            map.utype[i].move(secUserData.secUsrType());
            if (wsIdx == 1) {
                commarea.cu00().setUsridFirst(secUserData.secUsrId());
            } else if (wsIdx == 10) {
                commarea.cu00().setUsridLast(secUserData.secUsrId());
            }
        }

        // INITIALIZE-USER-DATA
        void initializeUserData() {
            if (wsIdx < 1 || wsIdx > 10) {
                return;
            }
            int i = wsIdx - 1;
            map.usrid[i].move(Cobol.spaces(8));
            map.fname[i].move(Cobol.spaces(20));
            map.lname[i].move(Cobol.spaces(20));
            map.utype[i].move(Cobol.spaces(1));
        }

        // RETURN-TO-PREV-SCREEN
        void returnToPrevScreen() {
            if (Cobol.isSpacesOrLowValues(commarea.general().getToProgram())) {
                commarea.general().setToProgram("COSGN00C");
            }
            commarea.general().setFromTranid(WS_TRANID);
            commarea.general().setFromProgram(WS_PGMNAME);
            commarea.general().setPgmContext(0);
            throw new Xctl(commarea.general().getToProgram());
        }

        // SEND-USRLST-SCREEN
        void sendUsrlstScreen() {
            populateHeaderInfo();
            map.errmsg.move(wsMessage);
            sends.add(new ScreenSend(Cousr0aScreen.of(map), sendErase));
        }

        // RECEIVE-USRLST-SCREEN
        void receiveUsrlstScreen() {
            if (eibaid.isShortRead()) {
                return; // RESP(MAPFAIL): INTO area left unchanged
            }
            Cousr0aInput in = request.input();
            map.trnname.receive(in.trnname());
            map.title01.receive(in.title01());
            map.curdate.receive(in.curdate());
            map.pgmname.receive(in.pgmname());
            map.title02.receive(in.title02());
            map.curtime.receive(in.curtime());
            map.pagenum.receive(in.pagenum());
            map.usridin.receive(in.usridin());
            for (int i = 0; i < Cousr00Map.ROWS; i++) {
                Cousr0aScreen.Row row = in.rows().get(i);
                map.sel[i].receive(row.sel());
                map.usrid[i].receive(row.usrid());
                map.fname[i].receive(row.fname());
                map.lname[i].receive(row.lname());
                map.utype[i].receive(row.utype());
            }
            map.errmsg.receive(in.errmsg());
        }

        // POPULATE-HEADER-INFO
        void populateHeaderInfo() {
            DateTimeFields dt = DateTimeFields.fromCurrentDate(LocalDateTime.now(clock));
            map.title01.move(ScreenTitle.CCDA_TITLE01);
            map.title02.move(ScreenTitle.CCDA_TITLE02);
            map.trnname.move(WS_TRANID);
            map.pgmname.move(WS_PGMNAME);
            map.curdate.move(dt.curdateMmDdYy());
            map.curtime.move(dt.curtimeHhMmSs());
        }

        // STARTBR-USER-SEC-FILE
        void startbrUserSecFile() {
            CicsResponse r = usrsec.startBr(secUsrId);
            switch (r.resp()) {
                case NORMAL -> {
                }
                case NOTFND -> {
                    userSecEof = true;
                    wsMessage = Cobol.alnum(MSG_AT_TOP, 80);
                    map.usridin.setL(-1);
                    sendUsrlstScreen();
                }
                default -> fileError(r);
            }
        }

        // READNEXT-USER-SEC-FILE
        void readnextUserSecFile() {
            BrowseRead r = usrsec.readNext();
            switch (r.response().resp()) {
                case NORMAL -> {
                    secUserData = r.record();
                    secUsrId = r.record().secUsrId();
                }
                case ENDFILE -> {
                    userSecEof = true;
                    wsMessage = Cobol.alnum(MSG_REACHED_BOTTOM, 80);
                    map.usridin.setL(-1);
                    sendUsrlstScreen();
                }
                default -> fileError(r.response());
            }
        }

        // READPREV-USER-SEC-FILE
        void readprevUserSecFile() {
            BrowseRead r = usrsec.readPrev();
            switch (r.response().resp()) {
                case NORMAL -> {
                    secUserData = r.record();
                    secUsrId = r.record().secUsrId();
                }
                case ENDFILE -> {
                    userSecEof = true;
                    wsMessage = Cobol.alnum(MSG_REACHED_TOP, 80);
                    map.usridin.setL(-1);
                    sendUsrlstScreen();
                }
                default -> fileError(r.response());
            }
        }

        private void fileError(CicsResponse r) {
            displays.add("RESP:" + Cobol.numericDisplay(r.resp().code(), 9) + "REAS:"
                    + Cobol.numericDisplay(r.resp2(), 9));
            errFlg = true;
            wsMessage = Cobol.alnum(MSG_UNABLE_TO_LOOKUP, 80);
            map.usridin.setL(-1);
            sendUsrlstScreen();
        }

        // ENDBR-USER-SEC-FILE
        void endbrUserSecFile() {
            CicsResponse r = usrsec.endBr();
            if (r.resp() != CicsResp.NORMAL) {
                throw new CicsAbendException(r.resp().abendCode(), r, sends);
            }
        }
    }
}
