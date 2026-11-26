package com.carddemo.programs.coadm01c;

import com.carddemo.programs.coadm01c.copybook.AdminMenuOptions;
import com.carddemo.programs.coadm01c.copybook.CardDemoCommarea;
import com.carddemo.programs.coadm01c.copybook.Coadm1aOutput;
import com.carddemo.programs.coadm01c.copybook.CobolFields;
import com.carddemo.programs.coadm01c.copybook.CommonMessages;
import com.carddemo.programs.coadm01c.copybook.DateTimeFields;
import com.carddemo.programs.coadm01c.copybook.Dfhaid;
import com.carddemo.programs.coadm01c.copybook.Dfhbmsca;
import com.carddemo.programs.coadm01c.copybook.ScreenTitle;

import java.time.Clock;
import java.time.LocalDateTime;
import java.util.Objects;

/** COADM01C - CardDemo admin menu (transaction CA00, map COADM1A / mapset COADM01). */
public final class Coadm01cService {

    public static final String WS_PGMNAME = "COADM01C";
    public static final String WS_TRANID = "CA00";
    public static final String WS_USRSEC_FILE = "USRSEC  ";
    public static final String SIGNON_PROGRAM = "COSGN00C";
    public static final String MAP = "COADM1A";
    public static final String MAPSET = "COADM01";

    public static final String MSG_INVALID_OPTION = "Please enter a valid option number...";
    public static final String MSG_NOT_INSTALLED = "This option is not installed ...";

    static final int WS_MESSAGE_LENGTH = 80;
    static final int WS_ADMIN_OPT_TXT_LENGTH = 40;
    static final int WS_OPTION_LENGTH = 2;

    private final ProgramControlGateway programControl;
    private final Clock clock;
    private final AdminMenuOptions menuOptions;

    public Coadm01cService(ProgramControlGateway programControl, Clock clock) {
        this(programControl, clock, AdminMenuOptions.standard());
    }

    public Coadm01cService(ProgramControlGateway programControl, Clock clock, AdminMenuOptions menuOptions) {
        this.programControl = Objects.requireNonNull(programControl);
        this.clock = Objects.requireNonNull(clock);
        this.menuOptions = Objects.requireNonNull(menuOptions);
    }

    /** Working storage of one task. */
    private static final class Task {
        String wsMessage = CobolFields.spaces(WS_MESSAGE_LENGTH);
        boolean errFlg;
        int wsRespCd;
        CardDemoCommarea commarea = new CardDemoCommarea();
        Coadm1aOutput screen = Coadm1aOutput.lowValues();
        Coadm1aOutput sent;
    }

    // MAIN-PARA
    public Coadm01cOutcome execute(Coadm01cRequest request) {
        Task t = new Task();
        // EXEC CICS HANDLE CONDITION PGMIDERR(PGMIDERR-ERR-PARA)
        try {
            t.errFlg = false;
            t.wsMessage = CobolFields.spaces(WS_MESSAGE_LENGTH);
            t.screen.setErrmsg(null);

            if (request.eibcalen() == 0) {
                t.commarea.setFromProgram(SIGNON_PROGRAM);
                return returnToSignonScreen(t);
            }
            t.commarea = CardDemoCommarea.fromRecord(request.dfhCommarea());
            if (!t.commarea.isPgmReenter()) {
                t.commarea.setPgmContext(CardDemoCommarea.PgmContext.REENTER);
                t.screen = Coadm1aOutput.lowValues();
                sendMenuScreen(t);
            } else {
                receiveMenuScreen(t, request);
                Dfhaid aid = request.eibAid();
                if (aid == Dfhaid.DFHENTER) {
                    Coadm01cOutcome transferred = processEnterKey(t);
                    if (transferred != null) {
                        return transferred;
                    }
                } else if (aid == Dfhaid.DFHPF3) {
                    t.commarea.setToProgram(SIGNON_PROGRAM);
                    return returnToSignonScreen(t);
                } else {
                    t.errFlg = true;
                    t.wsMessage = CobolFields.alnum(CommonMessages.CCDA_MSG_INVALID_KEY, WS_MESSAGE_LENGTH);
                    sendMenuScreen(t);
                }
            }
            return new Coadm01cOutcome.Returned(WS_TRANID, t.commarea.copy(), t.sent);
        } catch (PgmIdErrException e) {
            return pgmidErrErrPara(t);
        }
    }

    // PROCESS-ENTER-KEY
    private Coadm01cOutcome processEnterKey(Task t) throws PgmIdErrException {
        String optionI = t.screen.option();
        int wsIdx = Coadm1aOutput.OPTION_LENGTH;
        while (!(optionI.charAt(wsIdx - 1) != ' ' || wsIdx == 1)) {
            wsIdx--;
        }
        String wsOptionX = CobolFields.justRight(optionI.substring(0, wsIdx), WS_OPTION_LENGTH)
                .replace(' ', '0');
        // MOVE WS-OPTION-X TO WS-OPTION (PIC 9(02)): same-length byte copy
        String wsOption = wsOptionX;
        t.screen.setOption(wsOption);

        if (!CobolFields.isNumeric(wsOption)
                || Integer.parseInt(wsOption) > menuOptions.count()
                || Integer.parseInt(wsOption) == 0) {
            t.errFlg = true;
            t.wsMessage = CobolFields.alnum(MSG_INVALID_OPTION, WS_MESSAGE_LENGTH);
            sendMenuScreen(t);
        }

        if (!t.errFlg) {
            String pgmName = menuOptions.option(Integer.parseInt(wsOption)).pgmName();
            if (!pgmName.substring(0, 5).equals("DUMMY")) {
                t.commarea.setFromTranid(WS_TRANID);
                t.commarea.setFromProgram(WS_PGMNAME);
                t.commarea.setPgmContext(CardDemoCommarea.PgmContext.ENTER);
                return xctl(pgmName, t.commarea);
            }
            t.wsMessage = CobolFields.alnum(MSG_NOT_INSTALLED, WS_MESSAGE_LENGTH);
            t.screen.setErrmsgc(Dfhbmsca.DFHGREEN);
            sendMenuScreen(t);
        }
        return null;
    }

    // RETURN-TO-SIGNON-SCREEN
    private Coadm01cOutcome returnToSignonScreen(Task t) throws PgmIdErrException {
        if (CobolFields.isLowValuesOrSpaces(t.commarea.toProgram())) {
            t.commarea.setToProgram(SIGNON_PROGRAM);
        }
        return xctl(t.commarea.toProgram(), null);
    }

    // SEND-MENU-SCREEN
    private void sendMenuScreen(Task t) {
        populateHeaderInfo(t);
        buildMenuOptions(t);
        t.screen.setErrmsg(t.wsMessage);
        // EXEC CICS SEND MAP('COADM1A') MAPSET('COADM01') FROM(COADM1AO) ERASE
        t.sent = t.screen.copy();
    }

    // RECEIVE-MENU-SCREEN
    private void receiveMenuScreen(Task t, Coadm01cRequest request) {
        // EXEC CICS RECEIVE MAP('COADM1A') MAPSET('COADM01') INTO(COADM1AI) RESP(WS-RESP-CD)
        if (request.receivedMap() == null) {
            t.wsRespCd = 36; // DFHRESP(MAPFAIL): map area left unchanged, RESP not checked by the program
        } else {
            t.screen = Coadm1aOutput.fromInput(request.receivedMap(), t.screen);
            t.wsRespCd = 0;
        }
    }

    // POPULATE-HEADER-INFO
    private void populateHeaderInfo(Task t) {
        DateTimeFields now = DateTimeFields.currentDate(LocalDateTime.now(clock));
        t.screen.setTitle01(ScreenTitle.CCDA_TITLE01);
        t.screen.setTitle02(ScreenTitle.CCDA_TITLE02);
        t.screen.setTrnname(WS_TRANID);
        t.screen.setPgmname(WS_PGMNAME);
        t.screen.setCurdate(now.curdateMmDdYy());
        t.screen.setCurtime(now.curtimeHhMmSs());
    }

    // BUILD-MENU-OPTIONS
    private void buildMenuOptions(Task t) {
        for (int wsIdx = 1; wsIdx <= menuOptions.count(); wsIdx++) {
            AdminMenuOptions.AdminMenuOption opt = menuOptions.option(wsIdx);
            String wsAdminOptTxt = CobolFields.alnum(
                    CobolFields.unsigned(opt.num(), 2) + ". " + opt.name(), WS_ADMIN_OPT_TXT_LENGTH);
            if (wsIdx <= 10) {
                t.screen.setOptn(wsIdx, wsAdminOptTxt);
            }
        }
    }

    // PGMIDERR-ERR-PARA
    private Coadm01cOutcome pgmidErrErrPara(Task t) {
        t.wsMessage = CobolFields.alnum(MSG_NOT_INSTALLED, WS_MESSAGE_LENGTH);
        t.screen.setErrmsgc(Dfhbmsca.DFHGREEN);
        sendMenuScreen(t);
        return new Coadm01cOutcome.Returned(WS_TRANID, t.commarea.copy(), t.sent);
    }

    private Coadm01cOutcome xctl(String program, CardDemoCommarea commarea) throws PgmIdErrException {
        String target = program.stripTrailing();
        CardDemoCommarea passed = commarea == null ? null : commarea.copy();
        programControl.xctl(target, passed);
        return new Coadm01cOutcome.Transferred(target, passed);
    }
}
