package com.carddemo.programs.comen01c;

import com.carddemo.programs.comen01c.copybook.CardDemoCommarea;
import com.carddemo.programs.comen01c.copybook.CobolText;
import com.carddemo.programs.comen01c.copybook.Comen01Map;
import com.carddemo.programs.comen01c.copybook.CommonMessages;
import com.carddemo.programs.comen01c.copybook.Dfhaid;
import com.carddemo.programs.comen01c.copybook.Dfhbmsca;
import com.carddemo.programs.comen01c.copybook.MainMenuOptions;
import com.carddemo.programs.comen01c.copybook.MainMenuOptions.MenuOption;
import com.carddemo.programs.comen01c.copybook.ScreenTitle;
import com.carddemo.programs.comen01c.copybook.WsDateTime;
import com.carddemo.programs.comen01c.dto.MenuScreenRequest;
import com.carddemo.programs.comen01c.dto.MenuScreenResponse;
import com.carddemo.programs.comen01c.gateway.ProgramInquiryGateway;
import com.carddemo.programs.comen01c.gateway.ScreenGateway;

import java.time.Clock;
import java.time.LocalDateTime;

/**
 * COMEN01C - Main Menu for the regular users (transaction CM00).
 *
 * <p>One call to {@link #execute(CicsRequest)} is one pseudo-conversational CICS task. Paragraphs are
 * ported as methods; XCTL (which never returns in CICS) is modelled with {@link XctlSignal}.
 */
public final class Comen01cService {

    public static final String WS_PGMNAME = "COMEN01C";
    public static final String WS_TRANID = "CM00";
    public static final String WS_USRSEC_FILE = "USRSEC  ";
    public static final String SIGNON_PROGRAM = "COSGN00C";

    public static final String MSG_INVALID_OPTION = "Please enter a valid option number...";
    public static final String MSG_ADMIN_ONLY = "No access - Admin Only option... ";

    private final ScreenGateway screen;
    private final ProgramInquiryGateway programs;
    private final MainMenuOptions menu;
    private final Clock clock;

    public Comen01cService(ScreenGateway screen, ProgramInquiryGateway programs) {
        this(screen, programs, MainMenuOptions.standard(), Clock.systemDefaultZone());
    }

    public Comen01cService(ScreenGateway screen, ProgramInquiryGateway programs, MainMenuOptions menu, Clock clock) {
        this.screen = screen;
        this.programs = programs;
        this.menu = menu;
        this.clock = clock;
    }

    /** Per-task WORKING-STORAGE. */
    private static final class Ws {
        String message = CobolText.spaces(80);   // WS-MESSAGE PIC X(80)
        boolean errFlg;                          // WS-ERR-FLG: 88 ERR-FLG-ON 'Y' / ERR-FLG-OFF 'N'
        String optionX = CobolText.spaces(2);    // WS-OPTION-X PIC X(02) JUST RIGHT
        String option = "00";                    // WS-OPTION PIC 9(02) (DISPLAY bytes)
        CardDemoCommarea commarea = new CardDemoCommarea();
        final Comen01Map map = new Comen01Map();
        final WsDateTime dateTime = new WsDateTime();

        void message(String text) {
            message = CobolText.move(text, 80);
        }
    }

    private static final class XctlSignal extends RuntimeException {
        final ProgramOutcome.Xctl xctl;

        XctlSignal(String program, CardDemoCommarea commarea) {
            super(null, null, false, false);
            this.xctl = new ProgramOutcome.Xctl(program.strip(), commarea == null ? null : commarea.toBytes());
        }
    }

    // MAIN-PARA
    public ProgramOutcome execute(CicsRequest request) {
        Ws ws = new Ws();
        try {
            ws.errFlg = false;
            ws.message = CobolText.spaces(80);
            ws.map.errmsg().move("");

            if (request.eibcalen() == 0) {
                ws.commarea.fromProgram(SIGNON_PROGRAM);
                returnToSignonScreen(ws);
            } else {
                ws.commarea = CardDemoCommarea.fromBytes(request.commarea());
                if (!ws.commarea.isPgmReenter()) {
                    ws.commarea.setPgmContext(CardDemoCommarea.PgmContext.REENTER);
                    ws.map.lowValues();
                    sendMenuScreen(ws);
                } else {
                    receiveMenuScreen(ws);
                    Dfhaid aid = request.eibaid();
                    if (aid == Dfhaid.DFHENTER) {
                        processEnterKey(ws);
                    } else if (aid == Dfhaid.DFHPF3) {
                        ws.commarea.toProgram(SIGNON_PROGRAM);
                        returnToSignonScreen(ws);
                    } else {
                        ws.errFlg = true;
                        ws.message(CommonMessages.CCDA_MSG_INVALID_KEY);
                        sendMenuScreen(ws);
                    }
                }
            }
            return new ProgramOutcome.Return(WS_TRANID, ws.commarea.toBytes());
        } catch (XctlSignal signal) {
            return signal.xctl;
        }
    }

    // PROCESS-ENTER-KEY
    private void processEnterKey(Ws ws) {
        String optionI = ws.map.option().data();
        int idx = optionI.length();
        while (!(optionI.charAt(idx - 1) != ' ' || idx == 1)) {
            idx--;
        }
        ws.optionX = CobolText.moveJustRight(optionI.substring(0, idx), 2);
        ws.optionX = ws.optionX.replace(' ', '0');
        ws.option = ws.optionX;
        ws.map.option().move(ws.option);

        boolean numeric = CobolText.isNumeric(ws.option);
        int option = numeric ? Integer.parseInt(ws.option) : -1;

        if (!numeric || option > menu.count() || option == 0) {
            ws.errFlg = true;
            ws.message(MSG_INVALID_OPTION);
            sendMenuScreen(ws);
        }

        MenuOption selected = menu.option(option);
        if (ws.commarea.isUsrtypUser() && selected.isAdminOnly()) {
            ws.errFlg = true;
            ws.message = CobolText.spaces(80);
            ws.message(MSG_ADMIN_ONLY);
            sendMenuScreen(ws);
        }

        if (!ws.errFlg) {
            if (selected.pgmName().equals("COPAUS0C")) {
                if (programs.isInstalled(selected.pgmName())) {
                    ws.commarea.fromTranid(WS_TRANID);
                    ws.commarea.fromProgram(WS_PGMNAME);
                    ws.commarea.pgmContext(0);
                    throw new XctlSignal(selected.pgmName(), ws.commarea);
                }
                ws.message = CobolText.spaces(80);
                ws.map.errmsg().color(Dfhbmsca.DFHRED);
                ws.message = CobolText.stringInto(ws.message,
                        "This option ",
                        CobolText.delimitedBy(selected.name(), "  "),
                        " is not installed...");
            } else if (selected.pgmName().startsWith("DUMMY")) {
                ws.message = CobolText.spaces(80);
                ws.map.errmsg().color(Dfhbmsca.DFHGREEN);
                ws.message = CobolText.stringInto(ws.message,
                        "This option ",
                        CobolText.delimitedBy(selected.name(), " "),
                        "is coming soon ...");
            } else {
                ws.commarea.fromTranid(WS_TRANID);
                ws.commarea.fromProgram(WS_PGMNAME);
                ws.commarea.pgmContext(0);
                throw new XctlSignal(selected.pgmName(), ws.commarea);
            }
            sendMenuScreen(ws);
        }
    }

    // RETURN-TO-SIGNON-SCREEN
    private void returnToSignonScreen(Ws ws) {
        if (CobolText.isSpacesOrLowValues(ws.commarea.toProgram())) {
            ws.commarea.toProgram(SIGNON_PROGRAM);
        }
        throw new XctlSignal(ws.commarea.toProgram(), null);
    }

    // SEND-MENU-SCREEN
    private void sendMenuScreen(Ws ws) {
        populateHeaderInfo(ws);
        buildMenuOptions(ws);
        ws.map.errmsg().move(ws.message);
        screen.send(MenuScreenResponse.from(ws.map, true));
    }

    // RECEIVE-MENU-SCREEN
    private void receiveMenuScreen(Ws ws) {
        MenuScreenRequest in = screen.receive();
        Comen01Map.MapField option = ws.map.option();
        if (in.optionI() == null) {
            option.length((short) 0);
            option.store(CobolText.lowValues(2));
        } else {
            option.length((short) in.optionI().length());
            option.store(in.optionI());
        }
    }

    // POPULATE-HEADER-INFO
    private void populateHeaderInfo(Ws ws) {
        ws.dateTime.moveCurrentDate(LocalDateTime.now(clock));

        ws.map.title01().move(ScreenTitle.CCDA_TITLE01);
        ws.map.title02().move(ScreenTitle.CCDA_TITLE02);
        ws.map.trnname().move(WS_TRANID);
        ws.map.pgmname().move(WS_PGMNAME);

        ws.dateTime.curdateMm(ws.dateTime.curdateMonth());
        ws.dateTime.curdateDd(ws.dateTime.curdateDay());
        ws.dateTime.curdateYy(ws.dateTime.curdateYear() % 100);
        ws.map.curdate().move(ws.dateTime.curdateMmDdYy());

        ws.dateTime.curtimeHh(ws.dateTime.curtimeHours());
        ws.dateTime.curtimeMm(ws.dateTime.curtimeMinute());
        ws.dateTime.curtimeSs(ws.dateTime.curtimeSecond());
        ws.map.curtime().move(ws.dateTime.curtimeHhMmSs());
    }

    // BUILD-MENU-OPTIONS
    private void buildMenuOptions(Ws ws) {
        for (int idx = 1; idx <= menu.count(); idx++) {
            MenuOption opt = menu.option(idx);
            String text = CobolText.stringInto(CobolText.spaces(40), opt.numDisplay(), ". ", opt.name());
            if (idx <= Comen01Map.OPTION_LINES) {
                ws.map.optn(idx).move(text);
            }
        }
    }
}
