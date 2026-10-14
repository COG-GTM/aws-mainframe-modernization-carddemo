package com.carddemo.online.comen01c;

import com.carddemo.common.copybook.CardDemoCommarea;
import com.carddemo.common.copybook.CommonMessages;
import com.carddemo.common.copybook.DateTimeFields;
import com.carddemo.common.copybook.ScreenTitles;
import com.carddemo.runtime.bms.Dfhbmsca;
import com.carddemo.runtime.bms.SymbolicMap;
import com.carddemo.runtime.cics.CicsContext;
import com.carddemo.runtime.cics.CicsContext.SendMapOption;
import com.carddemo.runtime.cics.CicsProgram;
import com.carddemo.runtime.cics.CicsResp;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.cobol.RecordLayout;
import java.util.Optional;
import org.springframework.stereotype.Component;

/**
 * COMEN01C - CardDemo main menu for regular users (transaction CM00, mapset COMEN01 / map COMEN1A).
 * Each method is the COBOL paragraph of the same name; line numbers refer to app/cbl/COMEN01C.cbl.
 */
@Component
public class Comen01c implements CicsProgram {

    public static final String PROGRAM = "COMEN01C";
    public static final String TRANSID = "CM00";
    public static final String MAPSET = "COMEN01";
    public static final String MAP = "COMEN1A";
    public static final String SIGNON_PROGRAM = "COSGN00C";
    public static final String PENDING_AUTH_PROGRAM = "COPAUS0C";

    static final String MSG_INVALID_OPTION = "Please enter a valid option number...";
    static final String MSG_ADMIN_ONLY = "No access - Admin Only option... ";

    private final MenuOptions menuOptions;

    public Comen01c() {
        this(MenuOptions.comen02y());
    }

    /** With a different COMEN02Y table (tests of table-driven paths the shipped table never takes). */
    Comen01c(MenuOptions menuOptions) {
        this.menuOptions = menuOptions;
    }

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
        new Task(ctx, menuOptions).mainPara();
    }

    /** One task's WORKING-STORAGE (lines 35-58) plus the procedure division. */
    static final class Task {
        private final CicsContext ctx;

        // WS-VARIABLES (lines 35-48)
        final String wsPgmname = PicX.of(PROGRAM, 8);
        final String wsTranid = PicX.of(TRANSID, 4);
        String wsMessage = PicX.spaces(80);
        final String wsUsrsecFile = PicX.of("USRSEC", 8);
        String wsErrFlg = "N";
        int wsRespCd;
        int wsReasCd;
        String wsOptionX = PicX.lowValues(2);
        String wsOption = "00";
        int wsIdx;
        String wsMenuOptTxt = PicX.spaces(40);

        // COPY COCOM01Y / COMEN02Y / COMEN01 (COCOM01Y and the map have no VALUE clauses: LOW-VALUES)
        final CardDemoCommarea commarea = new CardDemoCommarea();
        final MenuOptions menu;
        final SymbolicMap comen1a = SymbolicMap.of(MAPSET, MAP);

        Task(CicsContext ctx, MenuOptions menu) {
            this.ctx = ctx;
            this.menu = menu;
        }

        boolean errFlgOn() {
            return "Y".equals(wsErrFlg);
        }

        /** {@code WS-OPTION IS NUMERIC} for PIC 9(02) DISPLAY: both bytes are digits. */
        boolean wsOptionNumeric() {
            return wsOption.chars().allMatch(c -> c >= '0' && c <= '9');
        }

        /** Value of WS-OPTION used as a subscript (only meaningful when numeric). */
        int wsOptionValue() {
            return wsOptionNumeric() ? Integer.parseInt(wsOption) : -1;
        }

        /** MAIN-PARA (lines 75-110). */
        void mainPara() {
            wsErrFlg = "N";
            wsMessage = PicX.spaces(80);
            comen1a.setData("ERRMSG", "");

            if (ctx.eib().eibcalen() == 0) {
                commarea.setFromProgram(SIGNON_PROGRAM);
                returnToSignonScreen();
            } else {
                commarea.setBytes(ctx.commarea());
                if (!commarea.isPgmReenter()) {
                    commarea.setPgmContext(1);
                    comen1a.lowValues();
                    sendMenuScreen();
                } else {
                    receiveMenuScreen();
                    switch (ctx.eib().eibaid()) {
                        case ENTER -> processEnterKey();
                        case PF3 -> {
                            commarea.setToProgram(SIGNON_PROGRAM);
                            returnToSignonScreen();
                        }
                        default -> {
                            wsErrFlg = "Y";
                            wsMessage = PicX.of(CommonMessages.CCDA_MSG_INVALID_KEY, 80);
                            sendMenuScreen();
                        }
                    }
                }
            }

            ctx.returnTransid(wsTranid, commarea);
        }

        /** PROCESS-ENTER-KEY (lines 115-191). */
        void processEnterKey() {
            String optionI = comen1a.data("OPTION");
            wsIdx = optionI.length();
            while (!(optionI.charAt(wsIdx - 1) != ' ' || wsIdx == 1)) {
                wsIdx--;
            }
            wsOptionX = PicX.justifiedRight(optionI.substring(0, wsIdx), 2);
            wsOptionX = wsOptionX.replace(' ', '0');
            wsOption = moveToUnsignedDisplay(wsOptionX, 2);
            comen1a.setData("OPTION", wsOption);

            if (!wsOptionNumeric() || wsOptionValue() > menu.count() || wsOptionValue() == 0) {
                wsErrFlg = "Y";
                wsMessage = PicX.of(MSG_INVALID_OPTION, 80);
                sendMenuScreen();
            }

            if (commarea.isUser() && wsOptionNumeric() && "A".equals(menu.usrType(wsOptionValue()))) {
                wsErrFlg = "Y";
                wsMessage = PicX.spaces(80);
                wsMessage = PicX.of(MSG_ADMIN_ONLY, 80);
                sendMenuScreen();
            }

            if (!errFlgOn()) {
                int opt = wsOptionValue();
                String pgm = menu.pgmName(opt);
                if (pgm.equals(PENDING_AUTH_PROGRAM)) {
                    ctx.inquireProgram(pgm);
                    if (ctx.eib().eibresp() == CicsResp.NORMAL.code()) {
                        commarea.setFromTranid(wsTranid);
                        commarea.setFromProgram(wsPgmname);
                        commarea.setPgmContext(0);
                        ctx.xctl(pgm, commarea);
                    } else {
                        wsMessage = PicX.spaces(80);
                        comen1a.setColor("ERRMSG", Dfhbmsca.DFHRED);
                        wsMessage = stringInto(wsMessage, "This option ", delimitedBy(menu.name(opt), "  "),
                                " is not installed...");
                    }
                } else if (pgm.startsWith("DUMMY")) {
                    wsMessage = PicX.spaces(80);
                    comen1a.setColor("ERRMSG", Dfhbmsca.DFHGREEN);
                    wsMessage = stringInto(wsMessage, "This option ", delimitedBy(menu.name(opt), " "),
                            "is coming soon ...");
                } else {
                    commarea.setFromTranid(wsTranid);
                    commarea.setFromProgram(wsPgmname);
                    commarea.setFromProgram(wsPgmname);
                    commarea.setPgmContext(0);
                    ctx.xctl(pgm, commarea);
                }

                sendMenuScreen();
            }
        }

        /** RETURN-TO-SIGNON-SCREEN (lines 196-203): XCTL without COMMAREA. */
        void returnToSignonScreen() {
            if (PicX.isLowValues(commarea.toProgram()) || PicX.isSpaces(commarea.toProgram())) {
                commarea.setToProgram(SIGNON_PROGRAM);
            }
            ctx.xctl(commarea.toProgram(), null);
        }

        /** SEND-MENU-SCREEN (lines 208-220). */
        void sendMenuScreen() {
            populateHeaderInfo();
            buildMenuOptions();
            comen1a.setData("ERRMSG", wsMessage);
            ctx.sendMap(comen1a, SendMapOption.ERASE);
        }

        /** RECEIVE-MENU-SCREEN (lines 225-233). */
        void receiveMenuScreen() {
            CicsResp resp = ctx.receiveMap(comen1a);
            wsRespCd = resp.code();
            wsReasCd = ctx.eib().eibresp2();
        }

        /** POPULATE-HEADER-INFO (lines 238-257). */
        void populateHeaderInfo() {
            DateTimeFields dt = DateTimeFields.fromCurrentDate(ctx.currentDate());
            comen1a.setData("TITLE01", ScreenTitles.CCDA_TITLE01);
            comen1a.setData("TITLE02", ScreenTitles.CCDA_TITLE02);
            comen1a.setData("TRNNAME", wsTranid);
            comen1a.setData("PGMNAME", wsPgmname);
            comen1a.setData("CURDATE", dt.curdateMmDdYy());
            comen1a.setData("CURTIME", dt.curtimeHhMmSs());
        }

        /** BUILD-MENU-OPTIONS (lines 262-303). */
        void buildMenuOptions() {
            for (wsIdx = 1; wsIdx <= menu.count(); wsIdx++) {
                wsMenuOptTxt = PicX.spaces(40);
                wsMenuOptTxt = stringInto(wsMenuOptTxt, menu.num(wsIdx), ". ", menu.name(wsIdx));
                if (wsIdx <= 12) {
                    comen1a.setData(String.format("OPTN%03d", wsIdx), wsMenuOptTxt);
                }
            }
        }

        /**
         * MOVE alphanumeric TO PIC 9(n) DISPLAY, as GnuCOBOL does it (golden file comen01c-golden.txt): the
         * sending value is read left to right as a number - digits are kept, '+', '-' and ',' are skipped, '.'
         * ends the integer part and any other character ends the scan - then right-aligned and zero-filled.
         * So {@code "1A"} gives {@code "01"}, {@code "A1"} gives {@code "00"}, {@code "-1"} gives {@code "01"}.
         */
        static String moveToUnsignedDisplay(String source, int digits) {
            StringBuilder integer = new StringBuilder();
            for (char c : source.toCharArray()) {
                if (c >= '0' && c <= '9') {
                    integer.append(c);
                } else if (c == '+' || c == '-' || c == ',') {
                    continue;
                } else {
                    break;
                }
            }
            String value = integer.length() > digits ? integer.substring(integer.length() - digits) : integer.toString();
            return "0".repeat(digits - value.length()) + value;
        }

        /** STRING a DELIMITED BY SIZE b ... INTO target (no POINTER): overlays from position 1, stops at the end. */
        static String stringInto(String target, String... parts) {
            String joined = String.join("", parts);
            if (joined.length() >= target.length()) {
                return joined.substring(0, target.length());
            }
            return joined + target.substring(joined.length());
        }

        /** The part of {@code source} before the first occurrence of {@code delimiter} (whole source if none). */
        static String delimitedBy(String source, String delimiter) {
            int at = source.indexOf(delimiter);
            return at < 0 ? source : source.substring(0, at);
        }
    }
}
