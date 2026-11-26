package com.carddemo.programs.cosgn00c;

import static com.carddemo.programs.cosgn00c.copybook.CobolText.isSpacesOrLowValues;
import static com.carddemo.programs.cosgn00c.copybook.CobolText.picX;
import static com.carddemo.programs.cosgn00c.copybook.CobolText.spaces;
import static com.carddemo.programs.cosgn00c.copybook.CobolText.upperCase;

import com.carddemo.programs.cosgn00c.copybook.CardDemoCommarea;
import com.carddemo.programs.cosgn00c.copybook.CicsAid;
import com.carddemo.programs.cosgn00c.copybook.CommonMessages;
import com.carddemo.programs.cosgn00c.copybook.Cosgn0aField;
import com.carddemo.programs.cosgn00c.copybook.Cosgn0aInput;
import com.carddemo.programs.cosgn00c.copybook.Cosgn0aOutput;
import com.carddemo.programs.cosgn00c.copybook.DateTimeWorkArea;
import com.carddemo.programs.cosgn00c.copybook.ScreenTitles;

import java.time.Clock;
import java.time.LocalDateTime;
import java.util.Objects;

/** COSGN00C - Signon Screen for the CardDemo Application (CICS transaction CC00). */
public final class SignonService {

    public static final String WS_PGMNAME = "COSGN00C";
    public static final String WS_TRANID = "CC00";
    public static final String ADMIN_MENU_PROGRAM = "COADM01C";
    public static final String USER_MENU_PROGRAM = "COMEN01C";

    public static final String MSG_ENTER_USER_ID = "Please enter User ID ...";
    public static final String MSG_ENTER_PASSWORD = "Please enter Password ...";
    public static final String MSG_WRONG_PASSWORD = "Wrong Password. Try again ...";
    public static final String MSG_USER_NOT_FOUND = "User not found. Try again ...";
    public static final String MSG_UNABLE_TO_VERIFY = "Unable to verify the User ...";

    private static final int WS_MESSAGE_LEN = 80;
    private static final int WS_USER_ID_LEN = 8;
    private static final int WS_USER_PWD_LEN = 8;

    private final UserSecurityRepository userSecurity;
    private final CicsSystemInfo systemInfo;
    private final Clock clock;

    public SignonService(UserSecurityRepository userSecurity, CicsSystemInfo systemInfo, Clock clock) {
        this.userSecurity = Objects.requireNonNull(userSecurity);
        this.systemInfo = Objects.requireNonNull(systemInfo);
        this.clock = Objects.requireNonNull(clock);
    }

    /** Runs one task. Working storage is fresh per task, exactly as for a CICS program. */
    public SignonOutcome run(SignonRequest request) {
        return new Task(request).mainPara();
    }

    /** One task's WORKING-STORAGE plus the paragraphs operating on it. */
    private final class Task {
        private final SignonRequest request;

        // WS-VARIABLES
        private String wsMessage = spaces(WS_MESSAGE_LEN);   // WS-MESSAGE PIC X(80)
        private boolean errFlg;                               // WS-ERR-FLG, 88 ERR-FLG-ON / ERR-FLG-OFF
        private String wsUserId = spaces(WS_USER_ID_LEN);     // WS-USER-ID PIC X(08)
        private String wsUserPwd = spaces(WS_USER_PWD_LEN);   // WS-USER-PWD PIC X(08)

        private final CardDemoCommarea commarea = new CardDemoCommarea();   // COCOM01Y
        private final Cosgn0aOutput mapOut = new Cosgn0aOutput();          // COSGN00 (COSGN0AO)
        private final DateTimeWorkArea dateTime = new DateTimeWorkArea();   // CSDAT01Y
        private Cosgn0aInput mapIn;                                         // COSGN00 (COSGN0AI)

        private SignonOutcome outcome;

        Task(SignonRequest request) {
            this.request = Objects.requireNonNull(request);
        }

        // MAIN-PARA
        SignonOutcome mainPara() {
            errFlg = false;
            wsMessage = spaces(WS_MESSAGE_LEN);
            mapOut.errmsgO(spaces(Cosgn0aField.ERRMSG.length()));

            if (request.eibcalen() == 0) {
                mapOut.clearToLowValues();
                mapOut.cursorField(Cosgn0aField.USERID);
                sendSignonScreen();
            } else {
                CicsAid aid = request.eibaid();
                if (aid == CicsAid.DFHENTER) {
                    processEnterKey();
                } else if (aid == CicsAid.DFHPF3) {
                    wsMessage = picX(CommonMessages.CCDA_MSG_THANK_YOU, WS_MESSAGE_LEN);
                    sendPlainText();
                } else {
                    errFlg = true;
                    wsMessage = picX(CommonMessages.CCDA_MSG_INVALID_KEY, WS_MESSAGE_LEN);
                    sendSignonScreen();
                }
            }

            // XCTL and SEND-PLAIN-TEXT's RETURN never fall through to the final RETURN.
            if (outcome instanceof SignonOutcome.ControlTransferred || outcome instanceof SignonOutcome.TextSent) {
                return outcome;
            }
            return new SignonOutcome.ScreenSent(mapOut, wsMessage, errFlg, WS_TRANID, commarea);
        }

        // PROCESS-ENTER-KEY
        private void processEnterKey() {
            mapIn = request.map() == null ? Cosgn0aInput.of(null, null) : request.map();
            mapOut.overlayReceived(mapIn);

            if (isSpacesOrLowValues(mapIn.useridI())) {
                errFlg = true;
                wsMessage = picX(MSG_ENTER_USER_ID, WS_MESSAGE_LEN);
                mapOut.cursorField(Cosgn0aField.USERID);
                sendSignonScreen();
            } else if (isSpacesOrLowValues(mapIn.passwdI())) {
                errFlg = true;
                wsMessage = picX(MSG_ENTER_PASSWORD, WS_MESSAGE_LEN);
                mapOut.cursorField(Cosgn0aField.PASSWD);
                sendSignonScreen();
            }

            // Executed even after a validation error, as in the COBOL.
            String upperUserId = upperCase(mapIn.useridI());
            wsUserId = picX(upperUserId, WS_USER_ID_LEN);
            commarea.userId(upperUserId);
            wsUserPwd = picX(upperCase(mapIn.passwdI()), WS_USER_PWD_LEN);

            if (!errFlg) {
                readUserSecFile();
            }
        }

        // SEND-SIGNON-SCREEN
        private void sendSignonScreen() {
            populateHeaderInfo();
            mapOut.errmsgO(wsMessage);
            outcome = null;
        }

        // SEND-PLAIN-TEXT
        private void sendPlainText() {
            outcome = new SignonOutcome.TextSent(wsMessage);
        }

        // POPULATE-HEADER-INFO
        private void populateHeaderInfo() {
            dateTime.moveCurrentDate(LocalDateTime.now(clock));

            mapOut.title01O(ScreenTitles.CCDA_TITLE01);
            mapOut.title02O(ScreenTitles.CCDA_TITLE02);
            mapOut.trnnameO(WS_TRANID);
            mapOut.pgmnameO(WS_PGMNAME);

            dateTime.buildMmDdYy();
            mapOut.curdateO(dateTime.curdateMmDdYy());

            dateTime.buildHhMmSs();
            mapOut.curtimeO(dateTime.curtimeHhMmSs());

            mapOut.applidO(systemInfo.applid());
            mapOut.sysidO(systemInfo.sysid());
        }

        // READ-USER-SEC-FILE
        private void readUserSecFile() {
            UserSecurityRepository.ReadResult result = userSecurity.read(wsUserId);

            switch (result.resp()) {
                case CicsResp.NORMAL -> {
                    if (result.record().pwd().equals(wsUserPwd)) {
                        commarea.fromTranid(WS_TRANID);
                        commarea.fromProgram(WS_PGMNAME);
                        commarea.userId(wsUserId);
                        commarea.userType(result.record().type());
                        commarea.pgmContext(0);

                        if (commarea.isUserTypeAdmin()) {
                            outcome = new SignonOutcome.ControlTransferred(ADMIN_MENU_PROGRAM, commarea);
                        } else {
                            outcome = new SignonOutcome.ControlTransferred(USER_MENU_PROGRAM, commarea);
                        }
                    } else {
                        wsMessage = picX(MSG_WRONG_PASSWORD, WS_MESSAGE_LEN);
                        mapOut.cursorField(Cosgn0aField.PASSWD);
                        sendSignonScreen();
                    }
                }
                case CicsResp.NOTFND -> {
                    errFlg = true;
                    wsMessage = picX(MSG_USER_NOT_FOUND, WS_MESSAGE_LEN);
                    mapOut.cursorField(Cosgn0aField.USERID);
                    sendSignonScreen();
                }
                default -> {
                    errFlg = true;
                    wsMessage = picX(MSG_UNABLE_TO_VERIFY, WS_MESSAGE_LEN);
                    mapOut.cursorField(Cosgn0aField.USERID);
                    sendSignonScreen();
                }
            }
        }
    }
}
