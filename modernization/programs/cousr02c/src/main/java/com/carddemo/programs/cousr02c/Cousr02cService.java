package com.carddemo.programs.cousr02c;

import com.carddemo.programs.cousr02c.copybook.CardDemoCommarea;
import com.carddemo.programs.cousr02c.copybook.CicsResp;
import com.carddemo.programs.cousr02c.copybook.CommonMessages;
import com.carddemo.programs.cousr02c.copybook.Cousr2aMap;
import com.carddemo.programs.cousr02c.copybook.DateTimeFields;
import com.carddemo.programs.cousr02c.copybook.Dfhaid;
import com.carddemo.programs.cousr02c.copybook.Dfhbmsca;
import com.carddemo.programs.cousr02c.copybook.MapField;
import com.carddemo.programs.cousr02c.copybook.Pic;
import com.carddemo.programs.cousr02c.copybook.ScreenTitle;
import com.carddemo.programs.cousr02c.copybook.SecUserData;
import com.carddemo.programs.cousr02c.gateway.CicsResult;
import com.carddemo.programs.cousr02c.gateway.ReadResult;
import com.carddemo.programs.cousr02c.gateway.UserSecurityFile;

import java.time.Clock;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * COUSR02C - CardDemo "Update User" (transaction CU02): fetch a USRSEC record by user ID, let the operator edit
 * first/last name, password and user type, and REWRITE it.
 *
 * <p>Each {@link #execute} call is one pseudo-conversational CICS task. Paragraphs are kept as methods of
 * {@link Task}, in source order, so the COBOL control flow (including its quirks) is preserved.
 */
public final class Cousr02cService {

    static final String WS_PGMNAME = "COUSR02C";
    static final String WS_TRANID = "CU02";
    static final String WS_USRSEC_FILE = "USRSEC  ";

    static final String SIGNON_PROGRAM = "COSGN00C";
    static final String ADMIN_MENU_PROGRAM = "COADM01C";

    static final String MSG_USER_ID_EMPTY = "User ID can NOT be empty...";
    static final String MSG_FIRST_NAME_EMPTY = "First Name can NOT be empty...";
    static final String MSG_LAST_NAME_EMPTY = "Last Name can NOT be empty...";
    static final String MSG_PASSWORD_EMPTY = "Password can NOT be empty...";
    static final String MSG_USER_TYPE_EMPTY = "User Type can NOT be empty...";
    static final String MSG_PLEASE_MODIFY = "Please modify to update ...";
    static final String MSG_PRESS_PF5 = "Press PF5 key to save your updates ...";
    static final String MSG_USER_NOT_FOUND = "User ID NOT found...";
    static final String MSG_UNABLE_TO_LOOKUP = "Unable to lookup User...";
    static final String MSG_UNABLE_TO_UPDATE = "Unable to Update User...";

    private final UserSecurityFile usrsecFile;
    private final Clock clock;

    public Cousr02cService(UserSecurityFile usrsecFile, Clock clock) {
        this.usrsecFile = Objects.requireNonNull(usrsecFile);
        this.clock = Objects.requireNonNull(clock);
    }

    public Cousr02cResponse execute(Cousr02cRequest request) {
        Task task = new Task(request);
        try {
            task.mainPara();
        } catch (TaskEnded ended) {
            // XCTL: control never returns to this program
        } finally {
            usrsecFile.endTask();
        }
        return new Cousr02cResponse(List.copyOf(task.screens), task.outcome, List.copyOf(task.displayLines));
    }

    /** Raised by XCTL to abandon the rest of the task, as CICS does. */
    private static final class TaskEnded extends RuntimeException {
        TaskEnded() {
            super(null, null, false, false);
        }
    }

    /** Working storage of one task plus the PROCEDURE DIVISION. */
    private final class Task {

        private final Cousr02cRequest request;
        // 01 WS-VARIABLES
        private String wsMessage = Pic.spaces(80);          // X(80)
        private boolean errFlgOn;                           // WS-ERR-FLG 88 ERR-FLG-ON / ERR-FLG-OFF
        private int wsRespCd;                               // S9(09) COMP
        private int wsReasCd;                               // S9(09) COMP
        private boolean usrModifiedYes;                     // WS-USR-MODIFIED 88 USR-MODIFIED-YES / -NO
        // COPY COCOM01Y + CDEMO-CU02-INFO
        private CardDemoCommarea commarea = CardDemoCommarea.initialWorkingStorage();
        // COPY COUSR02
        private final Cousr2aMap map = new Cousr2aMap();
        // COPY CSDAT01Y
        private final DateTimeFields dateTime = new DateTimeFields();
        // COPY CSUSR01Y
        private SecUserData secUserData = SecUserData.lowValues();

        private final List<ScreenOutput> screens = new ArrayList<>();
        private final List<String> displayLines = new ArrayList<>();
        private TaskOutcome outcome;

        Task(Cousr02cRequest request) {
            this.request = request;
        }

        // MAIN-PARA
        void mainPara() {
            errFlgOn = false;
            usrModifiedYes = false;

            wsMessage = Pic.spaces(80);
            map.errmsg.setData(Pic.spaces(78));

            if (request.eibcalen() == 0) {
                commarea.setCdemoToProgram(SIGNON_PROGRAM);
                returnToPrevScreen();
            } else {
                commarea = CardDemoCommarea.fromBytes(request.dfhcommarea());
                if (!commarea.isCdemoPgmReenter()) {
                    commarea.setPgmContext(CardDemoCommarea.PgmContext.REENTER);
                    map.lowValues();
                    map.usridin.setCursor();
                    String selected = commarea.getCdemoCu02UsrSelected();
                    if (!Pic.isSpaces(selected) && !Pic.isLowValues(selected)) {
                        map.usridin.setData(selected);
                        processEnterKey();
                    }
                    sendUsrupdScreen();
                } else {
                    receiveUsrupdScreen();
                    Dfhaid aid = request.eibaid();
                    if (aid == Dfhaid.DFHENTER) {
                        processEnterKey();
                    } else if (aid == Dfhaid.DFHPF3) {
                        updateUserInfo();
                        String from = commarea.getCdemoFromProgram();
                        if (Pic.isSpacesOrLowValues(from)) {
                            commarea.setCdemoToProgram(ADMIN_MENU_PROGRAM);
                        } else {
                            commarea.setCdemoToProgram(from);
                        }
                        returnToPrevScreen();
                    } else if (aid == Dfhaid.DFHPF4) {
                        clearCurrentScreen();
                    } else if (aid == Dfhaid.DFHPF5) {
                        updateUserInfo();
                    } else if (aid == Dfhaid.DFHPF12) {
                        commarea.setCdemoToProgram(ADMIN_MENU_PROGRAM);
                        returnToPrevScreen();
                    } else {
                        errFlgOn = true;
                        wsMessage = Pic.x(CommonMessages.CCDA_MSG_INVALID_KEY, 80);
                        sendUsrupdScreen();
                    }
                }
            }

            // EXEC CICS RETURN TRANSID(WS-TRANID) COMMAREA(CARDDEMO-COMMAREA)
            outcome = new TaskOutcome.Return(WS_TRANID, commarea.toBytes());
        }

        // PROCESS-ENTER-KEY
        void processEnterKey() {
            if (Pic.isSpacesOrLowValues(map.usridin.getData())) {
                errFlgOn = true;
                wsMessage = Pic.x(MSG_USER_ID_EMPTY, 80);
                map.usridin.setCursor();
                sendUsrupdScreen();
            } else {
                map.usridin.setCursor();
            }

            if (!errFlgOn) {
                map.fname.setData(Pic.spaces(20));
                map.lname.setData(Pic.spaces(20));
                map.passwd.setData(Pic.spaces(8));
                map.usrtype.setData(Pic.spaces(1));
                secUserData.setSecUsrId(map.usridin.getData());
                readUserSecFile();
            }

            if (!errFlgOn) {
                map.fname.setData(secUserData.getSecUsrFname());
                map.lname.setData(secUserData.getSecUsrLname());
                map.passwd.setData(secUserData.getSecUsrPwd());
                map.usrtype.setData(secUserData.getSecUsrType());
                sendUsrupdScreen();
            }
        }

        // UPDATE-USER-INFO
        void updateUserInfo() {
            if (Pic.isSpacesOrLowValues(map.usridin.getData())) {
                validationError(MSG_USER_ID_EMPTY, map.usridin);
            } else if (Pic.isSpacesOrLowValues(map.fname.getData())) {
                validationError(MSG_FIRST_NAME_EMPTY, map.fname);
            } else if (Pic.isSpacesOrLowValues(map.lname.getData())) {
                validationError(MSG_LAST_NAME_EMPTY, map.lname);
            } else if (Pic.isSpacesOrLowValues(map.passwd.getData())) {
                validationError(MSG_PASSWORD_EMPTY, map.passwd);
            } else if (Pic.isSpacesOrLowValues(map.usrtype.getData())) {
                validationError(MSG_USER_TYPE_EMPTY, map.usrtype);
            } else {
                map.fname.setCursor();
            }

            if (!errFlgOn) {
                secUserData.setSecUsrId(map.usridin.getData());
                readUserSecFile();
                // NB: the COBOL does not test ERR-FLG after the READ, so a NOTFND / failed read still falls
                // through to the comparison and the REWRITE below.

                if (!map.fname.getData().equals(secUserData.getSecUsrFname())) {
                    secUserData.setSecUsrFname(map.fname.getData());
                    usrModifiedYes = true;
                }
                if (!map.lname.getData().equals(secUserData.getSecUsrLname())) {
                    secUserData.setSecUsrLname(map.lname.getData());
                    usrModifiedYes = true;
                }
                if (!map.passwd.getData().equals(secUserData.getSecUsrPwd())) {
                    secUserData.setSecUsrPwd(map.passwd.getData());
                    usrModifiedYes = true;
                }
                if (!map.usrtype.getData().equals(secUserData.getSecUsrType())) {
                    secUserData.setSecUsrType(map.usrtype.getData());
                    usrModifiedYes = true;
                }

                if (usrModifiedYes) {
                    updateUserSecFile();
                } else {
                    wsMessage = Pic.x(MSG_PLEASE_MODIFY, 80);
                    map.errmsg.setColor(Dfhbmsca.DFHRED);
                    sendUsrupdScreen();
                }
            }
        }

        private void validationError(String message, MapField cursorField) {
            errFlgOn = true;
            wsMessage = Pic.x(message, 80);
            cursorField.setCursor();
            sendUsrupdScreen();
        }

        // RETURN-TO-PREV-SCREEN
        void returnToPrevScreen() {
            if (Pic.isSpacesOrLowValues(commarea.getCdemoToProgram())) {
                commarea.setCdemoToProgram(SIGNON_PROGRAM);
            }
            commarea.setCdemoFromTranid(WS_TRANID);
            commarea.setCdemoFromProgram(WS_PGMNAME);
            commarea.setCdemoPgmContext(0);
            outcome = new TaskOutcome.Xctl(commarea.getCdemoToProgram(), commarea.toBytes());
            throw new TaskEnded();
        }

        // SEND-USRUPD-SCREEN
        void sendUsrupdScreen() {
            populateHeaderInfo();
            map.errmsg.setData(wsMessage);
            screens.add(ScreenOutput.of(map));
        }

        // RECEIVE-USRUPD-SCREEN
        void receiveUsrupdScreen() {
            MapInput in = request.mapInput() == null ? MapInput.empty() : request.mapInput();
            map.lowValues();
            receiveField(map.usridin, in.usridin());
            receiveField(map.fname, in.fname());
            receiveField(map.lname, in.lname());
            receiveField(map.passwd, in.passwd());
            receiveField(map.usrtype, in.usrtype());
            wsRespCd = CicsResp.NORMAL;
            wsReasCd = 0;
        }

        private void receiveField(MapField field, String value) {
            if (value == null) {
                return;
            }
            String data = Pic.x(value, field.size());
            field.setRawData(data);
            field.setLength((short) Math.min(value.length(), field.size()));
        }

        // POPULATE-HEADER-INFO
        void populateHeaderInfo() {
            dateTime.moveCurrentDate(LocalDateTime.now(clock));

            map.title01.setData(ScreenTitle.CCDA_TITLE01);
            map.title02.setData(ScreenTitle.CCDA_TITLE02);
            map.trnname.setData(WS_TRANID);
            map.pgmname.setData(WS_PGMNAME);

            map.curdate.setData(dateTime.wsCurdateMmDdYy());
            map.curtime.setData(dateTime.wsCurtimeHhMmSs());
        }

        // READ-USER-SEC-FILE
        void readUserSecFile() {
            ReadResult read = usrsecFile.readForUpdate(WS_USRSEC_FILE, secUserData.getSecUsrId());
            wsRespCd = read.result().resp();
            wsReasCd = read.result().resp2();

            if (wsRespCd == CicsResp.NORMAL) {
                secUserData = read.record().copy();
                wsMessage = Pic.x(MSG_PRESS_PF5, 80);
                map.errmsg.setColor(Dfhbmsca.DFHNEUTR);
                sendUsrupdScreen();
            } else if (wsRespCd == CicsResp.NOTFND) {
                errFlgOn = true;
                wsMessage = Pic.x(MSG_USER_NOT_FOUND, 80);
                map.usridin.setCursor();
                sendUsrupdScreen();
            } else {
                display();
                errFlgOn = true;
                wsMessage = Pic.x(MSG_UNABLE_TO_LOOKUP, 80);
                map.fname.setCursor();
                sendUsrupdScreen();
            }
        }

        // UPDATE-USER-SEC-FILE
        void updateUserSecFile() {
            CicsResult result = usrsecFile.rewrite(WS_USRSEC_FILE, secUserData.copy());
            wsRespCd = result.resp();
            wsReasCd = result.resp2();

            if (wsRespCd == CicsResp.NORMAL) {
                wsMessage = Pic.spaces(80);
                map.errmsg.setColor(Dfhbmsca.DFHGREEN);
                wsMessage = Pic.x("User " + Pic.delimitedBySpace(secUserData.getSecUsrId())
                        + " has been updated ...", 80);
                sendUsrupdScreen();
            } else if (wsRespCd == CicsResp.NOTFND) {
                errFlgOn = true;
                wsMessage = Pic.x(MSG_USER_NOT_FOUND, 80);
                map.usridin.setCursor();
                sendUsrupdScreen();
            } else {
                display();
                errFlgOn = true;
                wsMessage = Pic.x(MSG_UNABLE_TO_UPDATE, 80);
                map.fname.setCursor();
                sendUsrupdScreen();
            }
        }

        // CLEAR-CURRENT-SCREEN
        void clearCurrentScreen() {
            initializeAllFields();
            sendUsrupdScreen();
        }

        // INITIALIZE-ALL-FIELDS
        void initializeAllFields() {
            map.usridin.setCursor();
            map.usridin.setData(Pic.spaces(8));
            map.fname.setData(Pic.spaces(20));
            map.lname.setData(Pic.spaces(20));
            map.passwd.setData(Pic.spaces(8));
            map.usrtype.setData(Pic.spaces(1));
            wsMessage = Pic.spaces(80);
        }

        /** DISPLAY 'RESP:' WS-RESP-CD 'REAS:' WS-REAS-CD. */
        private void display() {
            displayLines.add("RESP:" + displaySigned(wsRespCd) + "REAS:" + displaySigned(wsReasCd));
        }
    }

    /**
     * DISPLAY of a PIC S9(09) COMP item, as rendered by GnuCOBOL -std=ibm: sign followed by 10 digits
     * (e.g. {@code +0000000013}).
     */
    static String displaySigned(int value) {
        String digits = Long.toString(Math.abs((long) value));
        return (value < 0 ? "-" : "+") + "0".repeat(Math.max(0, 10 - digits.length())) + digits;
    }
}
