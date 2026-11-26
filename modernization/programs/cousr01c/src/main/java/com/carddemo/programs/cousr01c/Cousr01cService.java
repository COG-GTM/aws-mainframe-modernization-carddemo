package com.carddemo.programs.cousr01c;

import com.carddemo.programs.cousr01c.copybook.CardDemoCommarea;
import com.carddemo.programs.cousr01c.copybook.CommonMessages;
import com.carddemo.programs.cousr01c.copybook.Cousr1aMap;
import com.carddemo.programs.cousr01c.copybook.Cousr1aMap.Field;
import com.carddemo.programs.cousr01c.copybook.DateTimeFields;
import com.carddemo.programs.cousr01c.copybook.Dfhbmsca;
import com.carddemo.programs.cousr01c.copybook.Pic;
import com.carddemo.programs.cousr01c.copybook.ScreenTitle;
import com.carddemo.programs.cousr01c.copybook.SecUserData;
import com.carddemo.programs.cousr01c.dto.CicsRequest;
import com.carddemo.programs.cousr01c.dto.CicsResponse;
import com.carddemo.programs.cousr01c.dto.UserAddScreenInput;
import com.carddemo.programs.cousr01c.dto.UserAddScreenOutput;
import com.carddemo.programs.cousr01c.gateway.UserSecRepository;
import com.carddemo.programs.cousr01c.gateway.WriteResult;

import java.time.Clock;
import java.time.LocalDateTime;
import java.util.Objects;

/** COUSR01C - add a new Regular/Admin user to the USRSEC file (transaction CU01). */
public final class Cousr01cService {

    public static final String WS_PGMNAME = "COUSR01C";
    public static final String WS_TRANID = "CU01";
    public static final String WS_USRSEC_FILE = "USRSEC  ";

    public static final String MSG_FNAME_EMPTY = "First Name can NOT be empty...";
    public static final String MSG_LNAME_EMPTY = "Last Name can NOT be empty...";
    public static final String MSG_USERID_EMPTY = "User ID can NOT be empty...";
    public static final String MSG_PASSWD_EMPTY = "Password can NOT be empty...";
    public static final String MSG_USRTYPE_EMPTY = "User Type can NOT be empty...";
    public static final String MSG_USER_EXISTS = "User ID already exist...";
    public static final String MSG_UNABLE_TO_ADD = "Unable to Add User...";

    private final UserSecRepository userSec;
    private final Clock clock;

    public Cousr01cService(UserSecRepository userSec, Clock clock) {
        this.userSec = Objects.requireNonNull(userSec);
        this.clock = Objects.requireNonNull(clock);
    }

    /** Runs one CU01 task. Working storage is fresh per task, as under CICS. */
    public CicsResponse handle(CicsRequest request) {
        return new Task(request).mainPara();
    }

    /** WORKING-STORAGE of one task plus the paragraphs operating on it. */
    final class Task {
        private final CicsRequest request;
        private String wsMessage = Pic.spaces(80);   // WS-MESSAGE PIC X(80)
        private boolean errFlg;                      // WS-ERR-FLG 88 ERR-FLG-ON / ERR-FLG-OFF
        private WriteResult writeResult;             // WS-RESP-CD / WS-REAS-CD
        private CardDemoCommarea commarea = new CardDemoCommarea();
        private final Cousr1aMap map = new Cousr1aMap();
        private final DateTimeFields dateTime = new DateTimeFields();
        private SecUserData secUserData;
        private UserAddScreenOutput sent;

        Task(CicsRequest request) {
            this.request = request;
        }

        // MAIN-PARA
        CicsResponse mainPara() {
            errFlg = false;
            wsMessage = Pic.spaces(80);
            map.field(Field.ERRMSG).data(Pic.spaces(78));

            if (request.commarea() == null) {
                commarea.toProgram("COSGN00C");
                return returnToPrevScreen();
            }
            commarea = request.commarea().copy();
            if (!commarea.isPgmReenter()) {
                commarea.setPgmContext(CardDemoCommarea.PgmContext.REENTER);
                map.lowValues();
                map.cursor(Field.FNAME);
                sendUsraddScreen();
            } else {
                receiveUsraddScreen();
                switch (request.aid()) {
                    case DFHENTER -> processEnterKey();
                    case DFHPF3 -> {
                        commarea.toProgram("COADM01C");
                        return returnToPrevScreen();
                    }
                    case DFHPF4 -> clearCurrentScreen();
                    default -> {
                        errFlg = true;
                        map.cursor(Field.FNAME);
                        wsMessage = Pic.x(CommonMessages.CCDA_MSG_INVALID_KEY, 80);
                        sendUsraddScreen();
                    }
                }
            }
            return new CicsResponse.SendAndReturn(sent, WS_TRANID, commarea);
        }

        // PROCESS-ENTER-KEY
        void processEnterKey() {
            if (Pic.isSpacesOrLowValues(map.field(Field.FNAME).data())) {
                validationError(MSG_FNAME_EMPTY, Field.FNAME);
            } else if (Pic.isSpacesOrLowValues(map.field(Field.LNAME).data())) {
                validationError(MSG_LNAME_EMPTY, Field.LNAME);
            } else if (Pic.isSpacesOrLowValues(map.field(Field.USERID).data())) {
                validationError(MSG_USERID_EMPTY, Field.USERID);
            } else if (Pic.isSpacesOrLowValues(map.field(Field.PASSWD).data())) {
                validationError(MSG_PASSWD_EMPTY, Field.PASSWD);
            } else if (Pic.isSpacesOrLowValues(map.field(Field.USRTYPE).data())) {
                validationError(MSG_USRTYPE_EMPTY, Field.USRTYPE);
            } else {
                map.cursor(Field.FNAME);
            }

            if (!errFlg) {
                // SEC-USR-FILLER is never moved by COUSR01C; modelled as spaces.
                secUserData = new SecUserData(
                        map.field(Field.USERID).data(),
                        map.field(Field.FNAME).data(),
                        map.field(Field.LNAME).data(),
                        map.field(Field.PASSWD).data(),
                        map.field(Field.USRTYPE).data(),
                        null);
                writeUserSecFile();
            }
        }

        private void validationError(String message, Field cursorField) {
            errFlg = true;
            wsMessage = Pic.x(message, 80);
            map.cursor(cursorField);
            sendUsraddScreen();
        }

        // RETURN-TO-PREV-SCREEN
        CicsResponse returnToPrevScreen() {
            String to = commarea.toProgram();
            if (Pic.isLowValues(to) || Pic.isSpaces(to)) {
                commarea.toProgram("COSGN00C");
            }
            commarea.fromTranid(WS_TRANID);
            commarea.fromProgram(WS_PGMNAME);
            commarea.pgmContext(0);
            return new CicsResponse.Xctl(commarea.toProgram(), commarea);
        }

        // SEND-USRADD-SCREEN
        void sendUsraddScreen() {
            populateHeaderInfo();
            map.field(Field.ERRMSG).data(wsMessage);
            sent = new UserAddScreenOutput(
                    map.field(Field.TRNNAME).data(),
                    map.field(Field.TITLE01).data(),
                    map.field(Field.CURDATE).data(),
                    map.field(Field.PGMNAME).data(),
                    map.field(Field.TITLE02).data(),
                    map.field(Field.CURTIME).data(),
                    map.field(Field.FNAME).data(),
                    map.field(Field.LNAME).data(),
                    map.field(Field.USERID).data(),
                    map.field(Field.PASSWD).data(),
                    map.field(Field.USRTYPE).data(),
                    map.field(Field.ERRMSG).data(),
                    map.field(Field.ERRMSG).color,
                    map.symbolicCursor().orElse(null));
        }

        // RECEIVE-USRADD-SCREEN
        void receiveUsraddScreen() {
            map.lowValues();
            UserAddScreenInput in = request.input() == null ? UserAddScreenInput.empty() : request.input();
            receiveField(Field.FNAME, in.fname());
            receiveField(Field.LNAME, in.lname());
            receiveField(Field.USERID, in.userid());
            receiveField(Field.PASSWD, in.passwd());
            receiveField(Field.USRTYPE, in.usrtype());
            writeResult = WriteResult.normal();
        }

        private void receiveField(Field f, String value) {
            Cousr1aMap.MapField mf = map.field(f);
            if (value == null || value.isEmpty()) {
                mf.length = 0;
                mf.dataLowValues();
            } else {
                mf.length = (short) Math.min(value.length(), f.length());
                mf.data(value);
            }
        }

        // POPULATE-HEADER-INFO
        void populateHeaderInfo() {
            dateTime.moveCurrentDate(LocalDateTime.now(clock));

            map.field(Field.TITLE01).data(ScreenTitle.CCDA_TITLE01);
            map.field(Field.TITLE02).data(ScreenTitle.CCDA_TITLE02);
            map.field(Field.TRNNAME).data(WS_TRANID);
            map.field(Field.PGMNAME).data(WS_PGMNAME);

            dateTime.curdateMm(dateTime.curdateMonth());
            dateTime.curdateDd(dateTime.curdateDay());
            dateTime.curdateYy(Pic.nine(dateTime.curdateYear(), 4).substring(2, 4));
            map.field(Field.CURDATE).data(dateTime.curdateMmDdYy());

            dateTime.curtimeHh(dateTime.curtimeHours());
            dateTime.curtimeMm(dateTime.curtimeMinute());
            dateTime.curtimeSs(dateTime.curtimeSecond());
            map.field(Field.CURTIME).data(dateTime.curtimeHhMmSs());
        }

        // WRITE-USER-SEC-FILE
        void writeUserSecFile() {
            writeResult = userSec.write(secUserData);
            switch (writeResult.resp()) {
                case NORMAL -> {
                    initializeAllFields();
                    wsMessage = Pic.spaces(80);
                    map.field(Field.ERRMSG).color = Dfhbmsca.DFHGREEN;
                    wsMessage = Pic.x("User " + delimitedBySpace(secUserData.secUsrId())
                            + " has been added ...", 80);
                    sendUsraddScreen();
                }
                case DUPKEY, DUPREC -> {
                    errFlg = true;
                    wsMessage = Pic.x(MSG_USER_EXISTS, 80);
                    map.cursor(Field.USERID);
                    sendUsraddScreen();
                }
                default -> {
                    errFlg = true;
                    wsMessage = Pic.x(MSG_UNABLE_TO_ADD, 80);
                    map.cursor(Field.FNAME);
                    sendUsraddScreen();
                }
            }
        }

        // CLEAR-CURRENT-SCREEN
        void clearCurrentScreen() {
            initializeAllFields();
            sendUsraddScreen();
        }

        // INITIALIZE-ALL-FIELDS
        void initializeAllFields() {
            map.cursor(Field.FNAME);
            map.field(Field.USERID).data(Pic.spaces(8));
            map.field(Field.FNAME).data(Pic.spaces(20));
            map.field(Field.LNAME).data(Pic.spaces(20));
            map.field(Field.PASSWD).data(Pic.spaces(8));
            map.field(Field.USRTYPE).data(Pic.spaces(1));
            wsMessage = Pic.spaces(80);
        }

        boolean errFlgOn() {
            return errFlg;
        }

        WriteResult writeResult() {
            return writeResult;
        }

        CardDemoCommarea commarea() {
            return commarea;
        }

        void commarea(CardDemoCommarea c) {
            commarea = c;
        }
    }

    /** STRING ... DELIMITED BY SPACE: the characters before the first space. */
    static String delimitedBySpace(String s) {
        int i = s.indexOf(' ');
        return i < 0 ? s : s.substring(0, i);
    }

    Task newTask(CicsRequest request) {
        return new Task(request);
    }
}
