package com.carddemo.programs.corpt00c;

import com.carddemo.programs.corpt00c.copybook.CarddemoCommarea;
import com.carddemo.programs.corpt00c.copybook.CommonMessages;
import com.carddemo.programs.corpt00c.copybook.Corpt0aMap;
import com.carddemo.programs.corpt00c.copybook.DateTimeWorkArea;
import com.carddemo.programs.corpt00c.copybook.Dfhbmsca;
import com.carddemo.programs.corpt00c.copybook.ScreenTitle;

import java.time.Clock;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * CORPT00C - CardDemo transaction report request screen (transaction CR00, map CORPT0A/CORPT00).
 * Lets the user pick a Monthly, Yearly or Custom date-range report and submits the TRANREPT job by writing
 * an in-line JCL deck to the JOBS TD queue. Each {@link #execute} call is one pseudo-conversational task;
 * working storage is fresh per call.
 */
public final class Corpt00c {

    public static final String WS_PGMNAME = "CORPT00C";
    public static final String WS_TRANID = "CR00";
    public static final String JOBS_QUEUE = "JOBS";
    public static final String WS_DATE_FORMAT = "YYYY-MM-DD";

    private final Clock clock;
    private final TransientDataQueue tdq;
    private final DateValidator dateValidator;
    private final Consumer<String> display;

    public Corpt00c(Clock clock, TransientDataQueue tdq, DateValidator dateValidator, Consumer<String> display) {
        this.clock = Objects.requireNonNull(clock);
        this.tdq = Objects.requireNonNull(tdq);
        this.dateValidator = Objects.requireNonNull(dateValidator);
        this.display = Objects.requireNonNull(display);
    }

    public Corpt00c(Clock clock, TransientDataQueue tdq) {
        this(clock, tdq, new Csutldtc(), System.out::println);
    }

    public Corpt00cResponse execute(Corpt00cRequest request) {
        return new Task(request).run();
    }

    /** Unwinds the task after EXEC CICS RETURN / XCTL (control never returns to the program). */
    private static final class TaskEnd extends RuntimeException {
        TaskEnd() {
            super(null, null, false, false);
        }
    }

    private final class Task {
        private final Corpt00cRequest request;

        // 01 WS-VARIABLES
        private String wsMessage = CobolText.spaces(80);
        private boolean errFlg;
        private boolean transactEof;
        private boolean sendErase = true;
        private boolean endLoop;
        private int wsRespCd;
        private int wsReasCd;
        private String wsReportName = CobolText.spaces(10);
        private String wsStartDateYyyy = CobolText.spaces(4);
        private String wsStartDateMm = CobolText.spaces(2);
        private String wsStartDateDd = CobolText.spaces(2);
        private String wsEndDateYyyy = CobolText.spaces(4);
        private String wsEndDateMm = CobolText.spaces(2);
        private String wsEndDateDd = CobolText.spaces(2);

        private final TranReportJob jobData = new TranReportJob();
        private final DateTimeWorkArea wsDateTime = new DateTimeWorkArea();
        private final Corpt0aMap map = new Corpt0aMap();
        private CarddemoCommarea commarea;
        private Corpt00cResponse response;

        Task(Corpt00cRequest request) {
            this.request = request;
        }

        Corpt00cResponse run() {
            try {
                mainPara();
            } catch (TaskEnd end) {
                // task ended via RETURN / XCTL
            }
            return response;
        }

        // MAIN-PARA
        private void mainPara() {
            errFlg = false;
            transactEof = false;
            sendErase = true;
            wsMessage = CobolText.spaces(80);
            map.errmsg.move(CobolText.spaces(78));

            if (request.commarea() == null) {
                commarea = new CarddemoCommarea();
                commarea.setToProgram("COSGN00C");
                returnToPrevScreen();
            } else {
                commarea = request.commarea().copy();
                if (!commarea.isPgmReenter()) {
                    commarea.setPgmReenter();
                    map.lowValues();
                    map.monthly.cursor();
                    sendTrnrptScreen();
                } else {
                    receiveTrnrptScreen();
                    switch (request.aid()) {
                        case DFHENTER -> processEnterKey();
                        case DFHPF3 -> {
                            commarea.setToProgram("COMEN01C");
                            returnToPrevScreen();
                        }
                        default -> {
                            errFlg = true;
                            map.monthly.cursor();
                            wsMessage = CobolText.move(CommonMessages.CCDA_MSG_INVALID_KEY, 80);
                            sendTrnrptScreen();
                        }
                    }
                }
            }
            returnToCics();
        }

        // PROCESS-ENTER-KEY
        private void processEnterKey() {
            display.accept("PROCESS ENTER KEY");

            if (selected(map.monthly.data())) {
                wsReportName = CobolText.move("Monthly", 10);
                wsDateTime.moveCurrentDate(LocalDateTime.now(clock));

                wsStartDateYyyy = pic9(wsDateTime.curdateYear, 4);
                wsStartDateMm = pic9(wsDateTime.curdateMonth, 2);
                wsStartDateDd = "01";
                jobData.setStartDate(wsStartDate());

                wsDateTime.curdateDay = 1;
                wsDateTime.curdateMonth = (wsDateTime.curdateMonth + 1) % 100;
                if (wsDateTime.curdateMonth > 12) {
                    wsDateTime.curdateYear = (wsDateTime.curdateYear + 1) % 10000;
                    wsDateTime.curdateMonth = 1;
                }
                // COMPUTE WS-CURDATE-N = DATE-OF-INTEGER(INTEGER-OF-DATE(WS-CURDATE-N) - 1)
                LocalDate lastOfMonth = wsDateTime.curdate().minusDays(1);
                wsDateTime.setCurdateN(lastOfMonth.getYear() * 10000 + lastOfMonth.getMonthValue() * 100
                        + lastOfMonth.getDayOfMonth());

                wsEndDateYyyy = pic9(wsDateTime.curdateYear, 4);
                wsEndDateMm = pic9(wsDateTime.curdateMonth, 2);
                wsEndDateDd = pic9(wsDateTime.curdateDay, 2);
                jobData.setEndDate(wsEndDate());

                submitJobToIntrdr();
            } else if (selected(map.yearly.data())) {
                wsReportName = CobolText.move("Yearly", 10);
                wsDateTime.moveCurrentDate(LocalDateTime.now(clock));

                wsStartDateYyyy = pic9(wsDateTime.curdateYear, 4);
                wsEndDateYyyy = wsStartDateYyyy;
                wsStartDateMm = "01";
                wsStartDateDd = "01";
                jobData.setStartDate(wsStartDate());

                wsEndDateMm = "12";
                wsEndDateDd = "31";
                jobData.setEndDate(wsEndDate());

                submitJobToIntrdr();
            } else if (selected(map.custom.data())) {
                processCustomRange();
            } else {
                fail("Select a report type to print report...", map.monthly);
            }

            if (!errFlg) {
                initializeAllFields();
                map.errmsg.c = Dfhbmsca.DFHGREEN;
                wsMessage = CobolText.stringInto(wsMessage, CobolText.delimitedBySpace(wsReportName),
                        " report submitted for printing ...");
                map.monthly.cursor();
                sendTrnrptScreen();
            }
        }

        // PROCESS-ENTER-KEY: WHEN CUSTOMI OF CORPT0AI NOT = SPACES AND LOW-VALUES
        private void processCustomRange() {
            if (CobolText.isSpacesOrLowValues(map.sdtmm.data())) {
                fail("Start Date - Month can NOT be empty...", map.sdtmm);
            } else if (CobolText.isSpacesOrLowValues(map.sdtdd.data())) {
                fail("Start Date - Day can NOT be empty...", map.sdtdd);
            } else if (CobolText.isSpacesOrLowValues(map.sdtyyyy.data())) {
                fail("Start Date - Year can NOT be empty...", map.sdtyyyy);
            } else if (CobolText.isSpacesOrLowValues(map.edtmm.data())) {
                fail("End Date - Month can NOT be empty...", map.edtmm);
            } else if (CobolText.isSpacesOrLowValues(map.edtdd.data())) {
                fail("End Date - Day can NOT be empty...", map.edtdd);
            } else if (CobolText.isSpacesOrLowValues(map.edtyyyy.data())) {
                fail("End Date - Year can NOT be empty...", map.edtyyyy);
            }

            // COMPUTE WS-NUM-99 / WS-NUM-9999 = FUNCTION NUMVAL-C(..); MOVE back to the map field
            map.sdtmm.move(CobolText.toUnsignedDisplay(CobolText.numvalC(map.sdtmm.data()), 2));
            map.sdtdd.move(CobolText.toUnsignedDisplay(CobolText.numvalC(map.sdtdd.data()), 2));
            map.sdtyyyy.move(CobolText.toUnsignedDisplay(CobolText.numvalC(map.sdtyyyy.data()), 4));
            map.edtmm.move(CobolText.toUnsignedDisplay(CobolText.numvalC(map.edtmm.data()), 2));
            map.edtdd.move(CobolText.toUnsignedDisplay(CobolText.numvalC(map.edtdd.data()), 2));
            map.edtyyyy.move(CobolText.toUnsignedDisplay(CobolText.numvalC(map.edtyyyy.data()), 4));

            if (!CobolText.isNumeric(map.sdtmm.data()) || map.sdtmm.data().compareTo("12") > 0) {
                fail("Start Date - Not a valid Month...", map.sdtmm);
            }
            if (!CobolText.isNumeric(map.sdtdd.data()) || map.sdtdd.data().compareTo("31") > 0) {
                fail("Start Date - Not a valid Day...", map.sdtdd);
            }
            if (!CobolText.isNumeric(map.sdtyyyy.data())) {
                fail("Start Date - Not a valid Year...", map.sdtyyyy);
            }
            if (!CobolText.isNumeric(map.edtmm.data()) || map.edtmm.data().compareTo("12") > 0) {
                fail("End Date - Not a valid Month...", map.edtmm);
            }
            if (!CobolText.isNumeric(map.edtdd.data()) || map.edtdd.data().compareTo("31") > 0) {
                fail("End Date - Not a valid Day...", map.edtdd);
            }
            if (!CobolText.isNumeric(map.edtyyyy.data())) {
                fail("End Date - Not a valid Year...", map.edtyyyy);
            }

            wsStartDateYyyy = map.sdtyyyy.data();
            wsStartDateMm = map.sdtmm.data();
            wsStartDateDd = map.sdtdd.data();
            wsEndDateYyyy = map.edtyyyy.data();
            wsEndDateMm = map.edtmm.data();
            wsEndDateDd = map.edtdd.data();

            CsutldtcResult start = dateValidator.validate(wsStartDate(), WS_DATE_FORMAT);
            if (!"0000".equals(start.sevCd()) && !"2513".equals(start.msgNum())) {
                fail("Start Date - Not a valid date...", map.sdtmm);
            }

            CsutldtcResult end = dateValidator.validate(wsEndDate(), WS_DATE_FORMAT);
            if (!"0000".equals(end.sevCd()) && !"2513".equals(end.msgNum())) {
                fail("End Date - Not a valid date...", map.edtmm);
            }

            jobData.setStartDate(wsStartDate());
            jobData.setEndDate(wsEndDate());
            wsReportName = CobolText.move("Custom", 10);
            if (!errFlg) {
                submitJobToIntrdr();
            }
        }

        // SUBMIT-JOB-TO-INTRDR
        private void submitJobToIntrdr() {
            if (CobolText.isSpacesOrLowValues(map.confirm.data())) {
                wsMessage = CobolText.stringInto(wsMessage, "Please confirm to print the ",
                        CobolText.delimitedBySpace(wsReportName), " report...");
                errFlg = true;
                map.confirm.cursor();
                sendTrnrptScreen();
            }

            if (!errFlg) {
                String confirm = map.confirm.data();
                if (confirm.equals("Y") || confirm.equals("y")) {
                    // CONTINUE
                } else if (confirm.equals("N") || confirm.equals("n")) {
                    initializeAllFields();
                    errFlg = true;
                    sendTrnrptScreen();
                } else {
                    wsMessage = CobolText.stringInto(wsMessage, "\"", CobolText.delimitedBySpace(confirm),
                            "\" is not a valid value to confirm...");
                    errFlg = true;
                    map.confirm.cursor();
                    sendTrnrptScreen();
                }

                endLoop = false;
                for (int wsIdx = 1; !(wsIdx > TranReportJob.MAX_LINES || endLoop || errFlg); wsIdx++) {
                    String jclRecord = jobData.jobLine(wsIdx);
                    if (jclRecord.equals(CobolText.move("/*EOF", 80)) || CobolText.isSpaces(jclRecord)
                            || CobolText.isLowValues(jclRecord)) {
                        endLoop = true;
                    }
                    wirteJobsubTdq(jclRecord);
                }
            }
        }

        // WIRTE-JOBSUB-TDQ
        private void wirteJobsubTdq(String jclRecord) {
            CicsResponse resp = tdq.writeq(JOBS_QUEUE, jclRecord);
            wsRespCd = resp.resp();
            wsReasCd = resp.resp2();
            if (wsRespCd != CicsResponse.NORMAL) {
                display.accept("RESP:" + CobolText.displaySigned(wsRespCd, 9)
                        + "REAS:" + CobolText.displaySigned(wsReasCd, 9));
                errFlg = true;
                wsMessage = CobolText.move("Unable to Write TDQ (JOBS)...", 80);
                map.monthly.cursor();
                sendTrnrptScreen();
            }
        }

        // RETURN-TO-PREV-SCREEN
        private void returnToPrevScreen() {
            if (CobolText.isLowValues(commarea.toProgram()) || CobolText.isSpaces(commarea.toProgram())) {
                commarea.setToProgram("COSGN00C");
            }
            commarea.setFromTranid(WS_TRANID);
            commarea.setFromProgram(WS_PGMNAME);
            commarea.setPgmContext(0);
            response = new Corpt00cResponse(Corpt00cResponse.Action.XCTL, null, commarea.toProgram(),
                    commarea.copy(), null, false);
            throw new TaskEnd();
        }

        // SEND-TRNRPT-SCREEN (falls through to RETURN-TO-CICS via GO TO)
        private void sendTrnrptScreen() {
            populateHeaderInfo();
            map.errmsg.move(wsMessage);
            response = new Corpt00cResponse(Corpt00cResponse.Action.SEND_MAP, WS_TRANID, null,
                    commarea.copy(), map.toOutput(), sendErase);
            throw new TaskEnd();
        }

        // RETURN-TO-CICS
        private void returnToCics() {
            if (response == null) {
                response = new Corpt00cResponse(Corpt00cResponse.Action.RETURN, WS_TRANID, null,
                        commarea.copy(), null, false);
            }
            throw new TaskEnd();
        }

        // RECEIVE-TRNRPT-SCREEN
        private void receiveTrnrptScreen() {
            map.receive(request.map());
            wsRespCd = CicsResponse.NORMAL;
            wsReasCd = 0;
        }

        // POPULATE-HEADER-INFO
        private void populateHeaderInfo() {
            wsDateTime.moveCurrentDate(LocalDateTime.now(clock));

            map.title01.move(ScreenTitle.CCDA_TITLE01);
            map.title02.move(ScreenTitle.CCDA_TITLE02);
            map.trnname.move(WS_TRANID);
            map.pgmname.move(WS_PGMNAME);

            wsDateTime.curdateMm = wsDateTime.curdateMonth;
            wsDateTime.curdateDd = wsDateTime.curdateDay;
            wsDateTime.curdateYy = wsDateTime.curdateYear % 100;
            map.curdate.move(wsDateTime.curdateMmDdYy());

            wsDateTime.curtimeHh = wsDateTime.curtimeHours;
            wsDateTime.curtimeMm = wsDateTime.curtimeMinute;
            wsDateTime.curtimeSs = wsDateTime.curtimeSecond;
            map.curtime.move(wsDateTime.curtimeHhMmSs());
        }

        // INITIALIZE-ALL-FIELDS
        private void initializeAllFields() {
            map.monthly.cursor();
            map.monthly.initialize();
            map.yearly.initialize();
            map.custom.initialize();
            map.sdtmm.initialize();
            map.sdtdd.initialize();
            map.sdtyyyy.initialize();
            map.edtmm.initialize();
            map.edtdd.initialize();
            map.edtyyyy.initialize();
            map.confirm.initialize();
            wsMessage = CobolText.spaces(80);
        }

        /** MOVE msg TO WS-MESSAGE, MOVE 'Y' TO WS-ERR-FLG, MOVE -1 TO field-L, PERFORM SEND-TRNRPT-SCREEN. */
        private void fail(String message, Corpt0aMap.Field cursorField) {
            wsMessage = CobolText.move(message, 80);
            errFlg = true;
            cursorField.cursor();
            sendTrnrptScreen();
        }

        /** {@code field NOT = SPACES AND LOW-VALUES}. */
        private boolean selected(String field) {
            return !CobolText.isSpaces(field) && !CobolText.isLowValues(field);
        }

        private String wsStartDate() {
            return wsStartDateYyyy + "-" + wsStartDateMm + "-" + wsStartDateDd;
        }

        private String wsEndDate() {
            return wsEndDateYyyy + "-" + wsEndDateMm + "-" + wsEndDateDd;
        }

        private String pic9(int value, int digits) {
            return CobolText.toUnsignedDisplay(java.math.BigDecimal.valueOf(value), digits);
        }
    }
}
