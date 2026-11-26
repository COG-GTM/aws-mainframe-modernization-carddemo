package com.carddemo.programs.csutldtc;

import com.carddemo.programs.csutldtc.copybook.FeedbackCode;
import com.carddemo.programs.csutldtc.copybook.FeedbackCondition;
import com.carddemo.programs.csutldtc.copybook.PicX;
import com.carddemo.programs.csutldtc.copybook.VString;
import com.carddemo.programs.csutldtc.copybook.WsMessage;

/**
 * CSUTLDTC: validates a date against a picture string via CEEDAYS.
 * {@code PROCEDURE DIVISION USING LS-DATE PIC X(10), LS-DATE-FORMAT PIC X(10), LS-RESULT PIC X(80)}.
 * Called by CORPT00C and COTRN02C with format 'YYYY-MM-DD'.
 */
public final class Csutldtc {
    public static final int LS_DATE_LENGTH = 10;
    public static final int LS_DATE_FORMAT_LENGTH = 10;

    private final CeedaysService ceedaysService;

    public Csutldtc() {
        this(new LeCeedays());
    }

    public Csutldtc(CeedaysService ceedaysService) {
        this.ceedaysService = ceedaysService;
    }

    /** Main procedure: INITIALIZE, PERFORM A000-MAIN THRU A000-MAIN-EXIT, set LS-RESULT and RETURN-CODE. */
    public CsutldtcResult call(String lsDate, String lsDateFormat) {
        String date = PicX.of(lsDate, LS_DATE_LENGTH);
        String format = PicX.of(lsDateFormat, LS_DATE_FORMAT_LENGTH);

        WsMessage wsMessage = new WsMessage();
        wsMessage.initialize();
        wsMessage.setDate(PicX.spaces(10));

        int outputLillian = a000Main(date, format, wsMessage);

        String lsResult = PicX.of(wsMessage.toRecord(), 80);
        int returnCode = wsMessage.severityN();
        return new CsutldtcResult(lsResult, returnCode, outputLillian);
    }

    // A000-MAIN
    private int a000Main(String lsDate, String lsDateFormat, WsMessage wsMessage) {
        VString wsDateToTest = VString.of(lsDate);
        wsMessage.setDate(lsDate);
        VString wsDateFormat = VString.of(lsDateFormat);
        wsMessage.setDateFmt(lsDateFormat);

        CeedaysResult ceedays = ceedaysService.ceedays(wsDateToTest, wsDateFormat);
        FeedbackCode feedbackCode = ceedays.feedbackCode();

        // MOVE WS-DATE-TO-TEST TO WS-DATE: group move copies the binary length prefix + first 8 chars.
        wsMessage.setDate(wsDateToTest.groupImage());
        wsMessage.setSeverityN(feedbackCode.severity());
        wsMessage.setMsgNoN(feedbackCode.msgNo());
        wsMessage.setResult(evaluateResult(feedbackCode));
        return ceedays.outputLillian();
    }

    // EVALUATE TRUE on FEEDBACK-CODE 88-levels (WS-RESULT is 15 characters)
    static String evaluateResult(FeedbackCode feedbackCode) {
        return FeedbackCondition.of(feedbackCode).map(c -> switch (c) {
            case FC_INVALID_DATE -> "Date is valid";
            case FC_INSUFFICIENT_DATA -> "Insufficient";
            case FC_BAD_DATE_VALUE -> "Datevalue error";
            case FC_INVALID_ERA -> "Invalid Era    ";
            case FC_UNSUPP_RANGE -> "Unsupp. Range  ";
            case FC_INVALID_MONTH -> "Invalid month  ";
            case FC_BAD_PIC_STRING -> "Bad Pic String ";
            case FC_NON_NUMERIC_DATA -> "Nonnumeric data";
            case FC_YEAR_IN_ERA_ZERO -> "YearInEra is 0 ";
        }).orElse("Date is invalid");
    }
}
