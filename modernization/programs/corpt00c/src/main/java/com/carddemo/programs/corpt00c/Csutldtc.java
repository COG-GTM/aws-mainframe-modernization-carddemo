package com.carddemo.programs.corpt00c;

import java.time.LocalDate;
import java.time.Year;
import java.time.temporal.ChronoUnit;

/**
 * Port of the called subprogram CSUTLDTC (app/cbl/CSUTLDTC.cbl), which wraps the LE service CEEDAYS.
 * CEEDAYS is emulated for picture strings made of the tokens YYYY, MM and DD plus separator characters
 * (CORPT00C only passes 'YYYY-MM-DD').
 */
public final class Csutldtc implements DateValidator {

    /** Lilian day 1 is 15 October 1582; day 0 is the day before. */
    private static final LocalDate LILIAN_EPOCH = LocalDate.of(1582, 10, 14);
    private static final LocalDate LILIAN_FIRST = LocalDate.of(1582, 10, 15);

    /** FEEDBACK-CODE 88-levels of CSUTLDTC (severity, message number, WS-RESULT text). */
    public enum Feedback {
        FC_INVALID_DATE(0, 0, "Date is valid"),
        FC_INSUFFICIENT_DATA(3, 2507, "Insufficient"),
        FC_BAD_DATE_VALUE(3, 2508, "Datevalue error"),
        FC_INVALID_ERA(3, 2509, "Invalid Era    "),
        FC_UNSUPP_RANGE(3, 2513, "Unsupp. Range  "),
        FC_INVALID_MONTH(3, 2517, "Invalid month  "),
        FC_BAD_PIC_STRING(3, 2518, "Bad Pic String "),
        FC_NON_NUMERIC_DATA(3, 2520, "Nonnumeric data"),
        FC_YEAR_IN_ERA_ZERO(3, 2521, "YearInEra is 0 ");

        public final int severity;
        public final int msgNo;
        public final String resultText;

        Feedback(int severity, int msgNo, String resultText) {
            this.severity = severity;
            this.msgNo = msgNo;
            this.resultText = resultText;
        }
    }

    /** CEEDAYS output: feedback condition and OUTPUT-LILLIAN. */
    public record CeedaysResult(Feedback feedback, int lilian) {
    }

    @Override
    public CsutldtcResult validate(String lsDate, String lsDateFormat) {
        String date = CobolText.move(lsDate, 10);
        String format = CobolText.move(lsDateFormat, 10);
        // A000-MAIN
        CeedaysResult r = ceedays(date, format);
        Feedback fc = r.feedback();
        // WS-MESSAGE: SEVERITY X(4) 'Mesg Code:' X(11) MSG-NO X(4) ' ' RESULT X(15) ' ' 'TstDate:' X(9)
        //             DATE X(10) ' ' 'Mask used:' X(10) FMT X(10) ' ' X(3)
        String wsMessage = String.format(java.util.Locale.ROOT, "%04d", fc.severity)
                + CobolText.move("Mesg Code:", 11)
                + String.format(java.util.Locale.ROOT, "%04d", fc.msgNo)
                + " "
                + CobolText.move(fc.resultText, 15)
                + " "
                + CobolText.move("TstDate:", 9)
                + date
                + " "
                + CobolText.move("Mask used:", 10)
                + format
                + " "
                + CobolText.spaces(3);
        return new CsutldtcResult(wsMessage);
    }

    /** RETURN-CODE set by CSUTLDTC (WS-SEVERITY-N). */
    public static int returnCode(CsutldtcResult result) {
        return Integer.parseInt(result.sevCd());
    }

    /** Emulation of CEEDAYS for a fixed-length date string and picture string. */
    public static CeedaysResult ceedays(String date, String picture) {
        String pic = picture.stripTrailing();
        int y = -1;
        int m = -1;
        int d = -1;
        int i = 0;
        while (i < pic.length()) {
            char c = pic.charAt(i);
            if (Character.isLetter(c)) {
                int j = i;
                while (j < pic.length() && pic.charAt(j) == c) {
                    j++;
                }
                int run = j - i;
                if (c == 'Y' && run == 4 && y < 0) {
                    y = i;
                } else if (c == 'M' && run == 2 && m < 0) {
                    m = i;
                } else if (c == 'D' && run == 2 && d < 0) {
                    d = i;
                } else {
                    return new CeedaysResult(Feedback.FC_BAD_PIC_STRING, 0);
                }
                i = j;
            } else {
                i++;
            }
        }
        if (y < 0 || m < 0 || d < 0) {
            return new CeedaysResult(Feedback.FC_BAD_PIC_STRING, 0);
        }
        if (date.length() < pic.length() || CobolText.isSpacesOrLowValues(date)) {
            return new CeedaysResult(Feedback.FC_INSUFFICIENT_DATA, 0);
        }
        String ys = date.substring(y, y + 4);
        String ms = date.substring(m, m + 2);
        String ds = date.substring(d, d + 2);
        if (!CobolText.isNumeric(ys) || !CobolText.isNumeric(ms) || !CobolText.isNumeric(ds)) {
            return new CeedaysResult(Feedback.FC_NON_NUMERIC_DATA, 0);
        }
        int year = Integer.parseInt(ys);
        int month = Integer.parseInt(ms);
        int day = Integer.parseInt(ds);
        if (month < 1 || month > 12) {
            return new CeedaysResult(Feedback.FC_INVALID_MONTH, 0);
        }
        int maxDay = java.time.Month.of(month).length(Year.isLeap(year));
        if (day < 1 || day > maxDay) {
            return new CeedaysResult(Feedback.FC_BAD_DATE_VALUE, 0);
        }
        LocalDate ld = LocalDate.of(year, month, day);
        if (ld.isBefore(LILIAN_FIRST)) {
            return new CeedaysResult(Feedback.FC_UNSUPP_RANGE, 0);
        }
        return new CeedaysResult(Feedback.FC_INVALID_DATE, (int) ChronoUnit.DAYS.between(LILIAN_EPOCH, ld));
    }
}
