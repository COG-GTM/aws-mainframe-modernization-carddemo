package com.carddemo.programs.cotrn02c.gateway;

import com.carddemo.programs.cotrn02c.copybook.CsutldtcParm;
import com.carddemo.programs.cotrn02c.copybook.Pic;

import java.time.YearMonth;

/**
 * Port of CSUTLDTC, which wraps the LE service CEEDAYS and formats its feedback code into an 80-byte
 * message. CEEDAYS itself is emulated for the 'YYYY-MM-DD' picture used by COTRN02C:
 * <ul>
 *   <li>valid date (on/after 1582-10-15) -> severity 0000, msg 0000, 'Date is valid'</li>
 *   <li>other picture string -> 0003 / 2518 'Bad Pic String '</li>
 *   <li>non-digit in a digit position -> 0003 / 2520 'Nonnumeric data'</li>
 *   <li>month not 01-12 -> 0003 / 2517 'Invalid month  '</li>
 *   <li>before the Lilian epoch 1582-10-15 -> 0003 / 2513 'Unsupp. Range  '</li>
 *   <li>day not valid for the month -> 0003 / 2508 'Datevalue error'</li>
 * </ul>
 */
public final class Csutldtc implements DateValidator {

    static final int MSG_BAD_DATE_VALUE = 2508;
    static final int MSG_UNSUPP_RANGE = 2513;
    static final int MSG_INVALID_MONTH = 2517;
    static final int MSG_BAD_PIC_STRING = 2518;
    static final int MSG_NON_NUMERIC_DATA = 2520;

    @Override
    public CsutldtcParm validate(String date, String dateFormat) {
        String lsDate = Pic.x(date, 10);
        String lsFormat = Pic.x(dateFormat, 10);
        int[] feedback = ceedays(lsDate, lsFormat.trim());
        int severity = feedback[0];
        int msgNo = feedback[1];
        String result = switch (msgNo) {
            case 0 -> "Date is valid";
            case MSG_BAD_DATE_VALUE -> "Datevalue error";
            case MSG_UNSUPP_RANGE -> "Unsupp. Range  ";
            case MSG_INVALID_MONTH -> "Invalid month  ";
            case MSG_BAD_PIC_STRING -> "Bad Pic String ";
            case MSG_NON_NUMERIC_DATA -> "Nonnumeric data";
            default -> "Date is invalid";
        };
        // 01 WS-MESSAGE (80 bytes)
        String wsMessage = Pic.nine(severity, 4)
                + Pic.x("Mesg Code:", 11)
                + Pic.nine(msgNo, 4)
                + " "
                + Pic.x(result, 15)
                + " "
                + Pic.x("TstDate:", 9)
                + lsDate
                + " "
                + Pic.x("Mask used:", 10)
                + lsFormat
                + " "
                + "   ";
        return new CsutldtcParm(lsDate, lsFormat, wsMessage);
    }

    /** Returns {severity, msg-no}. */
    private static int[] ceedays(String date, String picture) {
        if (!"YYYY-MM-DD".equals(picture)) {
            return new int[] {3, MSG_BAD_PIC_STRING};
        }
        String yyyy = date.substring(0, 4);
        String mm = date.substring(5, 7);
        String dd = date.substring(8, 10);
        if (!Pic.isNumeric(yyyy) || !Pic.isNumeric(mm) || !Pic.isNumeric(dd)
                || date.charAt(4) != '-' || date.charAt(7) != '-') {
            return new int[] {3, MSG_NON_NUMERIC_DATA};
        }
        int year = Integer.parseInt(yyyy);
        int month = Integer.parseInt(mm);
        int day = Integer.parseInt(dd);
        if (month < 1 || month > 12) {
            return new int[] {3, MSG_INVALID_MONTH};
        }
        if (year < 1582 || (year == 1582 && (month < 10 || (month == 10 && day < 15)))) {
            return new int[] {3, MSG_UNSUPP_RANGE};
        }
        if (day < 1 || day > YearMonth.of(year, month).lengthOfMonth()) {
            return new int[] {3, MSG_BAD_DATE_VALUE};
        }
        return new int[] {0, 0};
    }
}
