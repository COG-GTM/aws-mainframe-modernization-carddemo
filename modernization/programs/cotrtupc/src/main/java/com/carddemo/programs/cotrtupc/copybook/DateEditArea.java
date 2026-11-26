package com.carddemo.programs.cotrtupc.copybook;

import com.carddemo.programs.cotrtupc.support.CobolText;

/**
 * CSUTLDWY - generic date edit work fields (embedded in WS-MISC-STORAGE). COTRTUPC copies the
 * layout but never references it; fields and 88-levels are carried over for the integration stage.
 */
public final class DateEditArea {

    // 10 WS-EDIT-DATE-CCYYMMDD
    public String wsEditDateCc = "  ";    // X(2), redefined 9(2)
    public String wsEditDateYy = "  ";    // X(2), redefined 9(2)
    public String wsEditDateMm = "  ";    // X(2), redefined 9(2)
    public String wsEditDateDd = "  ";    // X(2), redefined 9(2)
    public int wsEditDateBinary;           // S9(9) BINARY
    // 10 WS-CURRENT-DATE
    public String wsCurrentDateYyyymmdd = CobolText.spaces(8);
    public int wsCurrentDateBinary;        // S9(9) BINARY
    // 10 WS-EDIT-DATE-FLGS
    public char wsEditYearFlg = ' ';
    public char wsEditMonth = ' ';
    public char wsEditDay = ' ';
    public String wsDateFormat = "YYYYMMDD";
    // 10 WS-DATE-VALIDATION-RESULT
    public String wsSeverity = CobolText.spaces(4);
    public String wsMsgNo = CobolText.spaces(4);
    public String wsResult = CobolText.spaces(15);
    public String wsDate = CobolText.spaces(10);
    public String wsDateFmt = CobolText.spaces(10);

    /** INITIALIZE (named alphanumerics to SPACES, numerics to ZERO; FILLERs untouched). */
    public void initialize() {
        wsEditDateCc = "  ";
        wsEditDateYy = "  ";
        wsEditDateMm = "  ";
        wsEditDateDd = "  ";
        wsEditDateBinary = 0;
        wsCurrentDateYyyymmdd = CobolText.spaces(8);
        wsCurrentDateBinary = 0;
        wsEditYearFlg = ' ';
        wsEditMonth = ' ';
        wsEditDay = ' ';
        wsDateFormat = CobolText.spaces(8);
        wsSeverity = CobolText.spaces(4);
        wsMsgNo = CobolText.spaces(4);
        wsResult = CobolText.spaces(15);
        wsDate = CobolText.spaces(10);
        wsDateFmt = CobolText.spaces(10);
    }

    public String wsEditDateCcyymmdd() { return wsEditDateCc + wsEditDateYy + wsEditDateMm + wsEditDateDd; }

    private static int num(String s) {
        return s.chars().allMatch(Character::isDigit) ? Integer.parseInt(s) : -1;
    }

    public boolean isThisCentury() { return num(wsEditDateCc) == 20; }
    public boolean isLastCentury() { return num(wsEditDateCc) == 19; }
    public boolean isValidMonth() { int m = num(wsEditDateMm); return m >= 1 && m <= 12; }
    public boolean is31DayMonth() { int m = num(wsEditDateMm); return m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12; }
    public boolean isFebruary() { return num(wsEditDateMm) == 2; }
    public boolean isValidDay() { int d = num(wsEditDateDd); return d >= 1 && d <= 31; }
    public boolean isDay31() { return num(wsEditDateDd) == 31; }
    public boolean isDay30() { return num(wsEditDateDd) == 30; }
    public boolean isDay29() { return num(wsEditDateDd) == 29; }
    public boolean isValidFebDay() { int d = num(wsEditDateDd); return d >= 1 && d <= 28; }

    public boolean isEditDateValid() { return wsEditYearFlg == '\u0000' && wsEditMonth == '\u0000' && wsEditDay == '\u0000'; }
    public boolean isEditDateInvalid() { return wsEditYearFlg == '0' && wsEditMonth == '0' && wsEditDay == '0'; }
    public boolean isYearValid() { return wsEditYearFlg == '\u0000'; }
    public boolean isYearNotOk() { return wsEditYearFlg == '0'; }
    public boolean isYearBlank() { return wsEditYearFlg == 'B'; }
    public boolean isMonthValid() { return wsEditMonth == '\u0000'; }
    public boolean isMonthNotOk() { return wsEditMonth == '0'; }
    public boolean isMonthBlank() { return wsEditMonth == 'B'; }
    public boolean isDayValid() { return wsEditDay == '\u0000'; }
    public boolean isDayNotOk() { return wsEditDay == '0'; }
    public boolean isDayBlank() { return wsEditDay == 'B'; }
}
