package com.carddemo.programs.cotrn01c.copybook;

import java.time.LocalDateTime;

/**
 * CSDAT01Y - 01 WS-DATE-TIME. Holds FUNCTION CURRENT-DATE (WS-CURDATE-DATA) and the derived
 * display groups WS-CURDATE-MM-DD-YY (X(8)), WS-CURTIME-HH-MM-SS (X(8)) and WS-TIMESTAMP (X(26)).
 */
public final class Csdat01y {

    // WS-CURDATE: YEAR 9(4), MONTH 9(2), DAY 9(2); WS-CURDATE-N REDEFINES as 9(8)
    private int curdateYear;
    private int curdateMonth;
    private int curdateDay;
    // WS-CURTIME: HOURS 9(2), MINUTE 9(2), SECOND 9(2), MILSEC 9(2); WS-CURTIME-N REDEFINES as 9(8)
    private int curtimeHours;
    private int curtimeMinute;
    private int curtimeSecond;
    private int curtimeMilsec;
    private int curtimeMicros;

    /** MOVE FUNCTION CURRENT-DATE TO WS-CURDATE-DATA. */
    public void moveCurrentDate(LocalDateTime now) {
        curdateYear = now.getYear();
        curdateMonth = now.getMonthValue();
        curdateDay = now.getDayOfMonth();
        curtimeHours = now.getHour();
        curtimeMinute = now.getMinute();
        curtimeSecond = now.getSecond();
        curtimeMilsec = now.getNano() / 10_000_000; // hundredths: CURRENT-DATE positions 15-16
        curtimeMicros = now.getNano() / 1_000;
    }

    /** WS-CURDATE-N PIC 9(08). */
    public String curdateN() {
        return Cobol.moveUnsignedNumeric(curdateYear, 4) + Cobol.moveUnsignedNumeric(curdateMonth, 2)
                + Cobol.moveUnsignedNumeric(curdateDay, 2);
    }

    /** WS-CURTIME-N PIC 9(08). */
    public String curtimeN() {
        return Cobol.moveUnsignedNumeric(curtimeHours, 2) + Cobol.moveUnsignedNumeric(curtimeMinute, 2)
                + Cobol.moveUnsignedNumeric(curtimeSecond, 2) + Cobol.moveUnsignedNumeric(curtimeMilsec, 2);
    }

    /** WS-CURDATE-MM-DD-YY: MM '/' DD '/' YY, YY = WS-CURDATE-YEAR(3:2). */
    public String curdateMmDdYy() {
        return Cobol.moveUnsignedNumeric(curdateMonth, 2) + "/" + Cobol.moveUnsignedNumeric(curdateDay, 2) + "/"
                + Cobol.moveUnsignedNumeric(curdateYear, 4).substring(2, 4);
    }

    /** WS-CURTIME-HH-MM-SS: HH ':' MM ':' SS. */
    public String curtimeHhMmSs() {
        return Cobol.moveUnsignedNumeric(curtimeHours, 2) + ":" + Cobol.moveUnsignedNumeric(curtimeMinute, 2) + ":"
                + Cobol.moveUnsignedNumeric(curtimeSecond, 2);
    }

    /** WS-TIMESTAMP: YYYY-MM-DD HH:MM:SS.ffffff (X(26)). */
    public String timestamp() {
        return Cobol.moveUnsignedNumeric(curdateYear, 4) + "-" + Cobol.moveUnsignedNumeric(curdateMonth, 2) + "-"
                + Cobol.moveUnsignedNumeric(curdateDay, 2) + " " + curtimeHhMmSs() + "."
                + Cobol.moveUnsignedNumeric(curtimeMicros, 6);
    }
}
