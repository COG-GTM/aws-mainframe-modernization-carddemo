package com.carddemo.programs.cousr00c.copybook;

import com.carddemo.programs.cousr00c.Cobol;
import java.time.LocalDateTime;

/**
 * CSDAT01Y - WS-DATE-TIME, populated from {@code FUNCTION CURRENT-DATE}.
 * All components are unsigned PIC 9(nn) DISPLAY.
 */
public record DateTimeFields(
        int curdateYear,     // WS-CURDATE-YEAR   PIC 9(04)
        int curdateMonth,    // WS-CURDATE-MONTH  PIC 9(02)
        int curdateDay,      // WS-CURDATE-DAY    PIC 9(02)
        int curtimeHours,    // WS-CURTIME-HOURS  PIC 9(02)
        int curtimeMinute,   // WS-CURTIME-MINUTE PIC 9(02)
        int curtimeSecond,   // WS-CURTIME-SECOND PIC 9(02)
        int curtimeMilsec) { // WS-CURTIME-MILSEC PIC 9(02) (hundredths from CURRENT-DATE)

    /** MOVE FUNCTION CURRENT-DATE TO WS-CURDATE-DATA. */
    public static DateTimeFields fromCurrentDate(LocalDateTime now) {
        return new DateTimeFields(now.getYear(), now.getMonthValue(), now.getDayOfMonth(),
                now.getHour(), now.getMinute(), now.getSecond(), now.getNano() / 10_000_000);
    }

    /** WS-CURDATE-N REDEFINES WS-CURDATE PIC 9(08): YYYYMMDD. */
    public String curdateN() {
        return Cobol.numericDisplay(curdateYear, 4) + Cobol.numericDisplay(curdateMonth, 2)
                + Cobol.numericDisplay(curdateDay, 2);
    }

    /** WS-CURTIME-N REDEFINES WS-CURTIME PIC 9(08): HHMMSSss. */
    public String curtimeN() {
        return Cobol.numericDisplay(curtimeHours, 2) + Cobol.numericDisplay(curtimeMinute, 2)
                + Cobol.numericDisplay(curtimeSecond, 2) + Cobol.numericDisplay(curtimeMilsec, 2);
    }

    /** WS-CURDATE-MM-DD-YY; YY is WS-CURDATE-YEAR(3:2). */
    public String curdateMmDdYy() {
        return Cobol.numericDisplay(curdateMonth, 2) + "/" + Cobol.numericDisplay(curdateDay, 2) + "/"
                + Cobol.numericDisplay(curdateYear, 4).substring(2, 4);
    }

    /** WS-CURTIME-HH-MM-SS. */
    public String curtimeHhMmSs() {
        return Cobol.numericDisplay(curtimeHours, 2) + ":" + Cobol.numericDisplay(curtimeMinute, 2) + ":"
                + Cobol.numericDisplay(curtimeSecond, 2);
    }

    /** WS-TIMESTAMP: YYYY-MM-DD HH:MM:SS.ffffff (MS6 holds the hundredths in its high digits). */
    public String timestamp() {
        return Cobol.numericDisplay(curdateYear, 4) + "-" + Cobol.numericDisplay(curdateMonth, 2) + "-"
                + Cobol.numericDisplay(curdateDay, 2) + " " + curtimeHhMmSs() + "."
                + Cobol.numericDisplay(curtimeMilsec * 10_000L, 6);
    }
}
