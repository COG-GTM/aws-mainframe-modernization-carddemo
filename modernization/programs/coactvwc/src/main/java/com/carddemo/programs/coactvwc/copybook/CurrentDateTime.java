package com.carddemo.programs.coactvwc.copybook;

import com.carddemo.programs.coactvwc.support.Cobol;
import java.time.LocalDateTime;

/** CSDAT01Y: {@code WS-DATE-TIME}, populated from {@code FUNCTION CURRENT-DATE}. */
public record CurrentDateTime(int year, int month, int day, int hours, int minute, int second, int milsec) {

    public static CurrentDateTime of(LocalDateTime t) {
        return new CurrentDateTime(t.getYear(), t.getMonthValue(), t.getDayOfMonth(), t.getHour(), t.getMinute(),
                t.getSecond(), t.getNano() / 10_000_000);
    }

    /** WS-CURDATE-N PIC 9(08) (YYYYMMDD). */
    public String curdateN() {
        return Cobol.digits(year, 4) + Cobol.digits(month, 2) + Cobol.digits(day, 2);
    }

    /** WS-CURTIME-N PIC 9(08) (HHMMSSss). */
    public String curtimeN() {
        return Cobol.digits(hours, 2) + Cobol.digits(minute, 2) + Cobol.digits(second, 2) + Cobol.digits(milsec, 2);
    }

    /** WS-CURDATE-MM-DD-YY: MM/DD/YY with YY = WS-CURDATE-YEAR(3:2). */
    public String curdateMmDdYy() {
        return Cobol.digits(month, 2) + "/" + Cobol.digits(day, 2) + "/" + Cobol.digits(year, 4).substring(2, 4);
    }

    /** WS-CURTIME-HH-MM-SS: HH:MM:SS. */
    public String curtimeHhMmSs() {
        return Cobol.digits(hours, 2) + ":" + Cobol.digits(minute, 2) + ":" + Cobol.digits(second, 2);
    }

    /** WS-TIMESTAMP: YYYY-MM-DD HH:MM:SS.ffffff (CURRENT-DATE gives hundredths, so the low 4 digits are 0). */
    public String timestamp() {
        return Cobol.digits(year, 4) + "-" + Cobol.digits(month, 2) + "-" + Cobol.digits(day, 2) + " "
                + Cobol.digits(hours, 2) + ":" + Cobol.digits(minute, 2) + ":" + Cobol.digits(second, 2) + "."
                + Cobol.digits(milsec * 10_000L, 6);
    }
}
