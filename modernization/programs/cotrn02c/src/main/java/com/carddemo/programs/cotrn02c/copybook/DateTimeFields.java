package com.carddemo.programs.cotrn02c.copybook;

import java.time.LocalDateTime;

/**
 * CSDAT01Y {@code WS-DATE-TIME}: {@code WS-CURDATE-DATA} as filled by {@code FUNCTION CURRENT-DATE}
 * plus the edited {@code WS-CURDATE-MM-DD-YY}, {@code WS-CURTIME-HH-MM-SS} and {@code WS-TIMESTAMP} views.
 */
public record DateTimeFields(
        int curdateYear,     // 9(04)
        int curdateMonth,    // 9(02)
        int curdateDay,      // 9(02)
        int curtimeHours,    // 9(02)
        int curtimeMinute,   // 9(02)
        int curtimeSecond,   // 9(02)
        int curtimeMilsec,   // 9(02) (hundredths)
        int timestampMs6) {  // 9(06)

    /** {@code MOVE FUNCTION CURRENT-DATE TO WS-CURDATE-DATA}. */
    public static DateTimeFields currentDate(LocalDateTime now) {
        return new DateTimeFields(now.getYear(), now.getMonthValue(), now.getDayOfMonth(),
                now.getHour(), now.getMinute(), now.getSecond(), now.getNano() / 10_000_000,
                now.getNano() / 1_000);
    }

    /** {@code WS-CURDATE-N} (REDEFINES WS-CURDATE, PIC 9(08)). */
    public String curdateN() {
        return Pic.nine(curdateYear, 4) + Pic.nine(curdateMonth, 2) + Pic.nine(curdateDay, 2);
    }

    /** {@code WS-CURTIME-N} (REDEFINES WS-CURTIME, PIC 9(08)). */
    public String curtimeN() {
        return Pic.nine(curtimeHours, 2) + Pic.nine(curtimeMinute, 2) + Pic.nine(curtimeSecond, 2)
                + Pic.nine(curtimeMilsec, 2);
    }

    /** {@code WS-CURDATE-MM-DD-YY}: MM '/' DD '/' YY, YY = WS-CURDATE-YEAR(3:2). */
    public String curdateMmDdYy() {
        return Pic.nine(curdateMonth, 2) + "/" + Pic.nine(curdateDay, 2) + "/"
                + Pic.nine(curdateYear, 4).substring(2, 4);
    }

    /** {@code WS-CURTIME-HH-MM-SS}: HH ':' MM ':' SS. */
    public String curtimeHhMmSs() {
        return Pic.nine(curtimeHours, 2) + ":" + Pic.nine(curtimeMinute, 2) + ":" + Pic.nine(curtimeSecond, 2);
    }

    /** {@code WS-TIMESTAMP}: YYYY-MM-DD HH:MM:SS.ffffff (26 bytes). */
    public String timestamp() {
        return Pic.nine(curdateYear, 4) + "-" + Pic.nine(curdateMonth, 2) + "-" + Pic.nine(curdateDay, 2) + " "
                + curtimeHhMmSs() + "." + Pic.nine(timestampMs6, 6);
    }
}
