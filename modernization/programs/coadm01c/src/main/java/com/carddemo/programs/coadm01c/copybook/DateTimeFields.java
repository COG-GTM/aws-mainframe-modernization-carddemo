package com.carddemo.programs.coadm01c.copybook;

import java.time.LocalDateTime;

/** CSDAT01Y - 01 WS-DATE-TIME, populated from {@code FUNCTION CURRENT-DATE}. */
public record DateTimeFields(int year, int month, int day, int hours, int minute, int second, int milsec) {

    /** {@code MOVE FUNCTION CURRENT-DATE TO WS-CURDATE-DATA}. WS-CURTIME-MILSEC holds hundredths of a second. */
    public static DateTimeFields currentDate(LocalDateTime now) {
        return new DateTimeFields(now.getYear(), now.getMonthValue(), now.getDayOfMonth(),
                now.getHour(), now.getMinute(), now.getSecond(), now.getNano() / 10_000_000);
    }

    /** WS-CURDATE-N PIC 9(08) (YYYYMMDD). */
    public String curdateN() {
        return CobolFields.unsigned(year, 4) + CobolFields.unsigned(month, 2) + CobolFields.unsigned(day, 2);
    }

    /** WS-CURTIME-N PIC 9(08) (HHMMSSss). */
    public String curtimeN() {
        return CobolFields.unsigned(hours, 2) + CobolFields.unsigned(minute, 2)
                + CobolFields.unsigned(second, 2) + CobolFields.unsigned(milsec, 2);
    }

    /** WS-CURDATE-MM-DD-YY: YY is WS-CURDATE-YEAR(3:2). */
    public String curdateMmDdYy() {
        return CobolFields.unsigned(month, 2) + "/" + CobolFields.unsigned(day, 2) + "/"
                + CobolFields.unsigned(year, 4).substring(2, 4);
    }

    /** WS-CURTIME-HH-MM-SS. */
    public String curtimeHhMmSs() {
        return CobolFields.unsigned(hours, 2) + ":" + CobolFields.unsigned(minute, 2) + ":"
                + CobolFields.unsigned(second, 2);
    }

    /** WS-TIMESTAMP (YYYY-MM-DD HH:MM:SS.ffffff); WS-TIMESTAMP-TM-MS6 is filled by callers, here from milsec. */
    public String timestamp() {
        return CobolFields.unsigned(year, 4) + "-" + CobolFields.unsigned(month, 2) + "-"
                + CobolFields.unsigned(day, 2) + " " + curtimeHhMmSs() + "."
                + CobolFields.unsigned(milsec * 10_000L, 6);
    }
}
