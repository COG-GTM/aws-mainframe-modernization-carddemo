package com.carddemo.programs.cousr02c.copybook;

import java.time.LocalDateTime;

/**
 * CSDAT01Y - {@code 01 WS-DATE-TIME}. Populated from {@code FUNCTION CURRENT-DATE}; all numeric parts are
 * unsigned display PIC 9(n), so values are kept as zero-padded strings of the exact PIC width.
 */
public final class DateTimeFields {

    // 05 WS-CURDATE-DATA
    private String wsCurdateYear = "0000";   // PIC 9(04)
    private String wsCurdateMonth = "00";    // PIC 9(02)
    private String wsCurdateDay = "00";      // PIC 9(02)
    private String wsCurtimeHours = "00";    // PIC 9(02)
    private String wsCurtimeMinute = "00";   // PIC 9(02)
    private String wsCurtimeSecond = "00";   // PIC 9(02)
    private String wsCurtimeMilsec = "00";   // PIC 9(02) (hundredths from CURRENT-DATE)

    /** MOVE FUNCTION CURRENT-DATE TO WS-CURDATE-DATA (first 16 chars: YYYYMMDDHHMMSShh). */
    public void moveCurrentDate(LocalDateTime now) {
        wsCurdateYear = pad(now.getYear(), 4);
        wsCurdateMonth = pad(now.getMonthValue(), 2);
        wsCurdateDay = pad(now.getDayOfMonth(), 2);
        wsCurtimeHours = pad(now.getHour(), 2);
        wsCurtimeMinute = pad(now.getMinute(), 2);
        wsCurtimeSecond = pad(now.getSecond(), 2);
        wsCurtimeMilsec = pad(now.getNano() / 10_000_000, 2);
    }

    private static String pad(int v, int width) {
        String s = Integer.toString(v);
        return s.length() >= width ? s.substring(s.length() - width) : "0".repeat(width - s.length()) + s;
    }

    /** WS-CURDATE-N REDEFINES WS-CURDATE PIC 9(08). */
    public long wsCurdateN() {
        return Long.parseLong(wsCurdateYear + wsCurdateMonth + wsCurdateDay);
    }

    /** WS-CURTIME-N REDEFINES WS-CURTIME PIC 9(08). */
    public long wsCurtimeN() {
        return Long.parseLong(wsCurtimeHours + wsCurtimeMinute + wsCurtimeSecond + wsCurtimeMilsec);
    }

    /** WS-CURDATE-MM-DD-YY: MM '/' DD '/' YY, with YY = WS-CURDATE-YEAR(3:2). */
    public String wsCurdateMmDdYy() {
        return wsCurdateMonth + "/" + wsCurdateDay + "/" + wsCurdateYear.substring(2, 4);
    }

    /** WS-CURTIME-HH-MM-SS: HH ':' MM ':' SS. */
    public String wsCurtimeHhMmSs() {
        return wsCurtimeHours + ":" + wsCurtimeMinute + ":" + wsCurtimeSecond;
    }

    /** WS-TIMESTAMP: YYYY-MM-DD HH:MM:SS.ffffff (MS6 from hundredths, low digits zero). */
    public String wsTimestamp() {
        return wsCurdateYear + "-" + wsCurdateMonth + "-" + wsCurdateDay + " " + wsCurtimeHours + ":"
                + wsCurtimeMinute + ":" + wsCurtimeSecond + "." + wsCurtimeMilsec + "0000";
    }

    public String getWsCurdateYear() {
        return wsCurdateYear;
    }

    public String getWsCurdateMonth() {
        return wsCurdateMonth;
    }

    public String getWsCurdateDay() {
        return wsCurdateDay;
    }

    public String getWsCurtimeHours() {
        return wsCurtimeHours;
    }

    public String getWsCurtimeMinute() {
        return wsCurtimeMinute;
    }

    public String getWsCurtimeSecond() {
        return wsCurtimeSecond;
    }

    public String getWsCurtimeMilsec() {
        return wsCurtimeMilsec;
    }
}
