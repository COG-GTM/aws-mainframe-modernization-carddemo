package com.carddemo.programs.cocrdlic.copybook;

import java.time.LocalDateTime;

/** CSDAT01Y - WS-DATE-TIME, populated from FUNCTION CURRENT-DATE. */
public final class DateTimeFields {
    // WS-CURDATE-DATA
    public int curdateYear;     // 9(04)
    public int curdateMonth;    // 9(02)
    public int curdateDay;      // 9(02)
    public int curtimeHours;    // 9(02)
    public int curtimeMinute;   // 9(02)
    public int curtimeSecond;   // 9(02)
    public int curtimeMilsec;   // 9(02) (hundredths in CURRENT-DATE)

    /** MOVE FUNCTION CURRENT-DATE TO WS-CURDATE-DATA. */
    public void setFromCurrentDate(LocalDateTime now) {
        curdateYear = now.getYear();
        curdateMonth = now.getMonthValue();
        curdateDay = now.getDayOfMonth();
        curtimeHours = now.getHour();
        curtimeMinute = now.getMinute();
        curtimeSecond = now.getSecond();
        curtimeMilsec = now.getNano() / 10_000_000;
    }

    public String curdateN() {
        return Pic.nine(curdateYear, 4) + Pic.nine(curdateMonth, 2) + Pic.nine(curdateDay, 2);
    }

    /** WS-CURDATE-MM-DD-YY (MM/DD/YY; YY = WS-CURDATE-YEAR(3:2)). */
    public String curdateMmDdYy() {
        return Pic.nine(curdateMonth, 2) + "/" + Pic.nine(curdateDay, 2) + "/"
                + Pic.nine(curdateYear, 4).substring(2, 4);
    }

    /** WS-CURTIME-HH-MM-SS. */
    public String curtimeHhMmSs() {
        return Pic.nine(curtimeHours, 2) + ":" + Pic.nine(curtimeMinute, 2) + ":" + Pic.nine(curtimeSecond, 2);
    }

    /** WS-TIMESTAMP layout YYYY-MM-DD HH:MM:SS.ffffff. */
    public String timestamp() {
        return Pic.nine(curdateYear, 4) + "-" + Pic.nine(curdateMonth, 2) + "-" + Pic.nine(curdateDay, 2) + " "
                + curtimeHhMmSs() + "." + Pic.nine(curtimeMilsec * 10_000L, 6);
    }
}
