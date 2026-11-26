package com.carddemo.programs.cocrdupc.copybook;

import static com.carddemo.programs.cocrdupc.Cobol.numeric;

import java.time.LocalDateTime;

/** CSDAT01Y: 01 WS-DATE-TIME. */
public final class DateTimeFields {

    // 10 WS-CURDATE
    private int wsCurdateYear;   // 9(04)
    private int wsCurdateMonth;  // 9(02)
    private int wsCurdateDay;    // 9(02)
    // 10 WS-CURTIME
    private int wsCurtimeHours;  // 9(02)
    private int wsCurtimeMinute; // 9(02)
    private int wsCurtimeSecond; // 9(02)
    private int wsCurtimeMilsec; // 9(02)  hundredths

    /** MOVE FUNCTION CURRENT-DATE TO WS-CURDATE-DATA. */
    public void setCurrentDate(LocalDateTime now) {
        wsCurdateYear = now.getYear();
        wsCurdateMonth = now.getMonthValue();
        wsCurdateDay = now.getDayOfMonth();
        wsCurtimeHours = now.getHour();
        wsCurtimeMinute = now.getMinute();
        wsCurtimeSecond = now.getSecond();
        wsCurtimeMilsec = now.getNano() / 10_000_000;
    }

    public int wsCurdateYear() { return wsCurdateYear; }
    public int wsCurdateMonth() { return wsCurdateMonth; }
    public int wsCurdateDay() { return wsCurdateDay; }
    public int wsCurtimeHours() { return wsCurtimeHours; }
    public int wsCurtimeMinute() { return wsCurtimeMinute; }
    public int wsCurtimeSecond() { return wsCurtimeSecond; }
    public int wsCurtimeMilsec() { return wsCurtimeMilsec; }

    /** WS-CURDATE-N REDEFINES WS-CURDATE PIC 9(08). */
    public String wsCurdateN() {
        return numeric(wsCurdateYear, 4) + numeric(wsCurdateMonth, 2) + numeric(wsCurdateDay, 2);
    }

    /** WS-CURTIME-N REDEFINES WS-CURTIME PIC 9(08). */
    public String wsCurtimeN() {
        return numeric(wsCurtimeHours, 2) + numeric(wsCurtimeMinute, 2) + numeric(wsCurtimeSecond, 2)
                + numeric(wsCurtimeMilsec, 2);
    }

    /** WS-CURDATE-MM-DD-YY (MM/DD/YY); YY is WS-CURDATE-YEAR(3:2). */
    public String wsCurdateMmDdYy() {
        return numeric(wsCurdateMonth, 2) + "/" + numeric(wsCurdateDay, 2) + "/"
                + numeric(wsCurdateYear, 4).substring(2, 4);
    }

    /** WS-CURTIME-HH-MM-SS (HH:MM:SS). */
    public String wsCurtimeHhMmSs() {
        return numeric(wsCurtimeHours, 2) + ":" + numeric(wsCurtimeMinute, 2) + ":"
                + numeric(wsCurtimeSecond, 2);
    }

    /** WS-TIMESTAMP (YYYY-MM-DD HH:MM:SS.NNNNNN). */
    public String wsTimestamp() {
        return numeric(wsCurdateYear, 4) + "-" + numeric(wsCurdateMonth, 2) + "-"
                + numeric(wsCurdateDay, 2) + " " + wsCurtimeHhMmSs() + "." + numeric(wsCurtimeMilsec, 6);
    }
}
