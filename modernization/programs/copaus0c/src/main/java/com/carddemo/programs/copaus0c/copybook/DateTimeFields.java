package com.carddemo.programs.copaus0c.copybook;

import java.util.Locale;

import java.time.LocalDateTime;

/** CSDAT01Y - WS-DATE-TIME. Working storage; mutable like the COBOL group. */
public final class DateTimeFields {
    // WS-CURDATE-DATA (FUNCTION CURRENT-DATE)
    public String wsCurdateYear = "0000";  // PIC 9(04)
    public String wsCurdateMonth = "00";   // PIC 9(02)
    public String wsCurdateDay = "00";     // PIC 9(02)
    public String wsCurtimeHours = "00";   // PIC 9(02)
    public String wsCurtimeMinute = "00";  // PIC 9(02)
    public String wsCurtimeSecond = "00";  // PIC 9(02)
    public String wsCurtimeMilsec = "00";  // PIC 9(02)
    // WS-CURDATE-MM-DD-YY
    public String wsCurdateMm = "00";      // PIC 9(02)
    public String wsCurdateDd = "00";      // PIC 9(02)
    public String wsCurdateYy = "00";      // PIC 9(02)
    // WS-CURTIME-HH-MM-SS
    public String wsCurtimeHh = "00";      // PIC 9(02)
    public String wsCurtimeMm = "00";      // PIC 9(02)
    public String wsCurtimeSs = "00";      // PIC 9(02)

    /** MOVE FUNCTION CURRENT-DATE TO WS-CURDATE-DATA. */
    public void setCurrentDate(LocalDateTime now) {
        wsCurdateYear = String.format(Locale.ROOT, "%04d", now.getYear());
        wsCurdateMonth = String.format(Locale.ROOT, "%02d", now.getMonthValue());
        wsCurdateDay = String.format(Locale.ROOT, "%02d", now.getDayOfMonth());
        wsCurtimeHours = String.format(Locale.ROOT, "%02d", now.getHour());
        wsCurtimeMinute = String.format(Locale.ROOT, "%02d", now.getMinute());
        wsCurtimeSecond = String.format(Locale.ROOT, "%02d", now.getSecond());
        wsCurtimeMilsec = String.format(Locale.ROOT, "%02d", now.getNano() / 10_000_000);
    }

    /** WS-CURDATE-MM-DD-YY as an 8-character group (MM/DD/YY). */
    public String curdateMmDdYy() {
        return wsCurdateMm + "/" + wsCurdateDd + "/" + wsCurdateYy;
    }

    /** WS-CURTIME-HH-MM-SS as an 8-character group (HH:MM:SS). */
    public String curtimeHhMmSs() {
        return wsCurtimeHh + ":" + wsCurtimeMm + ":" + wsCurtimeSs;
    }
}
