package com.carddemo.programs.cousr01c.copybook;

import java.time.LocalDateTime;

/** CSDAT01Y - WS-DATE-TIME. */
public final class DateTimeFields {

    // 05 WS-CURDATE-DATA (filled by MOVE FUNCTION CURRENT-DATE)
    private int curdateYear;     // PIC 9(04)
    private int curdateMonth;    // PIC 9(02)
    private int curdateDay;      // PIC 9(02)
    private int curtimeHours;    // PIC 9(02)
    private int curtimeMinute;   // PIC 9(02)
    private int curtimeSecond;   // PIC 9(02)
    private int curtimeMilsec;   // PIC 9(02) - hundredths of a second
    // 05 WS-CURDATE-MM-DD-YY
    private String curdateMm = "00";
    private String curdateDd = "00";
    private String curdateYy = "00";
    // 05 WS-CURTIME-HH-MM-SS
    private String curtimeHh = "00";
    private String curtimeMm = "00";
    private String curtimeSs = "00";

    /** {@code MOVE FUNCTION CURRENT-DATE TO WS-CURDATE-DATA}. */
    public void moveCurrentDate(LocalDateTime now) {
        curdateYear = now.getYear();
        curdateMonth = now.getMonthValue();
        curdateDay = now.getDayOfMonth();
        curtimeHours = now.getHour();
        curtimeMinute = now.getMinute();
        curtimeSecond = now.getSecond();
        curtimeMilsec = now.getNano() / 10_000_000;
    }

    /** WS-CURDATE-N REDEFINES WS-CURDATE PIC 9(08). */
    public String curdateN() {
        return Pic.nine(curdateYear, 4) + Pic.nine(curdateMonth, 2) + Pic.nine(curdateDay, 2);
    }

    /** WS-CURTIME-N REDEFINES WS-CURTIME PIC 9(08). */
    public String curtimeN() {
        return Pic.nine(curtimeHours, 2) + Pic.nine(curtimeMinute, 2) + Pic.nine(curtimeSecond, 2)
                + Pic.nine(curtimeMilsec, 2);
    }

    public int curdateYear() { return curdateYear; }
    public int curdateMonth() { return curdateMonth; }
    public int curdateDay() { return curdateDay; }
    public int curtimeHours() { return curtimeHours; }
    public int curtimeMinute() { return curtimeMinute; }
    public int curtimeSecond() { return curtimeSecond; }
    public int curtimeMilsec() { return curtimeMilsec; }

    public void curdateMm(int v) { curdateMm = Pic.nine(v, 2); }
    public void curdateDd(int v) { curdateDd = Pic.nine(v, 2); }
    /** {@code MOVE WS-CURDATE-YEAR(3:2) TO WS-CURDATE-YY}. */
    public void curdateYy(String v) { curdateYy = v; }
    public void curtimeHh(int v) { curtimeHh = Pic.nine(v, 2); }
    public void curtimeMm(int v) { curtimeMm = Pic.nine(v, 2); }
    public void curtimeSs(int v) { curtimeSs = Pic.nine(v, 2); }

    /** WS-CURDATE-MM-DD-YY (8 bytes). */
    public String curdateMmDdYy() {
        return curdateMm + "/" + curdateDd + "/" + curdateYy;
    }

    /** WS-CURTIME-HH-MM-SS (8 bytes). */
    public String curtimeHhMmSs() {
        return curtimeHh + ":" + curtimeMm + ":" + curtimeSs;
    }

    /** WS-TIMESTAMP layout (YYYY-MM-DD HH:MM:SS.ffffff, 26 bytes) from the current-date fields. */
    public String timestamp() {
        return Pic.nine(curdateYear, 4) + "-" + Pic.nine(curdateMonth, 2) + "-" + Pic.nine(curdateDay, 2)
                + " " + Pic.nine(curtimeHours, 2) + ":" + Pic.nine(curtimeMinute, 2) + ":"
                + Pic.nine(curtimeSecond, 2) + "." + Pic.nine(curtimeMilsec * 10_000L, 6);
    }
}
