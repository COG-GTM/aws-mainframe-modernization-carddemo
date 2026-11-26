package com.carddemo.programs.cousr03c.copybook;

import java.time.LocalDateTime;
import java.util.Locale;

/** CSDAT01Y: 01 WS-DATE-TIME, populated from FUNCTION CURRENT-DATE. */
public final class WsDateTime {

    // WS-CURDATE-DATA / WS-CURDATE (REDEFINED by WS-CURDATE-N 9(08))
    private int curdateYear;     // 9(04)
    private int curdateMonth;    // 9(02)
    private int curdateDay;      // 9(02)
    // WS-CURTIME (REDEFINED by WS-CURTIME-N 9(08))
    private int curtimeHours;    // 9(02)
    private int curtimeMinute;   // 9(02)
    private int curtimeSecond;   // 9(02)
    private int curtimeMilsec;   // 9(02) - hundredths of a second from CURRENT-DATE
    // WS-CURDATE-MM-DD-YY
    private int curdateMm;
    private int curdateDd;
    private int curdateYy;
    // WS-CURTIME-HH-MM-SS
    private int curtimeHh;
    private int curtimeMm;
    private int curtimeSs;

    /** MOVE FUNCTION CURRENT-DATE TO WS-CURDATE-DATA. */
    public void moveCurrentDate(LocalDateTime now) {
        curdateYear = now.getYear();
        curdateMonth = now.getMonthValue();
        curdateDay = now.getDayOfMonth();
        curtimeHours = now.getHour();
        curtimeMinute = now.getMinute();
        curtimeSecond = now.getSecond();
        curtimeMilsec = now.getNano() / 10_000_000;
    }

    public int curdateYear() { return curdateYear; }
    public int curdateMonth() { return curdateMonth; }
    public int curdateDay() { return curdateDay; }
    public int curtimeHours() { return curtimeHours; }
    public int curtimeMinute() { return curtimeMinute; }
    public int curtimeSecond() { return curtimeSecond; }
    public int curtimeMilsec() { return curtimeMilsec; }

    /** WS-CURDATE-N. */
    public int curdateN() { return curdateYear * 10000 + curdateMonth * 100 + curdateDay; }

    /** WS-CURTIME-N. */
    public int curtimeN() { return curtimeHours * 1000000 + curtimeMinute * 10000 + curtimeSecond * 100 + curtimeMilsec; }

    public void setCurdateMm(int v) { curdateMm = v % 100; }
    public void setCurdateDd(int v) { curdateDd = v % 100; }
    public void setCurdateYy(int v) { curdateYy = v % 100; }
    public void setCurtimeHh(int v) { curtimeHh = v % 100; }
    public void setCurtimeMm(int v) { curtimeMm = v % 100; }
    public void setCurtimeSs(int v) { curtimeSs = v % 100; }

    /** WS-CURDATE-MM-DD-YY, PIC X(08) "mm/dd/yy". */
    public String curdateMmDdYy() {
        return String.format(Locale.ROOT, "%02d/%02d/%02d", curdateMm, curdateDd, curdateYy);
    }

    /** WS-CURTIME-HH-MM-SS, PIC X(08) "hh:mm:ss". */
    public String curtimeHhMmSs() {
        return String.format(Locale.ROOT, "%02d:%02d:%02d", curtimeHh, curtimeMm, curtimeSs);
    }

    /** WS-TIMESTAMP "yyyy-mm-dd hh:mm:ss.ffffff" (26 bytes); microseconds derived from WS-CURTIME-MILSEC. */
    public String timestamp() {
        return String.format(Locale.ROOT, "%04d-%02d-%02d %02d:%02d:%02d.%06d",
                curdateYear, curdateMonth, curdateDay, curtimeHours, curtimeMinute, curtimeSecond, curtimeMilsec * 10000);
    }
}
