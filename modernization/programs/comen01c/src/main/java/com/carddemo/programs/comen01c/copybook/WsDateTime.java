package com.carddemo.programs.comen01c.copybook;

import java.time.LocalDateTime;

/** CSDAT01Y - {@code 01 WS-DATE-TIME}. All numeric items are unsigned PIC 9 DISPLAY. */
public final class WsDateTime {

    // 10 WS-CURDATE (WS-CURDATE-N REDEFINES as PIC 9(08))
    private int curdateYear;     // PIC 9(04)
    private int curdateMonth;    // PIC 9(02)
    private int curdateDay;      // PIC 9(02)
    // 10 WS-CURTIME (WS-CURTIME-N REDEFINES as PIC 9(08))
    private int curtimeHours;    // PIC 9(02)
    private int curtimeMinute;   // PIC 9(02)
    private int curtimeSecond;   // PIC 9(02)
    private int curtimeMilsec;   // PIC 9(02) (hundredths from FUNCTION CURRENT-DATE)
    // 05 WS-CURDATE-MM-DD-YY
    private int curdateMm;
    private int curdateDd;
    private int curdateYy;
    // 05 WS-CURTIME-HH-MM-SS
    private int curtimeHh;
    private int curtimeMm;
    private int curtimeSs;
    // 05 WS-TIMESTAMP
    private int timestampDtYyyy;
    private int timestampDtMm;
    private int timestampDtDd;
    private int timestampTmHh;
    private int timestampTmMm;
    private int timestampTmSs;
    private int timestampTmMs6;  // PIC 9(06)

    /** {@code MOVE FUNCTION CURRENT-DATE TO WS-CURDATE-DATA} (first 16 chars: YYYYMMDDHHMMSSss). */
    public void moveCurrentDate(LocalDateTime now) {
        curdateYear = now.getYear();
        curdateMonth = now.getMonthValue();
        curdateDay = now.getDayOfMonth();
        curtimeHours = now.getHour();
        curtimeMinute = now.getMinute();
        curtimeSecond = now.getSecond();
        curtimeMilsec = now.getNano() / 10_000_000;
    }

    public String curdateN() {
        return CobolText.pic9(curdateYear, 4) + CobolText.pic9(curdateMonth, 2) + CobolText.pic9(curdateDay, 2);
    }

    public String curtimeN() {
        return CobolText.pic9(curtimeHours, 2) + CobolText.pic9(curtimeMinute, 2)
                + CobolText.pic9(curtimeSecond, 2) + CobolText.pic9(curtimeMilsec, 2);
    }

    public int curdateYear() { return curdateYear; }
    public int curdateMonth() { return curdateMonth; }
    public int curdateDay() { return curdateDay; }
    public int curtimeHours() { return curtimeHours; }
    public int curtimeMinute() { return curtimeMinute; }
    public int curtimeSecond() { return curtimeSecond; }
    public int curtimeMilsec() { return curtimeMilsec; }

    public void curdateMm(int v) { curdateMm = v % 100; }
    public void curdateDd(int v) { curdateDd = v % 100; }
    public void curdateYy(int v) { curdateYy = v % 100; }
    public void curtimeHh(int v) { curtimeHh = v % 100; }
    public void curtimeMm(int v) { curtimeMm = v % 100; }
    public void curtimeSs(int v) { curtimeSs = v % 100; }

    /** WS-CURDATE-MM-DD-YY: {@code MM/DD/YY}. */
    public String curdateMmDdYy() {
        return CobolText.pic9(curdateMm, 2) + "/" + CobolText.pic9(curdateDd, 2) + "/" + CobolText.pic9(curdateYy, 2);
    }

    /** WS-CURTIME-HH-MM-SS: {@code HH:MM:SS}. */
    public String curtimeHhMmSs() {
        return CobolText.pic9(curtimeHh, 2) + ":" + CobolText.pic9(curtimeMm, 2) + ":" + CobolText.pic9(curtimeSs, 2);
    }

    public void timestamp(int yyyy, int mm, int dd, int hh, int mi, int ss, int ms6) {
        timestampDtYyyy = yyyy % 10_000;
        timestampDtMm = mm % 100;
        timestampDtDd = dd % 100;
        timestampTmHh = hh % 100;
        timestampTmMm = mi % 100;
        timestampTmSs = ss % 100;
        timestampTmMs6 = ms6 % 1_000_000;
    }

    /** WS-TIMESTAMP: {@code YYYY-MM-DD HH:MM:SS.nnnnnn}. */
    public String timestamp() {
        return CobolText.pic9(timestampDtYyyy, 4) + "-" + CobolText.pic9(timestampDtMm, 2) + "-"
                + CobolText.pic9(timestampDtDd, 2) + " " + CobolText.pic9(timestampTmHh, 2) + ":"
                + CobolText.pic9(timestampTmMm, 2) + ":" + CobolText.pic9(timestampTmSs, 2) + "."
                + CobolText.pic9(timestampTmMs6, 6);
    }
}
