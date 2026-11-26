package com.carddemo.programs.corpt00c.copybook;

import java.time.LocalDate;
import java.time.LocalDateTime;

/** CSDAT01Y - 01 WS-DATE-TIME, populated from FUNCTION CURRENT-DATE. */
public final class DateTimeWorkArea {

    // 10 WS-CURDATE (WS-CURDATE-N REDEFINES as 9(08))
    public int curdateYear;    // 9(04)
    public int curdateMonth;   // 9(02)
    public int curdateDay;     // 9(02)
    // 10 WS-CURTIME (WS-CURTIME-N REDEFINES as 9(08))
    public int curtimeHours;   // 9(02)
    public int curtimeMinute;  // 9(02)
    public int curtimeSecond;  // 9(02)
    public int curtimeMilsec;  // 9(02) - hundredths of a second from CURRENT-DATE
    // 05 WS-CURDATE-MM-DD-YY
    public int curdateMm;
    public int curdateDd;
    public int curdateYy;
    // 05 WS-CURTIME-HH-MM-SS
    public int curtimeHh;
    public int curtimeMm;
    public int curtimeSs;
    // 05 WS-TIMESTAMP
    public int timestampDtYyyy;
    public int timestampDtMm;
    public int timestampDtDd;
    public int timestampTmHh;
    public int timestampTmMm;
    public int timestampTmSs;
    public int timestampTmMs6;

    /** MOVE FUNCTION CURRENT-DATE TO WS-CURDATE-DATA. */
    public void moveCurrentDate(LocalDateTime now) {
        curdateYear = now.getYear() % 10000;
        curdateMonth = now.getMonthValue();
        curdateDay = now.getDayOfMonth();
        curtimeHours = now.getHour();
        curtimeMinute = now.getMinute();
        curtimeSecond = now.getSecond();
        curtimeMilsec = now.getNano() / 10_000_000;
    }

    /** WS-CURDATE-N (PIC 9(08) YYYYMMDD). */
    public int curdateN() {
        return curdateYear * 10000 + curdateMonth * 100 + curdateDay;
    }

    /** COMPUTE WS-CURDATE-N = n (stores YYYYMMDD back into the year/month/day subfields). */
    public void setCurdateN(int n) {
        curdateYear = (n / 10000) % 10000;
        curdateMonth = (n / 100) % 100;
        curdateDay = n % 100;
    }

    public LocalDate curdate() {
        return LocalDate.of(curdateYear, curdateMonth, curdateDay);
    }

    /** WS-CURDATE-MM-DD-YY: MM/DD/YY. */
    public String curdateMmDdYy() {
        return String.format("%02d/%02d/%02d", curdateMm, curdateDd, curdateYy);
    }

    /** WS-CURTIME-HH-MM-SS: HH:MM:SS. */
    public String curtimeHhMmSs() {
        return String.format("%02d:%02d:%02d", curtimeHh, curtimeMm, curtimeSs);
    }

    /** WS-TIMESTAMP: YYYY-MM-DD HH:MM:SS.NNNNNN. */
    public String timestamp() {
        return String.format("%04d-%02d-%02d %02d:%02d:%02d.%06d", timestampDtYyyy, timestampDtMm,
                timestampDtDd, timestampTmHh, timestampTmMm, timestampTmSs, timestampTmMs6);
    }
}
