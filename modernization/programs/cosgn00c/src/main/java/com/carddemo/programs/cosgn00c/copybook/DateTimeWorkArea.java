package com.carddemo.programs.cosgn00c.copybook;

import static com.carddemo.programs.cosgn00c.copybook.CobolText.pic9;

import java.time.LocalDateTime;

/**
 * CSDAT01Y - WS-DATE-TIME. Populated from {@code FUNCTION CURRENT-DATE} (first 16 bytes:
 * YYYYMMDDHHMMSSss) and formatted into the MM/DD/YY, HH:MM:SS and timestamp display groups.
 */
public final class DateTimeWorkArea {

    // WS-CURDATE-DATA
    private int curdateYear;      // WS-CURDATE-YEAR   PIC 9(04)
    private int curdateMonth;     // WS-CURDATE-MONTH  PIC 9(02)
    private int curdateDay;       // WS-CURDATE-DAY    PIC 9(02)
    private int curtimeHours;     // WS-CURTIME-HOURS  PIC 9(02)
    private int curtimeMinute;    // WS-CURTIME-MINUTE PIC 9(02)
    private int curtimeSecond;    // WS-CURTIME-SECOND PIC 9(02)
    private int curtimeMilsec;    // WS-CURTIME-MILSEC PIC 9(02) (hundredths)

    // WS-CURDATE-MM-DD-YY
    private int curdateMm;        // WS-CURDATE-MM PIC 9(02)
    private int curdateDd;        // WS-CURDATE-DD PIC 9(02)
    private int curdateYy;        // WS-CURDATE-YY PIC 9(02)

    // WS-CURTIME-HH-MM-SS
    private int curtimeHh;        // WS-CURTIME-HH PIC 9(02)
    private int curtimeMm;        // WS-CURTIME-MM PIC 9(02)
    private int curtimeSs;        // WS-CURTIME-SS PIC 9(02)

    // WS-TIMESTAMP
    private int timestampDtYyyy;  // WS-TIMESTAMP-DT-YYYY PIC 9(04)
    private int timestampDtMm;    // WS-TIMESTAMP-DT-MM   PIC 9(02)
    private int timestampDtDd;    // WS-TIMESTAMP-DT-DD   PIC 9(02)
    private int timestampTmHh;    // WS-TIMESTAMP-TM-HH   PIC 9(02)
    private int timestampTmMm;    // WS-TIMESTAMP-TM-MM   PIC 9(02)
    private int timestampTmSs;    // WS-TIMESTAMP-TM-SS   PIC 9(02)
    private int timestampTmMs6;   // WS-TIMESTAMP-TM-MS6  PIC 9(06)

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

    /** WS-CURDATE-N REDEFINES WS-CURDATE PIC 9(08). */
    public String curdateN() {
        return pic9(curdateYear, 4) + pic9(curdateMonth, 2) + pic9(curdateDay, 2);
    }

    /** WS-CURTIME-N REDEFINES WS-CURTIME PIC 9(08). */
    public String curtimeN() {
        return pic9(curtimeHours, 2) + pic9(curtimeMinute, 2) + pic9(curtimeSecond, 2) + pic9(curtimeMilsec, 2);
    }

    /**
     * MOVE WS-CURDATE-MONTH TO WS-CURDATE-MM, WS-CURDATE-DAY TO WS-CURDATE-DD,
     * WS-CURDATE-YEAR(3:2) TO WS-CURDATE-YY.
     */
    public void buildMmDdYy() {
        curdateMm = curdateMonth;
        curdateDd = curdateDay;
        curdateYy = Integer.parseInt(pic9(curdateYear, 4).substring(2, 4));
    }

    /** MOVE WS-CURTIME-HOURS/MINUTE/SECOND TO WS-CURTIME-HH/MM/SS. */
    public void buildHhMmSs() {
        curtimeHh = curtimeHours;
        curtimeMm = curtimeMinute;
        curtimeSs = curtimeSecond;
    }

    /** WS-TIMESTAMP fields from WS-CURDATE-DATA (not used by COSGN00C, kept for layout completeness). */
    public void buildTimestamp() {
        timestampDtYyyy = curdateYear;
        timestampDtMm = curdateMonth;
        timestampDtDd = curdateDay;
        timestampTmHh = curtimeHours;
        timestampTmMm = curtimeMinute;
        timestampTmSs = curtimeSecond;
        timestampTmMs6 = curtimeMilsec * 10_000;
    }

    /** WS-CURDATE-MM-DD-YY display group, 8 bytes. */
    public String curdateMmDdYy() {
        return pic9(curdateMm, 2) + "/" + pic9(curdateDd, 2) + "/" + pic9(curdateYy, 2);
    }

    /** WS-CURTIME-HH-MM-SS display group, 8 bytes. */
    public String curtimeHhMmSs() {
        return pic9(curtimeHh, 2) + ":" + pic9(curtimeMm, 2) + ":" + pic9(curtimeSs, 2);
    }

    /** WS-TIMESTAMP display group, 26 bytes (YYYY-MM-DD HH:MM:SS.ffffff). */
    public String timestamp() {
        return pic9(timestampDtYyyy, 4) + "-" + pic9(timestampDtMm, 2) + "-" + pic9(timestampDtDd, 2)
                + " " + pic9(timestampTmHh, 2) + ":" + pic9(timestampTmMm, 2) + ":" + pic9(timestampTmSs, 2)
                + "." + pic9(timestampTmMs6, 6);
    }
}
