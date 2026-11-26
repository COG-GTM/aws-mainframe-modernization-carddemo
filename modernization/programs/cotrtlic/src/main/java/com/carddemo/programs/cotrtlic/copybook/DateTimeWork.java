package com.carddemo.programs.cotrtlic.copybook;

import com.carddemo.programs.cotrtlic.support.Cob;
import java.time.LocalDateTime;

/** Copybook CSDAT01Y: 01 WS-DATE-TIME. */
public final class DateTimeWork {
    // 10 WS-CURDATE
    /** WS-CURDATE-YEAR PIC 9(04). */
    public int curdateYear;
    /** WS-CURDATE-MONTH PIC 9(02). */
    public int curdateMonth;
    /** WS-CURDATE-DAY PIC 9(02). */
    public int curdateDay;
    // 10 WS-CURTIME
    /** WS-CURTIME-HOURS PIC 9(02). */
    public int curtimeHours;
    /** WS-CURTIME-MINUTE PIC 9(02). */
    public int curtimeMinute;
    /** WS-CURTIME-SECOND PIC 9(02). */
    public int curtimeSecond;
    /** WS-CURTIME-MILSEC PIC 9(02) (hundredths). */
    public int curtimeMilsec;
    // 05 WS-CURDATE-MM-DD-YY
    public int curdateMm;
    public int curdateDd;
    public int curdateYy;
    // 05 WS-CURTIME-HH-MM-SS
    public int curtimeHh;
    public int curtimeMm;
    public int curtimeSs;

    /** MOVE FUNCTION CURRENT-DATE TO WS-CURDATE-DATA (first 16 bytes: YYYYMMDDHHMMSSss). */
    public void setCurrentDate(LocalDateTime now) {
        curdateYear = now.getYear();
        curdateMonth = now.getMonthValue();
        curdateDay = now.getDayOfMonth();
        curtimeHours = now.getHour();
        curtimeMinute = now.getMinute();
        curtimeSecond = now.getSecond();
        curtimeMilsec = now.getNano() / 10_000_000;
    }

    /** WS-CURDATE-N PIC 9(08). */
    public int curdateN() {
        return curdateYear * 10_000 + curdateMonth * 100 + curdateDay;
    }

    /** WS-CURTIME-N PIC 9(08). */
    public int curtimeN() {
        return curtimeHours * 1_000_000 + curtimeMinute * 10_000 + curtimeSecond * 100 + curtimeMilsec;
    }

    /** WS-CURDATE-MM-DD-YY: MM/DD/YY. */
    public String curdateMmDdYy() {
        return Cob.zeroPad(curdateMm, 2) + "/" + Cob.zeroPad(curdateDd, 2) + "/" + Cob.zeroPad(curdateYy, 2);
    }

    /** WS-CURTIME-HH-MM-SS: HH:MM:SS. */
    public String curtimeHhMmSs() {
        return Cob.zeroPad(curtimeHh, 2) + ":" + Cob.zeroPad(curtimeMm, 2) + ":" + Cob.zeroPad(curtimeSs, 2);
    }

    /** WS-TIMESTAMP: YYYY-MM-DD HH:MM:SS.ffffff (MS6 from the hundredths field). */
    public String timestamp() {
        return Cob.zeroPad(curdateYear, 4) + "-" + Cob.zeroPad(curdateMonth, 2) + "-" + Cob.zeroPad(curdateDay, 2)
                + " " + Cob.zeroPad(curtimeHours, 2) + ":" + Cob.zeroPad(curtimeMinute, 2) + ":"
                + Cob.zeroPad(curtimeSecond, 2) + "." + Cob.zeroPad(curtimeMilsec * 10_000L, 6);
    }
}
