package com.carddemo.programs.cocrdslc.copybook;

import com.carddemo.programs.cocrdslc.CobolText;
import java.time.LocalDateTime;

/**
 * CSDAT01Y - WS-DATE-TIME. Populated from FUNCTION CURRENT-DATE.
 * Only the pieces COCRDSLC formats are exposed as edited strings.
 */
public final class WsDateTime {
    public int wsCurdateYear;
    public int wsCurdateMonth;
    public int wsCurdateDay;
    public int wsCurtimeHours;
    public int wsCurtimeMinute;
    public int wsCurtimeSecond;
    public int wsCurtimeMilsec;

    /** MOVE FUNCTION CURRENT-DATE TO WS-CURDATE-DATA (hundredths of a second in MILSEC). */
    public void setFromCurrentDate(LocalDateTime now) {
        wsCurdateYear = now.getYear();
        wsCurdateMonth = now.getMonthValue();
        wsCurdateDay = now.getDayOfMonth();
        wsCurtimeHours = now.getHour();
        wsCurtimeMinute = now.getMinute();
        wsCurtimeSecond = now.getSecond();
        wsCurtimeMilsec = now.getNano() / 10_000_000;
    }

    /** WS-CURDATE-N PIC 9(08) = YYYYMMDD. */
    public String wsCurdateN() {
        return CobolText.zoned(wsCurdateYear, 4) + CobolText.zoned(wsCurdateMonth, 2)
                + CobolText.zoned(wsCurdateDay, 2);
    }

    /** WS-CURDATE-MM-DD-YY: MM/DD/YY, YY = WS-CURDATE-YEAR(3:2). */
    public String wsCurdateMmDdYy() {
        String yyyy = CobolText.zoned(wsCurdateYear, 4);
        return CobolText.zoned(wsCurdateMonth, 2) + "/" + CobolText.zoned(wsCurdateDay, 2) + "/"
                + yyyy.substring(2, 4);
    }

    /** WS-CURTIME-HH-MM-SS: HH:MM:SS. */
    public String wsCurtimeHhMmSs() {
        return CobolText.zoned(wsCurtimeHours, 2) + ":" + CobolText.zoned(wsCurtimeMinute, 2) + ":"
                + CobolText.zoned(wsCurtimeSecond, 2);
    }

    /** WS-TIMESTAMP: YYYY-MM-DD HH:MM:SS.ffffff (MS6 = hundredths * 10000). */
    public String wsTimestamp() {
        return CobolText.zoned(wsCurdateYear, 4) + "-" + CobolText.zoned(wsCurdateMonth, 2) + "-"
                + CobolText.zoned(wsCurdateDay, 2) + " " + wsCurtimeHhMmSs() + "."
                + CobolText.zoned(wsCurtimeMilsec * 10_000L, 6);
    }
}
