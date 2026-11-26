package com.carddemo.programs.cotrtupc.copybook;

import java.time.LocalDateTime;

/** CSDAT01Y - WS-DATE-TIME. Numeric display fields are held as zero-padded strings. */
public final class DateTimeArea {

    // 05 WS-CURDATE-DATA (WS-CURDATE 9(8) + WS-CURTIME 9(8))
    public String wsCurdateYear = "0000";   // 9(04)
    public String wsCurdateMonth = "00";    // 9(02)
    public String wsCurdateDay = "00";      // 9(02)
    public String wsCurtimeHours = "00";    // 9(02)
    public String wsCurtimeMinute = "00";   // 9(02)
    public String wsCurtimeSecond = "00";   // 9(02)
    public String wsCurtimeMilsec = "00";   // 9(02)
    // 05 WS-CURDATE-MM-DD-YY
    public String wsCurdateMm = "00";
    public String wsCurdateDd = "00";
    public String wsCurdateYy = "00";
    // 05 WS-CURTIME-HH-MM-SS
    public String wsCurtimeHh = "00";
    public String wsCurtimeMm = "00";
    public String wsCurtimeSs = "00";
    // 05 WS-TIMESTAMP
    public String wsTimestampDtYyyy = "0000";
    public String wsTimestampDtMm = "00";
    public String wsTimestampDtDd = "00";
    public String wsTimestampTmHh = "00";
    public String wsTimestampTmMm = "00";
    public String wsTimestampTmSs = "00";
    public String wsTimestampTmMs6 = "000000";

    /** MOVE FUNCTION CURRENT-DATE TO WS-CURDATE-DATA (first 16 bytes: YYYYMMDDHHMMSShh). */
    public void moveCurrentDate(LocalDateTime now) {
        wsCurdateYear = String.format("%04d", now.getYear());
        wsCurdateMonth = String.format("%02d", now.getMonthValue());
        wsCurdateDay = String.format("%02d", now.getDayOfMonth());
        wsCurtimeHours = String.format("%02d", now.getHour());
        wsCurtimeMinute = String.format("%02d", now.getMinute());
        wsCurtimeSecond = String.format("%02d", now.getSecond());
        wsCurtimeMilsec = String.format("%02d", now.getNano() / 10_000_000);
    }

    public String wsCurdateN() { return wsCurdateYear + wsCurdateMonth + wsCurdateDay; }
    public String wsCurtimeN() { return wsCurtimeHours + wsCurtimeMinute + wsCurtimeSecond + wsCurtimeMilsec; }
    public String wsCurdateMmDdYy() { return wsCurdateMm + "/" + wsCurdateDd + "/" + wsCurdateYy; }
    public String wsCurtimeHhMmSs() { return wsCurtimeHh + ":" + wsCurtimeMm + ":" + wsCurtimeSs; }

    public String wsTimestamp() {
        return wsTimestampDtYyyy + "-" + wsTimestampDtMm + "-" + wsTimestampDtDd + " "
                + wsTimestampTmHh + ":" + wsTimestampTmMm + ":" + wsTimestampTmSs + "." + wsTimestampTmMs6;
    }
}
