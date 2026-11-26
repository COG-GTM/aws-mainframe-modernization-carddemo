package com.carddemo.programs.cotrn00c.copybook;

import java.time.LocalDateTime;

/**
 * CSDAT01Y - WS-DATE-TIME working storage. Mutable like the COBOL group: COTRN00C shares
 * WS-CURDATE-MM-DD-YY between the screen header and each transaction row date.
 */
public final class DateTimeFields {

    // WS-CURDATE-DATA
    private String curdateYear = "0000";   // WS-CURDATE-YEAR   PIC 9(04)
    private String curdateMonth = "00";    // WS-CURDATE-MONTH  PIC 9(02)
    private String curdateDay = "00";      // WS-CURDATE-DAY    PIC 9(02)
    private String curtimeHours = "00";    // WS-CURTIME-HOURS  PIC 9(02)
    private String curtimeMinute = "00";   // WS-CURTIME-MINUTE PIC 9(02)
    private String curtimeSecond = "00";   // WS-CURTIME-SECOND PIC 9(02)
    private String curtimeMilsec = "00";   // WS-CURTIME-MILSEC PIC 9(02)

    // WS-CURDATE-MM-DD-YY
    private String curdateMm = "00";
    private String curdateDd = "00";
    private String curdateYy = "00";

    // WS-CURTIME-HH-MM-SS
    private String curtimeHh = "00";
    private String curtimeMm = "00";
    private String curtimeSs = "00";

    // WS-TIMESTAMP PIC X(26): YYYY-MM-DD HH:MM:SS.NNNNNN
    private String timestamp = "0000-00-00 00:00:00.000000";

    /** MOVE FUNCTION CURRENT-DATE TO WS-CURDATE-DATA. */
    public void setCurrentDate(LocalDateTime now) {
        curdateYear = CobolText.unsignedDigits(now.getYear(), 4);
        curdateMonth = CobolText.unsignedDigits(now.getMonthValue(), 2);
        curdateDay = CobolText.unsignedDigits(now.getDayOfMonth(), 2);
        curtimeHours = CobolText.unsignedDigits(now.getHour(), 2);
        curtimeMinute = CobolText.unsignedDigits(now.getMinute(), 2);
        curtimeSecond = CobolText.unsignedDigits(now.getSecond(), 2);
        curtimeMilsec = CobolText.unsignedDigits(now.getNano() / 10_000_000, 2);
    }

    /** WS-CURDATE-N (WS-CURDATE REDEFINES as PIC 9(08), YYYYMMDD). */
    public String curdateN() {
        return curdateYear + curdateMonth + curdateDay;
    }

    /** WS-CURTIME-N (WS-CURTIME REDEFINES as PIC 9(08), HHMMSSCC). */
    public String curtimeN() {
        return curtimeHours + curtimeMinute + curtimeSecond + curtimeMilsec;
    }

    public String curdateYear() {
        return curdateYear;
    }

    public String curdateMonth() {
        return curdateMonth;
    }

    public String curdateDay() {
        return curdateDay;
    }

    public String curtimeHours() {
        return curtimeHours;
    }

    public String curtimeMinute() {
        return curtimeMinute;
    }

    public String curtimeSecond() {
        return curtimeSecond;
    }

    public void setCurdateMm(String mm) {
        curdateMm = CobolText.fit(mm, 2);
    }

    public void setCurdateDd(String dd) {
        curdateDd = CobolText.fit(dd, 2);
    }

    public void setCurdateYy(String yy) {
        curdateYy = CobolText.fit(yy, 2);
    }

    public void setCurtimeHh(String hh) {
        curtimeHh = CobolText.fit(hh, 2);
    }

    public void setCurtimeMm(String mm) {
        curtimeMm = CobolText.fit(mm, 2);
    }

    public void setCurtimeSs(String ss) {
        curtimeSs = CobolText.fit(ss, 2);
    }

    /** WS-CURDATE-MM-DD-YY (MM/DD/YY). */
    public String curdateMmDdYy() {
        return curdateMm + "/" + curdateDd + "/" + curdateYy;
    }

    /** WS-CURTIME-HH-MM-SS (HH:MM:SS). */
    public String curtimeHhMmSs() {
        return curtimeHh + ":" + curtimeMm + ":" + curtimeSs;
    }

    /** MOVE x TO WS-TIMESTAMP (group move, overwrites the fillers too). */
    public void setTimestamp(String ts) {
        timestamp = CobolText.fit(ts, 26);
    }

    public String timestamp() {
        return timestamp;
    }

    public String timestampDtYyyy() {
        return timestamp.substring(0, 4);
    }

    public String timestampDtMm() {
        return timestamp.substring(5, 7);
    }

    public String timestampDtDd() {
        return timestamp.substring(8, 10);
    }

    public String timestampTmHh() {
        return timestamp.substring(11, 13);
    }

    public String timestampTmMm() {
        return timestamp.substring(14, 16);
    }

    public String timestampTmSs() {
        return timestamp.substring(17, 19);
    }

    public String timestampTmMs6() {
        return timestamp.substring(20, 26);
    }
}
