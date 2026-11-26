package com.carddemo.programs.copaus1c.copybook;

import java.time.LocalDateTime;

/** CSDAT01Y - WS-DATE-TIME work area (FUNCTION CURRENT-DATE target plus formatted views). */
public final class DateTimeWorkArea {

    // WS-CURDATE-DATA
    private String curdateYear = "0000";   // WS-CURDATE-YEAR   9(04)
    private String curdateMonth = "00";    // WS-CURDATE-MONTH  9(02)
    private String curdateDay = "00";      // WS-CURDATE-DAY    9(02)
    private String curtimeHours = "00";    // WS-CURTIME-HOURS  9(02)
    private String curtimeMinute = "00";   // WS-CURTIME-MINUTE 9(02)
    private String curtimeSecond = "00";   // WS-CURTIME-SECOND 9(02)
    private String curtimeMilsec = "00";   // WS-CURTIME-MILSEC 9(02)

    // WS-CURDATE-MM-DD-YY  (MM '/' DD '/' YY)
    private String curdateMm = "00";
    private String curdateDd = "00";
    private String curdateYy = "00";

    // WS-CURTIME-HH-MM-SS  (HH ':' MM ':' SS)
    private String curtimeHh = "00";
    private String curtimeMm = "00";
    private String curtimeSs = "00";

    /** MOVE FUNCTION CURRENT-DATE TO WS-CURDATE-DATA (first 16 characters: YYYYMMDDHHMMSShh). */
    public void moveCurrentDate(LocalDateTime now) {
        curdateYear = Cobol.nine(now.getYear(), 4);
        curdateMonth = Cobol.nine(now.getMonthValue(), 2);
        curdateDay = Cobol.nine(now.getDayOfMonth(), 2);
        curtimeHours = Cobol.nine(now.getHour(), 2);
        curtimeMinute = Cobol.nine(now.getMinute(), 2);
        curtimeSecond = Cobol.nine(now.getSecond(), 2);
        curtimeMilsec = Cobol.nine(now.getNano() / 10_000_000, 2);
    }

    /** WS-CURDATE-N (REDEFINES WS-CURDATE) PIC 9(08). */
    public String getCurdateN() { return curdateYear + curdateMonth + curdateDay; }
    /** WS-CURTIME-N (REDEFINES WS-CURTIME) PIC 9(08). */
    public String getCurtimeN() { return curtimeHours + curtimeMinute + curtimeSecond + curtimeMilsec; }

    /** WS-CURDATE-MM-DD-YY group as an 8-character alphanumeric. */
    public String getCurdateMmDdYy() { return curdateMm + "/" + curdateDd + "/" + curdateYy; }
    /** WS-CURTIME-HH-MM-SS group as an 8-character alphanumeric. */
    public String getCurtimeHhMmSs() { return curtimeHh + ":" + curtimeMm + ":" + curtimeSs; }

    public String getCurdateYear() { return curdateYear; }
    public String getCurdateMonth() { return curdateMonth; }
    public String getCurdateDay() { return curdateDay; }
    public String getCurtimeHours() { return curtimeHours; }
    public String getCurtimeMinute() { return curtimeMinute; }
    public String getCurtimeSecond() { return curtimeSecond; }
    public String getCurtimeMilsec() { return curtimeMilsec; }

    /** MOVE x TO WS-CURDATE-MM (PIC 9(02)): the two characters are copied as-is. */
    public void setCurdateMm(String v) { curdateMm = Cobol.x(v, 2); }
    public void setCurdateDd(String v) { curdateDd = Cobol.x(v, 2); }
    public void setCurdateYy(String v) { curdateYy = Cobol.x(v, 2); }
    public void setCurtimeHh(String v) { curtimeHh = Cobol.x(v, 2); }
    public void setCurtimeMm(String v) { curtimeMm = Cobol.x(v, 2); }
    public void setCurtimeSs(String v) { curtimeSs = Cobol.x(v, 2); }
}
