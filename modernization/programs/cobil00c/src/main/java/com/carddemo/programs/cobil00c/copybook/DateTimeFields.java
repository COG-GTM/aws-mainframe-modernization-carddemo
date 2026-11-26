package com.carddemo.programs.cobil00c.copybook;

import java.time.LocalDateTime;

/**
 * CSDAT01Y {@code WS-DATE-TIME}: {@code WS-CURDATE-DATA} (FUNCTION CURRENT-DATE prefix),
 * {@code WS-CURDATE-MM-DD-YY}, {@code WS-CURTIME-HH-MM-SS} and {@code WS-TIMESTAMP}.
 */
public final class DateTimeFields {

    // 10 WS-CURDATE (YEAR 9(4), MONTH 9(2), DAY 9(2)) / WS-CURDATE-N REDEFINES 9(08)
    private int curdateYear;
    private int curdateMonth;
    private int curdateDay;
    // 10 WS-CURTIME (HOURS, MINUTE, SECOND, MILSEC 9(2) each) / WS-CURTIME-N REDEFINES 9(08)
    private int curtimeHours;
    private int curtimeMinute;
    private int curtimeSecond;
    private int curtimeMilsec;

    /** WS-TIMESTAMP PIC X(26) as laid out by its FILLER VALUE clauses. */
    private String timestamp = "0000-00-00 00:00:00.000000";

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

    public int curdateN() {
        return curdateYear * 10000 + curdateMonth * 100 + curdateDay;
    }

    public int curtimeN() {
        return ((curtimeHours * 100 + curtimeMinute) * 100 + curtimeSecond) * 100 + curtimeMilsec;
    }

    /** WS-CURDATE-MM-DD-YY: MM '/' DD '/' YY where YY = WS-CURDATE-YEAR(3:2). */
    public String curdateMmDdYy() {
        String yyyy = Cobol.unsignedDigits(curdateYear, 4);
        return Cobol.unsignedDigits(curdateMonth, 2) + "/" + Cobol.unsignedDigits(curdateDay, 2) + "/"
                + yyyy.substring(2, 4);
    }

    /** WS-CURTIME-HH-MM-SS: HH ':' MM ':' SS. */
    public String curtimeHhMmSs() {
        return Cobol.unsignedDigits(curtimeHours, 2) + ":" + Cobol.unsignedDigits(curtimeMinute, 2) + ":"
                + Cobol.unsignedDigits(curtimeSecond, 2);
    }

    /**
     * {@code INITIALIZE WS-TIMESTAMP}: numeric elementary items become zero; FILLER separators
     * are not touched by INITIALIZE so they keep their VALUE clauses.
     */
    public void initializeTimestamp() {
        timestamp = "0000-00-00 00:00:00.000000";
    }

    /** Reference-modified MOVE {@code WS-TIMESTAMP(start:len)}; 1-based like COBOL. */
    public void moveToTimestamp(int start, int length, String value) {
        String v = Cobol.alnum(value, length);
        timestamp = timestamp.substring(0, start - 1) + v + timestamp.substring(start - 1 + length);
    }

    /** {@code MOVE ZEROS TO WS-TIMESTAMP-TM-MS6} (positions 21-26). */
    public void zeroTimestampMicros() {
        moveToTimestamp(21, 6, "000000");
    }

    public String timestamp() {
        return timestamp;
    }
}
