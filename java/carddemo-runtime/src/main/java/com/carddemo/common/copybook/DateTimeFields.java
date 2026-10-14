package com.carddemo.common.copybook;

/**
 * CSDAT01Y {@code WS-DATE-TIME}: the date/time work fields shared by most programs, filled from
 * {@code FUNCTION CURRENT-DATE} ({@code MOVE FUNCTION CURRENT-DATE TO WS-CURDATE-DATA} keeps the first 16
 * characters: YYYYMMDDHHMMSSss).
 */
public final class DateTimeFields {

    private final String curdateData;

    private DateTimeFields(String curdateData) {
        this.curdateData = curdateData;
    }

    /** {@code MOVE FUNCTION CURRENT-DATE TO WS-CURDATE-DATA}. */
    public static DateTimeFields fromCurrentDate(String currentDate21) {
        return new DateTimeFields(currentDate21.substring(0, 16));
    }

    public String curdateYear() {
        return curdateData.substring(0, 4);
    }

    public String curdateMonth() {
        return curdateData.substring(4, 6);
    }

    public String curdateDay() {
        return curdateData.substring(6, 8);
    }

    public String curtimeHours() {
        return curdateData.substring(8, 10);
    }

    public String curtimeMinute() {
        return curdateData.substring(10, 12);
    }

    public String curtimeSecond() {
        return curdateData.substring(12, 14);
    }

    public String curtimeMilsec() {
        return curdateData.substring(14, 16);
    }

    /** {@code WS-CURDATE-MM-DD-YY} built as in POPULATE-HEADER-INFO: MM/DD/YY (YY = WS-CURDATE-YEAR(3:2)). */
    public String curdateMmDdYy() {
        return curdateMonth() + "/" + curdateDay() + "/" + curdateYear().substring(2, 4);
    }

    /** {@code WS-CURTIME-HH-MM-SS}: HH:MM:SS. */
    public String curtimeHhMmSs() {
        return curtimeHours() + ":" + curtimeMinute() + ":" + curtimeSecond();
    }
}
