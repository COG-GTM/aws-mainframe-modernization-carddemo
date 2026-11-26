package com.carddemo.programs.coactupc.gateway;

import java.time.DateTimeException;
import java.time.LocalDate;

/**
 * Default CSUTLDTC/CEEDAYS emulation for the 'YYYYMMDD' picture string: severity 0 with
 * 'Date is valid' for a real Gregorian date in CEEDAYS' supported range (1582-10-15 .. 9999-12-31),
 * otherwise severity 3 with the CEEDAYS message number of the matching feedback token.
 */
public final class LeDateValidator implements DateValidator {

    public static final int MSG_BAD_DATE_VALUE = 2508;
    public static final int MSG_UNSUPP_RANGE = 2513;
    public static final int MSG_INVALID_MONTH = 2517;
    public static final int MSG_BAD_PIC_STRING = 2518;
    public static final int MSG_NON_NUMERIC_DATA = 2520;

    private static final LocalDate LILIAN_START = LocalDate.of(1582, 10, 15);

    @Override
    public DateValidationResult validate(String date, String format) {
        if (!format.trim().equals("YYYYMMDD")) {
            return new DateValidationResult(3, MSG_BAD_PIC_STRING, "Bad Pic String ");
        }
        String d = date.length() >= 8 ? date.substring(0, 8) : date;
        if (d.length() < 8 || !d.chars().allMatch(c -> c >= '0' && c <= '9')) {
            return new DateValidationResult(3, MSG_NON_NUMERIC_DATA, "Nonnumeric data");
        }
        int year = Integer.parseInt(d.substring(0, 4));
        int month = Integer.parseInt(d.substring(4, 6));
        int day = Integer.parseInt(d.substring(6, 8));
        if (month < 1 || month > 12) {
            return new DateValidationResult(3, MSG_INVALID_MONTH, "Invalid month  ");
        }
        LocalDate parsed;
        try {
            parsed = LocalDate.of(year, month, day);
        } catch (DateTimeException e) {
            return new DateValidationResult(3, MSG_BAD_DATE_VALUE, "Datevalue error");
        }
        if (parsed.isBefore(LILIAN_START)) {
            return new DateValidationResult(3, MSG_UNSUPP_RANGE, "Unsupp. Range  ");
        }
        return new DateValidationResult(0, 0, "Date is valid");
    }
}
