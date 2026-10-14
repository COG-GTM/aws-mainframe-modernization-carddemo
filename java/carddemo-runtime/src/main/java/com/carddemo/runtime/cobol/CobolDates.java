package com.carddemo.runtime.cobol;

import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

/** Date intrinsics and the Language Environment CEEDAYS service (Gregorian calendar, Java implementation). */
public final class CobolDates {

    private static final LocalDate INTEGER_OF_DATE_EPOCH = LocalDate.of(1600, 12, 31);
    private static final LocalDate LILIAN_EPOCH = LocalDate.of(1582, 10, 14);

    /** CEE message numbers returned in the CEEDAYS feedback token (facility CEE, severity 3). */
    public static final int CEE2EB_INSUFFICIENT_DATA = 2507;
    public static final int CEE2EC_BAD_DATE_VALUE = 2508;
    public static final int CEE2EH_UNSUPPORTED_RANGE = 2513;
    public static final int CEE2EM_BAD_PICTURE = 2518;
    public static final int CEE2EO_NON_NUMERIC = 2520;

    private CobolDates() {
    }

    /** {@code FUNCTION INTEGER-OF-DATE(yyyymmdd)}: days since 1600-12-31; 0 for an invalid argument. */
    public static int integerOfDate(long yyyymmdd) {
        try {
            LocalDate d = LocalDate.of((int) (yyyymmdd / 10000), (int) (yyyymmdd / 100 % 100), (int) (yyyymmdd % 100));
            if (d.getYear() < 1601 || d.getYear() > 9999) {
                return 0;
            }
            return (int) ChronoUnit.DAYS.between(INTEGER_OF_DATE_EPOCH, d);
        } catch (DateTimeException e) {
            return 0;
        }
    }

    /** CEEDAYS feedback: severity 0 and the Lilian day number when the date is valid. */
    public record CeedaysResult(int severity, int msgNo, long lilian) {
        public boolean valid() {
            return severity == 0;
        }
    }

    /**
     * CEEDAYS for the picture strings {@code YYYYMMDD} and {@code YYYY-MM-DD}: Lilian day (1582-10-15 = 1) or a
     * severity-3 condition (non-numeric data, bad date value, outside 1582-10-15..9999-12-31, bad picture).
     */
    public static CeedaysResult ceedays(String date, String picture) {
        String pic = picture.trim();
        String digits;
        if (pic.equals("YYYYMMDD")) {
            digits = date.length() >= 8 ? date.substring(0, 8) : date;
        } else if (pic.equals("YYYY-MM-DD")) {
            if (date.length() < 10 || date.charAt(4) != '-' || date.charAt(7) != '-') {
                return new CeedaysResult(3, CEE2EC_BAD_DATE_VALUE, 0);
            }
            digits = date.substring(0, 4) + date.substring(5, 7) + date.substring(8, 10);
        } else {
            return new CeedaysResult(3, CEE2EM_BAD_PICTURE, 0);
        }
        if (digits.length() < 8) {
            return new CeedaysResult(3, CEE2EB_INSUFFICIENT_DATA, 0);
        }
        if (!digits.chars().allMatch(c -> c >= '0' && c <= '9')) {
            return new CeedaysResult(3, CEE2EO_NON_NUMERIC, 0);
        }
        LocalDate d;
        try {
            d = LocalDate.of(Integer.parseInt(digits.substring(0, 4)), Integer.parseInt(digits.substring(4, 6)),
                    Integer.parseInt(digits.substring(6, 8)));
        } catch (DateTimeException e) {
            return new CeedaysResult(3, CEE2EC_BAD_DATE_VALUE, 0);
        }
        if (!d.isAfter(LILIAN_EPOCH)) {
            return new CeedaysResult(3, CEE2EH_UNSUPPORTED_RANGE, 0);
        }
        return new CeedaysResult(0, 0, ChronoUnit.DAYS.between(LILIAN_EPOCH, d));
    }
}
