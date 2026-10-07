package com.carddemo.common.date;

import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

/**
 * COBOL/LE calendar arithmetic: Gregorian leap years, {@code FUNCTION INTEGER-OF-DATE}/{@code DATE-OF-INTEGER}
 * (day 1 = 1601-01-01) and Lilian day numbers (day 1 = 1582-10-15, as returned by {@code CEEDAYS}).
 */
public final class CobolDates {

    /** 1601-01-01 is Lilian day 6654 and integer-of-date day 1. */
    public static final int LILIAN_OFFSET = 6653;

    private static final LocalDate INTEGER_DATE_EPOCH = LocalDate.of(1600, 12, 31);
    private static final LocalDate LILIAN_EPOCH = LocalDate.of(1582, 10, 14);

    private CobolDates() {
    }

    public static boolean isLeapYear(int year) {
        return year % 400 == 0 || (year % 4 == 0 && year % 100 != 0);
    }

    public static int daysInMonth(int year, int month) {
        return switch (month) {
            case 1, 3, 5, 7, 8, 10, 12 -> 31;
            case 4, 6, 9, 11 -> 30;
            case 2 -> isLeapYear(year) ? 29 : 28;
            default -> throw new IllegalArgumentException("month " + month);
        };
    }

    /** {@code FUNCTION INTEGER-OF-DATE(yyyymmdd)} for 1601-01-01 through 9999-12-31. */
    public static int integerOfDate(int yyyymmdd) {
        LocalDate date = toDate(yyyymmdd);
        if (date.getYear() < 1601) {
            throw new IllegalArgumentException("INTEGER-OF-DATE needs a year 1601..9999, got " + yyyymmdd);
        }
        return (int) ChronoUnit.DAYS.between(INTEGER_DATE_EPOCH, date);
    }

    /** {@code FUNCTION DATE-OF-INTEGER(days)} as a {@code yyyymmdd} number. */
    public static int dateOfInteger(int days) {
        if (days < 1 || days > integerOfDate(99991231)) {
            throw new IllegalArgumentException("DATE-OF-INTEGER argument " + days + " out of range");
        }
        LocalDate date = INTEGER_DATE_EPOCH.plusDays(days);
        return date.getYear() * 10000 + date.getMonthValue() * 100 + date.getDayOfMonth();
    }

    public static int lilian(LocalDate date) {
        return (int) ChronoUnit.DAYS.between(LILIAN_EPOCH, date);
    }

    public static LocalDate fromLilian(int lilian) {
        if (lilian < 1) {
            throw new IllegalArgumentException("Lilian days start at 1, got " + lilian);
        }
        return LILIAN_EPOCH.plusDays(lilian);
    }

    public static LocalDate toDate(int yyyymmdd) {
        try {
            return LocalDate.of(yyyymmdd / 10000, yyyymmdd / 100 % 100, yyyymmdd % 100);
        } catch (DateTimeException e) {
            throw new IllegalArgumentException("not a valid date: " + yyyymmdd, e);
        }
    }
}
