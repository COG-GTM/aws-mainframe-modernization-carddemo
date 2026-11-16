package com.carddemo.domain.date;

import java.time.LocalDate;
import java.time.Year;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

/**
 * The part of the Language Environment service {@code CEEDAYS} that CardDemo relies on: convert a date
 * string, read through a picture string, to a Lilian day number (day 1 = 15 October 1582) and report the outcome as one of the feedback tokens {@code CSUTLDTC} tests.
 *
 * <p>CEEDAYS is IBM code, not part of {@code app/}, so the rules here follow the IBM z/OS Language
 * Environment documentation for CEEDAYS and have not been run against LE itself:
 *
 * <ul>
 *   <li>Picture tokens {@code YYYY}, {@code MM}, {@code DD} and {@code DDD} (Julian day of year), case
 *       insensitive; any other non-alphanumeric character is a separator. Trailing blanks in the
 *       picture are ignored. Other tokens ({@code YY}, {@code MMM}, eras, ...) are not used by any
 *       CardDemo caller and are reported as {@link CeedaysFeedback#BAD_PIC_STRING}, as is a picture
 *       without a year, month and day (or year and day of year).
 *   <li>Leading blanks of the date are ignored. If what is left (without trailing blanks) is shorter
 *       than the picture: {@link CeedaysFeedback#INSUFFICIENT_DATA}. Characters past the picture are
 *       ignored.
 *   <li>A non-digit where the picture has a digit token, or a character other than the picture's own
 *       separator in a separator position: {@link CeedaysFeedback#NON_NUMERIC_DATA}.
 *   <li>Month outside 1-12: {@link CeedaysFeedback#INVALID_MONTH}. Day 0 or past the end of the month
 *       (Gregorian leap years), or day of year outside the year: {@link CeedaysFeedback#BAD_DATE_VALUE}.
 *   <li>A valid date before 15 October 1582: {@link CeedaysFeedback#UNSUPPORTED_RANGE}.
 * </ul>
 */
public final class Ceedays {

    /** Picture tokens, longest first so adjacent ones ({@code YYYYMMDD}, {@code YYYYDDD}) split. */
    private static final List<String> LETTER_TOKENS = List.of("YYYY", "DDD", "MM", "DD");

    /** Lilian day 1. */
    public static final LocalDate LILIAN_DAY_ONE = LocalDate.of(1582, 10, 15);

    /** The outcome: the feedback token and, when valid, the Lilian day ({@code OUTPUT-LILLIAN}), else 0. */
    public record Result(CeedaysFeedback feedback, int lilian) {

        public boolean valid() {
            return feedback == CeedaysFeedback.VALID;
        }
    }

    private Ceedays() {
    }

    /** {@code CALL "CEEDAYS" USING date, picture, lilian, feedback-code}. */
    public static Result ceedays(String date, String picture) {
        List<String> tokens = tokenize(stripTrailing(picture));
        if (tokens == null) {
            return fail(CeedaysFeedback.BAD_PIC_STRING);
        }
        String input = stripTrailing(date.stripLeading());
        int width = tokens.stream().mapToInt(String::length).sum();
        if (input.length() < width) {
            return fail(CeedaysFeedback.INSUFFICIENT_DATA);
        }
        int year = -1;
        int month = -1;
        int day = -1;
        int dayOfYear = -1;
        int pos = 0;
        for (String token : tokens) {
            String field = input.substring(pos, pos + token.length());
            pos += token.length();
            if (!Character.isLetter(token.charAt(0))) {
                if (!field.equals(token)) {
                    return fail(CeedaysFeedback.NON_NUMERIC_DATA);
                }
                continue;
            }
            if (!field.chars().allMatch(c -> c >= '0' && c <= '9')) {
                return fail(CeedaysFeedback.NON_NUMERIC_DATA);
            }
            int value = Integer.parseInt(field);
            switch (token) {
                case "YYYY" -> year = value;
                case "MM" -> month = value;
                case "DD" -> day = value;
                default -> dayOfYear = value;
            }
        }
        LocalDate resolved;
        if (dayOfYear >= 0) {
            if (dayOfYear < 1 || dayOfYear > Year.of(year).length()) {
                return fail(CeedaysFeedback.BAD_DATE_VALUE);
            }
            resolved = LocalDate.ofYearDay(year, dayOfYear);
        } else {
            if (month < 1 || month > 12) {
                return fail(CeedaysFeedback.INVALID_MONTH);
            }
            if (day < 1 || day > Year.of(year).atMonth(month).lengthOfMonth()) {
                return fail(CeedaysFeedback.BAD_DATE_VALUE);
            }
            resolved = LocalDate.of(year, month, day);
        }
        if (resolved.isBefore(LILIAN_DAY_ONE)) {
            return fail(CeedaysFeedback.UNSUPPORTED_RANGE);
        }
        return new Result(CeedaysFeedback.VALID,
                Math.toIntExact(ChronoUnit.DAYS.between(LILIAN_DAY_ONE, resolved) + 1));
    }

    /** Picture tokens in order (letter runs upper-cased, separators one character each), or null if unusable. */
    private static List<String> tokenize(String picture) {
        String trimmed = picture.stripLeading();
        List<String> tokens = new ArrayList<>();
        int i = 0;
        while (i < trimmed.length()) {
            char c = trimmed.charAt(i);
            if (Character.isDigit(c)) {
                return null;
            }
            if (!Character.isLetter(c)) {
                tokens.add(String.valueOf(c));
                i++;
                continue;
            }
            int end = i;
            while (end < trimmed.length() && Character.isLetter(trimmed.charAt(end))) {
                end++;
            }
            String run = trimmed.substring(i, end).toUpperCase(Locale.ROOT);
            int j = 0;
            while (j < run.length()) {
                String token = LETTER_TOKENS.stream().filter(run.substring(j)::startsWith).findFirst().orElse(null);
                if (token == null) {
                    return null;
                }
                tokens.add(token);
                j += token.length();
            }
            i = end;
        }
        long years = tokens.stream().filter("YYYY"::equals).count();
        long months = tokens.stream().filter("MM"::equals).count();
        long days = tokens.stream().filter("DD"::equals).count();
        long julian = tokens.stream().filter("DDD"::equals).count();
        boolean calendar = years == 1 && months == 1 && days == 1 && julian == 0;
        boolean ordinal = years == 1 && months == 0 && days == 0 && julian == 1;
        if (!(calendar || ordinal)) {
            return null;
        }
        return tokens;
    }

    private static String stripTrailing(String s) {
        int end = s.length();
        while (end > 0 && s.charAt(end - 1) == ' ') {
            end--;
        }
        return s.substring(0, end);
    }

    private static Result fail(CeedaysFeedback feedback) {
        return new Result(feedback, 0);
    }
}
