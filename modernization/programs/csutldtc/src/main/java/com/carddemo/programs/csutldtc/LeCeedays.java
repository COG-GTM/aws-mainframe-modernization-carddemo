package com.carddemo.programs.csutldtc;

import com.carddemo.programs.csutldtc.copybook.FeedbackCode;
import com.carddemo.programs.csutldtc.copybook.VString;
import java.time.Clock;
import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.Year;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

/**
 * Java emulation of LE CEEDAYS (character date -> Lilian day number).
 * Supported picture tokens: YYYY, YY, MM, DD, DDD (day of year), MMM / Mmm (month abbreviation);
 * any other non-alphanumeric character is a literal separator that must match exactly.
 * Era pictures (&lt;JJJJ&gt;, &lt;CCCC&gt;, ...) are not supported and yield CEE2EM (bad picture string).
 */
public final class LeCeedays implements CeedaysService {
    public static final int SEVERITY_ERROR = 3;
    public static final int CEE2EB_INSUFFICIENT_DATA = 2507;
    public static final int CEE2EC_BAD_DATE_VALUE = 2508;
    public static final int CEE2ED_INVALID_ERA = 2509;
    public static final int CEE2EH_UNSUPPORTED_RANGE = 2513;
    public static final int CEE2EL_INVALID_MONTH = 2517;
    public static final int CEE2EM_BAD_PICTURE_STRING = 2518;
    public static final int CEE2EO_NON_NUMERIC_DATA = 2520;
    public static final int CEE2EP_YEAR_IN_ERA_ZERO = 2521;

    /** Lilian day 1 is 15 October 1582; day 0 is the day before. */
    public static final LocalDate LILIAN_DAY_ZERO = LocalDate.of(1582, 10, 14);
    public static final LocalDate MIN_DATE = LocalDate.of(1582, 10, 15);
    public static final LocalDate MAX_DATE = LocalDate.of(9999, 12, 31);
    /** LE default century window (CEESCEN): 80 years before the current year. */
    public static final int DEFAULT_CENTURY_WINDOW = 80;

    private static final String[] MONTHS = {
        "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
    };

    private final Clock clock;
    private final int centuryWindow;

    public LeCeedays() {
        this(Clock.systemDefaultZone(), DEFAULT_CENTURY_WINDOW);
    }

    public LeCeedays(Clock clock, int centuryWindow) {
        this.clock = clock;
        this.centuryWindow = centuryWindow;
    }

    private enum Kind { LITERAL, YYYY, YY, MM, DD, DDD, MONTH_NAME }

    private record Token(Kind kind, int width, char literal) {
    }

    @Override
    public CeedaysResult ceedays(VString inputCharDate, VString pictureString) {
        String picture = rtrim(pictureString.text());
        String input = rtrim(inputCharDate.text());

        List<Token> tokens = parsePicture(picture);
        if (tokens == null) {
            return error(CEE2EM_BAD_PICTURE_STRING);
        }
        if (input.isEmpty() || input.length() < picture.length()) {
            return error(CEE2EB_INSUFFICIENT_DATA);
        }
        if (input.length() > picture.length()) {
            return error(CEE2EO_NON_NUMERIC_DATA);
        }

        int pos = 0;
        int year = -1;
        boolean twoDigitYear = false;
        int month = -1;
        int day = -1;
        int dayOfYear = -1;
        for (Token t : tokens) {
            String field = input.substring(pos, pos + t.width());
            pos += t.width();
            switch (t.kind()) {
                case LITERAL -> {
                    if (field.charAt(0) != t.literal()) {
                        return error(CEE2EO_NON_NUMERIC_DATA);
                    }
                }
                case MONTH_NAME -> {
                    month = monthFromName(field);
                    if (month < 0) {
                        return error(CEE2EL_INVALID_MONTH);
                    }
                }
                default -> {
                    if (!isDigits(field)) {
                        return error(CEE2EO_NON_NUMERIC_DATA);
                    }
                    int n = Integer.parseInt(field);
                    switch (t.kind()) {
                        case YYYY -> year = n;
                        case YY -> {
                            year = n;
                            twoDigitYear = true;
                        }
                        case MM -> month = n;
                        case DD -> day = n;
                        case DDD -> dayOfYear = n;
                        default -> throw new IllegalStateException();
                    }
                }
            }
        }
        if (twoDigitYear) {
            year = windowYear(year);
        }

        LocalDate date;
        try {
            if (dayOfYear >= 0) {
                if (dayOfYear < 1 || dayOfYear > Year.of(year).length()) {
                    return error(CEE2EC_BAD_DATE_VALUE);
                }
                date = LocalDate.ofYearDay(year, dayOfYear);
            } else {
                if (month < 1 || month > 12) {
                    return error(CEE2EL_INVALID_MONTH);
                }
                if (day < 1 || day > java.time.YearMonth.of(year, month).lengthOfMonth()) {
                    return error(CEE2EC_BAD_DATE_VALUE);
                }
                date = LocalDate.of(year, month, day);
            }
        } catch (DateTimeException e) {
            return error(CEE2EC_BAD_DATE_VALUE);
        }

        if (date.isBefore(MIN_DATE) || date.isAfter(MAX_DATE)) {
            return error(CEE2EH_UNSUPPORTED_RANGE);
        }
        return new CeedaysResult((int) ChronoUnit.DAYS.between(LILIAN_DAY_ZERO, date), FeedbackCode.success());
    }

    private int windowYear(int yy) {
        int base = Year.now(clock).getValue() - centuryWindow;
        return base + Math.floorMod(yy - base, 100);
    }

    private static List<Token> parsePicture(String picture) {
        if (picture.isEmpty()) {
            return null;
        }
        List<Token> tokens = new ArrayList<>();
        int years = 0;
        int months = 0;
        int days = 0;
        int daysOfYear = 0;
        int i = 0;
        while (i < picture.length()) {
            if (picture.startsWith("YYYY", i)) {
                tokens.add(new Token(Kind.YYYY, 4, ' '));
                years++;
                i += 4;
            } else if (picture.startsWith("YY", i)) {
                tokens.add(new Token(Kind.YY, 2, ' '));
                years++;
                i += 2;
            } else if (picture.startsWith("MMM", i) || picture.startsWith("Mmm", i)) {
                tokens.add(new Token(Kind.MONTH_NAME, 3, ' '));
                months++;
                i += 3;
            } else if (picture.startsWith("MM", i)) {
                tokens.add(new Token(Kind.MM, 2, ' '));
                months++;
                i += 2;
            } else if (picture.startsWith("DDD", i)) {
                tokens.add(new Token(Kind.DDD, 3, ' '));
                daysOfYear++;
                i += 3;
            } else if (picture.startsWith("DD", i)) {
                tokens.add(new Token(Kind.DD, 2, ' '));
                days++;
                i += 2;
            } else {
                char c = picture.charAt(i);
                if (Character.isLetterOrDigit(c) || c == '<' || c == '>') {
                    return null;
                }
                tokens.add(new Token(Kind.LITERAL, 1, c));
                i++;
            }
        }
        boolean calendar = years == 1 && months == 1 && days == 1 && daysOfYear == 0;
        boolean julian = years == 1 && daysOfYear == 1 && months == 0 && days == 0;
        return calendar || julian ? tokens : null;
    }

    private static int monthFromName(String name) {
        String upper = name.toUpperCase(Locale.ROOT);
        for (int m = 0; m < MONTHS.length; m++) {
            if (MONTHS[m].equals(upper)) {
                return m + 1;
            }
        }
        return -1;
    }

    private static boolean isDigits(String s) {
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c < '0' || c > '9') {
                return false;
            }
        }
        return true;
    }

    private static String rtrim(String s) {
        int end = s.length();
        while (end > 0 && s.charAt(end - 1) == ' ') {
            end--;
        }
        return s.substring(0, end);
    }

    private static CeedaysResult error(int msgNo) {
        return new CeedaysResult(0, FeedbackCode.ofCondition(SEVERITY_ERROR, msgNo));
    }
}
