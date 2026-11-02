package com.carddemo.account.validation;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The generic field edits of COACTUPC (paragraphs 1215-1280) and of the date edit copybook
 * {@code CSUTLDPY}, with the COBOL message text. Each edit returns the message the COBOL would have
 * moved to {@code WS-RETURN-MSG}, or {@code null} when the value passes. {@code label} is the
 * {@code WS-EDIT-VARIABLE-NAME} the COBOL sets before performing the edit.
 *
 * <p>Input conventions taken from {@code 1100-RECEIVE-MAP}: a field that is blank or holds {@code *}
 * counts as not supplied. Values are stripped of surrounding blanks; a BMS input field is padded or
 * right-justified with blanks, so "exactly n digits" is what {@code IS NUMERIC} on a {@code PIC X(n)}
 * field accepts.
 */
public final class CobolEdits {

    /** {@code FUNCTION TEST-NUMVAL-C}: optional sign (leading, trailing, CR/DB), optional $, commas. */
    private static final Pattern NUMVAL_C = Pattern.compile(
            "\\s*(?:([+-])\\s*)?(?:\\$\\s*)?(?:([+-])\\s*)?((?:\\d[\\d,]*)?(?:\\.\\d*)?)\\s*([+-]|CR|DB)?\\s*",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern ALPHA = Pattern.compile("[A-Za-z ]+");
    private static final int MONEY_INTEGER_DIGITS = 10;
    private static final int MONEY_SCALE = 2;

    private CobolEdits() {
    }

    // ---- input normalisation (1100-RECEIVE-MAP) -------------------------------------------------

    /** Blank or {@code *} becomes null (COBOL moves LOW-VALUES); anything else is stripped. */
    public static String normalize(String value) {
        if (value == null) {
            return null;
        }
        String stripped = value.strip();
        return stripped.isEmpty() || stripped.equals("*") ? null : stripped;
    }

    /** Splits a multi-part screen field ({@code YYYY-MM-DD}, {@code 999-99-9999}) into its parts. */
    static String[] parts(String value, int count) {
        String[] parts = new String[count];
        String normalized = normalize(value);
        if (normalized != null) {
            String[] split = normalized.split("-", count);
            for (int i = 0; i < split.length; i++) {
                parts[i] = normalize(split[i]);
            }
        }
        return parts;
    }

    /**
     * The three parts of a phone number: either the stored CUSTDAT form {@code (999)999-9999}, read by
     * position like {@code ACUP-OLD-CUST-PHONE-NUM-1A/B/C}, or {@code 999-999-9999}. Input whose
     * separators are not at those positions is returned whole as the area code, so the area-code edit
     * rejects it instead of a different number being stored.
     */
    public static String[] phoneParts(String value) {
        String normalized = normalize(value);
        if (normalized != null && normalized.startsWith("(")) {
            if (normalized.length() > 13 || normalized.indexOf(')') != 4
                    || (normalized.length() > 8 && normalized.charAt(8) != '-')) {
                return new String[] {normalized, null, null};
            }
            String padded = (normalized + " ".repeat(15)).substring(0, 15);
            return new String[] {normalize(padded.substring(1, 4)), normalize(padded.substring(5, 8)),
                    normalize(padded.substring(9, 13))};
        }
        return parts(normalized, 3);
    }

    /** The three SSN parts; nine digits without separators are split by position. */
    public static String[] ssnParts(String value) {
        String normalized = normalize(value);
        if (normalized != null && normalized.matches("\\d{9}")) {
            return new String[] {normalized.substring(0, 3), normalized.substring(3, 5), normalized.substring(5)};
        }
        return parts(normalized, 3);
    }

    // ---- messages -------------------------------------------------------------------------------

    public static String mustBeSupplied(String label) {
        return label + " must be supplied.";
    }

    public static String mustBeYesOrNo(String label) {
        return label + " must be Y or N.";
    }

    public static String alphabetsOnly(String label) {
        return label + " can have alphabets only.";
    }

    public static String mustBeAllNumeric(String label) {
        return label + " must be all numeric.";
    }

    public static String mustNotBeZero(String label) {
        return label + " must not be zero.";
    }

    public static String isNotValid(String label) {
        return label + " is not valid";
    }

    public static List<String> dateMessages(String label) {
        return List.of(
                label + " : Year must be supplied.",
                label + " must be 4 digit number.",
                label + " : Century is not valid.",
                label + " : Month must be supplied.",
                label + ": Month must be a number between 1 and 12.",
                label + " : Day must be supplied.",
                label + ":day must be a number between 1 and 31.",
                label + ":Cannot have 31 days in this month.",
                label + ":Cannot have 30 days in this month.",
                label + ":Not a leap year.Cannot have 29 days in this month.");
    }

    public static String cannotBeInTheFuture(String label) {
        return label + ":cannot be in the future";
    }

    public static List<String> phoneMessages(String label) {
        return List.of(
                label + ": Area code must be supplied.",
                label + ": Area code must be A 3 digit number.",
                label + ": Area code cannot be zero",
                label + ": Not valid North America general purpose area code",
                label + ": Prefix code must be supplied.",
                label + ": Prefix code must be A 3 digit number.",
                label + ": Prefix code cannot be zero",
                label + ": Line number code must be supplied.",
                label + ": Line number code must be A 4 digit number.",
                label + ": Line number code cannot be zero");
    }

    public static final String SSN_PART1 = "SSN: First 3 chars";
    public static final String SSN_PART2 = "SSN 4th & 5th chars";
    public static final String SSN_PART3 = "SSN Last 4 chars";
    public static final String SSN_PART1_RESERVED = SSN_PART1 + ": should not be 000, 666, or between 900 and 999";
    public static final String FICO_RANGE = "FICO Score: should be between 300 and 850";
    public static final String INVALID_STATE = "State: is not a valid state code";
    public static final String INVALID_ZIP_FOR_STATE = "Invalid zip code for state";

    // ---- edits ----------------------------------------------------------------------------------

    /** 1215-EDIT-MANDATORY. */
    public static String mandatory(String label, String value) {
        return normalize(value) == null ? mustBeSupplied(label) : null;
    }

    /** 1220-EDIT-YESNO: blank or zero is "not supplied"; only upper-case Y and N pass. */
    public static String yesNo(String label, String value) {
        String v = normalize(value);
        if (v == null || v.equals("0")) {
            return mustBeSupplied(label);
        }
        return v.equals("Y") || v.equals("N") ? null : mustBeYesOrNo(label);
    }

    /** 1225-EDIT-ALPHA-REQD: letters and spaces only. */
    public static String alphaRequired(String label, String value) {
        String v = normalize(value);
        if (v == null) {
            return mustBeSupplied(label);
        }
        return ALPHA.matcher(v).matches() ? null : alphabetsOnly(label);
    }

    /** 1235-EDIT-ALPHA-OPT: blank passes, otherwise letters and spaces only. */
    public static String alphaOptional(String label, String value) {
        String v = normalize(value);
        return v == null || ALPHA.matcher(v).matches() ? null : alphabetsOnly(label);
    }

    /** 1245-EDIT-NUM-REQD: exactly {@code length} digits, not all zero. */
    public static String numericRequired(String label, String value, int length) {
        String v = normalize(value);
        if (v == null) {
            return mustBeSupplied(label);
        }
        if (!v.matches("\\d{" + length + "}")) {
            return mustBeAllNumeric(label);
        }
        return Long.parseLong(v) == 0 ? mustNotBeZero(label) : null;
    }

    /** 1250-EDIT-SIGNED-9V2: anything {@code TEST-NUMVAL-C} accepts that fits {@code S9(10)V99}. */
    public static String signedAmount(String label, String value) {
        if (normalize(value) == null) {
            return mustBeSupplied(label);
        }
        return numvalC(value) == null ? isNotValid(label) : null;
    }

    /**
     * {@code FUNCTION NUMVAL-C} into {@code S9(10)V99}: decimals beyond two are truncated as the
     * COBOL {@code COMPUTE} (no ROUNDED) does. Returns null when {@code TEST-NUMVAL-C} would fail, or
     * when the integer part has more than 10 digits (COBOL truncates the high-order digits silently;
     * Java rejects the value instead).
     */
    public static BigDecimal numvalC(String value) {
        String v = normalize(value);
        if (v == null) {
            return null;
        }
        Matcher m = NUMVAL_C.matcher(v);
        if (!m.matches() || !m.group(3).matches(".*\\d.*")) {
            return null;
        }
        int signs = (m.group(1) != null ? 1 : 0) + (m.group(2) != null ? 1 : 0) + (m.group(4) != null ? 1 : 0);
        if (signs > 1) {
            return null;
        }
        BigDecimal amount = new BigDecimal(m.group(3).replace(",", "")).setScale(MONEY_SCALE, RoundingMode.DOWN);
        if (amount.precision() - amount.scale() > MONEY_INTEGER_DIGITS) {
            return null;
        }
        boolean negative = "-".equals(m.group(1)) || "-".equals(m.group(2))
                || (m.group(4) != null && !m.group(4).equals("+"));
        return negative ? amount.negate() : amount;
    }

    /**
     * {@code EDIT-DATE-CCYYMMDD} of CSUTLDPY: year, month and day are each checked (the first failing
     * message wins), then the day against the month and leap year. The final {@code CALL 'CSUTLDTC'}
     * (LE {@code CEEDAYS}) cannot fail for a date that passed these checks with a 19xx/20xx year, so it
     * has no Java counterpart.
     */
    public static String date(String label, String value) {
        List<String> messages = dateMessages(label);
        String[] p = parts(value, 3);
        String first = null;
        if (p[0] == null) {
            first = messages.get(0);
        } else if (!p[0].matches("\\d{4}")) {
            first = messages.get(1);
        } else if (!p[0].startsWith("19") && !p[0].startsWith("20")) {
            first = messages.get(2);
        }
        String monthError = null;
        if (p[1] == null) {
            monthError = messages.get(3);
        } else if (!p[1].matches("\\d{1,2}") || Integer.parseInt(p[1]) < 1 || Integer.parseInt(p[1]) > 12) {
            monthError = messages.get(4);
        }
        String dayError = null;
        if (p[2] == null) {
            dayError = messages.get(5);
        } else if (!p[2].matches("\\d{1,2}") || Integer.parseInt(p[2]) < 1 || Integer.parseInt(p[2]) > 31) {
            dayError = messages.get(6);
        }
        first = first != null ? first : monthError != null ? monthError : dayError;
        if (first != null) {
            return first;
        }
        int year = Integer.parseInt(p[0]);
        int month = Integer.parseInt(p[1]);
        int day = Integer.parseInt(p[2]);
        boolean thirtyOneDayMonth = List.of(1, 3, 5, 7, 8, 10, 12).contains(month);
        if (!thirtyOneDayMonth && day == 31) {
            return messages.get(7);
        }
        if (month == 2 && day == 30) {
            return messages.get(8);
        }
        if (month == 2 && day == 29 && year % (year % 100 == 0 ? 400 : 4) != 0) {
            return messages.get(9);
        }
        return null;
    }

    /** EDIT-DATE-CCYYMMDD followed by EDIT-DATE-OF-BIRTH: the date must be before today. */
    public static String dateOfBirth(String label, String value, LocalDate today) {
        String error = date(label, value);
        if (error != null) {
            return error;
        }
        return today.isAfter(toDate(value)) ? null : cannotBeInTheFuture(label);
    }

    /** A date that passed {@link #date}. */
    public static LocalDate toDate(String value) {
        String[] p = parts(value, 3);
        return LocalDate.of(Integer.parseInt(p[0]), Integer.parseInt(p[1]), Integer.parseInt(p[2]));
    }

    /**
     * 1260-EDIT-US-PHONE-NUM. The phone is optional: when it is blank the edit passes. The COBOL
     * blank test compares the area code twice and never the line number
     * ({@code (NUMA = SPACES OR NUMC = LOW-VALUES)} as its third term), so a phone with blank area
     * code and prefix passes whatever the line number holds; this is reproduced.
     */
    public static String usPhone(String label, String value) {
        List<String> messages = phoneMessages(label);
        String[] p = phoneParts(value);
        if (p[0] == null && p[1] == null) {
            return null;
        }
        String areaCode = phonePart(p[0], 3, messages.subList(0, 3));
        if (areaCode == null && !UsLookupCodes.GENERAL_PURPOSE_AREA_CODES.contains(p[0])) {
            areaCode = messages.get(3);
        }
        String prefix = phonePart(p[1], 3, messages.subList(4, 7));
        String lineNumber = phonePart(p[2], 4, messages.subList(7, 10));
        return areaCode != null ? areaCode : prefix != null ? prefix : lineNumber;
    }

    private static String phonePart(String part, int digits, List<String> messages) {
        if (part == null) {
            return messages.get(0);
        }
        if (!part.matches("\\d{" + digits + "}")) {
            return messages.get(1);
        }
        return Integer.parseInt(part) == 0 ? messages.get(2) : null;
    }

    /** The CUSTDAT form written by 9600-WRITE-PROCESSING: {@code (AAA)BBB-CCCC}; blank stays null. */
    public static String toStoredPhone(String value) {
        String[] p = phoneParts(value);
        if (p[0] == null && p[1] == null && p[2] == null) {
            return null;
        }
        return "(%3s)%3s-%4s".formatted(p[0] == null ? "" : p[0], p[1] == null ? "" : p[1],
                p[2] == null ? "" : p[2]);
    }

    /** 1265-EDIT-US-SSN: three numeric parts, the first not 000, 666 or 900-999. */
    public static String usSsn(String value) {
        String[] p = ssnParts(value);
        String first = numericRequired(SSN_PART1, p[0], 3);
        if (first == null) {
            int area = Integer.parseInt(p[0]);
            if (area == 0 || area == 666 || area >= 900) {
                first = SSN_PART1_RESERVED;
            }
        }
        String second = numericRequired(SSN_PART2, p[1], 2);
        String third = numericRequired(SSN_PART3, p[2], 4);
        return first != null ? first : second != null ? second : third;
    }

    public static List<String> ssnMessages() {
        return List.of(mustBeSupplied(SSN_PART1), mustBeAllNumeric(SSN_PART1), mustNotBeZero(SSN_PART1),
                SSN_PART1_RESERVED,
                mustBeSupplied(SSN_PART2), mustBeAllNumeric(SSN_PART2), mustNotBeZero(SSN_PART2),
                mustBeSupplied(SSN_PART3), mustBeAllNumeric(SSN_PART3), mustNotBeZero(SSN_PART3));
    }

    /** A SSN that passed {@link #usSsn}. */
    public static long toSsn(String value) {
        String[] p = ssnParts(value);
        return Long.parseLong(p[0] + p[1] + p[2]);
    }

    /** 1270-EDIT-US-STATE-CD (after 1225-EDIT-ALPHA-REQD passed). */
    public static String usStateCode(String value) {
        return UsLookupCodes.STATE_CODES.contains(normalize(value)) ? null : INVALID_STATE;
    }

    /** 1275-EDIT-FICO-SCORE (after 1245-EDIT-NUM-REQD passed): 88 FICO-RANGE-IS-VALID 300 THRU 850. */
    public static String ficoRange(String value) {
        int score = Integer.parseInt(normalize(value));
        return score >= 300 && score <= 850 ? null : FICO_RANGE;
    }

    /** 1280-EDIT-US-STATE-ZIP-CD: state code plus the first two ZIP digits. */
    public static String usStateZip(String state, String zip) {
        return UsLookupCodes.STATE_ZIP_PREFIXES.contains(normalize(state) + normalize(zip).substring(0, 2))
                ? null : INVALID_ZIP_FOR_STATE;
    }
}
