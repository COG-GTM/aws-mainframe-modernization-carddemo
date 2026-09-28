package com.carddemo.common;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Field edits of COACTUPC (1215-1280 paragraphs) and the date edits of CSUTLDPY. Messages are built exactly like
 * the legacy {@code STRING FUNCTION TRIM(WS-EDIT-VARIABLE-NAME) ...} statements. Each method records at most one
 * error for the field and returns whether the value is valid.
 */
public final class CobolEdits {

    private static final Pattern NUMVAL_C = Pattern.compile(
            "^\\s*([+-])?\\s*\\$?\\s*([0-9][0-9,]*)?(\\.[0-9]*)?\\s*([+-]|CR|DB)?\\s*$", Pattern.CASE_INSENSITIVE);
    private static final Pattern PHONE_PARTS = Pattern.compile("^\\((.*)\\)(.*)-(.*)$");
    private static final Pattern PHONE_DASHED = Pattern.compile("^(.{3})-(.{3})-(.{4})$");

    private final ValidationErrors errors;

    public CobolEdits(ValidationErrors errors) {
        this.errors = errors;
    }

    /** 1215-EDIT-MANDATORY. */
    public boolean mandatory(String field, String name, String value, int maxLength) {
        if (Text.isBlank(value)) {
            errors.add(field, name + " must be supplied.");
            return false;
        }
        return maxLength(field, name, value, maxLength);
    }

    /** 1220-EDIT-YESNO. */
    public boolean yesNo(String field, String name, String value) {
        if (Text.isBlank(value)) {
            errors.add(field, name + " must be supplied.");
            return false;
        }
        String v = value.strip();
        if (!v.equals("Y") && !v.equals("N")) {
            errors.add(field, name + " must be Y or N.");
            return false;
        }
        return true;
    }

    /** 1225-EDIT-ALPHA-REQD. */
    public boolean alphaRequired(String field, String name, String value, int maxLength) {
        if (Text.isBlank(value)) {
            errors.add(field, name + " must be supplied.");
            return false;
        }
        if (!Text.isAlphaOrSpace(value.strip())) {
            errors.add(field, name + " can have alphabets only.");
            return false;
        }
        return maxLength(field, name, value, maxLength);
    }

    /** 1235-EDIT-ALPHA-OPT. */
    public boolean alphaOptional(String field, String name, String value, int maxLength) {
        if (Text.isBlank(value)) {
            return true;
        }
        if (!Text.isAlphaOrSpace(value.strip())) {
            errors.add(field, name + " can have alphabets only.");
            return false;
        }
        return maxLength(field, name, value, maxLength);
    }

    /** 1245-EDIT-NUM-REQD: exactly {@code length} digits, not all zero. */
    public boolean numericRequired(String field, String name, String value, int length) {
        if (Text.isBlank(value)) {
            errors.add(field, name + " must be supplied.");
            return false;
        }
        String v = value.strip();
        if (v.length() != length || !Text.isDigits(v)) {
            errors.add(field, name + " must be all numeric.");
            return false;
        }
        if (Text.isAllZero(v)) {
            errors.add(field, name + " must not be zero.");
            return false;
        }
        return true;
    }

    /** 1250-EDIT-SIGNED-9V2: NUMVAL-C acceptable, fits S9(10)V99. Returns the parsed value or null. */
    public BigDecimal signedAmount(String field, String name, String value) {
        if (Text.isBlank(value)) {
            errors.add(field, name + " must be supplied.");
            return null;
        }
        BigDecimal parsed = parseNumvalC(value);
        if (parsed == null || parsed.abs().compareTo(new BigDecimal("10000000000")) >= 0) {
            errors.add(field, name + " is not valid");
            return null;
        }
        return parsed.setScale(2, RoundingMode.DOWN);
    }

    /** 1260-EDIT-US-PHONE-NUM. Blank phone numbers are accepted. Returns the canonical (AAA)BBB-CCCC or null. */
    public String usPhone(String field, String name, String value, UsLookups lookups) {
        if (Text.isBlank(value)) {
            return "";
        }
        String v = value.strip();
        String area;
        String prefix;
        String line;
        Matcher m = PHONE_PARTS.matcher(v);
        Matcher dashed = PHONE_DASHED.matcher(v);
        if (m.matches()) {
            area = m.group(1).strip();
            prefix = m.group(2).strip();
            line = m.group(3).strip();
        } else if (dashed.matches()) {
            area = dashed.group(1);
            prefix = dashed.group(2);
            line = dashed.group(3);
        } else if (v.length() == 10 && Text.isDigits(v)) {
            area = v.substring(0, 3);
            prefix = v.substring(3, 6);
            line = v.substring(6);
        } else {
            area = v;
            prefix = "";
            line = "";
        }
        if (area.isEmpty()) {
            errors.add(field, name + ": Area code must be supplied.");
            return null;
        }
        if (area.length() != 3 || !Text.isDigits(area)) {
            errors.add(field, name + ": Area code must be A 3 digit number.");
            return null;
        }
        if (Text.isAllZero(area)) {
            errors.add(field, name + ": Area code cannot be zero");
            return null;
        }
        if (!lookups.isValidGeneralPurposeAreaCode(area)) {
            errors.add(field, name + ": Not valid North America general purpose area code");
            return null;
        }
        if (prefix.isEmpty()) {
            errors.add(field, name + ": Prefix code must be supplied.");
            return null;
        }
        if (prefix.length() != 3 || !Text.isDigits(prefix)) {
            errors.add(field, name + ": Prefix code must be A 3 digit number.");
            return null;
        }
        if (Text.isAllZero(prefix)) {
            errors.add(field, name + ": Prefix code cannot be zero");
            return null;
        }
        if (line.isEmpty()) {
            errors.add(field, name + ": Line number code must be supplied.");
            return null;
        }
        if (line.length() != 4 || !Text.isDigits(line)) {
            errors.add(field, name + ": Line number code must be A 4 digit number.");
            return null;
        }
        if (Text.isAllZero(line)) {
            errors.add(field, name + ": Line number code cannot be zero");
            return null;
        }
        return "(" + area + ")" + prefix + "-" + line;
    }

    /** 1265-EDIT-US-SSN. Accepts 9 digits with or without dashes. Returns the 9-digit SSN or null. */
    public String usSsn(String field, String value) {
        String v = Text.trimToEmpty(value).replace("-", "");
        String part1 = v.length() >= 3 ? v.substring(0, 3) : v;
        String part2 = v.length() >= 5 ? v.substring(3, 5) : v.length() > 3 ? v.substring(3) : "";
        String part3 = v.length() > 5 ? v.substring(5) : "";
        if (!numericRequired(field, "SSN: First 3 chars", part1, 3)) {
            return null;
        }
        int area = Integer.parseInt(part1);
        if (area == 666 || area >= 900) {
            errors.add(field, "SSN: First 3 chars: should not be 000, 666, or between 900 and 999");
            return null;
        }
        if (!numericRequired(field, "SSN 4th & 5th chars", part2, 2)) {
            return null;
        }
        if (!numericRequired(field, "SSN Last 4 chars", part3, 4)) {
            return null;
        }
        return part1 + part2 + part3;
    }

    /** 1270-EDIT-US-STATE-CD. */
    public boolean usState(String field, String value, UsLookups lookups) {
        if (!alphaRequired(field, "State", value, 2)) {
            return false;
        }
        if (!lookups.isValidStateCode(value.strip().toUpperCase())) {
            errors.add(field, "State: is not a valid state code");
            return false;
        }
        return true;
    }

    /** 1275-EDIT-FICO-SCORE (after the 3-digit numeric edit). */
    public Integer ficoScore(String field, String value) {
        if (!numericRequired(field, "FICO Score", value, 3)) {
            return null;
        }
        int score = Integer.parseInt(value.strip());
        if (score < 300 || score > 850) {
            errors.add(field, "FICO Score: should be between 300 and 850");
            return null;
        }
        return score;
    }

    /** EDIT-DATE-CCYYMMDD (CSUTLDPY) on an ISO {@code yyyy-MM-dd} value. Returns the date or null. */
    public LocalDate date(String field, String name, String value) {
        String v = Text.trimToEmpty(value);
        String[] parts = v.split("-", -1);
        String year = parts.length > 0 ? parts[0].strip() : "";
        String month = parts.length > 1 ? parts[1].strip() : "";
        String day = parts.length > 2 ? parts[2].strip() : "";
        if (parts.length > 3) {
            errors.add(field, name + " must be in format YYYY-MM-DD");
            return null;
        }
        if (year.isEmpty()) {
            errors.add(field, name + " : Year must be supplied.");
            return null;
        }
        if (year.length() != 4 || !Text.isDigits(year)) {
            errors.add(field, name + " must be 4 digit number.");
            return null;
        }
        String century = year.substring(0, 2);
        if (!century.equals("19") && !century.equals("20")) {
            errors.add(field, name + " : Century is not valid.");
            return null;
        }
        if (month.isEmpty()) {
            errors.add(field, name + " : Month must be supplied.");
            return null;
        }
        if (!Text.isDigits(month) || month.length() > 2 || Integer.parseInt(month) < 1
                || Integer.parseInt(month) > 12) {
            errors.add(field, name + ": Month must be a number between 1 and 12.");
            return null;
        }
        if (day.isEmpty()) {
            errors.add(field, name + " : Day must be supplied.");
            return null;
        }
        if (!Text.isDigits(day) || day.length() > 2 || Integer.parseInt(day) < 1 || Integer.parseInt(day) > 31) {
            errors.add(field, name + ":day must be a number between 1 and 31.");
            return null;
        }
        int y = Integer.parseInt(year);
        int mo = Integer.parseInt(month);
        int d = Integer.parseInt(day);
        if (d == 31 && (mo == 2 || mo == 4 || mo == 6 || mo == 9 || mo == 11)) {
            errors.add(field, name + ":Cannot have 31 days in this month.");
            return null;
        }
        if (d == 30 && mo == 2) {
            errors.add(field, name + ":Cannot have 30 days in this month.");
            return null;
        }
        if (d == 29 && mo == 2) {
            boolean leap = y % 100 == 0 ? y % 400 == 0 : y % 4 == 0;
            if (!leap) {
                errors.add(field, name + ":Not a leap year.Cannot have 29 days in this month.");
                return null;
            }
        }
        return LocalDate.of(y, mo, d);
    }

    /** EDIT-DATE-OF-BIRTH: valid date that is not in the future. */
    public LocalDate dateOfBirth(String field, String name, String value) {
        LocalDate dob = date(field, name, value);
        if (dob != null && dob.isAfter(LocalDate.now())) {
            errors.add(field, name + ":cannot be in the future");
            return null;
        }
        return dob;
    }

    public boolean maxLength(String field, String name, String value, int maxLength) {
        if (value != null && value.strip().length() > maxLength) {
            errors.add(field, name + " can not be longer than " + maxLength + " characters.");
            return false;
        }
        return true;
    }

    /** FUNCTION NUMVAL-C equivalent: optional sign, currency symbol, thousands separators, trailing CR/DB. */
    public static BigDecimal parseNumvalC(String value) {
        if (value == null) {
            return null;
        }
        Matcher m = NUMVAL_C.matcher(value);
        if (!m.matches() || (m.group(2) == null && (m.group(3) == null || m.group(3).length() < 2))) {
            return null;
        }
        if (m.group(1) != null && m.group(4) != null) {
            return null;
        }
        String digits = (m.group(2) == null ? "0" : m.group(2).replace(",", ""))
                + (m.group(3) == null || m.group(3).equals(".") ? "" : m.group(3));
        BigDecimal number = new BigDecimal(digits);
        String sign = m.group(1) != null ? m.group(1) : m.group(4);
        if (sign != null && (sign.equals("-") || sign.equalsIgnoreCase("CR") || sign.equalsIgnoreCase("DB"))) {
            number = number.negate();
        }
        return number;
    }
}
