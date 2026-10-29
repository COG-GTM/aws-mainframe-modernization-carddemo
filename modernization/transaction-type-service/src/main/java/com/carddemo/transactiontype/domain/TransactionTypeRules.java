package com.carddemo.transactiontype.domain;

import static com.carddemo.transactiontype.domain.LegacyRuleException.Kind.VALIDATION;

import java.util.Locale;
import java.util.regex.Pattern;

/** Field edits from COTRTUPC (1210/1230/1245) and COTRTLIC (1211/1220/1230). */
public final class TransactionTypeRules {

    public static final int DESCRIPTION_MAX = 50;
    private static final Pattern DIGITS = Pattern.compile("\\d+");
    private static final Pattern TWO_DIGITS = Pattern.compile("\\d{2}");
    private static final Pattern ALPHANUMERIC_AND_SPACES = Pattern.compile("[A-Za-z0-9 ]+");

    private TransactionTypeRules() {
    }

    /** BR-TT-001..004: required, numeric, non-zero, normalised to two digits ("7" becomes "07"). */
    public static String normalizeTypeCode(String raw) {
        String value = raw == null ? "" : raw.trim();
        if (value.isEmpty()) {
            throw new LegacyRuleException("BR-TT-001", VALIDATION, "Tran Type code must be supplied.");
        }
        if (!DIGITS.matcher(value).matches() || value.length() > 2) {
            throw new LegacyRuleException("BR-TT-002", VALIDATION, "Tran Type code must be numeric.");
        }
        if (Integer.parseInt(value) == 0) {
            throw new LegacyRuleException("BR-TT-003", VALIDATION, "Tran Type code must not be zero.");
        }
        return value.length() == 1 ? "0" + value : value;
    }

    /** BR-TT-005..007: required, letters/digits/spaces only, at most 50 characters; stored trimmed. */
    public static String validateDescription(String raw) {
        String value = raw == null ? "" : raw.trim();
        if (value.isEmpty()) {
            throw new LegacyRuleException("BR-TT-005", VALIDATION, "Transaction Desc must be supplied.");
        }
        if (!ALPHANUMERIC_AND_SPACES.matcher(value).matches()) {
            throw new LegacyRuleException("BR-TT-006", VALIDATION,
                    "Transaction Desc can have numbers or alphabets only.");
        }
        if (value.length() > DESCRIPTION_MAX) {
            throw new LegacyRuleException("BR-TT-007", VALIDATION,
                    "Transaction Desc must not exceed " + DESCRIPTION_MAX + " characters.");
        }
        return value;
    }

    /** BR-TT-008: change detection is case-insensitive on trimmed values. */
    public static boolean isUnchanged(String current, String proposed) {
        String a = current == null ? "" : current.trim();
        String b = proposed == null ? "" : proposed.trim();
        return a.toUpperCase(Locale.ROOT).equals(b.toUpperCase(Locale.ROOT));
    }

    /** BR-TT-011: blank or "00" means no filter, otherwise exactly two digits. */
    public static String parseTypeCodeFilter(String raw) {
        if (raw == null || raw.isBlank() || raw.equals("00")) {
            return null;
        }
        if (!TWO_DIGITS.matcher(raw).matches()) {
            throw new LegacyRuleException("BR-TT-011", VALIDATION,
                    "TYPE CODE FILTER,IF SUPPLIED MUST BE A 2 DIGIT NUMBER");
        }
        return raw;
    }

    /** BR-TT-012: description filter is a contains match, LIKE '%' || TRIM(input) || '%'. */
    public static String descriptionFilterPattern(String raw) {
        if (raw == null || raw.isBlank()) {
            return null;
        }
        return "%" + raw.trim() + "%";
    }
}
