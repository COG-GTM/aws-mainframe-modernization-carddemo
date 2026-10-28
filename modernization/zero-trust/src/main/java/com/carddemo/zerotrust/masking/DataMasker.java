package com.carddemo.zerotrust.masking;

import java.util.regex.Pattern;

/**
 * PAN/PII masking for anything written to logs or audit detail.
 *
 * <p>{@link #scrub} deliberately has no Luhn check: over-masking is the safe direction,
 * so any run of 13–19 consecutive digits is masked — including, as a documented side
 * effect, 16-digit transaction ids appearing in free text.
 */
public final class DataMasker {

    private static final Pattern LONG_DIGITS = Pattern.compile("\\d{13,19}");
    private static final Pattern SSN = Pattern.compile("\\d{3}-\\d{2}-\\d{4}");

    public String maskPan(String pan) {
        if (pan == null || pan.isBlank()) {
            return pan;
        }
        String digits = pan.strip();
        if (digits.length() <= 4) {
            return digits;
        }
        return "*".repeat(digits.length() - 4) + digits.substring(digits.length() - 4);
    }

    public String maskSsn(String ssn) {
        if (ssn == null || ssn.isBlank()) {
            return ssn;
        }
        String digits = ssn.replaceAll("[^0-9]", "");
        return "***-**-" + digits.substring(digits.length() - 4);
    }

    /** Masks every card-number-length digit run and every NNN-NN-NNNN pattern in text. */
    public String scrub(String text) {
        if (text == null) {
            return null;
        }
        String out = LONG_DIGITS.matcher(text).replaceAll(m -> maskPan(m.group()));
        return SSN.matcher(out).replaceAll(m -> maskSsn(m.group()));
    }
}
