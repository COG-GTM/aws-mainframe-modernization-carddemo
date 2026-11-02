package com.carddemo.transaction.service;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.time.format.ResolverStyle;

/**
 * Minimal stand-in for {@code CSUTLDTC} (CEEDAYS with picture {@code YYYY-MM-DD}) as COTRN02C uses it:
 * the date is valid when CEEDAYS returns severity 0, and feedback message 2513 ({@code FC-UNSUPP-RANGE},
 * a date before the Lillian epoch 1582-10-15) is ignored by COTRN02C, so such dates pass here too.
 * Replace with the shared {@code common} date validation once UNT149-15 lands it.
 */
final class CsutldtcDate {

    private static final DateTimeFormatter FORMAT =
            DateTimeFormatter.ofPattern("uuuu-MM-dd").withResolverStyle(ResolverStyle.STRICT);

    private CsutldtcDate() {
    }

    static boolean isValid(String value) {
        try {
            LocalDate.parse(value, FORMAT);
            return true;
        } catch (DateTimeParseException e) {
            return false;
        }
    }
}
