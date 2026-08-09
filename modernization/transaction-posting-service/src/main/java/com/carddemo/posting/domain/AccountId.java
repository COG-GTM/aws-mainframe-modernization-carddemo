package com.carddemo.posting.domain;

/**
 * The 11-digit CardDemo account identifier ({@code ACCT-ID PIC 9(11)},
 * {@code app/cpy/CVACT01Y.cpy:5}), also the high-order part of the transaction-category-balance
 * key ({@code app/cpy/CVTRA01Y.cpy:6}).
 *
 * <p>Kept as a zero-padded value type rather than a {@code long}: the COBOL moves it between
 * {@code PIC 9(11)} fields of three different files ({@code app/cbl/CBTRN02C.cbl:394},
 * {@code app/cbl/CBTRN02C.cbl:469}) and compares it as characters.
 */
public record AccountId(String value) implements Comparable<AccountId> {

    private static final int DIGITS = 11;

    public AccountId {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException("Account id must not be blank");
        }
        value = pad(value.trim());
    }

    public static AccountId of(String value) {
        return new AccountId(value);
    }

    private static String pad(String raw) {
        return raw.length() >= DIGITS ? raw : "0".repeat(DIGITS - raw.length()) + raw;
    }

    @Override
    public int compareTo(AccountId other) {
        return value.compareTo(other.value);
    }

    @Override
    public String toString() {
        return value;
    }
}
