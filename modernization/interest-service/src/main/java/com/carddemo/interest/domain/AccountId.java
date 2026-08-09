package com.carddemo.interest.domain;

/**
 * The 11-digit CardDemo account identifier ({@code ACCT-ID PIC 9(11)},
 * {@code app/cpy/CVACT01Y.cpy:5}).
 *
 * <p>Kept as a value type rather than a {@code long} because it is a zero-padded key used to
 * match records across four datasets, and because the COBOL comparisons that drive the batch
 * (for example the account-break test at {@code app/cbl/CBACT04C.cbl:194}) are character
 * comparisons.
 */
public record AccountId(String value) implements Comparable<AccountId> {

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
        return raw.length() >= 11 ? raw : "0".repeat(11 - raw.length()) + raw;
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
