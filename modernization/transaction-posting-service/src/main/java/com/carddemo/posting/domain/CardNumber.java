package com.carddemo.posting.domain;

/**
 * A 16-character card number ({@code DALYTRAN-CARD-NUM PIC X(16)},
 * {@code app/cpy/CVTRA06Y.cpy:15}), the primary key of the card cross-reference KSDS
 * ({@code app/cbl/CBTRN02C.cbl:40-44}).
 *
 * <p>Alphanumeric on the mainframe and therefore alphanumeric here: it is an identifier, never a
 * number to compute with.
 */
public record CardNumber(String value) implements Comparable<CardNumber> {

    public CardNumber {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException("Card number must not be blank");
        }
        value = value.trim();
    }

    public static CardNumber of(String value) {
        return new CardNumber(value);
    }

    @Override
    public int compareTo(CardNumber other) {
        return value.compareTo(other.value);
    }

    @Override
    public String toString() {
        return value;
    }
}
