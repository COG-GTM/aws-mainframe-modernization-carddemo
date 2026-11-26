package com.carddemo.programs.copaus0c.copybook;

/**
 * BMS request DTO: the operator-modifiable fields of COPAU0AI (ACCTIDI, SEL0001I-SEL0005I).
 * A {@code null} value means "field not modified" (received as nulls).
 */
public record Copau0aInput(String acctId, String sel1, String sel2, String sel3, String sel4, String sel5) {

    public static Copau0aInput of(String acctId) {
        return new Copau0aInput(acctId, null, null, null, null, null);
    }

    public static Copau0aInput none() {
        return new Copau0aInput(null, null, null, null, null, null);
    }

    /** Returns a copy with SEL000n set (1-based). */
    public Copau0aInput withSelection(int n, String value) {
        return new Copau0aInput(acctId, n == 1 ? value : sel1, n == 2 ? value : sel2, n == 3 ? value : sel3,
                n == 4 ? value : sel4, n == 5 ? value : sel5);
    }

    public String selection(int n) {
        return switch (n) {
            case 1 -> sel1;
            case 2 -> sel2;
            case 3 -> sel3;
            case 4 -> sel4;
            case 5 -> sel5;
            default -> throw new IllegalArgumentException("SEL000" + n);
        };
    }
}
