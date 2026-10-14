package com.carddemo.runtime.cics;

/**
 * CSSTRPFY {@code YYYY-STORE-PFKEY}: maps EIBAID to the CVCRD01Y {@code CCARD-AID} value. PF13-PF24 fold onto
 * PFK01-PFK12; PA3 (no WHEN in the copybook) leaves CCARD-AID unchanged.
 */
public final class PfKeys {

    private PfKeys() {
    }

    /** {@code CCARD-AID} value (5 characters) for {@code eibaid}, or null when the copybook has no WHEN for it. */
    public static String storePfKey(AidKey eibaid) {
        return switch (eibaid) {
            case ENTER -> "ENTER";
            case CLEAR -> "CLEAR";
            case PA1 -> "PA1  ";
            case PA2 -> "PA2  ";
            case PA3 -> null;
            default -> {
                int n = Integer.parseInt(eibaid.name().substring(2));
                yield String.format("PFK%02d", n > 12 ? n - 12 : n);
            }
        };
    }

    /** {@code PERFORM YYYY-STORE-PFKEY}: the new CCARD-AID given its current value. */
    public static String storePfKey(AidKey eibaid, String currentCcardAid) {
        String v = storePfKey(eibaid);
        return v == null ? currentCcardAid : v;
    }
}
