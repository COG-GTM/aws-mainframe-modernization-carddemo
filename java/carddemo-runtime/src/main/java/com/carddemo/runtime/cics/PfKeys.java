package com.carddemo.runtime.cics;

import java.util.Locale;

/**
 * CSSTRPFY {@code YYYY-STORE-PFKEY}: maps EIBAID to the 5-character {@code CCARD-AID} value (CVCRD01Y 88-levels
 * {@code CCARD-AID-ENTER} ... {@code CCARD-AID-PFK12}). PF13-PF24 fold onto PFK01-PFK12.
 */
public final class PfKeys {

    private PfKeys() {
    }

    /**
     * Returns the value the paragraph SETs {@code CCARD-AID} to, or {@code null} when no WHEN matches
     * (PA3): CCARD-AID keeps its previous content.
     */
    public static String storePfKey(AidKey aid) {
        return switch (aid) {
            case ENTER -> "ENTER";
            case CLEAR -> "CLEAR";
            case PA1 -> "PA1  ";
            case PA2 -> "PA2  ";
            case PA3 -> null;
            default -> {
                int n = Integer.parseInt(aid.name().substring(2));
                yield String.format(Locale.ROOT, "PFK%02d", n > 12 ? n - 12 : n);
            }
        };
    }
}
