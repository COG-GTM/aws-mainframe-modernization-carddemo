package com.carddemo.runtime.cics;

import java.util.Objects;

/** CSSTRPFY {@code YYYY-STORE-PFKEY}: maps EIBAID to the CVCRD01Y {@code CCARD-AID} value. */
public final class PfKeys {

    private PfKeys() {
    }

    /**
     * Returns the new {@code CCARD-AID} (PIC X(5)) value: 'ENTER', 'CLEAR', 'PA1  ', 'PA2  ', 'PFK01'..'PFK12';
     * PF13-PF24 fold onto PFK01-PFK12. For an AID the EVALUATE does not list (PA3) no WHEN matches, so
     * {@code currentCcardAid} is returned unchanged.
     */
    public static String storePfKey(AidKey eibaid, String currentCcardAid) {
        Objects.requireNonNull(eibaid);
        return switch (eibaid) {
            case ENTER -> "ENTER";
            case CLEAR -> "CLEAR";
            case PA1 -> "PA1  ";
            case PA2 -> "PA2  ";
            case PA3 -> currentCcardAid;
            default -> {
                int n = Integer.parseInt(eibaid.name().substring(2));
                int folded = n > 12 ? n - 12 : n;
                yield String.format("PFK%02d", folded);
            }
        };
    }
}
