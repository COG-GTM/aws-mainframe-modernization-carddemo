package com.carddemo.runtime.cics;

import com.carddemo.runtime.cobol.PicX;

/**
 * CSSTRPFY {@code YYYY-STORE-PFKEY}: maps EIBAID to the 5-character {@code CCARD-AID} value of CVCRD01Y
 * ({@code ENTER}, {@code CLEAR}, {@code PA1  }, {@code PA2  }, {@code PFK01}..{@code PFK12}). PF13-PF24 fold
 * onto PFK01-PFK12. PA3 matches no WHEN, so {@code CCARD-AID} keeps its previous value.
 */
public final class PfKeys {

    private PfKeys() {
    }

    /** {@code PERFORM YYYY-STORE-PFKEY}: the new CCARD-AID, or {@code currentCcardAid} when no WHEN matches. */
    public static String storePfKey(AidKey eibaid, String currentCcardAid) {
        return switch (eibaid) {
            case ENTER -> "ENTER";
            case CLEAR -> "CLEAR";
            case PA1 -> "PA1  ";
            case PA2 -> "PA2  ";
            case PA3 -> PicX.of(currentCcardAid, 5);
            default -> {
                int n = Integer.parseInt(eibaid.name().substring(2));
                yield String.format("PFK%02d", n > 12 ? n - 12 : n);
            }
        };
    }

    /** As {@link #storePfKey(AidKey, String)} with CCARD-AID previously SPACES (after INITIALIZE CC-WORK-AREA). */
    public static String storePfKey(AidKey eibaid) {
        return storePfKey(eibaid, PicX.spaces(5));
    }
}
