package com.carddemo.runtime.cics;

/**
 * CSSTRPFY {@code YYYY-STORE-PFKEY}: maps EIBAID to the CVCRD01Y {@code CCARD-AID} value, folding PF13-PF24
 * onto PFK01-PFK12.
 */
public final class PfKeys {

    private PfKeys() {
    }

    /**
     * Returns the {@code CCARD-AID} (PIC X(5)) value for {@code eibaid}: ENTER, CLEAR, "PA1  ", "PA2  ",
     * PFK01-PFK12. AIDs with no WHEN in CSSTRPFY (PA3) leave {@code currentCcardAid} unchanged.
     */
    public static String storePfKey(AidKey eibaid, String currentCcardAid) {
        return switch (eibaid) {
            case ENTER -> "ENTER";
            case CLEAR -> "CLEAR";
            case PA1 -> "PA1  ";
            case PA2 -> "PA2  ";
            case PA3 -> currentCcardAid;
            default -> {
                int n = Integer.parseInt(eibaid.name().substring(2));
                yield String.format("PFK%02d", n > 12 ? n - 12 : n);
            }
        };
    }
}
