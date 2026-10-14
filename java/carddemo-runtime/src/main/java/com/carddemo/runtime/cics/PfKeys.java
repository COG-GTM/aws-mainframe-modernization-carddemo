package com.carddemo.runtime.cics;

/** CSSTRPFY {@code YYYY-STORE-PFKEY}: maps EIBAID to the CVCRD01Y {@code CCARD-AID} value, folding PF13-24 to PF1-12. */
public final class PfKeys {

    private PfKeys() {
    }

    /**
     * Returns the new CCARD-AID value ({@code ENTER}, {@code CLEAR}, {@code PA1  }, {@code PA2  },
     * {@code PFK01}..{@code PFK12}); an AID the copybook does not list (PA3) leaves {@code currentCcardAid} unchanged.
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
