package com.carddemo.runtime.cics;

import com.carddemo.runtime.cobol.PicX;

/**
 * CSSTRPFY {@code YYYY-STORE-PFKEY}: maps EIBAID to the CVCRD01Y {@code CCARD-AID} value, folding PF13-PF24
 * onto PF1-PF12. Keys the copybook does not test (e.g. PA3) leave {@code CCARD-AID} unchanged.
 */
public final class PfKeys {

    /** CCARD-AID is PIC X(5). */
    public static final int CCARD_AID_LENGTH = 5;

    private PfKeys() {
    }

    /** YYYY-STORE-PFKEY with CCARD-AID starting from {@code current} (returned unchanged for untested keys). */
    public static String storePfKey(AidKey eibaid, String current) {
        String value = switch (eibaid) {
            case ENTER -> "ENTER";
            case CLEAR -> "CLEAR";
            case PA1 -> "PA1";
            case PA2 -> "PA2";
            case PF1, PF13 -> "PFK01";
            case PF2, PF14 -> "PFK02";
            case PF3, PF15 -> "PFK03";
            case PF4, PF16 -> "PFK04";
            case PF5, PF17 -> "PFK05";
            case PF6, PF18 -> "PFK06";
            case PF7, PF19 -> "PFK07";
            case PF8, PF20 -> "PFK08";
            case PF9, PF21 -> "PFK09";
            case PF10, PF22 -> "PFK10";
            case PF11, PF23 -> "PFK11";
            case PF12, PF24 -> "PFK12";
            default -> current;
        };
        return PicX.of(value, CCARD_AID_LENGTH);
    }

    /** YYYY-STORE-PFKEY after {@code INITIALIZE CC-WORK-AREA} (CCARD-AID = SPACES). */
    public static String storePfKey(AidKey eibaid) {
        return storePfKey(eibaid, PicX.spaces(CCARD_AID_LENGTH));
    }
}
