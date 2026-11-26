package com.carddemo.programs.cocrdlic.copybook;

import com.carddemo.programs.cocrdlic.copybook.CardWorkAreas.CcardAid;

/** CSSTRPFY - YYYY-STORE-PFKEY: map EIBAID into CCARD-AID (PF13-PF24 fold onto PF1-PF12). */
public final class PfKeyStore {
    private PfKeyStore() {
    }

    /** YYYY-STORE-PFKEY. Keys not listed leave CCARD-AID unchanged (returned as {@code current}). */
    public static CcardAid storePfKey(EibAid eibaid, CcardAid current) {
        return switch (eibaid) {
            case ENTER -> CcardAid.ENTER;
            case CLEAR -> CcardAid.CLEAR;
            case PA1 -> CcardAid.PA1;
            case PA2 -> CcardAid.PA2;
            case PF1, PF13 -> CcardAid.PFK01;
            case PF2, PF14 -> CcardAid.PFK02;
            case PF3, PF15 -> CcardAid.PFK03;
            case PF4, PF16 -> CcardAid.PFK04;
            case PF5, PF17 -> CcardAid.PFK05;
            case PF6, PF18 -> CcardAid.PFK06;
            case PF7, PF19 -> CcardAid.PFK07;
            case PF8, PF20 -> CcardAid.PFK08;
            case PF9, PF21 -> CcardAid.PFK09;
            case PF10, PF22 -> CcardAid.PFK10;
            case PF11, PF23 -> CcardAid.PFK11;
            case PF12, PF24 -> CcardAid.PFK12;
            default -> current;
        };
    }
}
