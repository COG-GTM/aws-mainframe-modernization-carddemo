package com.carddemo.programs.cocrdupc.copybook;

import com.carddemo.programs.cocrdupc.copybook.CcWorkArea.CcardAid;

/** CSSTRPFY: paragraph YYYY-STORE-PFKEY (PF13-PF24 fold onto PF1-PF12). */
public final class StorePfKey {

    private StorePfKey() {
    }

    /** YYYY-STORE-PFKEY: leaves CCARD-AID untouched when EIBAID matches no WHEN (e.g. PA3). */
    public static void yyyyStorePfkey(AidKey eibaid, CcWorkArea cc) {
        CcardAid mapped = map(eibaid);
        if (mapped != null) {
            cc.setCcardAid(mapped);
        }
    }

    static CcardAid map(AidKey eibaid) {
        if (eibaid == null) {
            return null;
        }
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
            case PA3 -> null;
        };
    }
}
