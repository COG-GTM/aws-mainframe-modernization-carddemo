package com.carddemo.programs.coactupc.copybook;

import com.carddemo.programs.coactupc.copybook.CcWorkArea.CcardAid;

/** CSSTRPFY - paragraph YYYY-STORE-PFKEY: maps EIBAID to CCARD-AID (PF13-PF24 fold onto PF1-PF12). */
public final class PfKeyStore {

    private PfKeyStore() {
    }

    // YYYY-STORE-PFKEY
    public static void storePfkey(AidKey eibaid, CcWorkArea cc) {
        CcardAid aid = map(eibaid);
        if (aid != null) {
            cc.set(aid);
        }
    }

    static CcardAid map(AidKey eibaid) {
        if (eibaid == null) {
            return null;
        }
        return switch (eibaid) {
            case DFHENTER -> CcardAid.ENTER;
            case DFHCLEAR -> CcardAid.CLEAR;
            case DFHPA1 -> CcardAid.PA1;
            case DFHPA2 -> CcardAid.PA2;
            case DFHPF1, DFHPF13 -> CcardAid.PFK01;
            case DFHPF2, DFHPF14 -> CcardAid.PFK02;
            case DFHPF3, DFHPF15 -> CcardAid.PFK03;
            case DFHPF4, DFHPF16 -> CcardAid.PFK04;
            case DFHPF5, DFHPF17 -> CcardAid.PFK05;
            case DFHPF6, DFHPF18 -> CcardAid.PFK06;
            case DFHPF7, DFHPF19 -> CcardAid.PFK07;
            case DFHPF8, DFHPF20 -> CcardAid.PFK08;
            case DFHPF9, DFHPF21 -> CcardAid.PFK09;
            case DFHPF10, DFHPF22 -> CcardAid.PFK10;
            case DFHPF11, DFHPF23 -> CcardAid.PFK11;
            case DFHPF12, DFHPF24 -> CcardAid.PFK12;
            default -> null;
        };
    }
}
