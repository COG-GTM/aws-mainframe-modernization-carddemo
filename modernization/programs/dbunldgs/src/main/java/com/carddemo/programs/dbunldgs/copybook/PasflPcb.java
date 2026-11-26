package com.carddemo.programs.dbunldgs.copybook;

/** Copybook PASFLPCB: GSAM PCB mask for PASFLDBD (root summary output, PROCOPT=LS). KEYFB PIC X(100). */
public final class PasflPcb extends PcbMask {
    public static final int KEYFB_LENGTH = 100;

    public PasflPcb() {
        super("PASFLDBD", "LS", 0, KEYFB_LENGTH);
    }
}
