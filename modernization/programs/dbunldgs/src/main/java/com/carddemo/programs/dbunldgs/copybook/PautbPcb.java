package com.carddemo.programs.dbunldgs.copybook;

/** Copybook PAUTBPCB: DB PCB mask for DBPAUTP0 (PSB DLIGSAMP, PROCOPT=GOTP, KEYLEN=14). KEYFB PIC X(255). */
public final class PautbPcb extends PcbMask {
    public static final int KEYFB_LENGTH = 255;

    public PautbPcb() {
        super("DBPAUTP0", "GOTP", 2, KEYFB_LENGTH);
    }
}
