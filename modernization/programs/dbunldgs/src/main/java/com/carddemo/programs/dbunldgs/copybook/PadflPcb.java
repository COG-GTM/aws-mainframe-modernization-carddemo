package com.carddemo.programs.dbunldgs.copybook;

/** Copybook PADFLPCB: GSAM PCB mask for PADFLDBD (child detail output, PROCOPT=LS). KEYFB PIC X(255). */
public final class PadflPcb extends PcbMask {
    public static final int KEYFB_LENGTH = 255;

    public PadflPcb() {
        super("PADFLDBD", "LS", 0, KEYFB_LENGTH);
    }
}
