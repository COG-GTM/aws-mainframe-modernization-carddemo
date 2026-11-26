package com.carddemo.programs.paudblod.copybook;

/**
 * Copybook PAUTBPCB (01 PAUTBPCB): DB PCB mask for the PAUTB database. Mutable because IMS
 * updates the PCB after every DL/I call.
 */
public final class PautbPcb {

    public static final int KEYFB_LENGTH = 255;

    /** 05 PAUT-DBDNAME PIC X(08). */
    private String pautDbdname = CobolFields.fixed("", 8);
    /** 05 PAUT-SEG-LEVEL PIC X(02). */
    private String pautSegLevel = CobolFields.fixed("", 2);
    /** 05 PAUT-PCB-STATUS PIC X(02). */
    private String pautPcbStatus = CobolFields.fixed("", 2);
    /** 05 PAUT-PCB-PROCOPT PIC X(04). */
    private String pautPcbProcopt = CobolFields.fixed("", 4);
    /** 05 FILLER PIC S9(05) COMP (reserved for IMS). */
    private int filler;
    /** 05 PAUT-SEG-NAME PIC X(08). */
    private String pautSegName = CobolFields.fixed("", 8);
    /** 05 PAUT-KEYFB-NAME PIC S9(05) COMP (length of the key feedback area in use). */
    private int pautKeyfbName;
    /** 05 PAUT-NUM-SENSEGS PIC S9(05) COMP. */
    private int pautNumSensegs;
    /** 05 PAUT-KEYFB PIC X(255). */
    private byte[] pautKeyfb = new byte[KEYFB_LENGTH];

    public PautbPcb() {
    }

    public PautbPcb(String dbdname, String procopt, int numSensegs) {
        this.pautDbdname = CobolFields.fixed(dbdname, 8);
        this.pautPcbProcopt = CobolFields.fixed(procopt, 4);
        this.pautNumSensegs = numSensegs;
    }

    public String getPautDbdname() {
        return pautDbdname;
    }

    public void setPautDbdname(String v) {
        pautDbdname = CobolFields.fixed(v, 8);
    }

    public String getPautSegLevel() {
        return pautSegLevel;
    }

    public void setPautSegLevel(String v) {
        pautSegLevel = CobolFields.fixed(v, 2);
    }

    public String getPautPcbStatus() {
        return pautPcbStatus;
    }

    public void setPautPcbStatus(String v) {
        pautPcbStatus = CobolFields.fixed(v, 2);
    }

    public String getPautPcbProcopt() {
        return pautPcbProcopt;
    }

    public void setPautPcbProcopt(String v) {
        pautPcbProcopt = CobolFields.fixed(v, 4);
    }

    public int getFiller() {
        return filler;
    }

    public void setFiller(int v) {
        filler = v;
    }

    public String getPautSegName() {
        return pautSegName;
    }

    public void setPautSegName(String v) {
        pautSegName = CobolFields.fixed(v, 8);
    }

    public int getPautKeyfbName() {
        return pautKeyfbName;
    }

    public void setPautKeyfbName(int v) {
        pautKeyfbName = v;
    }

    public int getPautNumSensegs() {
        return pautNumSensegs;
    }

    public void setPautNumSensegs(int v) {
        pautNumSensegs = v;
    }

    /** Copy of the 255-byte key feedback area. */
    public byte[] getPautKeyfb() {
        return pautKeyfb.clone();
    }

    /** Replaces the key feedback area: left-justified, remaining bytes set to {@code pad}. */
    public void setPautKeyfb(byte[] key, byte pad) {
        byte[] area = new byte[KEYFB_LENGTH];
        java.util.Arrays.fill(area, pad);
        System.arraycopy(key, 0, area, 0, Math.min(key.length, KEYFB_LENGTH));
        pautKeyfb = area;
        pautKeyfbName = Math.min(key.length, KEYFB_LENGTH);
    }

    /** {@code PAUT-PCB-STATUS = SPACES}. */
    public boolean isStatusSpaces() {
        return pautPcbStatus.isBlank();
    }
}
