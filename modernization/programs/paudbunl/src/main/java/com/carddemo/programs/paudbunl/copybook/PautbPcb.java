package com.carddemo.programs.paudbunl.copybook;

import com.carddemo.programs.paudbunl.support.CobolData;

import java.nio.charset.Charset;
import java.util.Arrays;

/**
 * PAUTBPCB - DB PCB mask for PSB PAUTBUNL (DBD DBPAUTP0, PROCOPT=GOTP, KEYLEN=14).
 *
 * <pre>
 * 05 PAUT-DBDNAME      PIC X(08).
 * 05 PAUT-SEG-LEVEL    PIC X(02).
 * 05 PAUT-PCB-STATUS   PIC X(02).
 * 05 PAUT-PCB-PROCOPT  PIC X(04).
 * 05 FILLER            PIC S9(05) COMP.
 * 05 PAUT-SEG-NAME     PIC X(08).
 * 05 PAUT-KEYFB-NAME   PIC S9(05) COMP.   (length of key feedback)
 * 05 PAUT-NUM-SENSEGS  PIC S9(05) COMP.
 * 05 PAUT-KEYFB        PIC X(255).
 * </pre>
 * Mutable, like the PCB storage IMS updates on every call.
 */
public final class PautbPcb {

    public static final int LENGTH = 8 + 2 + 2 + 4 + 4 + 8 + 4 + 4 + 255;
    public static final int KEYFB_LENGTH = 255;
    public static final String SPACES_STATUS = "  ";

    private String dbdName = "        ";
    private String segLevel = "  ";
    private String pcbStatus = SPACES_STATUS;
    private String procopt = "    ";
    private int filler;
    private String segName = "        ";
    private int keyfbName;
    private int numSensegs;
    private final byte[] keyfb = new byte[KEYFB_LENGTH];

    public PautbPcb() {
        Arrays.fill(keyfb, (byte) 0);
    }

    public PautbPcb(String dbdName, String procopt, int numSensegs) {
        this();
        this.dbdName = pad(dbdName, 8);
        this.procopt = pad(procopt, 4);
        this.numSensegs = numSensegs;
    }

    /** {@code INITIALIZE PAUT-PCB-STATUS}: alphanumeric field is set to spaces. */
    public void initializeStatus() {
        pcbStatus = SPACES_STATUS;
    }

    public boolean isStatusSpaces() {
        return SPACES_STATUS.equals(pcbStatus);
    }

    public String dbdName() {
        return dbdName;
    }

    public String segLevel() {
        return segLevel;
    }

    public void segLevel(String v) {
        segLevel = pad(v, 2);
    }

    public String pcbStatus() {
        return pcbStatus;
    }

    public void pcbStatus(String v) {
        pcbStatus = pad(v, 2);
    }

    public String procopt() {
        return procopt;
    }

    public int filler() {
        return filler;
    }

    public String segName() {
        return segName;
    }

    public void segName(String v) {
        segName = pad(v, 8);
    }

    /** PAUT-KEYFB-NAME: despite its name, holds the key feedback length. */
    public int keyfbName() {
        return keyfbName;
    }

    public int numSensegs() {
        return numSensegs;
    }

    public byte[] keyfb() {
        return keyfb.clone();
    }

    /** IMS sets the concatenated key; bytes beyond the key length are left as they were. */
    public void keyFeedback(byte[] key) {
        int len = Math.min(key.length, KEYFB_LENGTH);
        System.arraycopy(key, 0, keyfb, 0, len);
        keyfbName = len;
    }

    /** PAUT-KEYFB as a DISPLAY would show it (all 255 bytes). */
    public String keyfbDisplay(Charset cs) {
        return CobolData.decodeAlpha(keyfb, 0, KEYFB_LENGTH, cs);
    }

    private static String pad(String v, int len) {
        String s = v == null ? "" : v;
        return s.length() >= len ? s.substring(0, len) : s + " ".repeat(len - s.length());
    }
}
