package com.carddemo.programs.dbunldgs.copybook;

import java.nio.charset.Charset;
import java.util.Arrays;

/**
 * Common layout of the DB/GSAM PCB masks (PAUTBPCB, PASFLPCB, PADFLPCB). IMS owns the storage and
 * updates it on every CBLTDLI call; the program reads the status and key feedback area.
 */
public abstract class PcbMask {
    private String dbdName;       // PIC X(08)
    private String segLevel;      // PIC X(02)
    private String pcbStatus;     // PIC X(02)
    private String pcbProcopt;    // PIC X(04)
    private int filler;           // PIC S9(05) COMP (reserved for IMS)
    private String segName;       // PIC X(08)
    private int keyFbName;        // PIC S9(05) COMP (length of key feedback)
    private int numSensegs;       // PIC S9(05) COMP
    private final byte[] keyFb;   // PIC X(n)

    protected PcbMask(String dbdName, String procopt, int numSensegs, int keyFbLength) {
        this.dbdName = CobolCodec.pic(dbdName, 8);
        this.segLevel = "  ";
        this.pcbStatus = "  ";
        this.pcbProcopt = CobolCodec.pic(procopt, 4);
        this.segName = CobolCodec.pic("", 8);
        this.numSensegs = numSensegs;
        this.keyFb = new byte[keyFbLength];
        Arrays.fill(keyFb, CobolCodec.EBCDIC.encode(" ").get());
    }

    public String getDbdName() { return dbdName; }
    public void setDbdName(String v) { dbdName = CobolCodec.pic(v, 8); }
    public String getSegLevel() { return segLevel; }
    public void setSegLevel(String v) { segLevel = CobolCodec.pic(v, 2); }
    public String getPcbStatus() { return pcbStatus; }
    public void setPcbStatus(String v) { pcbStatus = CobolCodec.pic(v, 2); }
    public String getPcbProcopt() { return pcbProcopt; }
    public void setPcbProcopt(String v) { pcbProcopt = CobolCodec.pic(v, 4); }
    public int getFiller() { return filler; }
    public void setFiller(int v) { filler = v; }
    public String getSegName() { return segName; }
    public void setSegName(String v) { segName = CobolCodec.pic(v, 8); }
    public int getKeyFbName() { return keyFbName; }
    public void setKeyFbName(int v) { keyFbName = v; }
    public int getNumSensegs() { return numSensegs; }
    public void setNumSensegs(int v) { numSensegs = v; }

    public int keyFbLength() { return keyFb.length; }

    /** Raw key feedback area. */
    public byte[] getKeyFb() { return keyFb.clone(); }

    /** IMS-side update of the key feedback area: concatenated key, rest of the area cleared to spaces. */
    public void setKeyFb(byte[] key, Charset cs) {
        Arrays.fill(keyFb, cs.encode(" ").get());
        System.arraycopy(key, 0, keyFb, 0, Math.min(key.length, keyFb.length));
        keyFbName = Math.min(key.length, keyFb.length);
    }

    /** Key feedback area as DISPLAY would render it. */
    public String keyFbAsDisplay(Charset cs) {
        return new String(keyFb, cs);
    }

    /** INITIALIZE xxx-PCB-STATUS. */
    public void initializeStatus() {
        pcbStatus = "  ";
    }

    public boolean statusIsSpaces() {
        return "  ".equals(pcbStatus);
    }
}
