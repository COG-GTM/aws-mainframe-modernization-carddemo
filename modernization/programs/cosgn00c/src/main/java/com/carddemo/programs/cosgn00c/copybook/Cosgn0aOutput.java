package com.carddemo.programs.cosgn00c.copybook;

import static com.carddemo.programs.cosgn00c.copybook.CobolText.lowValues;
import static com.carddemo.programs.cosgn00c.copybook.CobolText.picX;

import java.util.Optional;

/**
 * COSGN00 symbolic map, output side (COSGN0AO): the *O data fields sent by SEND MAP.
 * Starts as LOW-VALUES (fields BMS leaves untouched). {@code cursorField} reflects the
 * symbolic cursor ({@code MOVE -1 TO xxxL}) used by SEND MAP ... CURSOR.
 */
public final class Cosgn0aOutput {

    private String trnnameO = lowValues(Cosgn0aField.TRNNAME.length());
    private String title01O = lowValues(Cosgn0aField.TITLE01.length());
    private String curdateO = lowValues(Cosgn0aField.CURDATE.length());
    private String pgmnameO = lowValues(Cosgn0aField.PGMNAME.length());
    private String title02O = lowValues(Cosgn0aField.TITLE02.length());
    private String curtimeO = lowValues(Cosgn0aField.CURTIME.length());
    private String applidO = lowValues(Cosgn0aField.APPLID.length());
    private String sysidO = lowValues(Cosgn0aField.SYSID.length());
    private String useridO = lowValues(Cosgn0aField.USERID.length());
    private String passwdO = lowValues(Cosgn0aField.PASSWD.length());
    private String errmsgO = lowValues(Cosgn0aField.ERRMSG.length());
    private Cosgn0aField cursorField;

    public String trnnameO() { return trnnameO; }
    public void trnnameO(String v) { trnnameO = picX(v, Cosgn0aField.TRNNAME.length()); }
    public String title01O() { return title01O; }
    public void title01O(String v) { title01O = picX(v, Cosgn0aField.TITLE01.length()); }
    public String curdateO() { return curdateO; }
    public void curdateO(String v) { curdateO = picX(v, Cosgn0aField.CURDATE.length()); }
    public String pgmnameO() { return pgmnameO; }
    public void pgmnameO(String v) { pgmnameO = picX(v, Cosgn0aField.PGMNAME.length()); }
    public String title02O() { return title02O; }
    public void title02O(String v) { title02O = picX(v, Cosgn0aField.TITLE02.length()); }
    public String curtimeO() { return curtimeO; }
    public void curtimeO(String v) { curtimeO = picX(v, Cosgn0aField.CURTIME.length()); }
    public String applidO() { return applidO; }
    public void applidO(String v) { applidO = picX(v, Cosgn0aField.APPLID.length()); }
    public String sysidO() { return sysidO; }
    public void sysidO(String v) { sysidO = picX(v, Cosgn0aField.SYSID.length()); }
    public String useridO() { return useridO; }
    public void useridO(String v) { useridO = picX(v, Cosgn0aField.USERID.length()); }
    public String passwdO() { return passwdO; }
    public void passwdO(String v) { passwdO = picX(v, Cosgn0aField.PASSWD.length()); }
    public String errmsgO() { return errmsgO; }
    public void errmsgO(String v) { errmsgO = picX(v, Cosgn0aField.ERRMSG.length()); }

    public Optional<Cosgn0aField> cursorField() {
        return Optional.ofNullable(cursorField);
    }

    /** MOVE -1 TO xxxL OF COSGN0AI. */
    public void cursorField(Cosgn0aField field) {
        cursorField = field;
    }

    /**
     * RECEIVE MAP INTO COSGN0AI: COSGN0AO REDEFINES COSGN0AI, so every received *I value
     * occupies the bytes of the matching *O field and is sent back by the next SEND MAP.
     */
    public void overlayReceived(Cosgn0aInput in) {
        trnnameO = in.trnnameI();
        title01O = in.title01I();
        curdateO = in.curdateI();
        pgmnameO = in.pgmnameI();
        title02O = in.title02I();
        curtimeO = in.curtimeI();
        applidO = in.applidI();
        sysidO = in.sysidI();
        useridO = in.useridI();
        passwdO = in.passwdI();
        errmsgO = in.errmsgI();
    }

    /** MOVE LOW-VALUES TO COSGN0AO (the map shares storage with COSGN0AI, so the cursor flag resets too). */
    public void clearToLowValues() {
        Cosgn0aOutput fresh = new Cosgn0aOutput();
        trnnameO = fresh.trnnameO;
        title01O = fresh.title01O;
        curdateO = fresh.curdateO;
        pgmnameO = fresh.pgmnameO;
        title02O = fresh.title02O;
        curtimeO = fresh.curtimeO;
        applidO = fresh.applidO;
        sysidO = fresh.sysidO;
        useridO = fresh.useridO;
        passwdO = fresh.passwdO;
        errmsgO = fresh.errmsgO;
        cursorField = null;
    }
}
