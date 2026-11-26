package com.carddemo.programs.cocrdslc.copybook;

import com.carddemo.programs.cocrdslc.CobolText;

/**
 * COCRDSL - CCRDSLAI symbolic input map (request DTO for EXEC CICS RECEIVE MAP('CCRDSLA')).
 * A {@code null} field means "not transmitted" and is presented as LOW-VALUES; transmitted data is
 * left-justified and blank-padded (BMS default JUSTIFY=(LEFT,BLANK)).
 */
public record CcrdslaInput(
        String trnnamei, String title01i, String curdatei, String pgmnamei, String title02i,
        String curtimei, String acctsidi, String cardsidi, String crdnamei, String crdstcdi,
        String expmoni, String expyeari, String infomsgi, String errmsgi, String fkeysi) {

    public CcrdslaInput {
        trnnamei = norm(trnnamei, CcrdslaField.TRNNAME);
        title01i = norm(title01i, CcrdslaField.TITLE01);
        curdatei = norm(curdatei, CcrdslaField.CURDATE);
        pgmnamei = norm(pgmnamei, CcrdslaField.PGMNAME);
        title02i = norm(title02i, CcrdslaField.TITLE02);
        curtimei = norm(curtimei, CcrdslaField.CURTIME);
        acctsidi = norm(acctsidi, CcrdslaField.ACCTSID);
        cardsidi = norm(cardsidi, CcrdslaField.CARDSID);
        crdnamei = norm(crdnamei, CcrdslaField.CRDNAME);
        crdstcdi = norm(crdstcdi, CcrdslaField.CRDSTCD);
        expmoni = norm(expmoni, CcrdslaField.EXPMON);
        expyeari = norm(expyeari, CcrdslaField.EXPYEAR);
        infomsgi = norm(infomsgi, CcrdslaField.INFOMSG);
        errmsgi = norm(errmsgi, CcrdslaField.ERRMSG);
        fkeysi = norm(fkeysi, CcrdslaField.FKEYS);
    }

    /** Input carrying only the two enterable search fields (account id, card number). */
    public static CcrdslaInput of(String acctsidi, String cardsidi) {
        return new CcrdslaInput(null, null, null, null, null, null, acctsidi, cardsidi,
                null, null, null, null, null, null, null);
    }

    private static String norm(String v, CcrdslaField f) {
        return v == null ? CobolText.lowValues(f.length()) : CobolText.pic(v, f.length());
    }
}
