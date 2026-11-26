package com.carddemo.programs.coactvwc.copybook;

import com.carddemo.programs.coactvwc.support.Cobol;

/**
 * COACTVW (cpy-bms) symbolic map {@code CACTVWAI}: the data RECEIVE MAP returns. Only ACCTSID is unprotected
 * on the screen; the other display fields are listed for completeness. A {@code null} field is one the terminal
 * did not transmit (LOW-VALUES in the symbolic map).
 */
public record CactvwaInput(
        String trnnamei, String title01i, String curdatei, String pgmnamei, String title02i, String curtimei,
        String acctsidi,
        String acsttusi, String adtopeni, String acrdlimi, String aexpdti, String acshlimi, String areisdti,
        String acurbali, String acrcycri, String aaddgrpi, String acrcydbi, String acstnumi, String acstssni,
        String acstdobi, String acstfcoi, String acsfnami, String acsmnami, String acslnami, String acsadl1i,
        String acssttei, String acsadl2i, String acszipci, String acscityi, String acsctryi, String acsphn1i,
        String acsgovti, String acsphn2i, String acseftci, String acspflgi, String infomsgi, String errmsgi) {

    /** ACCTSID: LENGTH=11, PICIN='99999999999', ATTRB=(FSET,IC,NORM,UNPROT). */
    public static final int ACCTSID_LEN = 11;

    /** Input with only the account-id field keyed (everything else not transmitted). */
    public static CactvwaInput ofAcctsid(String acctsid) {
        return new CactvwaInput(null, null, null, null, null, null, acctsid, null, null, null, null, null, null,
                null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null,
                null, null, null, null, null, null, null);
    }

    /** ACCTSIDI PIC 99999999999 as an 11-byte field (BMS pads keyed data on the right with blanks). */
    public String acctsidiField() {
        return Cobol.alnum(acctsidi, ACCTSID_LEN);
    }
}
