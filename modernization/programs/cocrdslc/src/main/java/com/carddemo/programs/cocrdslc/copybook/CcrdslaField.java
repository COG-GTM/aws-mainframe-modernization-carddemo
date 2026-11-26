package com.carddemo.programs.cocrdslc.copybook;

/** COCRDSL / CCRDSLA - named fields of the symbolic map, in map order, with their data lengths. */
public enum CcrdslaField {
    TRNNAME(4), TITLE01(40), CURDATE(8), PGMNAME(8), TITLE02(40), CURTIME(8),
    ACCTSID(11), CARDSID(16), CRDNAME(50), CRDSTCD(1), EXPMON(2), EXPYEAR(4),
    INFOMSG(40), ERRMSG(80), FKEYS(75);

    private final int length;

    CcrdslaField(int length) {
        this.length = length;
    }

    public int length() {
        return length;
    }
}
