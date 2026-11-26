package com.carddemo.programs.cotrn02c.copybook;

/**
 * COTRN02C {@code CSUTLDTC-PARM}: date X(10), format X(10) and the 80-byte result
 * (SEV-CD X(4), FILLER X(11), MSG-NUM X(4), MSG X(61)).
 */
public record CsutldtcParm(String date, String dateFormat, String result) {

    public static final int RESULT_LENGTH = 80;

    public CsutldtcParm {
        date = Pic.x(date, 10);
        dateFormat = Pic.x(dateFormat, 10);
        result = Pic.x(result, RESULT_LENGTH);
    }

    /** CSUTLDTC-RESULT-SEV-CD. */
    public String resultSevCd() {
        return result.substring(0, 4);
    }

    /** CSUTLDTC-RESULT-MSG-NUM. */
    public String resultMsgNum() {
        return result.substring(15, 19);
    }

    /** CSUTLDTC-RESULT-MSG. */
    public String resultMsg() {
        return result.substring(19, 80);
    }
}
