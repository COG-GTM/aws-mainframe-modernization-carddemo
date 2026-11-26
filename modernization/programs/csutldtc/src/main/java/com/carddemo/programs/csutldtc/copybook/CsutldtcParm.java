package com.carddemo.programs.csutldtc.copybook;

/**
 * Caller-side parameter block declared in CORPT00C and COTRN02C (CSUTLDTC-PARM, 100 bytes):
 * <pre>
 *   05 CSUTLDTC-DATE                PIC X(10).
 *   05 CSUTLDTC-DATE-FORMAT         PIC X(10).
 *   05 CSUTLDTC-RESULT.
 *      10 CSUTLDTC-RESULT-SEV-CD    PIC X(04).
 *      10 FILLER                    PIC X(11).
 *      10 CSUTLDTC-RESULT-MSG-NUM   PIC X(04).
 *      10 CSUTLDTC-RESULT-MSG       PIC X(61).
 * </pre>
 */
public record CsutldtcParm(String date, String dateFormat, String result) {
    public static final int LENGTH = 100;
    public static final String UNSUPPORTED_RANGE_MSG_NUM = "2513";

    public CsutldtcParm {
        date = PicX.of(date, 10);
        dateFormat = PicX.of(dateFormat, 10);
        result = PicX.of(result, 80);
    }

    public static CsutldtcParm fromRecord(String record) {
        String r = PicX.of(record, LENGTH);
        return new CsutldtcParm(r.substring(0, 10), r.substring(10, 20), r.substring(20, 100));
    }

    public String toRecord() {
        return date + dateFormat + result;
    }

    public String resultSevCd() {
        return result.substring(0, 4);
    }

    public String resultMsgNum() {
        return result.substring(15, 19);
    }

    public String resultMsg() {
        return result.substring(19, 80);
    }

    /**
     * The check both callers apply after CALL 'CSUTLDTC':
     * {@code IF CSUTLDTC-RESULT-SEV-CD = '0000' ... ELSE IF CSUTLDTC-RESULT-MSG-NUM NOT = '2513' -> error}.
     */
    public boolean acceptedByCallers() {
        return "0000".equals(resultSevCd()) || UNSUPPORTED_RANGE_MSG_NUM.equals(resultMsgNum());
    }
}
