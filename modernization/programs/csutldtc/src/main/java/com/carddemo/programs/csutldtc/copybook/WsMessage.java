package com.carddemo.programs.csutldtc.copybook;

import java.util.Locale;

/**
 * WS-MESSAGE (80 bytes), moved to LS-RESULT:
 * <pre>
 *   02 WS-SEVERITY   PIC X(04).   (WS-SEVERITY-N REDEFINES PIC 9(4))
 *   02 FILLER        PIC X(11) VALUE 'Mesg Code:'.
 *   02 WS-MSG-NO     PIC X(04).   (WS-MSG-NO-N REDEFINES PIC 9(4))
 *   02 FILLER        PIC X(01) VALUE SPACE.
 *   02 WS-RESULT     PIC X(15).
 *   02 FILLER        PIC X(01) VALUE SPACE.
 *   02 FILLER        PIC X(09) VALUE 'TstDate:'.
 *   02 WS-DATE       PIC X(10) VALUE SPACES.
 *   02 FILLER        PIC X(01) VALUE SPACE.
 *   02 FILLER        PIC X(10) VALUE 'Mask used:'.
 *   02 WS-DATE-FMT   PIC X(10).
 *   02 FILLER        PIC X(01) VALUE SPACE.
 *   02 FILLER        PIC X(03) VALUE SPACES.
 * </pre>
 */
public final class WsMessage {
    public static final int LENGTH = 80;
    static final String FILLER_MESG_CODE = PicX.of("Mesg Code:", 11);
    static final String FILLER_TSTDATE = PicX.of("TstDate:", 9);
    static final String FILLER_MASK_USED = PicX.of("Mask used:", 10);

    private String wsSeverity = PicX.spaces(4);
    private String wsMsgNo = PicX.spaces(4);
    private String wsResult = PicX.spaces(15);
    private String wsDate = PicX.spaces(10);
    private String wsDateFmt = PicX.spaces(10);

    /** INITIALIZE: elementary non-FILLER alphanumerics to SPACES; FILLERs and REDEFINES untouched. */
    public void initialize() {
        wsSeverity = PicX.spaces(4);
        wsMsgNo = PicX.spaces(4);
        wsResult = PicX.spaces(15);
        wsDate = PicX.spaces(10);
        wsDateFmt = PicX.spaces(10);
    }

    /** MOVE binary S9(4) TO WS-SEVERITY-N PIC 9(4): sign dropped, high-order digits truncated. */
    public void setSeverityN(int value) {
        wsSeverity = toUnsignedDisplay4(value);
    }

    public void setMsgNoN(int value) {
        wsMsgNo = toUnsignedDisplay4(value);
    }

    public void setResult(String value) {
        wsResult = PicX.of(value, 15);
    }

    public void setDate(String value) {
        wsDate = PicX.of(value, 10);
    }

    public void setDateFmt(String value) {
        wsDateFmt = PicX.of(value, 10);
    }

    public String severity() {
        return wsSeverity;
    }

    /** WS-SEVERITY-N; only meaningful once set from a numeric value. */
    public int severityN() {
        return Integer.parseInt(wsSeverity);
    }

    public String msgNo() {
        return wsMsgNo;
    }

    public String result() {
        return wsResult;
    }

    public String date() {
        return wsDate;
    }

    public String dateFmt() {
        return wsDateFmt;
    }

    public String toRecord() {
        return wsSeverity + FILLER_MESG_CODE + wsMsgNo + " " + wsResult + " " + FILLER_TSTDATE
                + wsDate + " " + FILLER_MASK_USED + wsDateFmt + " " + "   ";
    }

    private static String toUnsignedDisplay4(int value) {
        return String.format(Locale.ROOT, "%04d", Math.abs(value) % 10_000);
    }
}
