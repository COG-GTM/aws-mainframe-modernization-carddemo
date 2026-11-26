package com.carddemo.programs.csutldtc;

import com.carddemo.programs.csutldtc.copybook.CsutldtcParm;
import java.nio.charset.StandardCharsets;

/**
 * Outcome of one CALL 'CSUTLDTC': LS-RESULT (80 bytes, one char per byte) and RETURN-CODE.
 * {@code outputLillian} is the CEEDAYS output the COBOL program computes but does not return; kept for callers.
 */
public record CsutldtcResult(String lsResult, int returnCode, int outputLillian) {
    public byte[] lsResultBytes() {
        return lsResult.getBytes(StandardCharsets.ISO_8859_1);
    }

    public String severityCode() {
        return lsResult.substring(0, 4);
    }

    public String msgNum() {
        return lsResult.substring(15, 19);
    }

    /** WS-RESULT text (15 bytes). */
    public String resultText() {
        return lsResult.substring(20, 35);
    }

    public CsutldtcParm toParm(String date, String dateFormat) {
        return new CsutldtcParm(date, dateFormat, lsResult);
    }
}
