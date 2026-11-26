package com.carddemo.programs.cousr00c;

import java.util.List;

/** Task abend raised by an unhandled CICS condition (e.g. AEIP for INVREQ on ENDBR). */
public class CicsAbendException extends RuntimeException {

    private final String abendCode;
    private final CicsResponse response;
    private final List<ScreenSend> sendsBeforeAbend;
    private final List<String> displaysBeforeAbend;

    public CicsAbendException(String abendCode, CicsResponse response, List<ScreenSend> sendsBeforeAbend,
            List<String> displaysBeforeAbend) {
        super("ABEND " + abendCode + " (" + response.resp() + ", RESP2=" + response.resp2() + ")");
        this.abendCode = abendCode;
        this.response = response;
        this.sendsBeforeAbend = List.copyOf(sendsBeforeAbend);
        this.displaysBeforeAbend = List.copyOf(displaysBeforeAbend);
    }

    /** DISPLAY (SYSOUT) lines written before the abend. */
    public List<String> displaysBeforeAbend() {
        return displaysBeforeAbend;
    }

    public String abendCode() {
        return abendCode;
    }

    public CicsResponse response() {
        return response;
    }

    public List<ScreenSend> sendsBeforeAbend() {
        return sendsBeforeAbend;
    }
}
