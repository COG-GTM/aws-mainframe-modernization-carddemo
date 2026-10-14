package com.carddemo.runtime.cics;

/**
 * How a task ended.
 *
 * @param type         RETURN (conversation ends), RETURN_TRANSID (pseudo-conversational return),
 *                     XCTL (transfer to another program), ABEND
 * @param nextTransid  transaction to start on the next terminal input (RETURN_TRANSID)
 * @param nextProgram  program to transfer to (XCTL)
 * @param commarea     commarea passed on (RETURN TRANSID / XCTL), null when none
 * @param abendCode    abend code (ABEND)
 */
public record CicsOutcome(Type type, String nextTransid, String nextProgram, byte[] commarea, String abendCode) {

    /** Kind of task termination. */
    public enum Type {
        RETURN, RETURN_TRANSID, XCTL, ABEND
    }

    public static CicsOutcome returnToCics() {
        return new CicsOutcome(Type.RETURN, null, null, null, null);
    }

    public static CicsOutcome returnTransid(String transid, byte[] commarea) {
        return new CicsOutcome(Type.RETURN_TRANSID, transid, null, commarea == null ? null : commarea.clone(), null);
    }

    public static CicsOutcome xctl(String program, byte[] commarea) {
        return new CicsOutcome(Type.XCTL, null, program, commarea == null ? null : commarea.clone(), null);
    }

    public static CicsOutcome abend(String code) {
        return new CicsOutcome(Type.ABEND, null, null, null, code);
    }
}
