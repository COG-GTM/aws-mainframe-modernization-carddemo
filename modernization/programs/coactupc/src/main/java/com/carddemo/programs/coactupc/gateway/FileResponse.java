package com.carddemo.programs.coactupc.gateway;

/**
 * Outcome of a CICS file control command: RESP / RESP2 and, for READ, the record read INTO.
 *
 * @param resp   WS-RESP-CD
 * @param resp2  WS-REAS-CD
 * @param record record returned by READ (null when not NORMAL, or for REWRITE)
 */
public record FileResponse<T>(int resp, int resp2, T record) {

    public static <T> FileResponse<T> normal(T record) {
        return new FileResponse<>(CicsResp.NORMAL, 0, record);
    }

    public static <T> FileResponse<T> of(int resp, int resp2) {
        return new FileResponse<>(resp, resp2, null);
    }

    public boolean isNormal() {
        return resp == CicsResp.NORMAL;
    }
}
