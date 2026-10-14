package com.carddemo.runtime.vsam;

import com.carddemo.runtime.cics.CicsResp;
import com.carddemo.runtime.cobol.PicX;

/**
 * Result of a file-control command.
 *
 * @param data     record read (null when none)
 * @param key      key of the record read/written (KSDS), null otherwise
 * @param recordId RBA (ESDS) or RRN (RRDS) of the record, -1 for KSDS
 */
public record FileResult(CicsResp resp, int resp2, byte[] data, String key, long recordId) {

    public static FileResult of(CicsResp resp) {
        return new FileResult(resp, 0, null, null, -1);
    }

    public static FileResult of(CicsResp resp, int resp2) {
        return new FileResult(resp, resp2, null, null, -1);
    }

    public static FileResult record(CicsResp resp, byte[] data, String key) {
        return new FileResult(resp, 0, data, key, -1);
    }

    public boolean isNormal() {
        return resp == CicsResp.NORMAL;
    }

    /** Record content as a string (ISO-8859-1), null when no record. */
    public String dataString() {
        return data == null ? null : PicX.fromBytes(data);
    }
}
