package com.carddemo.programs.cosgn00c;

import com.carddemo.programs.cosgn00c.copybook.SecUserData;

/** Gateway for {@code EXEC CICS READ DATASET('USRSEC') RIDFLD(user-id) KEYLENGTH(8)}. */
public interface UserSecurityRepository {

    /** CICS DATASET name used by COSGN00C (WS-USRSEC-FILE). */
    String DATASET = "USRSEC  ";

    /**
     * Keyed read of the USRSEC KSDS.
     *
     * @param userId the 8-byte RIDFLD (WS-USER-ID)
     * @return the CICS RESP / RESP2 and, when RESP = NORMAL, the record
     */
    ReadResult read(String userId);

    /** EXEC CICS READ outcome: RESP, RESP2 and the INTO area. */
    record ReadResult(int resp, int resp2, SecUserData record) {

        public static ReadResult normal(SecUserData record) {
            return new ReadResult(CicsResp.NORMAL, 0, record);
        }

        public static ReadResult notFound() {
            return new ReadResult(CicsResp.NOTFND, 0, null);
        }
    }
}
