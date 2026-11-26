package com.carddemo.programs.cotrn01c;

import com.carddemo.programs.cotrn01c.copybook.TranRecord;

/** CICS file control on the TRANSACT dataset (VSAM KSDS, key TRAN-ID X(16)). */
public interface TransactFileGateway {

    /** Result of EXEC CICS READ: RESP/RESP2 plus the record when RESP = NORMAL. */
    record ReadResult(int resp, int resp2, TranRecord record) {

        public static ReadResult normal(TranRecord record) {
            return new ReadResult(CicsResp.NORMAL, 0, record);
        }

        public static ReadResult notFound() {
            return new ReadResult(CicsResp.NOTFND, 0, null);
        }
    }

    /**
     * EXEC CICS READ DATASET('TRANSACT') INTO(TRAN-RECORD) RIDFLD(TRAN-ID) KEYLENGTH(16) UPDATE.
     *
     * @param tranId 16-character key, exactly as held in TRAN-ID
     */
    ReadResult readForUpdate(String tranId);
}
