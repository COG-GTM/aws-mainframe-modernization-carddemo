package com.carddemo.programs.paudbunl;

/**
 * Outcome of a PAUDBUNL run: the RETURN-CODE plus the working-storage counters/flags the
 * program maintains (WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT count both segment types).
 */
public record PaudbunlResult(
        int returnCode,
        long wsNoSumryRead,
        long wsAuthSmryProcCnt,
        boolean endOfAuthdb,
        boolean moreAuths,
        String currentDate,
        String currentYyddd,
        int rootRecordsWritten,
        int childRecordsWritten) {

    public boolean abended() {
        return returnCode == Paudbunl.ABEND_RETURN_CODE;
    }
}
