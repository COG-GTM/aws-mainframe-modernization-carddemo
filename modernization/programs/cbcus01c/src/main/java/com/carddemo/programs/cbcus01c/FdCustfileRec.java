package com.carddemo.programs.cbcus01c;

import com.carddemo.programs.cbcus01c.copybook.CustomerRecord;

/**
 * FD CUSTFILE-FILE / 01 FD-CUSTFILE-REC (500 bytes).
 * <pre>
 * 05 FD-CUST-ID    PIC 9(09).   record key
 * 05 FD-CUST-DATA  PIC X(491).
 * </pre>
 * The key is held as its raw 9-character image; KSDS ordering is by the key bytes.
 */
public record FdCustfileRec(String fdCustId, String fdCustData) {

    public static final int RECORD_LENGTH = 500;
    public static final int FD_CUST_ID_LEN = 9;
    public static final int FD_CUST_DATA_LEN = 491;

    public FdCustfileRec {
        fdCustId = CustomerRecord.pic(fdCustId, FD_CUST_ID_LEN);
        fdCustData = CustomerRecord.pic(fdCustData, FD_CUST_DATA_LEN);
    }

    public static FdCustfileRec fromFixedWidth(String image) {
        String padded = CustomerRecord.pic(image, RECORD_LENGTH);
        return new FdCustfileRec(padded.substring(0, FD_CUST_ID_LEN), padded.substring(FD_CUST_ID_LEN));
    }

    public String toFixedWidth() {
        return fdCustId + fdCustData;
    }
}
