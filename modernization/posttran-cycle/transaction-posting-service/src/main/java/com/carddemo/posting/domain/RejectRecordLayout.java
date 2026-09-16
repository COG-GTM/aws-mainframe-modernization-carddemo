package com.carddemo.posting.domain;

import com.carddemo.recordio.codec.FixedWidthRecord;

import java.math.BigDecimal;

/**
 * {@code CBTRN02C REJECT-RECORD} (app/cbl/CBTRN02C.cbl lines 174-179): the 350-byte daily record
 * followed by {@code WS-VALIDATION-TRAILER} = reason {@code 9(04)} + description {@code X(76)}.
 * JCL: {@code DALYREJS DCB=(RECFM=F,LRECL=430)}.
 */
public final class RejectRecordLayout {

    public static final int LENGTH = 430;
    static final int TRAILER = 350;
    static final int REASON_CODE = 350;   // 9(04)
    static final int REASON_DESC = 354;   // X(76)

    private RejectRecordLayout() {
    }

    /** The COBOL moves the raw DALYTRAN record area into the reject record: the first 350 bytes are the input image untouched. */
    public static FixedWidthRecord encode(FixedWidthRecord dailyImage, RejectReason reason) {
        FixedWidthRecord out = FixedWidthRecord.blank(LENGTH, dailyImage.encoding());
        out.setBytes(0, dailyImage);
        out.setZoned(REASON_CODE, 4, 0, false, BigDecimal.valueOf(reason.code()));
        out.setText(REASON_DESC, 76, reason.description());
        return out;
    }

    public static FixedWidthRecord encode(PostingOutcome.Rejected rejected) {
        return encode(rejected.image(), rejected.reason());
    }
}
