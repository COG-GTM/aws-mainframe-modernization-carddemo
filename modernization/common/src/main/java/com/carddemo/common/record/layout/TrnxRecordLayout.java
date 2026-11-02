package com.carddemo.common.record.layout;

import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.NumericField;
import com.carddemo.common.record.RecordLayout;

/**
 * {@code COSTM01} TRNX-RECORD: the TRXFL copy of TRANSACT that {@code CREASTMT} STEP010 sorts by card
 * number and transaction id (VSAM KSDS {@code TRXFL.VSAM.KSDS}, key TRNX-KEY = card + id, RECLN 350),
 * read by {@code CBSTM03B} for the statements of {@code CBSTM03A}.
 */
public final class TrnxRecordLayout {

    private static final RecordLayout.Builder B = RecordLayout.builder("COSTM01", "TRNX-RECORD");

    public static final AlphanumericField TRNX_KEY = B.group("TRNX-KEY", 32);
    public static final AlphanumericField TRNX_CARD_NUM = B.alpha("TRNX-CARD-NUM", "X(16)");
    public static final AlphanumericField TRNX_ID = B.alpha("TRNX-ID", "X(16)");
    public static final AlphanumericField TRNX_TYPE_CD = B.alpha("TRNX-TYPE-CD", "X(02)");
    public static final NumericField TRNX_CAT_CD = B.numeric("TRNX-CAT-CD", "9(04)");
    public static final AlphanumericField TRNX_SOURCE = B.alpha("TRNX-SOURCE", "X(10)");
    public static final AlphanumericField TRNX_DESC = B.alpha("TRNX-DESC", "X(100)");
    public static final NumericField TRNX_AMT = B.numeric("TRNX-AMT", "S9(09)V99");
    public static final NumericField TRNX_MERCHANT_ID = B.numeric("TRNX-MERCHANT-ID", "9(09)");
    public static final AlphanumericField TRNX_MERCHANT_NAME = B.alpha("TRNX-MERCHANT-NAME", "X(50)");
    public static final AlphanumericField TRNX_MERCHANT_CITY = B.alpha("TRNX-MERCHANT-CITY", "X(50)");
    public static final AlphanumericField TRNX_MERCHANT_ZIP = B.alpha("TRNX-MERCHANT-ZIP", "X(10)");
    public static final AlphanumericField TRNX_ORIG_TS = B.alpha("TRNX-ORIG-TS", "X(26)");
    public static final AlphanumericField TRNX_PROC_TS = B.alpha("TRNX-PROC-TS", "X(26)");
    public static final AlphanumericField FILLER = B.alpha("FILLER", "X(20)");
    public static final RecordLayout LAYOUT = B.build(350);

    private TrnxRecordLayout() {
    }
}
