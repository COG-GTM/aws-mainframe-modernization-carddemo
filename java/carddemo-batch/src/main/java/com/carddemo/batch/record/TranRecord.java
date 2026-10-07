package com.carddemo.batch.record;

import com.carddemo.batch.codec.Field;
import com.carddemo.batch.codec.FixedWidth;
import com.carddemo.batch.codec.Layout;

import java.math.BigDecimal;

/**
 * Copybook {@code CVTRA05Y} - {@code TRAN-RECORD} (RECLN 350), the TRANFILE KSDS record (key TRAN-ID).
 * <pre>
 * 05  TRAN-ID                     PIC X(16).
 * 05  TRAN-TYPE-CD                PIC X(02).
 * 05  TRAN-CAT-CD                 PIC 9(04).
 * 05  TRAN-SOURCE                 PIC X(10).
 * 05  TRAN-DESC                   PIC X(100).
 * 05  TRAN-AMT                    PIC S9(09)V99.
 * 05  TRAN-MERCHANT-ID            PIC 9(09).
 * 05  TRAN-MERCHANT-NAME          PIC X(50).
 * 05  TRAN-MERCHANT-CITY          PIC X(50).
 * 05  TRAN-MERCHANT-ZIP           PIC X(10).
 * 05  TRAN-CARD-NUM               PIC X(16).
 * 05  TRAN-ORIG-TS                PIC X(26).
 * 05  TRAN-PROC-TS                PIC X(26).
 * 05  FILLER                      PIC X(20).
 * </pre>
 */
public final class TranRecord extends FixedWidthRecord {

    private static final Layout.Builder B = Layout.builder("TRAN-RECORD");
    public static final Field TRAN_ID = B.text("TRAN-ID", 16);
    public static final Field TRAN_TYPE_CD = B.text("TRAN-TYPE-CD", 2);
    public static final Field TRAN_CAT_CD = B.unsigned("TRAN-CAT-CD", 4);
    public static final Field TRAN_SOURCE = B.text("TRAN-SOURCE", 10);
    public static final Field TRAN_DESC = B.text("TRAN-DESC", 100);
    public static final Field TRAN_AMT = B.zoned("TRAN-AMT", 9, 2);
    public static final Field TRAN_MERCHANT_ID = B.unsigned("TRAN-MERCHANT-ID", 9);
    public static final Field TRAN_MERCHANT_NAME = B.text("TRAN-MERCHANT-NAME", 50);
    public static final Field TRAN_MERCHANT_CITY = B.text("TRAN-MERCHANT-CITY", 50);
    public static final Field TRAN_MERCHANT_ZIP = B.text("TRAN-MERCHANT-ZIP", 10);
    public static final Field TRAN_CARD_NUM = B.text("TRAN-CARD-NUM", 16);
    public static final Field TRAN_ORIG_TS = B.text("TRAN-ORIG-TS", 26);
    public static final Field TRAN_PROC_TS = B.text("TRAN-PROC-TS", 26);
    public static final Field FILLER = B.filler(20);
    public static final Layout LAYOUT = B.build();
    public static final int LENGTH = LAYOUT.length();

    public TranRecord() {
        super(LAYOUT);
    }

    private TranRecord(byte[] raw) {
        super(LAYOUT, raw);
    }

    public static TranRecord decode(byte[] raw) {
        return new TranRecord(raw);
    }

    @Override
    public Layout layout() {
        return LAYOUT;
    }

    /** COBOL {@code READ ... INTO TRAN-RECORD}: replace the whole buffer. */
    public void moveFrom(byte[] raw) {
        System.arraycopy(FixedWidth.decode(LAYOUT, raw), 0, data, 0, data.length);
    }

    public String tranId() {
        return FixedWidth.text(data, TRAN_ID);
    }

    public void setTranId(String v) {
        FixedWidth.setText(data, TRAN_ID, v);
    }

    public String tranTypeCd() {
        return FixedWidth.text(data, TRAN_TYPE_CD);
    }

    public void setTranTypeCd(String v) {
        FixedWidth.setText(data, TRAN_TYPE_CD, v);
    }

    public long tranCatCd() {
        return FixedWidth.unsigned(data, TRAN_CAT_CD);
    }

    public void setTranCatCd(long v) {
        FixedWidth.setUnsigned(data, TRAN_CAT_CD, v);
    }

    public String tranSource() {
        return FixedWidth.text(data, TRAN_SOURCE);
    }

    public void setTranSource(String v) {
        FixedWidth.setText(data, TRAN_SOURCE, v);
    }

    public String tranDesc() {
        return FixedWidth.text(data, TRAN_DESC);
    }

    public void setTranDesc(String v) {
        FixedWidth.setText(data, TRAN_DESC, v);
    }

    public BigDecimal tranAmt() {
        return FixedWidth.decimal(data, TRAN_AMT);
    }

    public void setTranAmt(BigDecimal v) {
        FixedWidth.setDecimal(data, TRAN_AMT, v);
    }

    public long tranMerchantId() {
        return FixedWidth.unsigned(data, TRAN_MERCHANT_ID);
    }

    public void setTranMerchantId(long v) {
        FixedWidth.setUnsigned(data, TRAN_MERCHANT_ID, v);
    }

    public String tranMerchantName() {
        return FixedWidth.text(data, TRAN_MERCHANT_NAME);
    }

    public void setTranMerchantName(String v) {
        FixedWidth.setText(data, TRAN_MERCHANT_NAME, v);
    }

    public String tranMerchantCity() {
        return FixedWidth.text(data, TRAN_MERCHANT_CITY);
    }

    public void setTranMerchantCity(String v) {
        FixedWidth.setText(data, TRAN_MERCHANT_CITY, v);
    }

    public String tranMerchantZip() {
        return FixedWidth.text(data, TRAN_MERCHANT_ZIP);
    }

    public void setTranMerchantZip(String v) {
        FixedWidth.setText(data, TRAN_MERCHANT_ZIP, v);
    }

    public String tranCardNum() {
        return FixedWidth.text(data, TRAN_CARD_NUM);
    }

    public void setTranCardNum(String v) {
        FixedWidth.setText(data, TRAN_CARD_NUM, v);
    }

    public String tranOrigTs() {
        return FixedWidth.text(data, TRAN_ORIG_TS);
    }

    public void setTranOrigTs(String v) {
        FixedWidth.setText(data, TRAN_ORIG_TS, v);
    }

    public String tranProcTs() {
        return FixedWidth.text(data, TRAN_PROC_TS);
    }

    public void setTranProcTs(String v) {
        FixedWidth.setText(data, TRAN_PROC_TS, v);
    }

    /** What {@code DISPLAY field} prints for one field (signed numerics get GnuCOBOL's trailing sign). */
    public String display(Field f) {
        return FixedWidth.display(data, f);
    }
}
