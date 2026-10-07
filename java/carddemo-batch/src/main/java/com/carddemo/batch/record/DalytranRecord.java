package com.carddemo.batch.record;

import com.carddemo.batch.codec.Field;
import com.carddemo.batch.codec.FixedWidth;
import com.carddemo.batch.codec.Layout;

import java.math.BigDecimal;

/**
 * Copybook {@code CVTRA06Y} - {@code DALYTRAN-RECORD} (RECLN 350), the DALYTRAN sequential input record.
 * <pre>
 * 05  DALYTRAN-ID                 PIC X(16).
 * 05  DALYTRAN-TYPE-CD            PIC X(02).
 * 05  DALYTRAN-CAT-CD             PIC 9(04).
 * 05  DALYTRAN-SOURCE             PIC X(10).
 * 05  DALYTRAN-DESC               PIC X(100).
 * 05  DALYTRAN-AMT                PIC S9(09)V99.
 * 05  DALYTRAN-MERCHANT-ID        PIC 9(09).
 * 05  DALYTRAN-MERCHANT-NAME      PIC X(50).
 * 05  DALYTRAN-MERCHANT-CITY      PIC X(50).
 * 05  DALYTRAN-MERCHANT-ZIP       PIC X(10).
 * 05  DALYTRAN-CARD-NUM           PIC X(16).
 * 05  DALYTRAN-ORIG-TS            PIC X(26).
 * 05  DALYTRAN-PROC-TS            PIC X(26).
 * 05  FILLER                      PIC X(20).
 * </pre>
 */
public final class DalytranRecord extends FixedWidthRecord {

    private static final Layout.Builder B = Layout.builder("DALYTRAN-RECORD");
    public static final Field DALYTRAN_ID = B.text("DALYTRAN-ID", 16);
    public static final Field DALYTRAN_TYPE_CD = B.text("DALYTRAN-TYPE-CD", 2);
    public static final Field DALYTRAN_CAT_CD = B.unsigned("DALYTRAN-CAT-CD", 4);
    public static final Field DALYTRAN_SOURCE = B.text("DALYTRAN-SOURCE", 10);
    public static final Field DALYTRAN_DESC = B.text("DALYTRAN-DESC", 100);
    public static final Field DALYTRAN_AMT = B.zoned("DALYTRAN-AMT", 9, 2);
    public static final Field DALYTRAN_MERCHANT_ID = B.unsigned("DALYTRAN-MERCHANT-ID", 9);
    public static final Field DALYTRAN_MERCHANT_NAME = B.text("DALYTRAN-MERCHANT-NAME", 50);
    public static final Field DALYTRAN_MERCHANT_CITY = B.text("DALYTRAN-MERCHANT-CITY", 50);
    public static final Field DALYTRAN_MERCHANT_ZIP = B.text("DALYTRAN-MERCHANT-ZIP", 10);
    public static final Field DALYTRAN_CARD_NUM = B.text("DALYTRAN-CARD-NUM", 16);
    public static final Field DALYTRAN_ORIG_TS = B.text("DALYTRAN-ORIG-TS", 26);
    public static final Field DALYTRAN_PROC_TS = B.text("DALYTRAN-PROC-TS", 26);
    public static final Field FILLER = B.filler(20);
    public static final Layout LAYOUT = B.build();
    public static final int LENGTH = LAYOUT.length();

    public DalytranRecord() {
        super(LAYOUT);
    }

    private DalytranRecord(byte[] raw) {
        super(LAYOUT, raw);
    }

    public static DalytranRecord decode(byte[] raw) {
        return new DalytranRecord(raw);
    }

    @Override
    public Layout layout() {
        return LAYOUT;
    }

    /** COBOL {@code READ ... INTO DALYTRAN-RECORD}: replace the whole buffer. */
    public void moveFrom(byte[] raw) {
        System.arraycopy(FixedWidth.decode(LAYOUT, raw), 0, data, 0, data.length);
    }

    public String dalytranId() {
        return FixedWidth.text(data, DALYTRAN_ID);
    }

    public void setDalytranId(String v) {
        FixedWidth.setText(data, DALYTRAN_ID, v);
    }

    public String dalytranTypeCd() {
        return FixedWidth.text(data, DALYTRAN_TYPE_CD);
    }

    public void setDalytranTypeCd(String v) {
        FixedWidth.setText(data, DALYTRAN_TYPE_CD, v);
    }

    public long dalytranCatCd() {
        return FixedWidth.unsigned(data, DALYTRAN_CAT_CD);
    }

    public void setDalytranCatCd(long v) {
        FixedWidth.setUnsigned(data, DALYTRAN_CAT_CD, v);
    }

    public String dalytranSource() {
        return FixedWidth.text(data, DALYTRAN_SOURCE);
    }

    public void setDalytranSource(String v) {
        FixedWidth.setText(data, DALYTRAN_SOURCE, v);
    }

    public String dalytranDesc() {
        return FixedWidth.text(data, DALYTRAN_DESC);
    }

    public void setDalytranDesc(String v) {
        FixedWidth.setText(data, DALYTRAN_DESC, v);
    }

    public BigDecimal dalytranAmt() {
        return FixedWidth.decimal(data, DALYTRAN_AMT);
    }

    public void setDalytranAmt(BigDecimal v) {
        FixedWidth.setDecimal(data, DALYTRAN_AMT, v);
    }

    public long dalytranMerchantId() {
        return FixedWidth.unsigned(data, DALYTRAN_MERCHANT_ID);
    }

    public void setDalytranMerchantId(long v) {
        FixedWidth.setUnsigned(data, DALYTRAN_MERCHANT_ID, v);
    }

    public String dalytranMerchantName() {
        return FixedWidth.text(data, DALYTRAN_MERCHANT_NAME);
    }

    public void setDalytranMerchantName(String v) {
        FixedWidth.setText(data, DALYTRAN_MERCHANT_NAME, v);
    }

    public String dalytranMerchantCity() {
        return FixedWidth.text(data, DALYTRAN_MERCHANT_CITY);
    }

    public void setDalytranMerchantCity(String v) {
        FixedWidth.setText(data, DALYTRAN_MERCHANT_CITY, v);
    }

    public String dalytranMerchantZip() {
        return FixedWidth.text(data, DALYTRAN_MERCHANT_ZIP);
    }

    public void setDalytranMerchantZip(String v) {
        FixedWidth.setText(data, DALYTRAN_MERCHANT_ZIP, v);
    }

    public String dalytranCardNum() {
        return FixedWidth.text(data, DALYTRAN_CARD_NUM);
    }

    public void setDalytranCardNum(String v) {
        FixedWidth.setText(data, DALYTRAN_CARD_NUM, v);
    }

    public String dalytranOrigTs() {
        return FixedWidth.text(data, DALYTRAN_ORIG_TS);
    }

    public void setDalytranOrigTs(String v) {
        FixedWidth.setText(data, DALYTRAN_ORIG_TS, v);
    }

    public String dalytranProcTs() {
        return FixedWidth.text(data, DALYTRAN_PROC_TS);
    }

    public void setDalytranProcTs(String v) {
        FixedWidth.setText(data, DALYTRAN_PROC_TS, v);
    }

    /** What {@code DISPLAY field} prints for one field (signed numerics get GnuCOBOL's trailing sign). */
    public String display(Field f) {
        return FixedWidth.display(data, f);
    }
}
