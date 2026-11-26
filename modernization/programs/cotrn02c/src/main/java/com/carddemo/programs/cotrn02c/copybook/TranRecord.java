package com.carddemo.programs.cotrn02c.copybook;

import java.math.BigDecimal;

/** CVTRA05Y {@code TRAN-RECORD}, RECLN 350. Mutable, as COBOL working storage. */
public final class TranRecord {

    public static final int LENGTH = 350;

    private String tranId = Pic.spaces(16);            // X(16)
    private String typeCd = Pic.spaces(2);             // X(02)
    private int catCd;                                 // 9(04)
    private String source = Pic.spaces(10);            // X(10)
    private String desc = Pic.spaces(100);             // X(100)
    private BigDecimal amt = new BigDecimal("0.00");   // S9(09)V99
    private long merchantId;                           // 9(09)
    private String merchantName = Pic.spaces(50);      // X(50)
    private String merchantCity = Pic.spaces(50);      // X(50)
    private String merchantZip = Pic.spaces(10);       // X(10)
    private String cardNum = Pic.spaces(16);           // X(16)
    private String origTs = Pic.spaces(26);            // X(26)
    private String procTs = Pic.spaces(26);            // X(26)
    private String filler = Pic.spaces(20);            // X(20)

    public static TranRecord parse(String line) {
        String r = Pic.x(line, LENGTH);
        TranRecord t = new TranRecord();
        t.tranId = r.substring(0, 16);
        t.typeCd = r.substring(16, 18);
        t.catCd = Pic.alphaToUnsigned(r.substring(18, 22)).intValueExact();
        t.source = r.substring(22, 32);
        t.desc = r.substring(32, 132);
        t.amt = Pic.parseZonedSigned(r.substring(132, 143), 2);
        t.merchantId = Pic.alphaToUnsigned(r.substring(143, 152)).longValueExact();
        t.merchantName = r.substring(152, 202);
        t.merchantCity = r.substring(202, 252);
        t.merchantZip = r.substring(252, 262);
        t.cardNum = r.substring(262, 278);
        t.origTs = r.substring(278, 304);
        t.procTs = r.substring(304, 330);
        t.filler = r.substring(330, 350);
        return t;
    }

    public String toRecord() {
        return tranId + typeCd + Pic.nine(catCd, 4) + source + desc + Pic.zonedSigned(amt, 9, 2)
                + Pic.nine(merchantId, 9) + merchantName + merchantCity + merchantZip + cardNum
                + origTs + procTs + filler;
    }

    public TranRecord copy() {
        return parse(toRecord());
    }

    /** {@code INITIALIZE TRAN-RECORD}: alphanumerics to SPACES, numerics to ZERO; FILLER is left unchanged. */
    public void initialize() {
        String keepFiller = filler;
        TranRecord fresh = new TranRecord();
        tranId = fresh.tranId;
        typeCd = fresh.typeCd;
        catCd = 0;
        source = fresh.source;
        desc = fresh.desc;
        amt = fresh.amt;
        merchantId = 0;
        merchantName = fresh.merchantName;
        merchantCity = fresh.merchantCity;
        merchantZip = fresh.merchantZip;
        cardNum = fresh.cardNum;
        origTs = fresh.origTs;
        procTs = fresh.procTs;
        filler = keepFiller;
    }

    public String getTranId() {
        return tranId;
    }

    public void setTranId(String v) {
        tranId = Pic.x(v, 16);
    }

    public String getTypeCd() {
        return typeCd;
    }

    public void setTypeCd(String v) {
        typeCd = Pic.x(v, 2);
    }

    public int getCatCd() {
        return catCd;
    }

    public void setCatCd(int v) {
        catCd = Integer.parseInt(Pic.nine(v, 4));
    }

    /** TRAN-CAT-CD as its PIC 9(04) DISPLAY image. */
    public String catCdImage() {
        return Pic.nine(catCd, 4);
    }

    public String getSource() {
        return source;
    }

    public void setSource(String v) {
        source = Pic.x(v, 10);
    }

    public String getDesc() {
        return desc;
    }

    public void setDesc(String v) {
        desc = Pic.x(v, 100);
    }

    public BigDecimal getAmt() {
        return amt;
    }

    public void setAmt(BigDecimal v) {
        amt = Pic.fit(v, 9, 2);
    }

    public long getMerchantId() {
        return merchantId;
    }

    public void setMerchantId(long v) {
        merchantId = Long.parseLong(Pic.nine(v, 9));
    }

    /** TRAN-MERCHANT-ID as its PIC 9(09) DISPLAY image. */
    public String merchantIdImage() {
        return Pic.nine(merchantId, 9);
    }

    public String getMerchantName() {
        return merchantName;
    }

    public void setMerchantName(String v) {
        merchantName = Pic.x(v, 50);
    }

    public String getMerchantCity() {
        return merchantCity;
    }

    public void setMerchantCity(String v) {
        merchantCity = Pic.x(v, 50);
    }

    public String getMerchantZip() {
        return merchantZip;
    }

    public void setMerchantZip(String v) {
        merchantZip = Pic.x(v, 10);
    }

    public String getCardNum() {
        return cardNum;
    }

    public void setCardNum(String v) {
        cardNum = Pic.x(v, 16);
    }

    public String getOrigTs() {
        return origTs;
    }

    public void setOrigTs(String v) {
        origTs = Pic.x(v, 26);
    }

    public String getProcTs() {
        return procTs;
    }

    public void setProcTs(String v) {
        procTs = Pic.x(v, 26);
    }

    public String getFiller() {
        return filler;
    }

    public void setFiller(String v) {
        filler = Pic.x(v, 20);
    }
}
