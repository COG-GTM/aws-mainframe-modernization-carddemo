package com.carddemo.batch.posttran.domain;

import com.carddemo.batch.posttran.cobol.Pic;
import com.carddemo.batch.posttran.cobol.ZonedDecimal;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.math.BigDecimal;

/** TRANFILE KSDS (TRANSACT.VSAM.KSDS), TRAN-RECORD from CVTRA05Y, key TRAN-ID X(16). */
@Entity
@Table(name = "transact")
public class TransactionRecord {

    public static final int LENGTH = 350;

    @Id
    @Column(name = "tran_id", length = 16)
    private String tranId;
    @Column(name = "type_cd", length = 2, nullable = false)
    private String typeCd;
    @Column(name = "cat_cd", nullable = false)
    private int catCd;
    @Column(name = "source", length = 10, nullable = false)
    private String source;
    @Column(name = "description", length = 100, nullable = false)
    private String desc;
    @Column(name = "amt", precision = 11, scale = 2, nullable = false)
    private BigDecimal amt;
    @Column(name = "merchant_id", nullable = false)
    private long merchantId;
    @Column(name = "merchant_name", length = 50, nullable = false)
    private String merchantName;
    @Column(name = "merchant_city", length = 50, nullable = false)
    private String merchantCity;
    @Column(name = "merchant_zip", length = 10, nullable = false)
    private String merchantZip;
    @Column(name = "card_num", length = 16, nullable = false)
    private String cardNum;
    @Column(name = "orig_ts", length = 26, nullable = false)
    private String origTs;
    @Column(name = "proc_ts", length = 26, nullable = false)
    private String procTs;
    @Column(name = "filler", length = 20, nullable = false)
    private String filler = Pic.spaces(20);

    protected TransactionRecord() {
    }

    public TransactionRecord(String tranId) {
        this.tranId = tranId;
    }

    public static TransactionRecord fromRecord(String r) {
        TransactionRecord t = new TransactionRecord(r.substring(0, 16));
        t.typeCd = r.substring(16, 18);
        t.catCd = (int) ZonedDecimal.decodeUnsigned(r.substring(18, 22));
        t.source = r.substring(22, 32);
        t.desc = r.substring(32, 132);
        t.amt = ZonedDecimal.decodeSigned(r.substring(132, 143), 2);
        t.merchantId = ZonedDecimal.decodeUnsigned(r.substring(143, 152));
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
        return Pic.x(tranId, 16) + Pic.x(typeCd, 2) + ZonedDecimal.encodeUnsigned(catCd, 4)
                + Pic.x(source, 10) + Pic.x(desc, 100) + ZonedDecimal.encodeSigned(amt, 9, 2)
                + ZonedDecimal.encodeUnsigned(merchantId, 9) + Pic.x(merchantName, 50)
                + Pic.x(merchantCity, 50) + Pic.x(merchantZip, 10) + Pic.x(cardNum, 16)
                + Pic.x(origTs, 26) + Pic.x(procTs, 26) + Pic.x(filler, 20);
    }

    public String getTranId() { return tranId; }
    public String getTypeCd() { return typeCd; }
    public void setTypeCd(String typeCd) { this.typeCd = typeCd; }
    public int getCatCd() { return catCd; }
    public void setCatCd(int catCd) { this.catCd = catCd; }
    public String getSource() { return source; }
    public void setSource(String source) { this.source = source; }
    public String getDesc() { return desc; }
    public void setDesc(String desc) { this.desc = desc; }
    public BigDecimal getAmt() { return amt; }
    public void setAmt(BigDecimal amt) { this.amt = amt; }
    public long getMerchantId() { return merchantId; }
    public void setMerchantId(long merchantId) { this.merchantId = merchantId; }
    public String getMerchantName() { return merchantName; }
    public void setMerchantName(String merchantName) { this.merchantName = merchantName; }
    public String getMerchantCity() { return merchantCity; }
    public void setMerchantCity(String merchantCity) { this.merchantCity = merchantCity; }
    public String getMerchantZip() { return merchantZip; }
    public void setMerchantZip(String merchantZip) { this.merchantZip = merchantZip; }
    public String getCardNum() { return cardNum; }
    public void setCardNum(String cardNum) { this.cardNum = cardNum; }
    public String getOrigTs() { return origTs; }
    public void setOrigTs(String origTs) { this.origTs = origTs; }
    public String getProcTs() { return procTs; }
    public void setProcTs(String procTs) { this.procTs = procTs; }
}
