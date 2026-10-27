package com.carddemo.transaction.domain;

import com.carddemo.transaction.cobol.Pic;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

import java.math.BigDecimal;

/**
 * TRAN-RECORD (copybook CVTRA05Y, RECLN 350) - TRANSACT VSAM KSDS keyed by TRAN-ID.
 */
@Entity
@Table(name = "TRANSACT")
public class Transaction {

    @Id
    @Column(name = "TRAN_ID", length = 16)
    private String tranId;
    @Column(name = "TRAN_TYPE_CD", length = 2, nullable = false)
    private String typeCd;
    @Column(name = "TRAN_CAT_CD", nullable = false)
    private Integer catCd;
    @Column(name = "TRAN_SOURCE", length = 10, nullable = false)
    private String source;
    @Column(name = "TRAN_DESC", length = 100, nullable = false)
    private String desc;
    @Column(name = "TRAN_AMT", precision = 11, scale = 2, nullable = false)
    private BigDecimal amt;
    @Column(name = "TRAN_MERCHANT_ID", nullable = false)
    private Long merchantId;
    @Column(name = "TRAN_MERCHANT_NAME", length = 50, nullable = false)
    private String merchantName;
    @Column(name = "TRAN_MERCHANT_CITY", length = 50, nullable = false)
    private String merchantCity;
    @Column(name = "TRAN_MERCHANT_ZIP", length = 10, nullable = false)
    private String merchantZip;
    @Column(name = "TRAN_CARD_NUM", length = 16, nullable = false)
    private String cardNum;
    @Column(name = "TRAN_ORIG_TS", length = 26, nullable = false)
    private String origTs;
    @Column(name = "TRAN_PROC_TS", length = 26, nullable = false)
    private String procTs;

    /** {@code INITIALIZE TRAN-RECORD}: alphanumerics to spaces, numerics to zero. */
    public Transaction() {
        setTranId(null);
        setTypeCd(null);
        setCatCd(0);
        setSource(null);
        setDesc(null);
        setAmt(BigDecimal.ZERO);
        setMerchantId(0L);
        setMerchantName(null);
        setMerchantCity(null);
        setMerchantZip(null);
        setCardNum(null);
        setOrigTs(null);
        setProcTs(null);
    }

    public String getTranId() { return tranId; }
    public void setTranId(String v) { this.tranId = Pic.x(v, 16); }
    public String getTypeCd() { return typeCd; }
    public void setTypeCd(String v) { this.typeCd = Pic.x(v, 2); }
    public Integer getCatCd() { return catCd; }
    public void setCatCd(int v) { this.catCd = Pic.n9(v, 4); }
    public String getSource() { return source; }
    public void setSource(String v) { this.source = Pic.x(v, 10); }
    public String getDesc() { return desc; }
    public void setDesc(String v) { this.desc = Pic.x(v, 100); }
    public BigDecimal getAmt() { return amt; }
    public void setAmt(BigDecimal v) { this.amt = Pic.s9(v, 9, 2); }
    public Long getMerchantId() { return merchantId; }
    public void setMerchantId(long v) { this.merchantId = Pic.n9(v, 9); }
    public String getMerchantName() { return merchantName; }
    public void setMerchantName(String v) { this.merchantName = Pic.x(v, 50); }
    public String getMerchantCity() { return merchantCity; }
    public void setMerchantCity(String v) { this.merchantCity = Pic.x(v, 50); }
    public String getMerchantZip() { return merchantZip; }
    public void setMerchantZip(String v) { this.merchantZip = Pic.x(v, 10); }
    public String getCardNum() { return cardNum; }
    public void setCardNum(String v) { this.cardNum = Pic.x(v, 16); }
    public String getOrigTs() { return origTs; }
    public void setOrigTs(String v) { this.origTs = Pic.x(v, 26); }
    public String getProcTs() { return procTs; }
    public void setProcTs(String v) { this.procTs = Pic.x(v, 26); }
}
