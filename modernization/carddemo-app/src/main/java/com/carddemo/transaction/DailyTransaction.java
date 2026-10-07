package com.carddemo.transaction;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.math.BigDecimal;
import org.hibernate.annotations.Immutable;

/**
 * JPA entity for table {@code daily_transaction}. Daily transaction input record (DALYTRAN sequential file), read
 * in file order by CBTRN01C/CBTRN02C. The file has no key and may repeat DALYTRAN-ID, so rows are keyed by their
 * 1-based position {@code record_seq}. Field Javadocs carry the CVTRA06Y item names; {@link DailyTransactionRecord}
 * is the fixed-width value.
 */
@Entity
@Immutable
@Table(name = "daily_transaction")
public class DailyTransaction {

    /** 1-based position of the record in the DALYTRAN file (not a copybook field). */
    @Id
    @Column(name = "record_seq")
    private int recordSeq;

    /** DALYTRAN-ID PIC X(16). */
    @Column(name = "tran_id")
    private String tranId;

    /** DALYTRAN-TYPE-CD PIC X(02). */
    @Column(name = "tran_type_cd")
    private String tranTypeCd;

    /** DALYTRAN-CAT-CD PIC 9(04). */
    @Column(name = "tran_cat_cd")
    private int tranCatCd;

    /** DALYTRAN-SOURCE PIC X(10). */
    @Column(name = "source")
    private String source;

    /** DALYTRAN-DESC PIC X(100). */
    @Column(name = "description")
    private String description;

    /** DALYTRAN-AMT PIC S9(09)V99. */
    @Column(name = "amount", precision = 11, scale = 2)
    private BigDecimal amount;

    /** DALYTRAN-MERCHANT-ID PIC 9(09). */
    @Column(name = "merchant_id")
    private int merchantId;

    /** DALYTRAN-MERCHANT-NAME PIC X(50). */
    @Column(name = "merchant_name")
    private String merchantName;

    /** DALYTRAN-MERCHANT-CITY PIC X(50). */
    @Column(name = "merchant_city")
    private String merchantCity;

    /** DALYTRAN-MERCHANT-ZIP PIC X(10). */
    @Column(name = "merchant_zip")
    private String merchantZip;

    /** DALYTRAN-CARD-NUM PIC X(16). */
    @Column(name = "card_num")
    private String cardNum;

    /** DALYTRAN-ORIG-TS PIC X(26). */
    @Column(name = "orig_ts")
    private String origTs;

    /** DALYTRAN-PROC-TS PIC X(26). */
    @Column(name = "proc_ts")
    private String procTs;

    protected DailyTransaction() {
    }

    public static DailyTransaction from(int recordSeq, DailyTransactionRecord record) {
        DailyTransaction entity = new DailyTransaction();
        entity.recordSeq = recordSeq;
        entity.tranId = record.tranId();
        entity.tranTypeCd = record.tranTypeCd();
        entity.tranCatCd = record.tranCatCd();
        entity.source = record.source();
        entity.description = record.description();
        entity.amount = record.amount();
        entity.merchantId = record.merchantId();
        entity.merchantName = record.merchantName();
        entity.merchantCity = record.merchantCity();
        entity.merchantZip = record.merchantZip();
        entity.cardNum = record.cardNum();
        entity.origTs = record.origTs();
        entity.procTs = record.procTs();
        return entity;
    }

    public DailyTransactionRecord toRecord() {
        return new DailyTransactionRecord(tranId, tranTypeCd, tranCatCd, source, description, amount, merchantId,
                merchantName, merchantCity, merchantZip, cardNum, origTs, procTs);
    }

    public int getRecordSeq() {
        return recordSeq;
    }

    public String getTranId() {
        return tranId;
    }

    public String getTranTypeCd() {
        return tranTypeCd;
    }

    public int getTranCatCd() {
        return tranCatCd;
    }

    public String getSource() {
        return source;
    }

    public String getDescription() {
        return description;
    }

    public BigDecimal getAmount() {
        return amount;
    }

    public int getMerchantId() {
        return merchantId;
    }

    public String getMerchantName() {
        return merchantName;
    }

    public String getMerchantCity() {
        return merchantCity;
    }

    public String getMerchantZip() {
        return merchantZip;
    }

    public String getCardNum() {
        return cardNum;
    }

    public String getOrigTs() {
        return origTs;
    }

    public String getProcTs() {
        return procTs;
    }
}
