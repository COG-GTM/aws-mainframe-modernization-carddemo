package com.carddemo.transaction;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.math.BigDecimal;
import org.hibernate.annotations.Immutable;

/**
 * JPA entity for table {@code transaction}. Posted transaction record (TRANSACT KSDS, key TRAN-ID; AIX on TRAN-
 * PROC-TS); browsed by COTRN00C, read by COTRN01C, written by COTRN02C, COBIL00C, CBTRN02C and CBACT04C. Field
 * Javadocs carry the CVTRA05Y item names; {@link TransactionRecord} is the fixed-width value.
 */
@Entity
@Immutable
@Table(name = "transaction")
public class Transaction {

    /** TRAN-ID PIC X(16). */
    @Id
    @Column(name = "tran_id")
    private String tranId;

    /** TRAN-TYPE-CD PIC X(02). */
    @Column(name = "tran_type_cd")
    private String tranTypeCd;

    /** TRAN-CAT-CD PIC 9(04). */
    @Column(name = "tran_cat_cd")
    private int tranCatCd;

    /** TRAN-SOURCE PIC X(10). */
    @Column(name = "source")
    private String source;

    /** TRAN-DESC PIC X(100). */
    @Column(name = "description")
    private String description;

    /** TRAN-AMT PIC S9(09)V99. */
    @Column(name = "amount", precision = 11, scale = 2)
    private BigDecimal amount;

    /** TRAN-MERCHANT-ID PIC 9(09). */
    @Column(name = "merchant_id")
    private int merchantId;

    /** TRAN-MERCHANT-NAME PIC X(50). */
    @Column(name = "merchant_name")
    private String merchantName;

    /** TRAN-MERCHANT-CITY PIC X(50). */
    @Column(name = "merchant_city")
    private String merchantCity;

    /** TRAN-MERCHANT-ZIP PIC X(10). */
    @Column(name = "merchant_zip")
    private String merchantZip;

    /** TRAN-CARD-NUM PIC X(16). */
    @Column(name = "card_num")
    private String cardNum;

    /** TRAN-ORIG-TS PIC X(26). */
    @Column(name = "orig_ts")
    private String origTs;

    /** TRAN-PROC-TS PIC X(26). */
    @Column(name = "proc_ts")
    private String procTs;

    protected Transaction() {
    }

    public static Transaction from(TransactionRecord record) {
        Transaction entity = new Transaction();
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

    public TransactionRecord toRecord() {
        return new TransactionRecord(tranId, tranTypeCd, tranCatCd, source, description, amount, merchantId,
                merchantName, merchantCity, merchantZip, cardNum, origTs, procTs);
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
