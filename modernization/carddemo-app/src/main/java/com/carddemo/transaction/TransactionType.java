package com.carddemo.transaction;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import org.hibernate.annotations.Immutable;

/**
 * JPA entity for table {@code transaction_type}. Transaction type record (TRANTYPE KSDS, key TRAN-TYPE); read by
 * CBTRN03C. Field Javadocs carry the CVTRA03Y item names; {@link TransactionTypeRecord} is the fixed-width value.
 */
@Entity
@Immutable
@Table(name = "transaction_type")
public class TransactionType {

    /** TRAN-TYPE PIC X(02). */
    @Id
    @Column(name = "tran_type_cd")
    private String tranTypeCd;

    /** TRAN-TYPE-DESC PIC X(50). */
    @Column(name = "description")
    private String description;

    protected TransactionType() {
    }

    public static TransactionType from(TransactionTypeRecord record) {
        TransactionType entity = new TransactionType();
        entity.tranTypeCd = record.tranTypeCd();
        entity.description = record.description();
        return entity;
    }

    public TransactionTypeRecord toRecord() {
        return new TransactionTypeRecord(tranTypeCd, description);
    }

    public String getTranTypeCd() {
        return tranTypeCd;
    }

    public String getDescription() {
        return description;
    }
}
