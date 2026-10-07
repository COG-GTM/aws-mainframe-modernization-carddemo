package com.carddemo.transaction;

import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import org.hibernate.annotations.Immutable;

/**
 * JPA entity for table {@code transaction_category}. Transaction category record (TRANCATG KSDS, key TRAN-CAT-KEY =
 * TRAN-TYPE-CD + TRAN-CAT-CD); read by CBTRN03C. Field Javadocs carry the CVTRA04Y item names; {@link
 * TransactionCategoryRecord} is the fixed-width value.
 */
@Entity
@Immutable
@Table(name = "transaction_category")
public class TransactionCategory {

    @EmbeddedId
    private TransactionCategoryId id;

    /** TRAN-CAT-TYPE-DESC PIC X(50). */
    @Column(name = "description")
    private String description;

    protected TransactionCategory() {
    }

    public static TransactionCategory from(TransactionCategoryRecord record) {
        TransactionCategory entity = new TransactionCategory();
        entity.id = new TransactionCategoryId(record.tranTypeCd(), record.tranCatCd());
        entity.description = record.description();
        return entity;
    }

    public TransactionCategoryRecord toRecord() {
        return new TransactionCategoryRecord(id.tranTypeCd(), id.tranCatCd(), description);
    }

    public TransactionCategoryId getId() {
        return id;
    }

    public String getDescription() {
        return description;
    }
}
