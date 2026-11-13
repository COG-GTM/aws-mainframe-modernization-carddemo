package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

/**
 * Transaction category reference data (VSAM KSDS TRANCATG, RECLN 60; DB2 TRANSACTION_TYPE_CATEGORY in the optional module).
 * <p>Source copybook: CVTRA04Y.
 */
@Entity
@Table(name = "transaction_category")
public class TransactionCategory {

    @EmbeddedId
    private TransactionCategoryId id;

    /** TRAN-CAT-TYPE-DESC PIC X(50) */
    @Column(name = "description", length = 50)
    private String description;

    public TransactionCategoryId getId() {
        return id;
    }

    public void setId(TransactionCategoryId id) {
        this.id = id;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }
}
