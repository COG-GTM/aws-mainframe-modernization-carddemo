package com.carddemo.transaction.domain;

import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

/** TRANCATG record, copybook CVTRA04Y. */
@Entity
@Table(name = "transaction_categories")
public class TransactionCategoryType {

    @EmbeddedId
    private TransactionCategoryId id;

    @Column(name = "cat_type_desc", length = 50)
    private String description;

    protected TransactionCategoryType() {
    }

    public TransactionCategoryType(TransactionCategoryId id, String description) {
        this.id = id;
        this.description = description;
    }

    public TransactionCategoryId getId() {
        return id;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }
}
