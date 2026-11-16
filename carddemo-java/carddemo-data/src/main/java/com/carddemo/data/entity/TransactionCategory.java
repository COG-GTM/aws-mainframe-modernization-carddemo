package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

/** TRANCATG (copybook CVTRA04Y), VSAM KSDS keyed by TRAN-CAT-KEY (type, category). */
@Entity
@Table(name = "tran_category")
public class TransactionCategory {

    @EmbeddedId
    private TransactionCategoryId id;

    @Column(name = "description", nullable = false, length = 50)
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

    @Override
    public boolean equals(Object o) {
        return this == o || (o instanceof TransactionCategory other && id != null && id.equals(other.getId()));
    }

    @Override
    public int hashCode() {
        return TransactionCategory.class.hashCode();
    }
}
