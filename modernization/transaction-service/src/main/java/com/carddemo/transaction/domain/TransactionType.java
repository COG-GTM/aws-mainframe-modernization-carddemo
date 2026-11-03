package com.carddemo.transaction.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

/** TRANTYPE record, copybook CVTRA03Y. */
@Entity
@Table(name = "transaction_types")
public class TransactionType {

    @Id
    @Column(name = "type_cd", length = 2, nullable = false)
    private String typeCd;

    @Column(name = "type_desc", length = 50)
    private String typeDesc;

    protected TransactionType() {
    }

    public TransactionType(String typeCd, String typeDesc) {
        this.typeCd = typeCd;
        this.typeDesc = typeDesc;
    }

    public String getTypeCd() {
        return typeCd;
    }

    public String getTypeDesc() {
        return typeDesc;
    }

    public void setTypeDesc(String typeDesc) {
        this.typeDesc = typeDesc;
    }
}
