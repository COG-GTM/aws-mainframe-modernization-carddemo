package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

/** TRANTYPE (copybook CVTRA03Y), VSAM KSDS keyed by TRAN-TYPE. */
@Entity
@Table(name = "tran_type")
public class TransactionType {

    @Id
    @Column(name = "type_cd", nullable = false, length = 2)
    private String typeCd;

    @Column(name = "description", nullable = false, length = 50)
    private String description;

    public String getTypeCd() {
        return typeCd;
    }

    public void setTypeCd(String typeCd) {
        this.typeCd = typeCd;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    @Override
    public boolean equals(Object o) {
        return this == o || (o instanceof TransactionType other && typeCd != null && typeCd.equals(other.getTypeCd()));
    }

    @Override
    public int hashCode() {
        return TransactionType.class.hashCode();
    }
}
