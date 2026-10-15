package com.carddemo.common.domain;

import jakarta.persistence.*;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

/** Table transaction_type; legacy source TRANTYPE / CVTRA03Y. */
@Entity
@Table(name = "transaction_type")
public class TransactionType {

    @Id
    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "type_cd")
    private String typeCd;

    @Column(name = "description")
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
}
