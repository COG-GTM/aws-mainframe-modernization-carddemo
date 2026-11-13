package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

/**
 * Transaction type reference data (VSAM KSDS TRANTYPE, RECLN 60; DB2 TRANSACTION_TYPE in the optional module).
 * <p>Source copybook: CVTRA03Y.
 */
@Entity
@Table(name = "transaction_type")
public class TransactionType {

    /** TRAN-TYPE PIC X(02) - VSAM KSDS key (TRANTYPE) */
    @Id
    @Column(name = "type_cd", length = 2)
    private String typeCode;

    /** TRAN-TYPE-DESC PIC X(50) */
    @Column(name = "description", length = 50)
    private String description;

    public String getTypeCode() {
        return typeCode;
    }

    public void setTypeCode(String typeCode) {
        this.typeCode = typeCode;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }
}
