package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import java.math.BigDecimal;

/**
 * Interest rate per account group / transaction type / category (VSAM KSDS DISCGRP, RECLN 50).
 * <p>Source copybook: CVTRA02Y.
 */
@Entity
@Table(name = "disclosure_group")
public class DisclosureGroup {

    @EmbeddedId
    private DisclosureGroupId id;

    /** DIS-INT-RATE PIC S9(04)V99 (annual %) */
    @Column(name = "interest_rate", precision = 6, scale = 2)
    private BigDecimal interestRate;

    public DisclosureGroupId getId() {
        return id;
    }

    public void setId(DisclosureGroupId id) {
        this.id = id;
    }

    public BigDecimal getInterestRate() {
        return interestRate;
    }

    public void setInterestRate(BigDecimal interestRate) {
        this.interestRate = interestRate;
    }
}
