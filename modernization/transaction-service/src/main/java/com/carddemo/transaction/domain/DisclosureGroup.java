package com.carddemo.transaction.domain;

import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import java.math.BigDecimal;

/** DISCGRP record, copybook CVTRA02Y: the annual interest rate per group, type and category. */
@Entity
@Table(name = "disclosure_groups")
public class DisclosureGroup {

    /** CBACT04C falls back to this group id when the account group has no disclosure record. */
    public static final String DEFAULT_GROUP_ID = "DEFAULT";

    @EmbeddedId
    private DisclosureGroupId id;

    @Column(name = "int_rate", precision = 6, scale = 2, nullable = false)
    private BigDecimal interestRate;

    protected DisclosureGroup() {
    }

    public DisclosureGroup(DisclosureGroupId id, BigDecimal interestRate) {
        this.id = id;
        this.interestRate = interestRate;
    }

    public DisclosureGroupId getId() {
        return id;
    }

    public BigDecimal getInterestRate() {
        return interestRate;
    }

    public void setInterestRate(BigDecimal interestRate) {
        this.interestRate = interestRate;
    }
}
