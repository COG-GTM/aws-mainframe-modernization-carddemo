package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import java.math.BigDecimal;

/** DISCGRP (copybook CVTRA02Y), VSAM KSDS keyed by DIS-GROUP-KEY (account group, type, category). */
@Entity
@Table(name = "disclosure_group")
public class DisclosureGroup {

    @EmbeddedId
    private DisclosureGroupId id;

    @Column(name = "int_rate", nullable = false, precision = 6, scale = 2)
    private BigDecimal intRate;

    public DisclosureGroupId getId() {
        return id;
    }

    public void setId(DisclosureGroupId id) {
        this.id = id;
    }

    public BigDecimal getIntRate() {
        return intRate;
    }

    public void setIntRate(BigDecimal intRate) {
        this.intRate = intRate;
    }

    @Override
    public boolean equals(Object o) {
        return this == o || (o instanceof DisclosureGroup other && id != null && id.equals(other.getId()));
    }

    @Override
    public int hashCode() {
        return DisclosureGroup.class.hashCode();
    }
}
