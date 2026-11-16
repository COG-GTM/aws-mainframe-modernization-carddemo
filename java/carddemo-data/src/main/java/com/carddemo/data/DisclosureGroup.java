package com.carddemo.data;

import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import java.math.BigDecimal;
import java.util.Objects;

/** Interest rate of an account group per transaction category, DISCGRP (CVTRA02Y). */
@Entity
@Table(name = "disclosure_groups")
public class DisclosureGroup {

    @EmbeddedId
    private DisclosureGroupId id;

    @Column(name = "int_rate", nullable = false, precision = 6, scale = 2)
    private BigDecimal intRate;

    public DisclosureGroup() {
    }

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
    public boolean equals(Object other) {
        return other instanceof DisclosureGroup that && id != null && id.equals(that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }

    @Override
    public String toString() {
        return "DisclosureGroup[id=" + id + "]";
    }
}
