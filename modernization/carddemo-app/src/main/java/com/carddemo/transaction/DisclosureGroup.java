package com.carddemo.transaction;

import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import java.math.BigDecimal;
import org.hibernate.annotations.Immutable;

/**
 * JPA entity for table {@code disclosure_group}. Disclosure group (interest rate) record (DISCGRP KSDS, key DIS-
 * GROUP-KEY); read by CBACT04C, falling back to group {@code DEFAULT}. Field Javadocs carry the CVTRA02Y item
 * names; {@link DisclosureGroupRecord} is the fixed-width value.
 */
@Entity
@Immutable
@Table(name = "disclosure_group")
public class DisclosureGroup {

    @EmbeddedId
    private DisclosureGroupId id;

    /** DIS-INT-RATE PIC S9(04)V99. */
    @Column(name = "int_rate", precision = 6, scale = 2)
    private BigDecimal intRate;

    protected DisclosureGroup() {
    }

    public static DisclosureGroup from(DisclosureGroupRecord record) {
        DisclosureGroup entity = new DisclosureGroup();
        entity.id = new DisclosureGroupId(record.acctGroupId(), record.tranTypeCd(), record.tranCatCd());
        entity.intRate = record.intRate();
        return entity;
    }

    public DisclosureGroupRecord toRecord() {
        return new DisclosureGroupRecord(id.acctGroupId(), id.tranTypeCd(), id.tranCatCd(), intRate);
    }

    public DisclosureGroupId getId() {
        return id;
    }

    public BigDecimal getIntRate() {
        return intRate;
    }
}
