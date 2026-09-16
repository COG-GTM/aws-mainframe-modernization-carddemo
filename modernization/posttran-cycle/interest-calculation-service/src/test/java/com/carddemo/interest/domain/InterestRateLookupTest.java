package com.carddemo.interest.domain;

import com.carddemo.recordio.codec.RecordEncoding;
import com.carddemo.recordio.layout.DisclosureGroup;
import com.carddemo.recordio.layout.DisclosureGroupLayout;
import com.carddemo.recordio.store.KeyedRecordStore;
import com.carddemo.recordio.store.RecordNotFoundException;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/** CBACT04C 1200-GET-INTEREST-RATE (lines 356-385): account group first, then 'DEFAULT' on status 23. */
class InterestRateLookupTest {

    private final InterestRateLookup lookup = new InterestRateLookup(KeyedRecordStore.of("DISCGRP", List.of(
            new DisclosureGroup("GOLD", "01", 1, new BigDecimal("9.50")),
            new DisclosureGroup("DEFAULT", "01", 1, new BigDecimal("19.99")),
            new DisclosureGroup("DEFAULT", "01", 2, new BigDecimal("0.00"))),
            DisclosureGroupLayout.INSTANCE, RecordEncoding.EBCDIC, DisclosureGroup::key));

    @Test
    void accountGroupRateWins() {
        assertThat(lookup.rateFor("GOLD", "01", 1)).isEqualByComparingTo("9.50");
    }

    @Test
    void missingGroupFallsBackToDefault() {
        assertThat(lookup.rateFor("SILVER", "01", 1)).isEqualByComparingTo("19.99");
    }

    @Test
    void blankGroupIdAsShippedInAcctdataFallsBackToDefault() {
        assertThat(lookup.rateFor("", "01", 1)).isEqualByComparingTo("19.99");
    }

    @Test
    void missingDefaultRowIsFatalLikeTheCobolAbend() {
        assertThatThrownBy(() -> lookup.rateFor("GOLD", "02", 9))
                .isInstanceOf(RecordNotFoundException.class)
                .hasMessageContaining("DEFAULT");
    }
}
