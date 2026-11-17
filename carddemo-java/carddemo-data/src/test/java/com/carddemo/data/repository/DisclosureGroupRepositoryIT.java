package com.carddemo.data.repository;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.data.entity.DisclosureGroup;
import com.carddemo.data.entity.DisclosureGroupId;
import java.math.BigDecimal;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

class DisclosureGroupRepositoryIT extends RepositoryIT {

    @Autowired
    DisclosureGroupRepository groups;

    @Test
    void readsInterestRateByGroupTypeAndCategory() {
        var group = new DisclosureGroup();
        group.setId(new DisclosureGroupId("A000000000", "01", (short) 1));
        group.setIntRate(new BigDecimal("15.00"));
        groups.save(group);
        var fallback = new DisclosureGroup();
        fallback.setId(new DisclosureGroupId("DEFAULT", "01", (short) 1));
        fallback.setIntRate(new BigDecimal("-99.99"));
        groups.save(fallback);
        flushAndClear();

        assertThat(groups.findById(new DisclosureGroupId("A000000000", "01", (short) 1))).get()
                .extracting(DisclosureGroup::getIntRate).isEqualTo(new BigDecimal("15.00"));
        assertThat(groups.findById(new DisclosureGroupId("DEFAULT", "01", (short) 1))).get()
                .extracting(DisclosureGroup::getIntRate).isEqualTo(new BigDecimal("-99.99"));
        assertThat(groups.findById(new DisclosureGroupId("A000000000", "01", (short) 2))).isEmpty();
    }
}
