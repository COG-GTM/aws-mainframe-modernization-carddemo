package com.carddemo.interest.rules;

import com.carddemo.interest.domain.DisclosureGroup;
import com.carddemo.interest.domain.DisclosureGroupKey;
import com.carddemo.interest.domain.TransactionCategory;
import com.carddemo.interest.exception.DisclosureGroupNotFoundException;
import com.carddemo.interest.repository.InMemoryDisclosureGroupRepository;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/** Business-rule tests for BR-3 (rate selection with DEFAULT fallback). */
class DisclosureGroupRateResolverTest {

    private static final TransactionCategory CATEGORY = TransactionCategory.of("01", 1);

    private final RateResolver resolver = new DisclosureGroupRateResolver(
            new InMemoryDisclosureGroupRepository(List.of(
                    new DisclosureGroup(DisclosureGroupKey.of("A000000001", CATEGORY), new BigDecimal("15.00")),
                    new DisclosureGroup(DisclosureGroupKey.of("DEFAULT", CATEGORY), new BigDecimal("22.50")))));

    @Test
    @DisplayName("An exact pricing-group match wins")
    void exactMatch() {
        assertEquals(new BigDecimal("15.00"), resolver.annualRatePercent("A000000001", CATEGORY));
    }

    @Test
    @DisplayName("An unknown or blank pricing group falls back to the DEFAULT group")
    void defaultFallback() {
        assertEquals(new BigDecimal("22.50"), resolver.annualRatePercent("A000000099", CATEGORY));
        assertEquals(new BigDecimal("22.50"), resolver.annualRatePercent("", CATEGORY));
    }

    @Test
    @DisplayName("A category with no DEFAULT row fails the run, as the COBOL abend does")
    void missingDefaultFails() {
        TransactionCategory unpriced = TransactionCategory.of("09", 9999);
        assertThrows(DisclosureGroupNotFoundException.class,
                () -> resolver.annualRatePercent("A000000001", unpriced));
    }
}
