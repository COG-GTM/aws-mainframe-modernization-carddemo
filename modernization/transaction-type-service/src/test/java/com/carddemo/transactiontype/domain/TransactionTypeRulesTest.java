package com.carddemo.transactiontype.domain;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class TransactionTypeRulesTest {

    private static void assertRule(Runnable r, String ruleId) {
        assertThatThrownBy(r::run).isInstanceOf(LegacyRuleException.class)
                .extracting(e -> ((LegacyRuleException) e).ruleId()).isEqualTo(ruleId);
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "  "})
    @DisplayName("BR-TT-001 type code is required")
    void br_tt_001_typeCodeRequired(String raw) {
        assertRule(() -> TransactionTypeRules.normalizeTypeCode(raw), "BR-TT-001");
        assertRule(() -> TransactionTypeRules.normalizeTypeCode(null), "BR-TT-001");
    }

    @ParameterizedTest
    @ValueSource(strings = {"AB", "1A", "123", "-1", "1.5"})
    @DisplayName("BR-TT-002 type code must be numeric (max 2 digits)")
    void br_tt_002_typeCodeNumeric(String raw) {
        assertRule(() -> TransactionTypeRules.normalizeTypeCode(raw), "BR-TT-002");
    }

    @ParameterizedTest
    @ValueSource(strings = {"0", "00"})
    @DisplayName("BR-TT-003 type code must not be zero")
    void br_tt_003_typeCodeNotZero(String raw) {
        assertRule(() -> TransactionTypeRules.normalizeTypeCode(raw), "BR-TT-003");
    }

    @Test
    @DisplayName("BR-TT-004 single digit type code is left-padded with zero")
    void br_tt_004_typeCodePadded() {
        assertThat(TransactionTypeRules.normalizeTypeCode("7")).isEqualTo("07");
        assertThat(TransactionTypeRules.normalizeTypeCode(" 12 ")).isEqualTo("12");
    }

    @Test
    @DisplayName("BR-TT-005 description is required")
    void br_tt_005_descriptionRequired() {
        assertRule(() -> TransactionTypeRules.validateDescription("   "), "BR-TT-005");
    }

    @ParameterizedTest
    @ValueSource(strings = {"CASH-BACK", "PAY@POS", "REFUND!", "CAFÉ"})
    @DisplayName("BR-TT-006 description allows letters, digits and spaces only")
    void br_tt_006_descriptionAlphanumeric(String raw) {
        assertRule(() -> TransactionTypeRules.validateDescription(raw), "BR-TT-006");
        assertThat(TransactionTypeRules.validateDescription("Cash Back 2")).isEqualTo("Cash Back 2");
    }

    @Test
    @DisplayName("BR-TT-007 description max 50 characters, trimmed before storing")
    void br_tt_007_descriptionLength() {
        assertRule(() -> TransactionTypeRules.validateDescription("A".repeat(51)), "BR-TT-007");
        assertThat(TransactionTypeRules.validateDescription("  " + "A".repeat(50) + "  ")).hasSize(50);
    }

    @Test
    @DisplayName("BR-TT-008 change detection is case-insensitive and trims")
    void br_tt_008_noChange() {
        assertThat(TransactionTypeRules.isUnchanged("PURCHASE", " purchase ")).isTrue();
        assertThat(TransactionTypeRules.isUnchanged("PURCHASE", "PURCHASES")).isFalse();
    }

    @Test
    @DisplayName("BR-TT-011 type code filter: blank/00 means none, else exactly 2 digits")
    void br_tt_011_typeFilter() {
        assertThat(TransactionTypeRules.parseTypeCodeFilter(null)).isNull();
        assertThat(TransactionTypeRules.parseTypeCodeFilter("00")).isNull();
        assertThat(TransactionTypeRules.parseTypeCodeFilter("05")).isEqualTo("05");
        assertRule(() -> TransactionTypeRules.parseTypeCodeFilter("5"), "BR-TT-011");
        assertRule(() -> TransactionTypeRules.parseTypeCodeFilter("AB"), "BR-TT-011");
    }

    @Test
    @DisplayName("BR-TT-012 description filter is LIKE %trim(input)%")
    void br_tt_012_descriptionFilter() {
        assertThat(TransactionTypeRules.descriptionFilterPattern(" PAY ")).isEqualTo("%PAY%");
        assertThat(TransactionTypeRules.descriptionFilterPattern("")).isNull();
    }
}
