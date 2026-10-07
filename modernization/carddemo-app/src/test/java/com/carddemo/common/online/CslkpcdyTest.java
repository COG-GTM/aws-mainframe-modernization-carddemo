package com.carddemo.common.online;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

class CslkpcdyTest {

    @Test
    void readsEveryConditionOfTheCopybook() {
        assertThat(Cslkpcdy.conditions()).containsOnlyKeys("VALID-PHONE-AREA-CODE", "VALID-GENERAL-PURP-CODE",
                "VALID-EASY-RECOG-AREA-CODE", "VALID-US-STATE-CODE", "VALID-US-STATE-ZIP-CD2-COMBO");
        assertThat(Cslkpcdy.conditions().get("VALID-PHONE-AREA-CODE")).hasSize(490);
        assertThat(Cslkpcdy.GENERAL_PURPOSE_AREA_CODES).hasSize(410);
        assertThat(Cslkpcdy.conditions().get("VALID-EASY-RECOG-AREA-CODE")).hasSize(80);
        assertThat(Cslkpcdy.US_STATE_CODES).hasSize(56);
        assertThat(Cslkpcdy.US_STATE_ZIP2_COMBOS).hasSize(240);
    }

    @Test
    void generalPurposeAreaCodesExcludeEasilyRecognisableCodes() {
        assertThat(Cslkpcdy.isGeneralPurposeAreaCode("908")).isTrue();
        assertThat(Cslkpcdy.isGeneralPurposeAreaCode("212")).isTrue();
        assertThat(Cslkpcdy.isGeneralPurposeAreaCode("800")).isFalse();
        assertThat(Cslkpcdy.isGeneralPurposeAreaCode("555")).isFalse();
        assertThat(Cslkpcdy.isGeneralPurposeAreaCode("373")).isFalse();
        assertThat(Cslkpcdy.GENERAL_PURPOSE_AREA_CODES)
                .doesNotContainAnyElementsOf(Cslkpcdy.conditions().get("VALID-EASY-RECOG-AREA-CODE"));
    }

    @Test
    void stateAndZipPrefixLookups() {
        assertThat(Cslkpcdy.isUsStateCode("NC")).isTrue();
        assertThat(Cslkpcdy.isUsStateCode("PR")).isTrue();
        assertThat(Cslkpcdy.isUsStateCode("XX")).isFalse();
        assertThat(Cslkpcdy.isUsStateCode("nc")).isFalse();
        assertThat(Cslkpcdy.isUsStateZip2Combo("NC27")).isTrue();
        assertThat(Cslkpcdy.isUsStateZip2Combo("NC12")).isFalse();
        assertThat(Cslkpcdy.isUsStateZip2Combo("NY12")).isTrue();
    }
}
