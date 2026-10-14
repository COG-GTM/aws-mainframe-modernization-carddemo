package com.carddemo.runtime.cics;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

class PfKeysTest {

    @Test
    void mapsAndFoldsLikeCsstrpfy() {
        assertThat(PfKeys.storePfKey(AidKey.ENTER, "     ")).isEqualTo("ENTER");
        assertThat(PfKeys.storePfKey(AidKey.CLEAR, "     ")).isEqualTo("CLEAR");
        assertThat(PfKeys.storePfKey(AidKey.PA1, "     ")).isEqualTo("PA1  ");
        assertThat(PfKeys.storePfKey(AidKey.PA2, "     ")).isEqualTo("PA2  ");
        assertThat(PfKeys.storePfKey(AidKey.PF1, "     ")).isEqualTo("PFK01");
        assertThat(PfKeys.storePfKey(AidKey.PF12, "     ")).isEqualTo("PFK12");
        assertThat(PfKeys.storePfKey(AidKey.PF13, "     ")).isEqualTo("PFK01");
        assertThat(PfKeys.storePfKey(AidKey.PF15, "     ")).isEqualTo("PFK03");
        assertThat(PfKeys.storePfKey(AidKey.PF24, "     ")).isEqualTo("PFK12");
        // PA3 is not in the EVALUATE: CCARD-AID unchanged
        assertThat(PfKeys.storePfKey(AidKey.PA3, "     ")).isEqualTo("     ");
    }
}
