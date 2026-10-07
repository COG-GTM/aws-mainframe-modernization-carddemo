package com.carddemo.common;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class PanMaskPathTest {

    @ParameterizedTest
    @CsvSource({
        "/api/v1/cards/4000000010000030,          /api/v1/cards/************0030",
        "/api/v1/cards/4000000010000030/x,        /api/v1/cards/************0030/x",
        "/api/v1/cards/l8aj7oPKCXMP4WENw1evDcbu,  /api/v1/cards/l8aj7oPKCXMP4WENw1evDcbu",
        "/api/v1/cards/by-account/00000000001,    /api/v1/cards/by-account/00000000001",
        "/api/v1/transactions/0000000000000001,   /api/v1/transactions/0000000000000001",
        "/api/v1/cards/123,                       /api/v1/cards/123"
    })
    void masksOnlyACardNumberInTheCardPath(String path, String expected) {
        assertThat(PanMask.maskCardPath(path)).isEqualTo(expected);
    }
}
