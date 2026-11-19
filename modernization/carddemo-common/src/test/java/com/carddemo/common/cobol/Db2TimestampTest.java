package com.carddemo.common.cobol;

import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;

import static org.assertj.core.api.Assertions.assertThat;

class Db2TimestampTest {

    @Test
    void formatIsYyyyMmDdHhMmSsHundredthsAndFourZeros() {
        assertThat(Db2Timestamp.format(LocalDateTime.of(2022, 7, 18, 10, 30, 45, 987_654_321)))
                .isEqualTo("2022-07-18-10.30.45.980000").hasSize(26);
        assertThat(Db2Timestamp.format(LocalDateTime.of(2022, 7, 18, 10, 30, 45))).isEqualTo("2022-07-18-10.30.45.000000");
        assertThat(Db2Timestamp.format(LocalDateTime.of(2022, 1, 2, 3, 4, 5, 50_000_000))).isEqualTo("2022-01-02-03.04.05.050000");
    }
}
