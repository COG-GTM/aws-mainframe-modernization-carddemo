package com.carddemo.transaction.domain;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.time.LocalDateTime;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class TransactionTimestampTest {

    private static final LocalDateTime T = LocalDateTime.of(2022, 7, 18, 10, 30, 45, 570_000_000);

    @Test
    void parsesBothLayouts() {
        assertThat(TransactionTimestamp.parse("2022-07-18 10:30:45.570000")).contains(T);
        assertThat(TransactionTimestamp.parse("2022-07-18-10.30.45.570000")).contains(T);
    }

    @ParameterizedTest
    @ValueSource(strings = {"                          ", "2022-02-30 10:30:45.570000", "2022-07-18T10:30:45.570000",
            "2022-07-18 10:30:45.57", "2022-07-18-10.30.45.570000 ", "NOT A TIMESTAMP AT ALL...."})
    void anythingElseHasNoValue(String text) {
        assertThat(TransactionTimestamp.parse(text)).isEmpty();
    }

    @Test
    void processingTextIsDb2WithHundredthsThenZeros() {
        assertThat(TransactionTimestamp.processingText(T.plusNanos(8_999_999))).isEqualTo("2022-07-18-10.30.45.570000");
    }

    @Test
    void entityKeepsTheTextAndParsesWhatItCan() {
        Transaction t = new Transaction("0000000000000001");
        t.setOrigTsText("2022-07-18-10.30.45.570000");
        t.setProcTsText("2022-07-18 10:30:45.570000");
        assertThat(t.getOrigTs()).isEqualTo(T);
        assertThat(t.getProcTs()).isEqualTo(T);
        t.setOrigTsText("garbage ts from a writer..");
        assertThat(t.getOrigTsText()).isEqualTo("garbage ts from a writer..");
        assertThat(t.getOrigTs()).isNull();
        assertThatThrownBy(() -> t.setProcTsText("short")).isInstanceOf(IllegalArgumentException.class);
    }
}
