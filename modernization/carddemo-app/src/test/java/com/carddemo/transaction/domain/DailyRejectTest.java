package com.carddemo.transaction.domain;

import com.carddemo.transaction.service.DailyTransactions;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class DailyRejectTest {

    @Test
    void rejectCopiesTheDalytranBytesSoANegativeZeroAmountKeepsItsSign() {
        DailyTransaction t = DailyTransactions.withRawAmount(DailyTransactions.of("T1", "4444333322221111", "0.00"), "0000000000}");
        String record = new DailyReject(t, 100, "INVALID CARD NUMBER FOUND").toRecord();
        assertThat(record).hasSize(430).startsWith(t.record());
        assertThat(record.substring(132, 143)).isEqualTo("0000000000}");
        assertThat(record.substring(350, 354)).isEqualTo("0100");
    }
}
