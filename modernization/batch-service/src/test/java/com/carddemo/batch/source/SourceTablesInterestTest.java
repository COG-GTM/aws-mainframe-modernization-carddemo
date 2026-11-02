package com.carddemo.batch.source;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

class SourceTablesInterestTest {

    @Test
    void onlyTheCbact04cRecordShapeCountsAsInterest() {
        assertThat(SourceTables.interestTransaction("01", 5, "System", "Int. for a/c 00000000001")).isTrue();
        assertThat(SourceTables.interestTransaction("01", 1, "System", "Purchase at shop")).isFalse();
        assertThat(SourceTables.interestTransaction("01", 5, "POS TERM", "Int. for a/c 00000000001")).isFalse();
        assertThat(SourceTables.interestTransaction("02", 5, "System", "Int. for a/c 00000000001")).isFalse();
    }
}
