package com.carddemo.programs.copaus2c;

import com.carddemo.programs.copaus2c.copybook.FraudCommarea;
import com.carddemo.programs.copaus2c.copybook.FraudCommarea.FraudStatusRecord;
import com.carddemo.programs.copaus2c.copybook.PendingAuthDetails;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;

final class TestData {

    static final Clock CLOCK = Clock.fixed(Instant.parse("2026-11-26T06:22:00Z"), ZoneOffset.UTC);

    private TestData() {
    }

    static PendingAuthDetails auth(long authTime9C, String origDate) {
        return new PendingAuthDetails(
                76699, authTime9C, origDate, "041252", "4154603697010011", "0100", "1122", "1234",
                "102030", "041252", "00", "0000", 5000, new BigDecimal("1234567890.12"), new BigDecimal("-7.50"),
                "5442", "USA", 1, "235010006754230", "Amazon.com", "Wilmington", "DE", "19801",
                "960b9b5480d045a", "P", " ", "", "");
    }

    static FraudCommarea commarea(String action, PendingAuthDetails auth, String initialMsg) {
        return new FraudCommarea(41_000_000_123L, 123_456_789L, auth,
                new FraudStatusRecord(action, " ", initialMsg));
    }
}
