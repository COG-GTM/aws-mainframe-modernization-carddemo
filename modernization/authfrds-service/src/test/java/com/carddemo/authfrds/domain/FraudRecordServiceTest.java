package com.carddemo.authfrds.domain;

import static org.assertj.core.api.Assertions.assertThat;

import java.time.Clock;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class FraudRecordServiceTest {

    final InMemoryFraudRecordStore store = new InMemoryFraudRecordStore();
    final FraudRecordService service = new FraudRecordService(store,
            Clock.fixed(Instant.parse("2026-10-29T12:00:00Z"), ZoneOffset.UTC));
    static final String CARD = "4111222233334444";

    @Test
    @DisplayName("BR-AF-004 first INSERT -> status S, 'ADD SUCCESS'")
    void add() {
        FraudResult r = service.record(Fixtures.request(CARD, "F"));
        assertThat(r.updateStatus()).isEqualTo("S");
        assertThat(r.message()).isEqualTo("ADD SUCCESS");
        assertThat(r.record().authFraud()).isEqualTo("F");
    }

    @Test
    @DisplayName("BR-AF-003 FRAUD_RPT_DATE is CURRENT DATE on insert and on update")
    void currentDate() {
        service.record(Fixtures.request(CARD, "F"));
        store.currentDate = LocalDate.of(2026, 11, 2);
        FraudResult r = service.record(Fixtures.request(CARD, "R"));
        assertThat(r.record().fraudRptDate()).isEqualTo(LocalDate.of(2026, 11, 2));
    }

    @Test
    @DisplayName("BR-AF-005 SQLCODE -803 on INSERT -> FRAUD-UPDATE changes only AUTH_FRAUD / FRAUD_RPT_DATE")
    void duplicateUpdates() {
        service.record(Fixtures.request(CARD, "F"));
        AuthorizationDetail changed = Fixtures.detail(CARD, " ").withFraud(" ", " ");
        FraudRequest second = new FraudRequest(99L, 99L, "R", new AuthorizationDetail(changed.authDate9c(),
                changed.authTime9c(), changed.authOrigDate(), changed.authOrigTime(), CARD, "9999", "0000", "X", "X",
                "X", "05", "9999", "999999", java.math.BigDecimal.ONE, java.math.BigDecimal.ONE, "0000", "CAN", "01",
                "OTHER", "Other merchant", "Toronto", "ON", "M5V", "OTHER", "M", " ", " "));
        FraudResult r = service.record(second);
        assertThat(r.operation()).isEqualTo("UPDT");
        assertThat(r.record().authFraud()).isEqualTo("R");
        assertThat(r.record().merchantName().trim()).isEqualTo("Demo Grocery");
        assertThat(r.record().acctId()).isEqualTo(1L);
    }

    @Test
    @DisplayName("BR-AF-006 UPDATE SQLCODE 0 -> status S, 'UPDT SUCCESS'")
    void updt() {
        service.record(Fixtures.request(CARD, "F"));
        FraudResult r = service.record(Fixtures.request(CARD, "R"));
        assertThat(r.updateStatus()).isEqualTo("S");
        assertThat(r.message()).isEqualTo("UPDT SUCCESS");
    }

    @Test
    @DisplayName("BR-AF-007 UPDATE failure (SQLCODE +100) -> status F, ' UPDT ERROR DB2: CODE:+000100, ...'")
    void updtError() {
        service.record(Fixtures.request(CARD, "F"));
        store.failUpdate = 100;
        FraudResult r = service.record(Fixtures.request(CARD, "R"));
        assertThat(r.updateStatus()).isEqualTo("F");
        assertThat(r.message()).isEqualTo(" UPDT ERROR DB2: CODE:+000100, STATE: +000002000");
    }

    @Test
    @DisplayName("BR-AF-008 INSERT failure other than -803 -> status F, ' SYSTEM ERROR DB2: ...'")
    void sysError() {
        store.failInsert = -911;
        store.failInsertState = "40001";
        FraudResult r = service.record(Fixtures.request(CARD, "F"));
        assertThat(r.updateStatus()).isEqualTo("F");
        assertThat(r.sqlCode()).isEqualTo(-911);
        assertThat(r.message()).isEqualTo(" SYSTEM ERROR DB2: CODE:-000911, STATE: +000040001");
        assertThat(store.rows).isEmpty();
    }

    @Test
    @DisplayName("BR-AF-010 returned commarea PA-FRAUD-RPT-DATE = MM/DD/YY from the CICS clock")
    void rptDate() {
        assertThat(service.record(Fixtures.request(CARD, "F")).fraudRptDate()).isEqualTo("10/29/26");
    }
}
