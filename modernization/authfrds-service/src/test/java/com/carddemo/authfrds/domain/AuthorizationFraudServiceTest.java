package com.carddemo.authfrds.domain;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.authfrds.adapters.ims.InMemoryPendingAuthorizationStore;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.ClassPathResource;

class AuthorizationFraudServiceTest {

    final InMemoryFraudRecordStore db2 = new InMemoryFraudRecordStore();
    final InMemoryPendingAuthorizationStore ims = new InMemoryPendingAuthorizationStore(
            new ClassPathResource("ims-stub/pending-authorizations.json"), new ObjectMapper());
    final AuthorizationFraudService service = new AuthorizationFraudService(ims, new FraudRecordService(db2,
            Clock.fixed(Instant.parse("2026-10-29T12:00:00Z"), ZoneOffset.UTC)));
    final AuthKey first = ims.listDetails(1L).get(0).key();

    @Test
    @DisplayName("BR-AF-020 PF5 on an unflagged authorization reports fraud (F); PF5 again removes it (R)")
    void toggleTwice() {
        var r1 = service.toggleFraud(1L, null, first);
        assertThat(r1.authorization().authFraud()).isEqualTo("F");
        var r2 = service.toggleFraud(1L, null, first);
        assertThat(r2.authorization().authFraud()).isEqualTo("R");
        assertThat(r2.fraud().message()).isEqualTo("UPDT SUCCESS");
    }

    @Test
    @DisplayName("BR-AF-021 commarea carries acct id, cust id (CDEMO-CUST-ID or PA-CUST-ID) and the toggled flag")
    void commarea() {
        service.toggleFraud(1L, 42L, first);
        FraudRecord row = db2.rows.values().iterator().next();
        assertThat(row.acctId()).isEqualTo(1L);
        assertThat(row.custId()).isEqualTo(42L);
        assertThat(row.authFraud()).isEqualTo("F");
    }

    @Test
    @DisplayName("BR-AF-022 success -> REPL IMS detail with flag + MM/DD/YY report date, 'AUTH MARKED FRAUD...'")
    void replOnSuccess() {
        var r = service.toggleFraud(1L, null, first);
        assertThat(r.message()).isEqualTo("AUTH MARKED FRAUD...");
        assertThat(ims.findDetail(1L, first).orElseThrow().authFraud()).isEqualTo("F");
        assertThat(ims.findDetail(1L, first).orElseThrow().fraudRptDate()).isEqualTo("10/29/26");
    }

    @Test
    @DisplayName("BR-AF-023 COPAUS2C failure -> WS-FRD-ACT-MSG returned, IMS not updated (rollback)")
    void copaus2cFailure() {
        db2.failInsert = -911;
        db2.failInsertState = "40001";
        assertThatThrownBy(() -> service.toggleFraud(1L, null, first))
                .isInstanceOfSatisfying(LegacyRuleException.class, e -> {
                    assertThat(e.ruleId()).isEqualTo("BR-AF-023");
                    assertThat(e.getMessage()).isEqualTo(" SYSTEM ERROR DB2: CODE:-000911, STATE: +000040001");
                });
        assertThat(ims.findDetail(1L, first).orElseThrow().authFraud()).isEqualTo(" ");
    }

    @Test
    @DisplayName("BR-AF-024 IMS REPL failure -> ' System error while FRAUD Tagging, ROLLBACK||' + status")
    void replFailure() {
        ims.failNextReplace("AI");
        assertThatThrownBy(() -> service.toggleFraud(1L, null, first))
                .isInstanceOfSatisfying(LegacyRuleException.class, e -> {
                    assertThat(e.ruleId()).isEqualTo("BR-AF-024");
                    assertThat(e.getMessage()).isEqualTo(" System error while FRAUD Tagging, ROLLBACK||AI");
                });
    }

    @Test
    @DisplayName("BR-AF-025 READ-AUTH-RECORD: GU summary + GNP detail by key; GE -> not found")
    void read() {
        assertThat(service.view(1L, first).cardNum()).hasSize(16);
        assertThatThrownBy(() -> service.view(1L, new AuthKey(1, 1)))
                .isInstanceOfSatisfying(LegacyRuleException.class, e -> assertThat(e.ruleId()).isEqualTo("BR-AF-025"));
        assertThatThrownBy(() -> service.view(424242L, first))
                .isInstanceOfSatisfying(LegacyRuleException.class, e -> assertThat(e.ruleId()).isEqualTo("BR-AF-025"));
    }

    @Test
    @DisplayName("BR-AF-026 selected authorization key must be present and well formed")
    void keyRequired() {
        assertThatThrownBy(() -> AuthKey.parse(" "))
                .isInstanceOfSatisfying(LegacyRuleException.class, e -> assertThat(e.ruleId()).isEqualTo("BR-AF-026"));
        assertThat(AuthKey.parse("76699-998747444")).isEqualTo(new AuthKey(76699, 998747444));
    }

    @Test
    @DisplayName("BR-AF-027 PF8 returns the next PAUTDTL1; at the end 'Already at the last Authorization...'")
    void next() {
        var all = ims.listDetails(1L);
        assertThat(service.next(1L, all.get(0).key()).key()).isEqualTo(all.get(1).key());
        assertThatThrownBy(() -> service.next(1L, all.get(all.size() - 1).key()))
                .hasMessage("Already at the last Authorization...");
    }
}
