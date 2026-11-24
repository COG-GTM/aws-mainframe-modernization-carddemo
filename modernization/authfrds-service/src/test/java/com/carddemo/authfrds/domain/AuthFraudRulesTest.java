package com.carddemo.authfrds.domain;

import static org.assertj.core.api.Assertions.assertThat;

import java.time.LocalDate;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class AuthFraudRulesTest {

    @Test
    @DisplayName("BR-AF-001 AUTH-TS = YY-MM-DD from ORIG-DATE + (999999999 - TIME-9C) as HH.MI.SSsss + '000', X(26)")
    void authTs() {
        String ts = AuthFraudRules.authTsHostVar("231027", 998747444L);
        assertThat(ts).isEqualTo("23-10-27 00.12.52555000   ").hasSize(26);
        assertThat(AuthFraudRules.authTsHostVar("261029", 849999877L).trim()).isEqualTo("26-10-29 15.00.00122000");
    }

    @Test
    @DisplayName("BR-AF-002 MERCHANT-NAME-LEN = LENGTH OF PA-MERCHANT-NAME (22), flag = WS-FRD-ACTION, ids from commarea")
    void mapping() {
        FraudRow r = AuthFraudRules.toRow(new FraudRequest(7L, 9L, "F", Fixtures.detail("4111222233334444", " ")));
        assertThat(r.merchantName()).hasSize(22).startsWith("Demo Grocery");
        assertThat(r.authFraud()).isEqualTo("F");
        assertThat(r.acctId()).isEqualTo(7L);
        assertThat(r.custId()).isEqualTo(9L);
        assertThat(r.posEntryMode()).isEqualTo(5);
    }

    @Test
    @DisplayName("BR-AF-007 UPDT ERROR text uses +9(06) SQLCODE and +9(09) SQLSTATE")
    void updtError() {
        assertThat(AuthFraudRules.updateErrorMessage(100, "02000"))
                .isEqualTo(" UPDT ERROR DB2: CODE:+000100, STATE: +000002000");
    }

    @Test
    @DisplayName("BR-AF-008 SYSTEM ERROR text uses +9(06) SQLCODE and +9(09) SQLSTATE")
    void systemError() {
        assertThat(AuthFraudRules.systemErrorMessage(-20448, "22007"))
                .isEqualTo(" SYSTEM ERROR DB2: CODE:-020448, STATE: +000022007");
        assertThat(AuthFraudRules.sqlCodeEdited(-803)).isEqualTo("-000803");
    }

    @Test
    @DisplayName("BR-AF-009 WS-FRD-ACT-MSG is X(50): the SYSTEM ERROR text fills it exactly, longer text is cut")
    void x50() {
        assertThat(AuthFraudRules.systemErrorMessage(-911, "40001")).hasSize(50);
        assertThat(AuthFraudRules.actMsg("x".repeat(60))).hasSize(50);
    }

    @Test
    @DisplayName("BR-AF-010 PA-FRAUD-RPT-DATE = FORMATTIME MMDDYY DATESEP -> MM/DD/YY")
    void rptDate() {
        assertThat(AuthFraudRules.fraudRptDate(LocalDate.of(2026, 10, 29))).isEqualTo("10/29/26");
    }

    @Test
    @DisplayName("BR-AF-020 toggle: F -> R (remove), anything else -> F (report)")
    void toggle() {
        assertThat(AuthFraudRules.toggle("F")).isEqualTo(FraudAction.REMOVE);
        assertThat(AuthFraudRules.toggle("R")).isEqualTo(FraudAction.REPORT);
        assertThat(AuthFraudRules.toggle(" ")).isEqualTo(FraudAction.REPORT);
    }

    @Test
    @DisplayName("BR-AF-022 messages: AUTH MARKED FRAUD... / AUTH FRAUD REMOVED...")
    void toggleMsg() {
        assertThat(AuthFraudRules.toggleMessage(FraudAction.REPORT)).isEqualTo("AUTH MARKED FRAUD...");
        assertThat(AuthFraudRules.toggleMessage(FraudAction.REMOVE)).isEqualTo("AUTH FRAUD REMOVED...");
    }

    @Test
    @DisplayName("BR-AF-024 ROLLBACK message = literal || IMS-RETURN-CODE")
    void rollbackMsg() {
        assertThat(AuthFraudRules.fraudTaggingRollback("AI")).isEqualTo(" System error while FRAUD Tagging, ROLLBACK||AI");
    }
}
