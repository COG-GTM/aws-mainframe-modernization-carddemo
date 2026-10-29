package com.carddemo.authfrds.adapters.db2;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.authfrds.adapters.ims.InMemoryPendingAuthorizationStore;
import com.carddemo.authfrds.domain.Fixtures;
import com.carddemo.authfrds.domain.FraudRow;
import com.carddemo.authfrds.domain.AuthFraudRules;
import com.carddemo.authfrds.domain.SqlCodeException;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.jdbc.core.JdbcTemplate;

/**
 * Runs against the real IBM Db2 from docker-compose (make db2-up). Profile: mvn -Pdb2-it verify.
 * AUTHFRDS has no legacy load cards, so every test owns the rows it writes (cards 4111222233339xxx + IMS acct 1).
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class AuthFrdsDb2IT {

    @Autowired
    TestRestTemplate http;
    @Autowired
    JdbcTemplate jdbc;
    @Autowired
    JdbcFraudRecordStore store;
    @Autowired
    InMemoryPendingAuthorizationStore ims;

    static final String CARD = "4111222233339001";
    String acct1Card;

    @BeforeEach
    @AfterEach
    void cleanUp() {
        ims.reset();
        acct1Card = ims.listDetails(1L).get(0).cardNum();
        jdbc.update("DELETE FROM CARDDEMO.AUTHFRDS WHERE CARD_NUM LIKE '411122223333%' OR CARD_NUM = ?", acct1Card);
    }

    private ResponseEntity<Map> post(String card, String action) {
        return http.postForEntity("/api/v1/fraud-records", Fixtures.request(card, action), Map.class);
    }

    private Map<String, Object> row(String card) {
        List<Map<String, Object>> r = jdbc.queryForList("SELECT AUTH_FRAUD, FRAUD_RPT_DATE, CURRENT DATE AS TODAY, "
                + "MERCHANT_NAME, LENGTH(MERCHANT_NAME) AS MLEN, ACCT_ID, CHAR(AUTH_TS) AS TS "
                + "FROM CARDDEMO.AUTHFRDS WHERE CARD_NUM = ?", card);
        return r.isEmpty() ? null : r.get(0);
    }

    @Test
    @DisplayName("BR-AF-004/001 POST inserts a real row; AUTH_TS from TIMESTAMP_FORMAT('26-10-29 15.00.00122000')")
    void insert() {
        ResponseEntity<Map> r = post(CARD, "F");
        assertThat(r.getStatusCode()).isEqualTo(HttpStatus.CREATED);
        assertThat(r.getBody().get("message")).isEqualTo("ADD SUCCESS");
        assertThat(row(CARD).get("TS").toString()).isEqualTo("2026-10-29-15.00.00.122000");
        assertThat(row(CARD).get("AUTH_FRAUD")).isEqualTo("F");
    }

    @Test
    @DisplayName("BR-AF-003/002 FRAUD_RPT_DATE = Db2 CURRENT DATE; MERCHANT_NAME stored with length 22")
    void currentDateAndVarchar() {
        post(CARD, "F");
        Map<String, Object> row = row(CARD);
        assertThat(row.get("FRAUD_RPT_DATE")).isEqualTo(row.get("TODAY"));
        assertThat(((Number) row.get("MLEN")).intValue()).isEqualTo(22);
    }

    @Test
    @DisplayName("BR-AF-005/006 real SQLCODE -803 on second INSERT -> FRAUD-UPDATE, 200 UPDT SUCCESS, one row")
    void duplicateBecomesUpdate() {
        post(CARD, "F");
        FraudRow again = AuthFraudRules.toRow(Fixtures.request(CARD, "R"));
        try {
            store.insert(again);
            throw new AssertionError("expected -803");
        } catch (SqlCodeException e) {
            assertThat(e.sqlCode()).isEqualTo(-803);
        }
        ResponseEntity<Map> r = post(CARD, "R");
        assertThat(r.getStatusCode()).isEqualTo(HttpStatus.OK);
        assertThat(r.getBody().get("message")).isEqualTo("UPDT SUCCESS");
        assertThat(row(CARD).get("AUTH_FRAUD")).isEqualTo("R");
        assertThat(jdbc.queryForObject("SELECT COUNT(*) FROM CARDDEMO.AUTHFRDS WHERE CARD_NUM = ?", Integer.class, CARD))
                .isEqualTo(1);
    }

    @Test
    @DisplayName("BR-AF-008 real Db2 error (month 13 -> SQLCODE -20448) -> 422 ' SYSTEM ERROR DB2: CODE:-020448, ...'")
    void realSqlError() {
        var req = Fixtures.request(CARD, "F");
        var bad = new com.carddemo.authfrds.domain.FraudRequest(1L, 1L, "F", new com.carddemo.authfrds.domain.AuthorizationDetail(
                req.authorization().authDate9c(), req.authorization().authTime9c(), "261329", "150000", CARD, "0100",
                "1128", "0100", "POS", "A1B2C3", "00", "0000", "000000", java.math.BigDecimal.TEN,
                java.math.BigDecimal.TEN, "5411", "USA", "05", "M", "Demo", "Seattle", "WA", "98101", "T", "P", " ", " "));
        ResponseEntity<Map> r = http.postForEntity("/api/v1/fraud-records", bad, Map.class);
        assertThat(r.getStatusCode()).isEqualTo(HttpStatus.UNPROCESSABLE_ENTITY);
        assertThat(r.getBody().get("message")).isEqualTo(" SYSTEM ERROR DB2: CODE:-020448, STATE: +000022007");
        assertThat(row(CARD)).isNull();
    }

    @Test
    @DisplayName("BR-AF-007 real searched UPDATE with no row -> SQLCODE +100")
    void updateNotFound() {
        try {
            store.updateFraudFlag(CARD, AuthFraudRules.authTsHostVar("261029", 849999877L), "F");
            throw new AssertionError("expected +100");
        } catch (SqlCodeException e) {
            assertThat(e.sqlCode()).isEqualTo(100);
        }
        assertThat(http.getForEntity("/api/v1/fraud-records/" + CARD + "/2026-10-29T15:00:00.122", Map.class)
                .getStatusCode()).isEqualTo(HttpStatus.NOT_FOUND);
    }

    @Test
    @DisplayName("BR-AF-030 paging + filter on real Db2 in XAUTHFRD order")
    void paging() {
        for (int i = 0; i < 7; i++) {
            post("411122223333" + (9100 + i), i % 2 == 0 ? "F" : "R");
        }
        Map<?, ?> p0 = http.getForObject("/api/v1/fraud-records?page=0", Map.class);
        assertThat((List<?>) p0.get("items")).hasSize(5);
        assertThat(((Map<?, ?>) ((List<?>) p0.get("items")).get(0)).get("cardNum")).isEqualTo("4111222233339100");
        Map<?, ?> p1 = http.getForObject("/api/v1/fraud-records?page=1", Map.class);
        assertThat((List<?>) p1.get("items")).hasSize(2);
        assertThat(http.getForObject("/api/v1/fraud-records?authFraud=R", Map.class).get("total")).isEqualTo(3);
    }

    @Test
    @DisplayName("BR-AF-020/022 PF5 toggle on IMS stub acct 1 writes F then R to real Db2")
    void toggle() {
        String key = ims.listDetails(1L).get(0).key().toString();
        Map<?, ?> r1 = http.postForObject("/api/v1/accounts/1/authorizations/" + key + "/fraud", null, Map.class);
        assertThat(r1.get("message")).isEqualTo("AUTH MARKED FRAUD...");
        assertThat(row(acct1Card).get("AUTH_FRAUD")).isEqualTo("F");
        Map<?, ?> r2 = http.postForObject("/api/v1/accounts/1/authorizations/" + key + "/fraud", null, Map.class);
        assertThat(r2.get("message")).isEqualTo("AUTH FRAUD REMOVED...");
        assertThat(row(acct1Card).get("AUTH_FRAUD")).isEqualTo("R");
    }

    @Test
    @DisplayName("BR-AF-024/023 IMS REPL failure rolls back the real Db2 INSERT (SYNCPOINT ROLLBACK)")
    void rollback() {
        String key = ims.listDetails(1L).get(0).key().toString();
        http.postForObject("/api/v1/ims-stub/fail-next-repl?status=AI", null, Map.class);
        ResponseEntity<Map> r = http.postForEntity("/api/v1/accounts/1/authorizations/" + key + "/fraud", null, Map.class);
        assertThat(r.getStatusCode()).isEqualTo(HttpStatus.INTERNAL_SERVER_ERROR);
        assertThat(r.getBody().get("message")).isEqualTo(" System error while FRAUD Tagging, ROLLBACK||AI");
        assertThat(row(acct1Card)).isNull();
    }
}
