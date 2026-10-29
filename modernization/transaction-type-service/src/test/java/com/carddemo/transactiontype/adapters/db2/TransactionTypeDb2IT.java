package com.carddemo.transactiontype.adapters.db2;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.jdbc.core.JdbcTemplate;

/**
 * Runs against the real IBM Db2 started by docker-compose (make db2-up). Profile: mvn -Pdb2-it verify.
 * Uses codes 90-99 so the seed rows 01-07 stay untouched.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class TransactionTypeDb2IT {

    @Autowired
    TestRestTemplate http;
    @Autowired
    JdbcTemplate jdbc;

    @BeforeEach
    @AfterEach
    void cleanUp() {
        jdbc.update("DELETE FROM CARDDEMO.TRANSACTION_TYPE WHERE TR_TYPE BETWEEN '90' AND '99'");
    }

    private String dbDescription(String code) {
        List<String> r = jdbc.queryForList(
                "SELECT TR_DESCRIPTION FROM CARDDEMO.TRANSACTION_TYPE WHERE TR_TYPE = ?", String.class, code);
        return r.isEmpty() ? null : r.get(0);
    }

    @Test
    @DisplayName("BR-TT-010/013 seed data from DB2LTTYP.ctl is served in TR_TYPE order")
    void listSeed() {
        Map<?, ?> page = http.getForObject("/api/v1/transaction-types", Map.class);
        List<?> items = (List<?>) page.get("items");
        assertThat(items).hasSize(7);
        assertThat(((Map<?, ?>) items.get(0)).get("description")).isEqualTo("PURCHASE");
    }

    @Test
    @DisplayName("BR-TT-013/014 paging across a real cursor boundary")
    void paging() {
        for (int i = 90; i <= 97; i++) {
            jdbc.update("INSERT INTO CARDDEMO.TRANSACTION_TYPE VALUES (?, ?)", String.valueOf(i), "PAGING TEST " + i);
        }
        Map<?, ?> p1 = http.getForObject("/api/v1/transaction-types?description=PAGING", Map.class);
        assertThat(p1.get("nextStartKey")).isEqualTo("97");
        Map<?, ?> p2 = http.getForObject(
                "/api/v1/transaction-types?description=PAGING&direction=NEXT&startKey=97", Map.class);
        assertThat((List<?>) p2.get("items")).hasSize(1);
        Map<?, ?> back = http.getForObject(
                "/api/v1/transaction-types?description=PAGING&direction=PREVIOUS&startKey=97", Map.class);
        assertThat(((Map<?, ?>) ((List<?>) back.get("items")).get(0)).get("code")).isEqualTo("90");
    }

    @Test
    @DisplayName("BR-TT-030/040/050 create, update, delete land in Db2")
    void crudLandsInDb2() {
        var created = http.postForEntity("/api/v1/transaction-types",
                Map.of("code", "91", "description", "IT CASHBACK"), Map.class);
        assertThat(created.getStatusCode()).isEqualTo(HttpStatus.CREATED);
        assertThat(dbDescription("91")).isEqualTo("IT CASHBACK");

        http.put("/api/v1/transaction-types/91", Map.of("description", "IT CASHBACK V2"));
        assertThat(dbDescription("91")).isEqualTo("IT CASHBACK V2");

        var unconfirmed = http.exchange("/api/v1/transaction-types/91", HttpMethod.DELETE, null, Map.class);
        assertThat(unconfirmed.getStatusCode()).isEqualTo(HttpStatus.PRECONDITION_REQUIRED);
        assertThat(dbDescription("91")).isNotNull();

        var deleted = http.exchange("/api/v1/transaction-types/91?confirm=true", HttpMethod.DELETE, null, Map.class);
        assertThat(deleted.getStatusCode()).isEqualTo(HttpStatus.OK);
        assertThat(dbDescription("91")).isNull();
    }

    @Test
    @DisplayName("BR-TT-030 duplicate insert surfaces real Db2 SQLCODE -803")
    void duplicate() {
        var r = http.postForEntity("/api/v1/transaction-types", Map.of("code", "01", "description", "DUP"), Map.class);
        assertThat(r.getStatusCode()).isEqualTo(HttpStatus.CONFLICT);
        assertThat(r.getBody().get("sqlCode")).isEqualTo(-803);
    }

    @Test
    @DisplayName("BR-TT-051 FK ON DELETE RESTRICT surfaces real Db2 SQLCODE -532")
    void deleteRestricted() {
        var r = http.exchange("/api/v1/transaction-types/01?confirm=true", HttpMethod.DELETE, null, Map.class);
        assertThat(r.getStatusCode()).isEqualTo(HttpStatus.CONFLICT);
        assertThat(r.getBody().get("sqlCode")).isEqualTo(-532);
        assertThat(dbDescription("01")).isEqualTo("PURCHASE");
    }

    @Test
    @DisplayName("BR-TT-070..075 batch run against Db2 keeps going after an error")
    void batch() {
        HttpHeaders h = new HttpHeaders();
        h.setContentType(MediaType.TEXT_PLAIN);
        String input = "A92BATCH ADD\nX00BAD\nU92BATCH UPDATED\nD99\nA93TO DELETE\nD93\n";
        Map<?, ?> r = http.postForObject("/api/v1/transaction-types/batch", new HttpEntity<>(input, h), Map.class);
        assertThat(r.get("returnCode")).isEqualTo(4);
        assertThat(r.get("errors")).isEqualTo(2);
        assertThat(dbDescription("92")).isEqualTo("BATCH UPDATED");
        assertThat(dbDescription("93")).isNull();
    }

    @Test
    @DisplayName("BR-TT-080/081 extract matches TRANEXTR 60-byte layout from Db2")
    void extract() {
        String types = http.getForObject("/api/v1/transaction-types/extract", String.class);
        assertThat(types.lines().findFirst().orElseThrow())
                .isEqualTo("01" + String.format("%-50s", "PURCHASE") + "00000000");
        String cats = http.getForObject("/api/v1/transaction-type-categories/extract", String.class);
        assertThat(cats.lines().count()).isEqualTo(18);
        assertThat(cats.lines()).allMatch(l -> l.length() == 60);
    }
}
