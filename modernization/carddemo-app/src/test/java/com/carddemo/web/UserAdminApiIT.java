package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.load.VsamDatasetLoader;
import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.support.Samples;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.testcontainers.service.connection.ServiceConnection;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.JdkClientHttpRequestFactory;
import org.springframework.jdbc.core.JdbcTemplate;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

/**
 * COUSR00C..03C on PostgreSQL 16 with the sample USRSEC: keyset pages in {@code COLLATE "C"} order, insert-only add
 * (duplicate → 409), update with no-change detection and version check under {@code SELECT ... FOR UPDATE}, delete
 * after confirmation, and 403 NOTAUTH for a USER token on every user endpoint.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Testcontainers
class UserAdminApiIT {

    @Container
    @ServiceConnection
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine");

    @Autowired
    TestRestTemplate http;
    @Autowired
    VsamDatasetLoader loader;
    @Autowired
    JdbcTemplate jdbc;
    @Autowired
    ObjectMapper json;

    @BeforeEach
    void loadUsrsec() {
        http.getRestTemplate().setRequestFactory(new JdkClientHttpRequestFactory());
        loader.load(Dataset.USRSEC, Samples.read(Dataset.USRSEC, RecordEncoding.EBCDIC));
    }

    private String token(String user) {
        return http.postForEntity("/api/v1/auth/login", Map.of("userId", user, "password", "PASSWORD"),
                JsonNode.class).getBody().get("token").asText();
    }

    private ResponseEntity<JsonNode> call(String token, HttpMethod method, String path, Object body) {
        HttpHeaders headers = new HttpHeaders();
        headers.setBearerAuth(token);
        return http.exchange(path, method, new HttpEntity<>(body, headers), JsonNode.class);
    }

    private static List<String> idsOf(JsonNode page) {
        List<String> ids = new ArrayList<>();
        page.get("rows").forEach(r -> ids.add(r.get("userId").asText()));
        return ids;
    }

    private ObjectNode addForm(String id) {
        return json.createObjectNode().put("firstName", "Test").put("lastName", "User " + id).put("userId", id)
                .put("password", "PW" + id.substring(id.length() - 2)).put("userType", "U");
    }

    private List<String> tableIds() {
        return jdbc.queryForList("select usr_id from user_security order by usr_id", String.class);
    }

    @Test
    void listAddUpdateDeleteAgainstPostgres() {
        String admin = token("ADMIN001");
        for (String id : List.of("ZZTEST01", "ZZTEST02", "ZZTEST03")) {
            assertThat(call(admin, HttpMethod.POST, "/api/v1/users", addForm(id)).getStatusCode())
                    .isEqualTo(HttpStatus.CREATED);
        }
        List<String> all = tableIds();
        assertThat(all).hasSizeGreaterThan(10);

        JsonNode first = call(admin, HttpMethod.GET, "/api/v1/users?limit=10", null).getBody();
        assertThat(idsOf(first)).isEqualTo(all.subList(0, 10));
        assertThat(first.get("hasNextPage").asBoolean()).isTrue();
        JsonNode next = call(admin, HttpMethod.GET,
                "/api/v1/users?after=" + first.get("nextPage").asText() + "&page=1", null).getBody();
        assertThat(idsOf(next)).isEqualTo(all.subList(10, Math.min(20, all.size())));
        assertThat(next.get("pageNumber").asInt()).isEqualTo(2);
        JsonNode previous = call(admin, HttpMethod.GET,
                "/api/v1/users?before=" + next.get("previousPage").asText() + "&page=2", null).getBody();
        assertThat(idsOf(previous)).isEqualTo(all.subList(0, 10));

        ResponseEntity<JsonNode> duplicate = call(admin, HttpMethod.POST, "/api/v1/users", addForm("ZZTEST01"));
        assertThat(duplicate.getStatusCode()).isEqualTo(HttpStatus.CONFLICT);
        assertThat(duplicate.getBody().get("message").asText()).isEqualTo("User ID already exist...");
        assertThat(jdbc.queryForObject("select version from user_security where usr_id = 'ZZTEST01'", Long.class))
                .isZero();

        JsonNode shown = call(admin, HttpMethod.GET, "/api/v1/users/ZZTEST02", null).getBody();
        ObjectNode same = json.createObjectNode().put("firstName", "Test").put("lastName", "User ZZTEST02")
                .put("password", "PW02").put("userType", "u").put("version", shown.at("/user/version").asLong());
        JsonNode unchanged = call(admin, HttpMethod.PUT, "/api/v1/users/ZZTEST02", same).getBody();
        assertThat(unchanged.get("message").asText()).isEqualTo("Please modify to update ...");
        ResponseEntity<JsonNode> updated = call(admin, HttpMethod.PUT, "/api/v1/users/ZZTEST02",
                same.deepCopy().put("password", "NEWPW").put("userType", "A"));
        assertThat(updated.getStatusCode()).isEqualTo(HttpStatus.OK);
        assertThat(updated.getBody().get("message").asText()).isEqualTo("User ZZTEST02 has been updated ...");
        assertThat(jdbc.queryForMap("select password, usr_type, version from user_security where usr_id = 'ZZTEST02'"))
                .containsEntry("password", "NEWPW").containsEntry("usr_type", "A").containsEntry("version", 1L);
        ResponseEntity<JsonNode> stale = call(admin, HttpMethod.PUT, "/api/v1/users/ZZTEST02",
                same.deepCopy().put("lastName", "Stale"));
        assertThat(stale.getStatusCode()).isEqualTo(HttpStatus.CONFLICT);
        assertThat(stale.getBody().get("code").asText()).isEqualTo("CHANGED");

        JsonNode confirm = call(admin, HttpMethod.DELETE, "/api/v1/users/ZZTEST03", null).getBody();
        assertThat(confirm.get("message").asText()).isEqualTo("Press PF5 key to delete this user ...");
        assertThat(tableIds()).contains("ZZTEST03");
        ResponseEntity<JsonNode> deleted = call(admin, HttpMethod.DELETE,
                "/api/v1/users/ZZTEST03?confirm=Y&version=" + confirm.at("/user/version").asLong(), null);
        assertThat(deleted.getBody().get("message").asText()).isEqualTo("User ZZTEST03 has been deleted ...");
        assertThat(tableIds()).doesNotContain("ZZTEST03");
        assertThat(call(admin, HttpMethod.GET, "/api/v1/users/ZZTEST03", null).getStatusCode())
                .isEqualTo(HttpStatus.NOT_FOUND);

        jdbc.update("delete from user_security where usr_id like 'ZZTEST%'");
    }

    @Test
    void aUserTokenIsRefusedEverywhere() {
        String user = token("USER0001");
        List<ResponseEntity<JsonNode>> answers = List.of(
                call(user, HttpMethod.GET, "/api/v1/users", null),
                call(user, HttpMethod.POST, "/api/v1/users/selection", Map.of("rows", List.of())),
                call(user, HttpMethod.POST, "/api/v1/users", addForm("ZZTEST09")),
                call(user, HttpMethod.GET, "/api/v1/users/ADMIN001", null),
                call(user, HttpMethod.PUT, "/api/v1/users/ADMIN001", Map.of()),
                call(user, HttpMethod.DELETE, "/api/v1/users/ADMIN001?confirm=Y&version=0", null),
                call(user, HttpMethod.GET, "/api/v1/menu/admin", null));
        for (ResponseEntity<JsonNode> answer : answers) {
            assertThat(answer.getStatusCode()).isEqualTo(HttpStatus.FORBIDDEN);
            assertThat(answer.getBody().get("code").asText()).isEqualTo("NOTAUTH");
            assertThat(answer.getBody().get("message").asText()).isEqualTo("No access - Admin Only option...");
        }
        assertThat(tableIds()).contains("ADMIN001").doesNotContain("ZZTEST09");
    }

    @Test
    void openApiDocumentsTheUserAndReportEndpoints() throws Exception {
        JsonNode doc = json.readTree(http.getForEntity("/v3/api-docs", String.class).getBody());
        String problem = "/content/application~1problem+json/examples/";
        JsonNode users = doc.at("/paths/~1api~1v1~1users");
        assertThat(users.at("/get/responses/403" + problem + "adminOnly/value/message").asText())
                .isEqualTo("No access - Admin Only option...");
        assertThat(users.at("/post/responses/409" + problem + "duplicateUser/value/message").asText())
                .isEqualTo("User ID already exist...");
        JsonNode user = doc.at("/paths/~1api~1v1~1users~1{id}");
        for (String method : List.of("get", "put", "delete")) {
            assertThat(user.at("/" + method + "/responses/403").isMissingNode()).as(method).isFalse();
            assertThat(user.at("/" + method + "/security").isMissingNode()).as(method).isFalse();
        }
        assertThat(user.at("/put/responses/409").isMissingNode()).isFalse();
        assertThat(doc.at("/paths/~1api~1v1~1users~1selection/post/responses/400").isMissingNode()).isFalse();
        JsonNode reports = doc.at("/paths/~1api~1v1~1reports~1transactions/post");
        assertThat(reports.at("/responses/400" + problem + "reversedRange/value/message").asText())
                .isEqualTo("Start Date can NOT be after End Date...");
        assertThat(reports.at("/responses/202").isMissingNode()).isFalse();
        assertThat(reports.at("/responses/503" + problem + "queueFull/value/code").asText()).isEqualTo("NOSPACE");
        assertThat(doc.at("/paths/~1api~1v1~1reports~1transactions~1{executionId}/get/responses/404")
                .isMissingNode()).isFalse();
        assertThat(doc.at("/components/schemas/ReportExecutionResponse/properties/report").isMissingNode()).isFalse();
    }
}
