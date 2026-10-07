package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.harness.DdParameters;
import com.carddemo.batch.load.UnloadJobConfiguration;
import com.carddemo.batch.load.VsamDatasetLoader;
import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.support.Samples;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
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
 * s6.4 on PostgreSQL 16 with the sample USRSEC (ADR-0023): plain-text sign-on before the first hash, hash stored by
 * it, sign-on against the hash afterwards, COUSR01C/02C writing both values, the {@code unload} of USRSEC unchanged
 * byte for byte, and an administrator demoted by COUSR02C refused on the next admin request with the old token.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
        properties = "carddemo.batch.output-dir=target/password-it/batch-output")
@Testcontainers
class PasswordAndAdminHardeningIT {

    private static final AtomicLong RUN = new AtomicLong();

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
    @Autowired
    JobLauncher jobLauncher;
    @Autowired
    @Qualifier("unloadJob")
    Job unloadJob;

    @TempDir
    Path dir;

    @BeforeEach
    void loadUsrsec() {
        http.getRestTemplate().setRequestFactory(new JdkClientHttpRequestFactory());
        loader.load(Dataset.USRSEC, Samples.read(Dataset.USRSEC, RecordEncoding.EBCDIC));
    }

    private ResponseEntity<JsonNode> login(String user, String password) {
        return http.postForEntity("/api/v1/auth/login", Map.of("userId", user, "password", password), JsonNode.class);
    }

    private String token(String user, String password) {
        ResponseEntity<JsonNode> r = login(user, password);
        assertThat(r.getStatusCode()).as("sign-on " + user).isEqualTo(HttpStatus.OK);
        return r.getBody().get("token").asText();
    }

    private ResponseEntity<JsonNode> call(String token, HttpMethod method, String path, Object body) {
        HttpHeaders headers = new HttpHeaders();
        headers.setBearerAuth(token);
        return http.exchange(path, method, new HttpEntity<>(body, headers), JsonNode.class);
    }

    private String hashOf(String user) {
        return jdbc.queryForObject("select password_hash from user_security where usr_id = ?", String.class, user);
    }

    private byte[] unloadUsrsec() throws Exception {
        Path out = dir.resolve("USRSEC-" + RUN.incrementAndGet() + ".ebcdic");
        BatchStatus status = jobLauncher.run(unloadJob, new JobParametersBuilder()
                .addString(UnloadJobConfiguration.DATASET, Dataset.USRSEC.name())
                .addString(UnloadJobConfiguration.OUTFILE, out.toString())
                .addString(DdParameters.ENCODING, "EBCDIC").addLong("run.id", System.nanoTime())
                .toJobParameters()).getStatus();
        assertThat(status).isEqualTo(BatchStatus.COMPLETED);
        return Files.readAllBytes(out);
    }

    @Test
    void firstSignOnHashesAndTheUnloadStaysByteIdentical() throws Exception {
        assertThat(jdbc.queryForObject("select count(*) from user_security where password_hash is not null",
                Integer.class)).as("initial load stores no hash").isZero();
        byte[] before = unloadUsrsec();

        assertThat(login("USER0001", "WRONG").getStatusCode()).isEqualTo(HttpStatus.UNAUTHORIZED);
        assertThat(hashOf("USER0001")).isNull();
        token("user0001", "password");
        assertThat(hashOf("USER0001")).startsWith("{bcrypt}$2");
        assertThat(jdbc.queryForMap("select password, version from user_security where usr_id = 'USER0001'"))
                .containsEntry("password", "PASSWORD").containsEntry("version", 0L);

        String stored = hashOf("USER0001");
        token("USER0001", "PASSWORD");
        assertThat(hashOf("USER0001")).as("not re-hashed").isEqualTo(stored);
        ResponseEntity<JsonNode> wrong = login("USER0001", "PASSWORX");
        assertThat(wrong.getStatusCode()).isEqualTo(HttpStatus.UNAUTHORIZED);
        assertThat(wrong.getBody().get("code").asText()).isEqualTo("WRONG_PASSWORD");

        assertThat(unloadUsrsec()).as("SEC-USR-PWD round trip unchanged by hashing").isEqualTo(before);
    }

    @Test
    void addAndUpdateWriteThePlainFieldAndTheHash() throws Exception {
        String admin = token("ADMIN001", "PASSWORD");
        ObjectNode add = json.createObjectNode().put("firstName", "Pw").put("lastName", "Test").put("userId", "ZZPW0001")
                .put("password", "PW01").put("userType", "U");
        assertThat(call(admin, HttpMethod.POST, "/api/v1/users", add).getStatusCode()).isEqualTo(HttpStatus.CREATED);
        assertThat(hashOf("ZZPW0001")).startsWith("{bcrypt}");
        token("ZZPW0001", "pw01");

        long version = call(admin, HttpMethod.GET, "/api/v1/users/ZZPW0001", null).getBody().at("/user/version")
                .asLong();
        ObjectNode update = json.createObjectNode().put("firstName", "Pw").put("lastName", "Test")
                .put("password", "NEWPW1").put("userType", "U").put("version", version);
        assertThat(call(admin, HttpMethod.PUT, "/api/v1/users/ZZPW0001", update).getStatusCode())
                .isEqualTo(HttpStatus.OK);
        assertThat(login("ZZPW0001", "PW01").getStatusCode()).isEqualTo(HttpStatus.UNAUTHORIZED);
        token("ZZPW0001", "newpw1");
        assertThat(jdbc.queryForObject("select password from user_security where usr_id = 'ZZPW0001'", String.class))
                .isEqualTo("NEWPW1");
        assertThat(new String(unloadUsrsec(), "Cp037")).contains("NEWPW1  ");
        jdbc.update("delete from user_security where usr_id like 'ZZPW%'");
    }

    @Test
    void aDemotedAdministratorIsRefusedWithTheOldToken() {
        String admin = token("ADMIN001", "PASSWORD");
        ObjectNode add = json.createObjectNode().put("firstName", "Demo").put("lastName", "Admin")
                .put("userId", "ZZADM001").put("password", "ADMPW").put("userType", "A");
        assertThat(call(admin, HttpMethod.POST, "/api/v1/users", add).getStatusCode()).isEqualTo(HttpStatus.CREATED);
        String demoted = token("ZZADM001", "ADMPW");
        assertThat(call(demoted, HttpMethod.GET, "/api/v1/users", null).getStatusCode()).isEqualTo(HttpStatus.OK);

        long version = call(admin, HttpMethod.GET, "/api/v1/users/ZZADM001", null).getBody().at("/user/version")
                .asLong();
        ObjectNode demote = json.createObjectNode().put("firstName", "Demo").put("lastName", "Admin")
                .put("password", "ADMPW").put("userType", "U").put("version", version);
        assertThat(call(admin, HttpMethod.PUT, "/api/v1/users/ZZADM001", demote).getStatusCode())
                .isEqualTo(HttpStatus.OK);

        for (String path : new String[] {"/api/v1/users", "/api/v1/menu/admin"}) {
            ResponseEntity<JsonNode> refused = call(demoted, HttpMethod.GET, path, null);
            assertThat(refused.getStatusCode()).as(path).isEqualTo(HttpStatus.FORBIDDEN);
            assertThat(refused.getBody().get("code").asText()).isEqualTo("NOTAUTH");
        }
        assertThat(call(demoted, HttpMethod.GET, "/api/v1/menu/main", null).getStatusCode())
                .as("non-admin screens still work until the token expires").isEqualTo(HttpStatus.OK);

        call(admin, HttpMethod.DELETE, "/api/v1/users/ZZADM001?confirm=Y&version=" + (version + 1), null);
        assertThat(call(demoted, HttpMethod.GET, "/api/v1/users", null).getStatusCode())
                .isEqualTo(HttpStatus.FORBIDDEN);
    }
}
