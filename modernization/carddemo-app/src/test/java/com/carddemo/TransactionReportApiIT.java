package com.carddemo;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.harness.BatchJobLauncher;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.load.ReproJobConfiguration;
import com.carddemo.batch.load.VsamDatasetLoader;
import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.TestData;
import com.carddemo.support.Samples;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.core.JobParametersBuilder;
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
 * CORPT00C on PostgreSQL 16 with the async queue on (as in the web app): Monthly / Yearly / Custom submissions run the
 * {@code tranrept} stream in the background, polling reaches COMPLETED, and the downloaded TRANREPT generation is
 * byte-identical to the one {@code --job=tranrept} writes from the command line for the same range and run date.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT, properties = {
        "carddemo.reports.async.enabled=true",
        "carddemo.clock.fixed=2022-07-06T10:00:00",
        "carddemo.batch.output-dir=target/report-api-it-output"})
@Testcontainers
class TransactionReportApiIT {

    private static final String REPORTS = "/api/v1/reports/transactions";

    @Container
    @ServiceConnection
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine");

    @Autowired
    TestRestTemplate http;
    @Autowired
    VsamDatasetLoader loader;
    @Autowired
    BatchJobLauncher launcher;
    @Autowired
    JdbcTemplate jdbc;
    @Autowired
    ObjectMapper json;

    @TempDir
    Path dir;

    private static boolean loaded;

    @BeforeEach
    void loadOnce() {
        http.getRestTemplate().setRequestFactory(new JdkClientHttpRequestFactory());
        if (!loaded) {
            for (Dataset dataset : List.of(Dataset.USRSEC, Dataset.CUSTDATA, Dataset.ACCTDATA, Dataset.CARDDATA,
                    Dataset.CARDXREF, Dataset.TRANTYPE, Dataset.TRANCATG)) {
                loader.load(dataset, Samples.read(dataset, RecordEncoding.EBCDIC));
            }
            assertThat(launcher.run(ReproJobConfiguration.REPRO, new JobParametersBuilder()
                    .addLocalDate("run-date", LocalDate.of(2022, 7, 6)).addLong("run.id", System.nanoTime())
                    .addString("encoding", "ASCII")
                    .addString(ReproJobConfiguration.DATASET, Dataset.TRANSACT.name())
                    .addString(ReproJobConfiguration.INFILE,
                            TestData.resolve("docs/validation/baseline/POSTTRAN/TRANSACT.ksds.txt").toString())
                    .toJobParameters()).returnCode()).isEqualTo(ReturnCode.OK);
            loaded = true;
        }
    }

    private String token(String user) {
        return http.postForEntity("/api/v1/auth/login", Map.of("userId", user, "password", "PASSWORD"),
                JsonNode.class).getBody().get("token").asText();
    }

    private <T> ResponseEntity<T> call(String token, HttpMethod method, String path, Object body, Class<T> type) {
        HttpHeaders headers = new HttpHeaders();
        headers.setBearerAuth(token);
        return http.exchange(path, method, new HttpEntity<>(body, headers), type);
    }

    private ObjectNode request(String type, String confirm) {
        return json.createObjectNode().put("reportType", type).put("confirm", confirm);
    }

    private ObjectNode custom(String start, String end, String confirm) {
        ObjectNode body = request("Custom", confirm);
        String[] s = start.split("-");
        String[] e = end.split("-");
        body.putObject("startDate").put("month", s[1]).put("day", s[2]).put("year", s[0]);
        body.putObject("endDate").put("month", e[1]).put("day", e[2]).put("year", e[0]);
        return body;
    }

    private JsonNode pollUntilFinished(String token, long id) throws InterruptedException {
        long deadline = System.nanoTime() + Duration.ofMinutes(2).toNanos();
        List<String> seen = new ArrayList<>();
        while (true) {
            JsonNode status = call(token, HttpMethod.GET, REPORTS + "/" + id, null, JsonNode.class).getBody();
            String state = status.get("status").asText();
            if (seen.isEmpty() || !seen.get(seen.size() - 1).equals(state)) {
                seen.add(state);
            }
            if (state.equals("COMPLETED") || state.equals("FAILED")) {
                assertThat(seen).last().isEqualTo("COMPLETED");
                return status;
            }
            assertThat(System.nanoTime()).as("report %d still %s", id, state).isLessThan(deadline);
            Thread.sleep(200);
        }
    }

    private byte[] submitAndDownload(String token, ObjectNode body, String start, String end) throws Exception {
        ResponseEntity<JsonNode> submitted = call(token, HttpMethod.POST, REPORTS, body, JsonNode.class);
        assertThat(submitted.getStatusCode()).isEqualTo(HttpStatus.ACCEPTED);
        assertThat(submitted.getBody().get("parmStartDate").asText()).isEqualTo(start);
        assertThat(submitted.getBody().get("parmEndDate").asText()).isEqualTo(end);
        long id = submitted.getBody().get("executionId").asLong();
        assertThat(submitted.getHeaders().getLocation().getPath()).isEqualTo(REPORTS + "/" + id);

        JsonNode done = pollUntilFinished(token, id);
        assertThat(done.get("returnCode").asInt()).isLessThan(8);
        assertThat(done.get("jobs")).hasSize(3);
        JsonNode report = done.get("report");
        assertThat(report.get("lines")).hasSize(report.get("recordCount").asInt());
        assertThat(report.get("lines").get(0).asText()).contains("Date Range: " + start + " to " + end);
        assertThat(jdbc.queryForObject("select report_job_execution_id from report_request where "
                + "report_request_id = ?", Long.class, id)).isEqualTo(done.get("jobs").get(2).get("jobExecutionId")
                .asLong());

        ResponseEntity<byte[]> file = call(token, HttpMethod.GET, report.get("downloadUrl").asText(), null,
                byte[].class);
        assertThat(file.getStatusCode()).isEqualTo(HttpStatus.OK);
        return file.getBody();
    }

    private byte[] cli(String start, String end) throws Exception {
        long before = jdbc.queryForObject("select coalesce(max(output_file_id), 0) from batch_output_file",
                Long.class);
        int rc = CardDemoApplication.runBatch("--job=tranrept", "--run-date=2022-07-06",
                "--PARM-START-DATE=" + start, "--PARM-END-DATE=" + end,
                "--STEP15.SYSOUT=" + dir.resolve("cbtrn03c-cli.txt"),
                "--carddemo.batch.output-dir=" + dir.resolve("cli"),
                "--spring.datasource.url=" + postgres.getJdbcUrl(),
                "--spring.datasource.username=" + postgres.getUsername(),
                "--spring.datasource.password=" + postgres.getPassword(),
                "--carddemo.initial-load.on-startup=false", "--spring.main.banner-mode=off");
        assertThat(rc).isLessThan(8);
        String path = jdbc.queryForObject("select file_path from batch_output_file where gdg_base = 'TRANREPT' "
                + "and output_file_id > ? order by output_file_id desc limit 1", String.class, before);
        return Files.readAllBytes(Path.of(path));
    }

    @Test
    void monthlyYearlyAndCustomRunAsynchronouslyAndMatchTheCli() throws Exception {
        String user = token("USER0001");
        byte[] monthly = submitAndDownload(user, request("Monthly", "Y"), "2022-07-01", "2022-07-31");
        byte[] yearly = submitAndDownload(user, request("Yearly", "Y"), "2022-01-01", "2022-12-31");
        byte[] custom = submitAndDownload(user, custom("2022-01-01", "2022-07-06", "Y"), "2022-01-01",
                "2022-07-06");

        assertThat(custom).isEqualTo(cli("2022-01-01", "2022-07-06"));
        assertThat(monthly).isEqualTo(cli("2022-07-01", "2022-07-31"));
        assertThat(yearly).isEqualTo(cli("2022-01-01", "2022-12-31"));
    }

    @Test
    void invalidAndReversedRangesNeverReachTheQueue() {
        String user = token("USER0001");
        long before = jdbc.queryForObject("select count(*) from report_request", Long.class);
        ResponseEntity<JsonNode> invalid = call(user, HttpMethod.POST, REPORTS,
                custom("2022-02-30", "2022-07-06", "Y"), JsonNode.class);
        assertThat(invalid.getStatusCode()).isEqualTo(HttpStatus.BAD_REQUEST);
        assertThat(invalid.getBody().get("message").asText()).isEqualTo("Start Date - Not a valid date...");
        ResponseEntity<JsonNode> reversed = call(user, HttpMethod.POST, REPORTS,
                custom("2022-07-06", "2022-01-01", "Y"), JsonNode.class);
        assertThat(reversed.getStatusCode()).isEqualTo(HttpStatus.BAD_REQUEST);
        assertThat(reversed.getBody().get("message").asText()).isEqualTo("Start Date can NOT be after End Date...");
        assertThat(jdbc.queryForObject("select count(*) from report_request", Long.class)).isEqualTo(before);
    }

    @Test
    void anotherUsersExecutionIsNotFoundButAnAdminSeesIt() throws Exception {
        String user = token("USER0001");
        long id = call(user, HttpMethod.POST, REPORTS, request("Monthly", "Y"), JsonNode.class).getBody()
                .get("executionId").asLong();
        pollUntilFinished(user, id);
        assertThat(call(token("USER0002"), HttpMethod.GET, REPORTS + "/" + id, null, JsonNode.class)
                .getStatusCode()).isEqualTo(HttpStatus.NOT_FOUND);
        assertThat(call(token("ADMIN001"), HttpMethod.GET, REPORTS + "/" + id, null, JsonNode.class)
                .getBody().get("requestedBy").asText()).isEqualTo("USER0001");
    }
}
