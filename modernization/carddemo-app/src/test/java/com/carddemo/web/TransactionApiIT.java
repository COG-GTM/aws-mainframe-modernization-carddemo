package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.harness.BatchJobLauncher;
import com.carddemo.batch.harness.JobChain;
import com.carddemo.batch.harness.JobStream;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.load.ReproJobConfiguration;
import com.carddemo.batch.load.VsamDatasetLoader;
import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.batch.tranrept.TranreptJobConfiguration;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.TestData;
import com.carddemo.support.Samples;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
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
 * COTRN00C/01C/02C and COBIL00C on PostgreSQL 16: sample USRSEC/customer/account/card/xref/TRANTYPE/TRANCATG plus
 * the POSTTRAN baseline TRANSACT (262 rows) repro'd into {@code transaction}. Proves paging, detail, add with the
 * advisory-locked id under concurrency, atomic bill payment (409 for the second of two concurrent attempts, rollback
 * when the account update fails) and that rows written online are reported by the TRANREPT job stream.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
        properties = "carddemo.batch.output-dir=target/transaction-api-it-output")
@Testcontainers
class TransactionApiIT {

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
    List<JobStream> streams;
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
            assertThat(launcher.run(ReproJobConfiguration.REPRO, parameters()
                    .addString(ReproJobConfiguration.DATASET, Dataset.TRANSACT.name())
                    .addString(ReproJobConfiguration.INFILE,
                            TestData.resolve("docs/validation/baseline/POSTTRAN/TRANSACT.ksds.txt").toString())
                    .toJobParameters()).returnCode()).isEqualTo(ReturnCode.OK);
            loaded = true;
        }
    }

    private static JobParametersBuilder parameters() {
        return new JobParametersBuilder().addLocalDate("run-date", LocalDate.of(2022, 7, 6))
                .addLong("run.id", System.nanoTime()).addString("encoding", "ASCII");
    }

    private String token() {
        return http.postForEntity("/api/v1/auth/login", Map.of("userId", "USER0001", "password", "PASSWORD"),
                JsonNode.class).getBody().get("token").asText();
    }

    private ResponseEntity<JsonNode> call(String token, HttpMethod method, String path, Object body) {
        HttpHeaders headers = new HttpHeaders();
        headers.setBearerAuth(token);
        return http.exchange(path, method, new HttpEntity<>(body, headers), JsonNode.class);
    }

    private List<String> tableIds(String sql, Object... args) {
        return jdbc.queryForList(sql, String.class, args);
    }

    private static List<String> idsOf(JsonNode page) {
        List<String> ids = new ArrayList<>();
        page.get("rows").forEach(r -> ids.add(r.get("tranId").asText()));
        return ids;
    }

    private ObjectNode form(String accountId) {
        ObjectNode form = json.createObjectNode();
        form.put("accountId", accountId);
        form.put("typeCode", "01");
        form.put("categoryCode", "0001");
        form.put("source", "POS TERM");
        form.put("description", "Online purchase IT");
        form.put("amount", "-00000012.34");
        form.put("origDate", "2022-07-06");
        form.put("procDate", "2022-07-06");
        form.put("merchantId", "000000001");
        form.put("merchantName", "Corner Store");
        form.put("merchantCity", "Seattle");
        form.put("merchantZip", "98101");
        form.put("confirm", "");
        return form;
    }

    private long accountWithCard(int offset) {
        return jdbc.queryForObject("""
                select a.acct_id from account a where a.curr_bal > 0
                and exists (select 1 from card_xref x where x.acct_id = a.acct_id)
                order by a.acct_id offset ? limit 1""", Long.class, offset);
    }

    private BigDecimal balance(long account) {
        return jdbc.queryForObject("select curr_bal from account where acct_id = ?", BigDecimal.class, account);
    }

    private long transactionCount() {
        return jdbc.queryForObject("select count(*) from transaction", Long.class);
    }

    @Test
    void listPagesTenRowsForwardAndBackAndShowsTheDetail() {
        String token = token();
        List<String> firstTwenty = tableIds("select tran_id from transaction order by tran_id limit 20");
        JsonNode first = call(token, HttpMethod.GET, "/api/v1/transactions?limit=10", null).getBody();
        assertThat(idsOf(first)).isEqualTo(firstTwenty.subList(0, 10));
        JsonNode second = call(token, HttpMethod.GET, "/api/v1/transactions?page=1&after="
                + first.get("nextPage").asText(), null).getBody();
        assertThat(idsOf(second)).isEqualTo(firstTwenty.subList(10, 20));
        assertThat(second.get("pageNumber").asInt()).isEqualTo(2);
        JsonNode back = call(token, HttpMethod.GET, "/api/v1/transactions?page=2&before="
                + second.get("previousPage").asText(), null).getBody();
        assertThat(idsOf(back)).isEqualTo(firstTwenty.subList(0, 10));
        assertThat(first.get("rows").get(0).get("amount").asText()).matches("[+-]\\d{8}\\.\\d{2}");

        String id = firstTwenty.get(3);
        ResponseEntity<JsonNode> detail = call(token, HttpMethod.GET, "/api/v1/transactions/" + id, null);
        assertThat(detail.getStatusCode()).isEqualTo(HttpStatus.OK);
        assertThat(detail.getBody().at("/transaction/tranId").asText()).isEqualTo(id);
        assertThat(detail.getBody().at("/transaction/cardNumber").asText()).isEqualTo(
                jdbc.queryForObject("select card_num from transaction where tran_id = ?", String.class, id));
        assertThat(call(token, HttpMethod.GET, "/api/v1/transactions/9999999999999999", null).getStatusCode())
                .isEqualTo(HttpStatus.NOT_FOUND);
    }

    @Test
    void addValidatesThenWritesTheNextIdAndTranreptReportsTheRow() throws IOException {
        String token = token();
        long account = accountWithCard(0);
        ResponseEntity<JsonNode> bad = call(token, HttpMethod.POST, "/api/v1/transactions",
                form(String.valueOf(account)).put("amount", "12.34"));
        assertThat(bad.getStatusCode()).isEqualTo(HttpStatus.BAD_REQUEST);
        assertThat(bad.getBody().get("field").asText()).isEqualTo("amount");
        ResponseEntity<JsonNode> validated = call(token, HttpMethod.POST, "/api/v1/transactions",
                form(String.valueOf(account)));
        assertThat(validated.getBody().get("state").asText()).isEqualTo("VALIDATED");

        String last = tableIds("select max(tran_id) from transaction").get(0);
        ResponseEntity<JsonNode> added = call(token, HttpMethod.POST, "/api/v1/transactions",
                form(String.valueOf(account)).put("confirm", "Y"));
        assertThat(added.getStatusCode()).isEqualTo(HttpStatus.CREATED);
        String tranId = added.getBody().at("/transaction/tranId").asText();
        assertThat(tranId).isEqualTo(String.format("%016d", Long.parseLong(last) + 1));
        Map<String, Object> row = jdbc.queryForMap("select * from transaction where tran_id = ?", tranId);
        assertThat(row.get("tran_type_cd")).isEqualTo("01");
        assertThat((BigDecimal) row.get("amount")).isEqualByComparingTo("-12.34");
        assertThat(row.get("card_num")).isEqualTo(jdbc.queryForObject(
                "select min(card_num) from card_xref where acct_id = ?", String.class, account));

        String paid = billPay(token, accountWithCard(1));
        String report = tranrept();
        assertThat(report).contains(tranId).contains(paid);
    }

    private String billPay(String token, long account) {
        String path = "/api/v1/accounts/" + account + "/bill-payment";
        long version = call(token, HttpMethod.POST, path, body("", null)).getBody().get("version").asLong();
        ResponseEntity<JsonNode> paid = call(token, HttpMethod.POST, path, body("Y", version));
        assertThat(paid.getStatusCode()).isEqualTo(HttpStatus.OK);
        return paid.getBody().at("/transaction/tranId").asText();
    }

    private String tranrept() throws IOException {
        JobStream stream = streams.stream().filter(s -> s.name().equals(TranreptJobConfiguration.TRANREPT))
                .findFirst().orElseThrow();
        JobChain.Result result = stream.chain(launcher, parameters()
                .addString(TranreptJobConfiguration.PARM_START_DATE, "2022-01-01")
                .addString(TranreptJobConfiguration.PARM_END_DATE, "2099-12-31")
                .addString("STEP15.SYSOUT", dir.resolve("cbtrn03c.txt").toString()).toJobParameters()).run();
        assertThat(result.maxReturnCode()).isEqualTo(ReturnCode.OK);
        String file = jdbc.queryForObject("""
                select file_path from batch_output_file where gdg_base = 'TRANREPT'
                order by output_file_id desc limit 1""", String.class);
        return Files.readString(Path.of(file), StandardCharsets.ISO_8859_1);
    }

    private static Map<String, Object> body(String confirm, Long version) {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("confirm", confirm);
        body.put("version", version);
        return body;
    }

    @Test
    void concurrentAddsGetDistinctConsecutiveIds() throws Exception {
        String token = token();
        String account = String.valueOf(accountWithCard(0));
        long before = Long.parseLong(tableIds("select max(tran_id) from transaction").get(0));
        int writers = 8;
        CountDownLatch start = new CountDownLatch(1);
        ExecutorService pool = Executors.newFixedThreadPool(writers);
        try {
            List<Future<ResponseEntity<JsonNode>>> results = new ArrayList<>();
            for (int i = 0; i < writers; i++) {
                Callable<ResponseEntity<JsonNode>> add = () -> {
                    start.await();
                    return call(token, HttpMethod.POST, "/api/v1/transactions", form(account).put("confirm", "Y"));
                };
                results.add(pool.submit(add));
            }
            start.countDown();
            Set<String> ids = new HashSet<>();
            for (Future<ResponseEntity<JsonNode>> result : results) {
                assertThat(result.get().getStatusCode()).isEqualTo(HttpStatus.CREATED);
                ids.add(result.get().getBody().at("/transaction/tranId").asText());
            }
            Set<String> expected = new HashSet<>();
            for (int i = 1; i <= writers; i++) {
                expected.add(String.format("%016d", before + i));
            }
            assertThat(ids).isEqualTo(expected);
        } finally {
            pool.shutdownNow();
        }
    }

    @Test
    void billPaymentIsAtomicAndTheSecondConcurrentAttemptIs409() throws Exception {
        String token = token();
        long account = accountWithCard(2);
        String path = "/api/v1/accounts/" + account + "/bill-payment";
        BigDecimal owed = balance(account);
        JsonNode shown = call(token, HttpMethod.POST, path, body("", null)).getBody();
        assertThat(shown.get("state").asText()).isEqualTo("SHOW");
        assertThat(new BigDecimal(shown.get("currentBalance").asText())).isEqualByComparingTo(owed);
        long version = shown.get("version").asLong();
        long count = transactionCount();

        CountDownLatch start = new CountDownLatch(1);
        ExecutorService pool = Executors.newFixedThreadPool(2);
        List<HttpStatus> statuses = new ArrayList<>();
        try {
            List<Future<ResponseEntity<JsonNode>>> attempts = new ArrayList<>();
            for (int i = 0; i < 2; i++) {
                attempts.add(pool.submit(() -> {
                    start.await();
                    return call(token, HttpMethod.POST, path, body("Y", version));
                }));
            }
            start.countDown();
            for (Future<ResponseEntity<JsonNode>> attempt : attempts) {
                statuses.add(HttpStatus.valueOf(attempt.get().getStatusCode().value()));
                if (attempt.get().getStatusCode() == HttpStatus.CONFLICT) {
                    assertThat(attempt.get().getBody().get("code").asText()).isEqualTo("CHANGED");
                }
            }
        } finally {
            pool.shutdownNow();
        }
        assertThat(statuses).containsExactlyInAnyOrder(HttpStatus.OK, HttpStatus.CONFLICT);
        assertThat(balance(account)).isEqualByComparingTo("0.00");
        assertThat(transactionCount()).isEqualTo(count + 1);
        Map<String, Object> payment = jdbc.queryForMap(
                "select * from transaction where tran_id = (select max(tran_id) from transaction)");
        assertThat(payment.get("tran_type_cd")).isEqualTo("02");
        assertThat(payment.get("tran_cat_cd")).isEqualTo(2);
        assertThat(payment.get("description")).isEqualTo("BILL PAYMENT - ONLINE");
        assertThat((BigDecimal) payment.get("amount")).isEqualByComparingTo(owed);
        assertThat((String) payment.get("proc_ts")).matches("\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\.000000");

        ResponseEntity<JsonNode> nothing = call(token, HttpMethod.POST, path, body("Y", version + 1));
        assertThat(nothing.getStatusCode()).isEqualTo(HttpStatus.BAD_REQUEST);
        assertThat(nothing.getBody().get("message").asText()).isEqualTo("You have nothing to pay...");
    }

    @Test
    void aFailedAccountUpdateRollsTheTransactionRowBack() {
        String token = token();
        long account = accountWithCard(3);
        String path = "/api/v1/accounts/" + account + "/bill-payment";
        BigDecimal owed = balance(account);
        long version = call(token, HttpMethod.POST, path, body("", null)).getBody().get("version").asLong();
        long count = transactionCount();
        String lastId = tableIds("select max(tran_id) from transaction").get(0);
        jdbc.execute("alter table account add constraint it_no_zero_balance check (acct_id <> " + account
                + " or curr_bal <> 0) not valid");
        try {
            ResponseEntity<JsonNode> failed = call(token, HttpMethod.POST, path, body("Y", version));
            assertThat(failed.getStatusCode()).isEqualTo(HttpStatus.INTERNAL_SERVER_ERROR);
            assertThat(failed.getBody().get("message").asText()).endsWith("Unable to Update Account...");
        } finally {
            jdbc.execute("alter table account drop constraint it_no_zero_balance");
        }
        assertThat(transactionCount()).isEqualTo(count);
        assertThat(tableIds("select max(tran_id) from transaction").get(0)).isEqualTo(lastId);
        assertThat(balance(account)).isEqualByComparingTo(owed);
        assertThat(jdbc.queryForObject("select version from account where acct_id = ?", Long.class, account))
                .isEqualTo(version);
    }
}
