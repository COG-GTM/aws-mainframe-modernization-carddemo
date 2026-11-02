package com.carddemo.batch.tranrept;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.batch.DdFiles;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.layout.CardXrefRecordLayout;
import com.carddemo.common.record.layout.TranCatRecordLayout;
import com.carddemo.common.record.layout.TranRecordLayout;
import com.carddemo.common.record.layout.TranTypeRecordLayout;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import com.carddemo.common.security.TokenService;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;

/**
 * TRANREPT submitted over HTTP for a date range, with no DD files: transactions, CARDXREF, TRANTYPE
 * and TRANCATG come from stand-ins of card-service and transaction-service that serve the sample
 * case's data in the services' JSON shape. The report must equal the COBOL report of the same data.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class TranreptServicesTest {

    private static final TranreptCases SAMPLE = new TranreptCases(TranreptCases.ROOT.resolve("sample"));
    private static final ObjectMapper JSON = new ObjectMapper().findAndRegisterModules();
    private static final AtomicInteger XREF_CALLS = new AtomicInteger();
    private static final HttpServer SERVICES = start();

    @Autowired
    private TestRestTemplate http;

    @Autowired
    private TokenService tokens;

    /** Every call the job made to the stand-in services carried this Authorization header. */
    private static final java.util.Set<String> FORWARDED = java.util.concurrent.ConcurrentHashMap.newKeySet();

    private HttpEntity<Void> signedOn() {
        HttpHeaders headers = new HttpHeaders();
        headers.setBearerAuth(tokens.issue("ADMIN001", "A").token());
        return new HttpEntity<>(headers);
    }

    @DynamicPropertySource
    static void serviceUrls(DynamicPropertyRegistry registry) {
        String url = "http://localhost:" + SERVICES.getAddress().getPort();
        registry.add("carddemo.services.card-url", () -> url);
        registry.add("carddemo.services.transaction-url", () -> url);
    }

    @AfterAll
    static void stop() {
        SERVICES.stop(0);
    }

    @Test
    void reportFromTheServicesEqualsTheCobolReport() throws IOException {
        XREF_CALLS.set(0);
        FORWARDED.clear();
        HttpEntity<Void> request = signedOn();
        ResponseEntity<Map> response = http.postForEntity(
                "/api/v1/batch/transaction-report?startDate=2022-01-01&endDate=2022-07-06", request, Map.class);

        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
        @SuppressWarnings("unchecked")
        Map<String, Object> body = response.getBody();
        assertThat(body).containsEntry("job", "TRANREPT").containsEntry("returnCode", 0);
        @SuppressWarnings("unchecked")
        Map<String, Object> results = (Map<String, Object>) body.get("results");
        assertThat(results).containsEntry("TRANSACT", "transaction-service")
                .containsEntry("TRANSACT.BKUP", 312).containsEntry("reported", 312);
        Path outputDir = Path.of((String) body.get("outputDir"));
        assertThat(new String(Files.readAllBytes(outputDir.resolve("TRANREPT")), StandardCharsets.ISO_8859_1))
                .isEqualTo(new String(SAMPLE.expectedReport().orElseThrow(), StandardCharsets.ISO_8859_1));
        // CARDXREF is read on a card change only: 50 cards, sorted.
        assertThat(XREF_CALLS.get()).isEqualTo(50);
        // The caller's token went on to card-service and transaction-service.
        assertThat(FORWARDED).containsExactly(request.getHeaders().getFirst(HttpHeaders.AUTHORIZATION));
    }

    @Test
    void submitNeedsABearerToken() throws Exception {
        // java.net.http: TestRestTemplate's JDK client cannot read a 401 on a streamed POST.
        java.net.http.HttpResponse<String> response = java.net.http.HttpClient.newHttpClient().send(
                java.net.http.HttpRequest.newBuilder(java.net.URI.create(http.getRootUri()
                                + "/api/v1/batch/transaction-report?startDate=2022-01-01&endDate=2022-07-06"))
                        .POST(java.net.http.HttpRequest.BodyPublishers.noBody()).build(),
                java.net.http.HttpResponse.BodyHandlers.ofString());
        assertThat(response.statusCode()).isEqualTo(401);
    }

    @Test
    void bothDatesAreRequired() {
        ResponseEntity<String> response = http.postForEntity(
                "/api/v1/batch/transaction-report?startDate=2022-01-01", signedOn(), String.class);
        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.BAD_REQUEST);
    }

    private static HttpServer start() {
        try {
            List<CobolRecord> xref = DdFiles.read(SAMPLE.reference("CARDXREF"), CardXrefRecordLayout.LAYOUT, CodePage.ASCII);
            List<Map<String, Object>> types = DdFiles.read(SAMPLE.reference("TRANTYPE"), TranTypeRecordLayout.LAYOUT,
                    CodePage.ASCII).stream().map(r -> row("typeCode", r.get(TranTypeRecordLayout.TRAN_TYPE),
                    "description", r.getTrimmed(TranTypeRecordLayout.TRAN_TYPE_DESC))).toList();
            List<Map<String, Object>> categories = DdFiles.read(SAMPLE.reference("TRANCATG"), TranCatRecordLayout.LAYOUT,
                    CodePage.ASCII).stream().map(r -> row("typeCode", r.get(TranCatRecordLayout.TRAN_TYPE_CD),
                    "categoryCode", r.getInt(TranCatRecordLayout.TRAN_CAT_CD),
                    "description", r.getTrimmed(TranCatRecordLayout.TRAN_CAT_TYPE_DESC))).toList();
            List<Map<String, Object>> transactions = SAMPLE.transactions().stream()
                    .sorted(java.util.Comparator.comparing(r -> r.get(TranRecordLayout.TRAN_ID)))
                    .map(TranreptServicesTest::transaction).toList();

            HttpServer server = HttpServer.create(new InetSocketAddress("localhost", 0), 0);
            server.createContext("/api/v1/cards/", exchange -> {
                XREF_CALLS.incrementAndGet();
                String card = exchange.getRequestURI().getPath().split("/")[4];
                var match = xref.stream().filter(r -> r.getTrimmed(CardXrefRecordLayout.XREF_CARD_NUM).equals(card)).findFirst();
                if (match.isEmpty()) {
                    send(exchange, 404, Map.of("status", 404));
                } else {
                    send(exchange, 200, row("cardNumber", card,
                            "customerId", String.format("%09d", match.get().getLong(CardXrefRecordLayout.XREF_CUST_ID)),
                            "accountId", String.format("%011d", match.get().getLong(CardXrefRecordLayout.XREF_ACCT_ID))));
                }
            });
            server.createContext("/api/v1/transaction-types", exchange -> send(exchange, 200, page(exchange, types)));
            server.createContext("/api/v1/transaction-categories", exchange -> send(exchange, 200, page(exchange, categories)));
            server.createContext("/api/v1/transactions", exchange -> send(exchange, 200, page(exchange, transactions)));
            server.start();
            return server;
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
    }

    private static Map<String, Object> transaction(CobolRecord r) {
        return row("transactionId", r.get(TranRecordLayout.TRAN_ID),
                "cardNumber", r.get(TranRecordLayout.TRAN_CARD_NUM),
                "typeCode", r.get(TranRecordLayout.TRAN_TYPE_CD),
                "categoryCode", r.getInt(TranRecordLayout.TRAN_CAT_CD),
                "source", r.getTrimmed(TranRecordLayout.TRAN_SOURCE),
                "description", r.getTrimmed(TranRecordLayout.TRAN_DESC),
                "amount", r.get(TranRecordLayout.TRAN_AMT),
                "merchantId", String.format("%09d", r.getLong(TranRecordLayout.TRAN_MERCHANT_ID)),
                "merchantName", r.getTrimmed(TranRecordLayout.TRAN_MERCHANT_NAME),
                "merchantCity", r.getTrimmed(TranRecordLayout.TRAN_MERCHANT_CITY),
                "merchantZip", r.getTrimmed(TranRecordLayout.TRAN_MERCHANT_ZIP),
                "originTimestamp", timestamp(r.get(TranRecordLayout.TRAN_ORIG_TS)),
                "processTimestamp", timestamp(r.get(TranRecordLayout.TRAN_PROC_TS)));
    }

    /** Both TRANSACT timestamp layouts ({@code yyyy-MM-dd HH:mm:ss.SSSSSS}, DB2 {@code yyyy-MM-dd-HH.mm.ss.SSSSSS}). */
    private static String timestamp(String text) {
        String pattern = text.charAt(10) == '-' ? "yyyy-MM-dd-HH.mm.ss.SSSSSS" : "yyyy-MM-dd HH:mm:ss.SSSSSS";
        return LocalDateTime.parse(text.strip(), DateTimeFormatter.ofPattern(pattern)).toString();
    }

    private static Map<String, Object> page(HttpExchange exchange, List<Map<String, Object>> rows) {
        Map<String, String> query = new LinkedHashMap<>();
        for (String pair : exchange.getRequestURI().getQuery().split("&")) {
            query.put(pair.split("=")[0], pair.split("=")[1]);
        }
        int page = Integer.parseInt(query.get("page"));
        int size = Integer.parseInt(query.get("size"));
        List<Map<String, Object>> content = new ArrayList<>(rows.subList(Math.min(page * size, rows.size()),
                Math.min((page + 1) * size, rows.size())));
        return row("content", content, "page", page, "size", size, "totalElements", rows.size(),
                "totalPages", (rows.size() + size - 1) / size);
    }

    private static Map<String, Object> row(Object... keyValues) {
        Map<String, Object> row = new LinkedHashMap<>();
        for (int i = 0; i < keyValues.length; i += 2) {
            row.put((String) keyValues[i], keyValues[i + 1]);
        }
        return row;
    }

    private static void send(HttpExchange exchange, int status, Object body) throws IOException {
        FORWARDED.add(String.valueOf(exchange.getRequestHeaders().getFirst("Authorization")));
        byte[] bytes = JSON.writeValueAsBytes(body);
        exchange.getResponseHeaders().add("Content-Type", "application/json");
        exchange.sendResponseHeaders(status, bytes.length);
        exchange.getResponseBody().write(bytes);
        exchange.close();
    }
}
