package com.carddemo.online.web;

import static org.assertj.core.api.Assertions.assertThat;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;

/** COACTUPC (CAUP) through HTTP with the H2-backed ACCTDAT / CUSTDAT / CCXREF + CXACAIX seeded from app/data/ASCII. */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class CoactupcRestTest {

    @LocalServerPort
    int port;

    private final HttpClient http = HttpClient.newHttpClient();

    private String post(String path, String json) throws Exception {
        HttpRequest req = HttpRequest.newBuilder(URI.create("http://localhost:" + port + path))
                .header("Content-Type", "application/json").POST(HttpRequest.BodyPublishers.ofString(json)).build();
        HttpResponse<String> r = http.send(req, HttpResponse.BodyHandlers.ofString());
        assertThat(r.statusCode()).isEqualTo(200);
        return r.body();
    }

    private static String commarea(String body) {
        Matcher m = Pattern.compile("\"commarea\":\"([^\"]+)\"").matcher(body);
        assertThat(m.find()).isTrue();
        return m.group(1);
    }

    @Test
    void searchAndShowAccount() throws Exception {
        String first = post("/api/cics/CAUP", "{}");
        assertThat(first).contains("\"outcome\":\"RETURN_TRANSID\"", "\"nextTransid\":\"CAUP\"",
                "Enter or update id of account to update", "\"cursorField\":\"ACCTSID\"");

        String shown = post("/api/cics/CAUP", "{\"aid\":\"ENTER\",\"commarea\":\"" + commarea(first)
                + "\",\"fields\":{\"ACCTSID\":\"00000000001\"}}");
        assertThat(shown).contains("Update account details presented above.", "Immanuel", "Kessler",
                "+        194.00", "+      2,020.00", "\"CDEMO-CUST-ID\":1");

        String notFound = post("/api/cics/programs/COACTUPC", "{\"aid\":\"ENTER\",\"commarea\":\""
                + commarea(first) + "\",\"fields\":{\"ACCTSID\":\"00000000099\"}}");
        assertThat(notFound).contains("Account:00000000099 not found in Cross ref file.");

        String pf3 = post("/api/cics/CAUP", "{\"aid\":\"PF3\",\"commarea\":\"" + commarea(shown) + "\"}");
        assertThat(pf3).contains("\"outcome\":\"XCTL\"", "\"nextProgram\":\"COMEN01C\"",
                "\"CDEMO-FROM-PROGRAM\":\"COACTUPC\"");
    }
}
