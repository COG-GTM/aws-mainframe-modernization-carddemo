package com.carddemo.online.cocrdslc;

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

/** CCDL end to end: HTTP -> Spring -> H2-backed CARDDAT seeded from app/data/ASCII/carddata.txt. */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class CocrdslcRestTest {

    @LocalServerPort
    int port;

    private final HttpClient http = HttpClient.newHttpClient();

    private String post(String path, String json) throws Exception {
        HttpRequest req = HttpRequest.newBuilder(URI.create("http://localhost:" + port + path))
                .header("Content-Type", "application/json").POST(HttpRequest.BodyPublishers.ofString(json)).build();
        return http.send(req, HttpResponse.BodyHandlers.ofString()).body();
    }

    private static String commareaOf(String body) {
        Matcher m = Pattern.compile("\"commarea\":\"([^\"]+)\"").matcher(body);
        assertThat(m.find()).isTrue();
        return m.group(1);
    }

    @Test
    void cardDetailConversation() throws Exception {
        String first = post("/api/cics/CCDL", "{}");
        assertThat(first).contains("\"outcome\":\"RETURN_TRANSID\"", "\"nextTransid\":\"CCDL\"",
                "Please enter Account and Card Number", "\"cursorField\":\"ACCTSID\"");

        String found = post("/api/cics/CCDL", "{\"aid\":\"ENTER\",\"commarea\":\"" + commareaOf(first)
                + "\",\"fields\":{\"ACCTSID\":\"00000000050\",\"CARDSID\":\"0500024453765740\"}}");
        assertThat(found).contains("Aniya Von", "Displaying requested details", "\"CDEMO-ACCT-ID\":50");

        String missing = post("/api/cics/CCDL", "{\"commarea\":\"" + commareaOf(found)
                + "\",\"fields\":{\"ACCTSID\":\"00000000050\",\"CARDSID\":\"1234123412341234\"}}");
        assertThat(missing).contains("Did not find cards for this search condition");

        String exit = post("/api/cics/CCDL", "{\"aid\":\"PF3\",\"commarea\":\"" + commareaOf(missing) + "\"}");
        assertThat(exit).contains("\"outcome\":\"XCTL\"", "\"nextProgram\":\"COMEN01C\"",
                "\"CDEMO-FROM-PROGRAM\":\"COCRDSLC\"");
    }
}
