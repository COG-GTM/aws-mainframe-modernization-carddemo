package com.carddemo.online.web;

import static org.assertj.core.api.Assertions.assertThat;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.Base64;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;

/** End-to-end through HTTP, Spring context and the H2-backed USRSEC file. */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class CicsTransactionControllerTest {

    @LocalServerPort
    int port;

    private final HttpClient http = HttpClient.newHttpClient();

    private HttpResponse<String> post(String path, String json) throws Exception {
        HttpRequest req = HttpRequest.newBuilder(URI.create("http://localhost:" + port + path))
                .header("Content-Type", "application/json").POST(HttpRequest.BodyPublishers.ofString(json)).build();
        return http.send(req, HttpResponse.BodyHandlers.ofString());
    }

    private static String commarea() {
        return Base64.getEncoder().encodeToString(new byte[160]);
    }

    @Test
    void signOnConversation() throws Exception {
        HttpResponse<String> first = post("/api/cics/CC00", "{}");
        assertThat(first.statusCode()).isEqualTo(200);
        assertThat(first.body()).contains("\"outcome\":\"RETURN_TRANSID\"", "\"nextTransid\":\"CC00\"",
                "\"cursorField\":\"USERID\"", "AWS Mainframe Modernization");

        HttpResponse<String> admin = post("/api/cics/CC00", "{\"aid\":\"ENTER\",\"commarea\":\"" + commarea()
                + "\",\"fields\":{\"USERID\":\"admin001\",\"PASSWD\":\"password\"}}");
        assertThat(admin.body()).contains("\"outcome\":\"XCTL\"", "\"nextProgram\":\"COADM01C\"",
                "\"CDEMO-USER-ID\":\"ADMIN001\"", "\"CDEMO-USER-TYPE\":\"A\"", "\"CDEMO-FROM-PROGRAM\":\"COSGN00C\"");

        HttpResponse<String> user = post("/api/cics/cc00", "{\"commarea\":\"" + commarea()
                + "\",\"fields\":{\"USERID\":\"USER0001\",\"PASSWD\":\"PASSWORD\"}}");
        assertThat(user.body()).contains("\"nextProgram\":\"COMEN01C\"");

        HttpResponse<String> wrong = post("/api/cics/CC00", "{\"commarea\":\"" + commarea()
                + "\",\"fields\":{\"USERID\":\"USER0001\",\"PASSWD\":\"nope\"}}");
        assertThat(wrong.body()).contains("Wrong Password. Try again ...", "\"cursorField\":\"PASSWD\"");

        HttpResponse<String> pf3 = post("/api/cics/CC00", "{\"aid\":\"PF3\",\"commarea\":\"" + commarea() + "\"}");
        assertThat(pf3.body()).contains("\"outcome\":\"RETURN\"", "Thank you for using CardDemo application...");
    }

    @Test
    void cardDetailFromCardList() throws Exception {
        com.carddemo.common.copybook.CardDemoCommarea c = new com.carddemo.common.copybook.CardDemoCommarea();
        c.initializeSpaces();
        c.setFromProgram("COCRDLIC");
        c.setFromTranid("CCLI");
        c.setLastMapset("COCRDLI");
        c.setPgmContext(0);
        com.carddemo.common.copybook.CardDemoCommarea.CUST_ID.set(c, 0);
        com.carddemo.common.copybook.CardDemoCommarea.ACCT_ID.set(c, 50);
        com.carddemo.common.copybook.CardDemoCommarea.CARD_NUM.set(c, new java.math.BigDecimal("0500024453765740"));
        String ca = Base64.getEncoder().encodeToString(c.toBytes());
        HttpResponse<String> shown = post("/api/cics/CCDL", "{\"commarea\":\"" + ca + "\"}");
        assertThat(shown.statusCode()).isEqualTo(200);
        assertThat(shown.body()).contains("\"outcome\":\"RETURN_TRANSID\"", "\"nextTransid\":\"CCDL\"", "Aniya Von",
                "Displaying requested details");

        HttpResponse<String> back = post("/api/cics/CCDL", "{\"aid\":\"PF3\",\"commarea\":\"" + ca + "\"}");
        assertThat(back.body()).contains("\"outcome\":\"XCTL\"", "\"nextProgram\":\"COCRDLIC\"");

        c.setFromProgram("COMEN01C");
        c.setLastMapset("COMEN01");
        c.setPgmContext(1);
        String menu = Base64.getEncoder().encodeToString(c.toBytes());
        HttpResponse<String> bad = post("/api/cics/CCDL", "{\"commarea\":\"" + menu
                + "\",\"fields\":{\"ACCTSID\":\"ABC\",\"CARDSID\":\"0500024453765740\"}}");
        assertThat(bad.body()).contains("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER")
                .containsPattern("\"name\":\"ACCTSID\"[^}]*\"color\":\"RED\"");
    }

    @Test
    void errors() throws Exception {
        assertThat(post("/api/cics/ZZZZ", "{}").statusCode()).isEqualTo(404);
        assertThat(post("/api/cics/programs/COADM01C", "{}").statusCode()).isEqualTo(404);
        assertThat(post("/api/cics/CC00", "{\"aid\":\"PF99\"}").statusCode()).isEqualTo(400);
        assertThat(post("/api/cics/CC00", "{\"commarea\":\"" + commarea() + "\",\"fields\":{\"BOGUS\":\"x\"}}")
                .statusCode()).isEqualTo(400);
        assertThat(post("/api/cics/programs/COSGN00C", "{}").body()).contains("\"program\":\"COSGN00C\"");
        HttpRequest get = HttpRequest.newBuilder(URI.create("http://localhost:" + port + "/api/cics/transactions")).build();
        assertThat(http.send(get, HttpResponse.BodyHandlers.ofString()).body()).contains("\"CC00\":\"COSGN00C\"", "\"CCDL\":\"COCRDSLC\"");
    }
}
