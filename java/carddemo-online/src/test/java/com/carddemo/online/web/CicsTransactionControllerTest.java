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
    void errors() throws Exception {
        assertThat(post("/api/cics/ZZZZ", "{}").statusCode()).isEqualTo(404);
        assertThat(post("/api/cics/programs/COADM01C", "{}").statusCode()).isEqualTo(404);
        assertThat(post("/api/cics/CC00", "{\"aid\":\"PF99\"}").statusCode()).isEqualTo(400);
        assertThat(post("/api/cics/CC00", "{\"commarea\":\"" + commarea() + "\",\"fields\":{\"BOGUS\":\"x\"}}")
                .statusCode()).isEqualTo(400);
        assertThat(post("/api/cics/programs/COSGN00C", "{}").body()).contains("\"program\":\"COSGN00C\"");
        HttpRequest get = HttpRequest.newBuilder(URI.create("http://localhost:" + port + "/api/cics/transactions")).build();
        assertThat(http.send(get, HttpResponse.BodyHandlers.ofString()).body()).contains("\"CC00\":\"COSGN00C\"");
    }

    @Test
    void cardListConversation() throws Exception {
        // COCRDLIC over the H2-backed CARDDAT seeded from app/data/ASCII/carddata.txt
        HttpResponse<String> first = post("/api/cics/CCLI", "{}");
        assertThat(first.body()).contains("\"program\":\"COCRDLIC\"", "\"outcome\":\"RETURN_TRANSID\"",
                "\"nextTransid\":\"CCLI\"", "0500024453765740", "\"WS-CA-NEXT-PAGE-IND\":\"Y\"",
                "TYPE S FOR DETAIL, U TO UPDATE ANY RECORD");
        String commarea = first.body().replaceAll("(?s).*\"commarea\":\"([^\"]+)\".*", "$1");

        HttpResponse<String> pf8 = post("/api/cics/CCLI", "{\"aid\":\"PF8\",\"commarea\":\"" + commarea + "\"}");
        assertThat(pf8.body()).contains("1561409106491600", "\"WS-CA-SCREEN-NUM\":2");

        HttpResponse<String> view = post("/api/cics/CCLI", "{\"commarea\":\"" + commarea
                + "\",\"fields\":{\"CRDSEL2\":\"S\"}}");
        assertThat(view.body()).contains("\"outcome\":\"XCTL\"", "\"nextProgram\":\"COCRDSLC\"",
                "\"CDEMO-CARD-NUM\":683586198171516", "\"CDEMO-ACCT-ID\":27");
    }
}
