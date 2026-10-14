package com.carddemo.online.comen01c;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.copybook.CardDemoCommarea;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.Base64;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;

/** CM00 end-to-end through HTTP and the Spring context (installed programs = base CSD). */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class Comen01cHttpTest {

    @LocalServerPort
    int port;

    private final HttpClient http = HttpClient.newHttpClient();

    private HttpResponse<String> post(String path, String json) throws Exception {
        HttpRequest req = HttpRequest.newBuilder(URI.create("http://localhost:" + port + path))
                .header("Content-Type", "application/json").POST(HttpRequest.BodyPublishers.ofString(json)).build();
        return http.send(req, HttpResponse.BodyHandlers.ofString());
    }

    private static String commarea(int pgmContext) {
        CardDemoCommarea c = new CardDemoCommarea();
        c.setFromTranid("CC00");
        c.setFromProgram("COSGN00C");
        c.setUserId("USER0001");
        c.setUserType("U");
        c.setPgmContext(pgmContext);
        return Base64.getEncoder().encodeToString(c.toBytes());
    }

    @Test
    void menuConversation() throws Exception {
        HttpResponse<String> first = post("/api/cics/CM00", "{\"commarea\":\"" + commarea(0) + "\"}");
        assertThat(first.statusCode()).isEqualTo(200);
        assertThat(first.body()).contains("\"outcome\":\"RETURN_TRANSID\"", "\"nextTransid\":\"CM00\"",
                "\"cursorField\":\"OPTION\"", "01. Account View", "11. Pending Authorization View",
                "ENTER=Continue  F3=Exit");

        HttpResponse<String> view = post("/api/cics/CM00", "{\"aid\":\"ENTER\",\"commarea\":\"" + commarea(1)
                + "\",\"fields\":{\"OPTION\":\"1\"}}");
        assertThat(view.body()).contains("\"outcome\":\"XCTL\"", "\"nextProgram\":\"COACTVWC\"",
                "\"CDEMO-FROM-PROGRAM\":\"COMEN01C\"");

        HttpResponse<String> pauth = post("/api/cics/CM00", "{\"aid\":\"ENTER\",\"commarea\":\"" + commarea(1)
                + "\",\"fields\":{\"OPTION\":\"11\"}}");
        assertThat(pauth.body()).contains("This option Pending Authorization View is not installed...");

        HttpResponse<String> invalid = post("/api/cics/CM00", "{\"aid\":\"ENTER\",\"commarea\":\"" + commarea(1)
                + "\",\"fields\":{\"OPTION\":\"12\"}}");
        assertThat(invalid.body()).contains("Please enter a valid option number...");

        HttpResponse<String> pf3 = post("/api/cics/CM00", "{\"aid\":\"PF3\",\"commarea\":\"" + commarea(1) + "\"}");
        assertThat(pf3.body()).contains("\"outcome\":\"XCTL\"", "\"nextProgram\":\"COSGN00C\"");

        assertThat(post("/api/cics/programs/COMEN01C", "{}").body()).contains("\"nextProgram\":\"COSGN00C\"");
        assertThat(http.send(HttpRequest.newBuilder(URI.create("http://localhost:" + port + "/api/cics/transactions"))
                .GET().build(), HttpResponse.BodyHandlers.ofString()).body()).contains("CM00");
    }
}
