package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.load.VsamDatasetLoader;
import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.support.Samples;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
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
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

/**
 * Whole application on PostgreSQL 16 with the sample USRSEC rows loaded: sign-on for both user types, the menus
 * behind the token, anonymous health and the OpenAPI document of the new endpoints.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Testcontainers
class OnlineApiIT {

    @Container
    @ServiceConnection
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine");

    @Autowired
    TestRestTemplate http;

    @Autowired
    VsamDatasetLoader loader;

    @Autowired
    ObjectMapper json;

    private static boolean loaded;

    @BeforeEach
    void loadUsrsecOnce() {
        // HttpURLConnection cannot read a 401 body; the JDK HttpClient can
        http.getRestTemplate().setRequestFactory(new JdkClientHttpRequestFactory());
        if (!loaded) {
            loader.load(Dataset.USRSEC, Samples.read(Dataset.USRSEC, RecordEncoding.EBCDIC));
            loaded = true;
        }
    }

    private ResponseEntity<JsonNode> login(String userId, String password) {
        return http.postForEntity("/api/v1/auth/login", Map.of("userId", userId, "password", password),
                JsonNode.class);
    }

    private ResponseEntity<JsonNode> menu(String token) {
        HttpHeaders headers = new HttpHeaders();
        headers.setBearerAuth(token);
        return http.exchange("/api/v1/menu", HttpMethod.GET, new HttpEntity<>(headers), JsonNode.class);
    }

    @Test
    void sampleAdministratorSignsOnAndGetsTheAdminMenu() {
        ResponseEntity<JsonNode> login = login("admin001", "password");
        assertThat(login.getStatusCode()).isEqualTo(HttpStatus.OK);
        assertThat(login.getBody().get("role").asText()).isEqualTo("ADMIN");
        assertThat(login.getBody().get("targetMenu").asText()).isEqualTo("COADM01C");
        JsonNode menu = menu(login.getBody().get("token").asText()).getBody();
        assertThat(menu.get("programId").asText()).isEqualTo("COADM01C");
        assertThat(menu.get("optionLines").get(0).asText()).isEqualTo("01. User List (Security)");
    }

    @Test
    void sampleUserSignsOnAndGetsTheMainMenu() {
        ResponseEntity<JsonNode> login = login("USER0001", "PASSWORD");
        assertThat(login.getStatusCode()).isEqualTo(HttpStatus.OK);
        assertThat(login.getBody().get("role").asText()).isEqualTo("USER");
        assertThat(login.getBody().get("targetMenu").asText()).isEqualTo("COMEN01C");
        JsonNode menu = menu(login.getBody().get("token").asText()).getBody();
        assertThat(menu.get("programId").asText()).isEqualTo("COMEN01C");
        assertThat(menu.get("options")).hasSize(11);
    }

    @Test
    void wrongPasswordAndUnknownUserUseTheCobolMessages() {
        ResponseEntity<JsonNode> wrong = login("USER0001", "WRONG");
        assertThat(wrong.getStatusCode()).isEqualTo(HttpStatus.UNAUTHORIZED);
        assertThat(wrong.getBody().get("message").asText()).isEqualTo("Wrong Password. Try again ...");
        ResponseEntity<JsonNode> unknown = login("NOBODY", "PASSWORD");
        assertThat(unknown.getStatusCode()).isEqualTo(HttpStatus.UNAUTHORIZED);
        assertThat(unknown.getBody().get("message").asText()).isEqualTo("User not found. Try again ...");
    }

    @Test
    void healthStaysAnonymous() {
        assertThat(http.getForEntity("/actuator/health/readiness", String.class).getStatusCode())
                .isEqualTo(HttpStatus.OK);
    }

    @Test
    void openApiDocumentsLoginAndMenuWithExamplesAndBearerScheme() throws Exception {
        ResponseEntity<String> response = http.getForEntity("/v3/api-docs", String.class);
        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
        JsonNode doc = json.readTree(response.getBody());
        JsonNode login = doc.at("/paths/~1api~1v1~1auth~1login/post");
        assertThat(login.isMissingNode()).isFalse();
        JsonNode examples = login.at("/requestBody/content/application~1json/examples");
        assertThat(examples.has("admin")).isTrue();
        assertThat(examples.has("wrongPassword")).isTrue();
        assertThat(login.at("/responses/401").isMissingNode()).isFalse();
        assertThat(login.at("/responses/401/content/application~1problem+json/examples/wrongPassword/value/code")
                .asText()).isEqualTo("WRONG_PASSWORD");
        assertThat(login.at("/responses/400/content/application~1problem+json/examples/blankUserId/value/message")
                .asText()).isEqualTo("Please enter User ID ...");
        assertThat(doc.at("/paths/~1api~1v1~1menu/get/responses/401/content/application~1problem+json/examples"
                + "/signOnRequired/value/code").asText()).isEqualTo("SIGNON_REQUIRED");
        assertThat(doc.at("/paths/~1api~1v1~1menu~1{menu}~1selection/post/responses/403/content"
                + "/application~1problem+json/examples/adminOnly/value/message").asText())
                .isEqualTo("No access - Admin Only option...");
        assertThat(doc.at("/paths/~1api~1v1~1menu/get").isMissingNode()).isFalse();
        assertThat(doc.at("/paths/~1api~1v1~1menu~1{menu}~1selection/post/requestBody/content/application~1json"
                + "/examples/accountView").isMissingNode()).isFalse();
        assertThat(doc.at("/components/securitySchemes/bearerAuth/scheme").asText()).isEqualTo("bearer");
        assertThat(doc.at("/components/schemas/ApiError/properties/code").isMissingNode()).isFalse();
        assertThat(doc.at("/components/schemas/NavigationContext/properties/cardNum").isMissingNode()).isFalse();
    }
}
