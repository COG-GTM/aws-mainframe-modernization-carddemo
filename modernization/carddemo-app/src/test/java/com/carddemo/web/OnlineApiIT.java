package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.load.VsamDatasetLoader;
import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.support.Samples;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.util.List;
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
 * Whole application on PostgreSQL 16 with the sample USRSEC, customer, account, card and xref rows loaded: sign-on
 * for both user types, the menus behind the token, account view/update, anonymous health and the OpenAPI document.
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
            for (Dataset dataset : List.of(Dataset.USRSEC, Dataset.CUSTDATA, Dataset.ACCTDATA, Dataset.CARDDATA,
                    Dataset.CARDXREF)) {
                loader.load(dataset, Samples.read(dataset, RecordEncoding.EBCDIC));
            }
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

    private String userToken() {
        return login("USER0001", "PASSWORD").getBody().get("token").asText();
    }

    private ResponseEntity<JsonNode> account(HttpMethod method, String id, Object body) {
        HttpHeaders headers = new HttpHeaders();
        headers.setBearerAuth(userToken());
        return http.exchange("/api/v1/accounts/" + id, method, new HttpEntity<>(body, headers), JsonNode.class);
    }

    private static ObjectNode set(ObjectNode form, String path, String value) {
        return AccountWebTest.set(form, path, value);
    }

    @Test
    void userViewsAnAccountWithTheCoactvwFieldSet() {
        ResponseEntity<JsonNode> view = account(HttpMethod.GET, "1", null);
        assertThat(view.getStatusCode()).isEqualTo(HttpStatus.OK);
        JsonNode screen = view.getBody();
        assertThat(screen.get("acctId").asText()).isEqualTo("00000000001");
        assertThat(screen.get("header").get("tranId").asText()).isEqualTo("CAVW");
        for (String field : List.of("activeStatus", "currentBalance", "creditLimit", "cashCreditLimit",
                "currentCycleCredit", "currentCycleDebit", "openDate", "expirationDate", "reissueDate", "groupId",
                "custId", "ssn", "ficoScore", "dateOfBirth", "firstName", "middleName", "lastName", "addressLine1",
                "addressLine2", "city", "state", "zip", "country", "phone1", "phone2", "governmentId",
                "eftAccountId", "primaryCardHolder", "cardNum")) {
            assertThat(screen.hasNonNull(field)).as(field).isTrue();
        }
        assertThat(screen.get("cardNumbers")).isNotEmpty();
        assertThat(screen.get("cardNumbers").get(0).asText()).isEqualTo(screen.get("cardNum").asText());
    }

    @Test
    void unknownAccountIsNotInTheCrossReference() {
        ResponseEntity<JsonNode> view = account(HttpMethod.GET, "99999999999", null);
        assertThat(view.getStatusCode()).isEqualTo(HttpStatus.NOT_FOUND);
        assertThat(view.getBody().get("code").asText()).isEqualTo("NOTFND");
        assertThat(view.getBody().get("message").asText())
                .isEqualTo("Account:99999999999 not found in Cross ref file.  Resp:000000013  Reas:0000");
    }

    @Test
    void accountUpdateValidatesCommitsBothRecordsAndDetectsAConcurrentChange() {
        ObjectNode form = (ObjectNode) account(HttpMethod.GET, "2", null).getBody().get("updateForm").deepCopy();
        ResponseEntity<JsonNode> unchanged = account(HttpMethod.PUT, "2", form);
        assertThat(unchanged.getStatusCode()).isEqualTo(HttpStatus.OK);
        assertThat(unchanged.getBody().get("message").asText())
                .isEqualTo("No change detected with respect to values fetched.");

        set(form, "state", "NC");
        set(form, "zip", "27601");
        set(form, "ficoScore", "704");
        set(form, "phone1.areaCode", "908");
        set(form, "phone2.areaCode", "212");
        set(form, "firstName", "Updated");
        set(form, "creditLimit", "$12,345.67");

        set(form, "ficoScore", "200");
        ResponseEntity<JsonNode> invalid = account(HttpMethod.PUT, "2", form);
        assertThat(invalid.getStatusCode()).isEqualTo(HttpStatus.BAD_REQUEST);
        assertThat(invalid.getBody().get("field").asText()).isEqualTo("ficoScore");
        assertThat(invalid.getBody().get("message").asText()).isEqualTo("FICO Score: should be between 300 and 850");
        set(form, "ficoScore", "704");

        ResponseEntity<JsonNode> validated = account(HttpMethod.PUT, "2", form);
        assertThat(validated.getBody().get("state").asText()).isEqualTo("VALIDATED");
        assertThat(account(HttpMethod.GET, "2", null).getBody().get("accountVersion").asLong()).isZero();

        ResponseEntity<JsonNode> committed = account(HttpMethod.PUT, "2", form.deepCopy().put("confirm", true));
        assertThat(committed.getStatusCode()).isEqualTo(HttpStatus.OK);
        assertThat(committed.getBody().get("state").asText()).isEqualTo("COMMITTED");
        JsonNode after = account(HttpMethod.GET, "2", null).getBody();
        assertThat(after.get("firstName").asText()).isEqualTo("Updated");
        assertThat(after.get("creditLimit").decimalValue()).isEqualByComparingTo("12345.67");
        assertThat(after.get("accountVersion").asLong()).isEqualTo(1);
        assertThat(after.get("customerVersion").asLong()).isEqualTo(1);

        ResponseEntity<JsonNode> stale = account(HttpMethod.PUT, "2",
                set(form.deepCopy().put("confirm", true), "lastName", "Other"));
        assertThat(stale.getStatusCode()).isEqualTo(HttpStatus.CONFLICT);
        assertThat(stale.getBody().get("code").asText()).isEqualTo("CHANGED");
        assertThat(stale.getBody().get("message").asText()).isEqualTo("Record changed by some one else. Please review");
        assertThat(account(HttpMethod.GET, "2", null).getBody().get("lastName").asText())
                .isEqualTo(after.get("lastName").asText());
    }

    @Test
    void openApiDocumentsTheAccountEndpoints() throws Exception {
        JsonNode doc = json.readTree(http.getForEntity("/v3/api-docs", String.class).getBody());
        JsonNode path = doc.at("/paths/~1api~1v1~1accounts~1{id}");
        assertThat(path.at("/get/responses/404/content/application~1problem+json/examples/notInCrossReference/value"
                + "/code").asText()).isEqualTo("NOTFND");
        assertThat(path.at("/put/responses/409/content/application~1problem+json/examples/changedByAnotherUser/value"
                + "/message").asText()).isEqualTo("Record changed by some one else. Please review");
        assertThat(path.at("/put/responses/400/content/application~1problem+json/examples/ficoRange/value/field")
                .asText()).isEqualTo("ficoScore");
        assertThat(path.at("/put/security").isMissingNode()).isFalse();
        assertThat(doc.at("/components/schemas/AccountUpdateRequest/properties/confirm").isMissingNode()).isFalse();
        assertThat(doc.at("/components/schemas/AccountViewScreen/properties/cardNumbers").isMissingNode()).isFalse();
    }
}
