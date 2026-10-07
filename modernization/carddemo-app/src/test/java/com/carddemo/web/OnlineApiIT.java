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

    private static final List<String> FIRST_PAGE = List.of("5740", "1516", "7330", "6232", "9795", "4350", "8931");
    private static final List<String> SECOND_PAGE = List.of("1600", "2090", "7565", "2490", "0449", "4312", "7560");

    private ResponseEntity<JsonNode> cards(String token, HttpMethod method, String pathAndQuery, Object body) {
        HttpHeaders headers = new HttpHeaders();
        headers.setBearerAuth(token);
        return http.exchange("/api/v1/cards" + pathAndQuery, method, new HttpEntity<>(body, headers), JsonNode.class);
    }

    private String adminToken() {
        return login("ADMIN001", "PASSWORD").getBody().get("token").asText();
    }

    private static List<String> lastFour(JsonNode page) {
        List<String> digits = new java.util.ArrayList<>();
        page.get("rows").forEach(r -> {
            String masked = r.get("cardNumber").asText();
            assertThat(masked).startsWith("************").hasSize(16);
            digits.add(masked.substring(12));
        });
        return digits;
    }

    @Test
    void cardListPagesSevenRowsInTheBaselineOrderForwardAndBack() {
        String admin = adminToken();
        JsonNode first = cards(admin, HttpMethod.GET, "", null).getBody();
        assertThat(lastFour(first)).isEqualTo(FIRST_PAGE);
        assertThat(first.get("hasPreviousPage").asBoolean()).isFalse();
        JsonNode second = cards(admin, HttpMethod.GET, "?after=" + first.get("nextPage").asText(), null).getBody();
        assertThat(lastFour(second)).isEqualTo(SECOND_PAGE);
        JsonNode back = cards(admin, HttpMethod.GET, "?before=" + second.get("previousPage").asText(), null).getBody();
        assertThat(lastFour(back)).isEqualTo(FIRST_PAGE);

        JsonNode page = first;
        int total = 0;
        while (true) {
            total += page.get("rows").size();
            if (!page.get("hasNextPage").asBoolean()) {
                break;
            }
            page = cards(admin, HttpMethod.GET, "?after=" + page.get("nextPage").asText(), null).getBody();
        }
        assertThat(total).isEqualTo(50);
        assertThat(page.get("rows")).hasSize(1);
        assertThat(page.get("message").asText()).isEqualTo("NO MORE RECORDS TO SHOW");
    }

    @Test
    void userIsRestrictedToTheAccountInContextAdminSeesAll() {
        ResponseEntity<JsonNode> noAccount = cards(userToken(), HttpMethod.GET, "", null);
        assertThat(noAccount.getStatusCode()).isEqualTo(HttpStatus.FORBIDDEN);
        assertThat(noAccount.getBody().get("code").asText()).isEqualTo("NOTAUTH");
        JsonNode filtered = cards(userToken(), HttpMethod.GET, "?accountId=50", null).getBody();
        assertThat(lastFour(filtered)).containsExactly("5740");
        assertThat(filtered.get("rows").get(0).get("accountId").asText()).isEqualTo("00000000050");
        assertThat(cards(adminToken(), HttpMethod.GET, "?accountId=99", null).getBody().get("rows")).isEmpty();
        ResponseEntity<JsonNode> otherAccount = cards(userToken(), HttpMethod.GET,
                "/0500024453765740?accountId=27", null);
        assertThat(otherAccount.getStatusCode()).isEqualTo(HttpStatus.NOT_FOUND);
    }

    @Test
    void cardDetailByReferenceAndByAccountPath() {
        JsonNode first = cards(adminToken(), HttpMethod.GET, "", null).getBody();
        String ref = first.get("rows").get(0).get("cardRef").asText();
        JsonNode detail = cards(userToken(), HttpMethod.GET, "/" + ref + "?accountId=50&fromProgram=COCRDLIC", null)
                .getBody();
        assertThat(detail.get("cardNumber").asText()).isEqualTo("0500024453765740");
        assertThat(detail.get("embossedName").asText()).isEqualTo("Aniya Von");
        assertThat(detail.get("expiryYear").asText()).isEqualTo("2023");
        assertThat(detail.get("exit").get("toProgram").asText()).isEqualTo("COCRDLIC");
        JsonNode byAccount = cards(userToken(), HttpMethod.GET, "/by-account/27", null).getBody();
        assertThat(byAccount.get("cardNumber").asText()).isEqualTo("0683586198171516");
        assertThat(cards(userToken(), HttpMethod.GET, "/9999999999999999?accountId=1", null).getStatusCode())
                .isEqualTo(HttpStatus.NOT_FOUND);
    }

    @Test
    void cardUpdateValidatesCommitsAndDetectsAConcurrentChange() {
        String card = "/0683586198171516";
        ObjectNode form = (ObjectNode) cards(userToken(), HttpMethod.GET, card + "?accountId=27", null).getBody()
                .get("updateForm").deepCopy();
        ResponseEntity<JsonNode> invalid = cards(userToken(), HttpMethod.PUT, card,
                form.deepCopy().put("embossedName", "Ward J0nes"));
        assertThat(invalid.getStatusCode()).isEqualTo(HttpStatus.BAD_REQUEST);
        assertThat(invalid.getBody().get("message").asText())
                .isEqualTo("Card name can only contain alphabets and spaces");

        form.put("embossedName", "Ward B Jones").put("expiryMonth", "12").put("expiryYear", "2029");
        assertThat(cards(userToken(), HttpMethod.PUT, card, form).getBody().get("state").asText())
                .isEqualTo("VALIDATED");
        ResponseEntity<JsonNode> committed = cards(userToken(), HttpMethod.PUT, card,
                form.deepCopy().put("confirm", true));
        assertThat(committed.getStatusCode()).isEqualTo(HttpStatus.OK);
        assertThat(committed.getBody().get("state").asText()).isEqualTo("COMMITTED");
        JsonNode after = cards(userToken(), HttpMethod.GET, card + "?accountId=27", null).getBody();
        assertThat(after.get("embossedName").asText()).isEqualTo("Ward B Jones");
        assertThat(after.get("expiryMonth").asText()).isEqualTo("12");
        assertThat(after.get("expiryYear").asText()).isEqualTo("2029");
        assertThat(after.get("version").asLong()).isEqualTo(1);

        ResponseEntity<JsonNode> stale = cards(userToken(), HttpMethod.PUT, card,
                form.deepCopy().put("activeStatus", "N").put("confirm", true));
        assertThat(stale.getStatusCode()).isEqualTo(HttpStatus.CONFLICT);
        assertThat(stale.getBody().get("code").asText()).isEqualTo("CHANGED");
        assertThat(cards(userToken(), HttpMethod.GET, card + "?accountId=27", null).getBody().get("activeStatus")
                .asText()).isEqualTo("Y");
    }

    @Test
    void openApiDocumentsTheCardEndpoints() throws Exception {
        JsonNode doc = json.readTree(http.getForEntity("/v3/api-docs", String.class).getBody());
        JsonNode list = doc.at("/paths/~1api~1v1~1cards/get");
        assertThat(list.at("/security").isMissingNode()).isFalse();
        assertThat(list.at("/responses/403/content/application~1problem+json/examples/userWithoutAccount/value/code")
                .asText()).isEqualTo("NOTAUTH");
        JsonNode card = doc.at("/paths/~1api~1v1~1cards~1{cardNumber}");
        assertThat(card.at("/get/responses/404/content/application~1problem+json/examples/cardNotFound/value/code")
                .asText()).isEqualTo("NOTFND");
        assertThat(card.at("/put/responses/409/content/application~1problem+json/examples/changedByAnotherUser/value"
                + "/code").asText()).isEqualTo("CHANGED");
        assertThat(card.at("/put/responses/400/content/application~1problem+json/examples/nameNotAlphabetic/value"
                + "/field").asText()).isEqualTo("embossedName");
        assertThat(doc.at("/paths/~1api~1v1~1cards~1by-account~1{accountId}/get").isMissingNode()).isFalse();
        JsonNode selection = doc.at("/paths/~1api~1v1~1cards~1selection/post/responses");
        assertThat(selection.at("/403/content/application~1problem+json/examples/userWithoutAccount/value/code")
                .asText()).isEqualTo("NOTAUTH");
        assertThat(selection.at("/404/content/application~1problem+json/examples/cardOfAnotherAccount/value/code")
                .asText()).isEqualTo("NOTFND");
        assertThat(doc.at("/components/schemas/CardListRow/properties/cardRef").isMissingNode()).isFalse();
        assertThat(doc.at("/components/schemas/CardUpdateRequest/properties/version").isMissingNode()).isFalse();
    }
}
