package com.carddemo.web;

import com.carddemo.account.online.AccountLookup;
import com.carddemo.account.online.AccountUpdateEdits;
import com.carddemo.account.online.AccountUpdateService;
import com.carddemo.card.online.CardKeys;
import com.carddemo.card.online.CardLookup;
import com.carddemo.card.online.CardSelection;
import com.carddemo.card.online.CardUpdateEdits;
import com.carddemo.card.online.CardUpdateService;
import com.carddemo.user.menu.MenuService;
import com.carddemo.user.signon.SignOnService;
import com.carddemo.web.card.CardController;
import com.carddemo.web.security.ProblemResponses;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.examples.Example;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.Content;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.oas.models.security.SecurityScheme;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.springdoc.core.customizers.OpenApiCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpStatus;

/**
 * OpenAPI document ({@code /v3/api-docs}, Swagger UI at {@code /swagger-ui.html}) with the bearer-token scheme, a
 * 401 {@code SIGNON_REQUIRED} response on every secured operation and status-specific error examples.
 */
@Configuration(proxyBeanMethods = false)
public class OpenApiConfiguration {

    public static final String BEARER = "bearerAuth";

    private static final String PROBLEM_JSON = "application/problem+json";
    private static final String API_ERROR_REF = "#/components/schemas/ApiError";

    private static final Map<String, List<ErrorExample>> EXAMPLES = Map.of(
            "POST /api/v1/auth/login 400", List.of(
                    new ErrorExample("blankUserId", "INVREQ", SignOnService.USER_ID_FIELD,
                            SignOnService.MSG_USER_ID_BLANK),
                    new ErrorExample("blankPassword", "INVREQ", SignOnService.PASSWORD_FIELD,
                            SignOnService.MSG_PASSWORD_BLANK)),
            "POST /api/v1/auth/login 401", List.of(
                    new ErrorExample("unknownUser", "NOTFND", SignOnService.USER_ID_FIELD,
                            SignOnService.MSG_USER_NOT_FOUND),
                    new ErrorExample("wrongPassword", "WRONG_PASSWORD", SignOnService.PASSWORD_FIELD,
                            SignOnService.MSG_WRONG_PASSWORD)),
            "POST /api/v1/auth/login 500", List.of(
                    new ErrorExample("unableToVerify", "OTHER", null, SignOnService.MSG_UNABLE_TO_VERIFY)),
            "GET /api/v1/menu/{menu} 403", List.of(
                    new ErrorExample("adminOnly", ProblemResponses.NOTAUTH, null, MenuService.MSG_ADMIN_ONLY)),
            "POST /api/v1/menu/{menu}/selection 400", List.of(
                    new ErrorExample("invalidOption", "INVREQ", "option", MenuService.MSG_INVALID_OPTION)),
            "POST /api/v1/menu/{menu}/selection 403", List.of(
                    new ErrorExample("adminOnly", ProblemResponses.NOTAUTH, "option", MenuService.MSG_ADMIN_ONLY)));

    private static final String ACCOUNT = "/api/v1/accounts/{id}";
    private static final long MISSING_ACCOUNT = 99_999_999_999L;
    private static final String CHANGED = "Record changed by some one else. Please review";

    private static final Map<String, List<ErrorExample>> ACCOUNT_EXAMPLES = Map.of(
            "GET " + ACCOUNT + " 400", List.of(
                    new ErrorExample("noInput", "INVREQ", AccountLookup.ACCOUNT_ID_FIELD, AccountLookup.MSG_NO_INPUT),
                    new ErrorExample("notANonZeroNumber", "INVREQ", AccountLookup.ACCOUNT_ID_FIELD,
                            AccountLookup.MSG_VIEW_ACCOUNT_INVALID)),
            "GET " + ACCOUNT + " 404", notFoundExamples(),
            "PUT " + ACCOUNT + " 400", List.of(
                    new ErrorExample("accountIdInvalid", "INVREQ", AccountLookup.ACCOUNT_ID_FIELD,
                            AccountLookup.MSG_UPDATE_ACCOUNT_INVALID),
                    new ErrorExample("statusNotYesNo", "INVREQ", "activeStatus", "Account Status must be Y or N."),
                    new ErrorExample("openMonth", "INVREQ", "openDate.month",
                            "Open Date: Month must be a number between 1 and 12."),
                    new ErrorExample("creditLimitNotNumeric", "INVREQ", "creditLimit", "Credit Limit is not valid"),
                    new ErrorExample("ssnFirstThree", "INVREQ", "ssn.part1",
                            "SSN: First 3 chars: should not be 000, 666, or between 900 and 999"),
                    new ErrorExample("ficoRange", "INVREQ", "ficoScore", "FICO Score: should be between 300 and 850"),
                    new ErrorExample("firstNameAlpha", "INVREQ", "firstName", "First Name can have alphabets only."),
                    new ErrorExample("stateCode", "INVREQ", "state", "State: is not a valid state code"),
                    new ErrorExample("zipForState", "INVREQ", "zip", AccountUpdateEdits.MSG_INVALID_ZIP_FOR_STATE),
                    new ErrorExample("phoneAreaCode", "INVREQ", "phone1.areaCode",
                            "Phone Number 1: Not valid North America general purpose area code")),
            "PUT " + ACCOUNT + " 404", notFoundExamples(),
            "PUT " + ACCOUNT + " 409", List.of(new ErrorExample("changedByAnotherUser", "CHANGED", null, CHANGED)),
            "PUT " + ACCOUNT + " 500", List.of(
                    new ErrorExample("rewriteFailed", "ABEND", null, AccountUpdateService.MSG_UPDATE_FAILED)));

    private static final String CARDS = "/api/v1/cards";
    private static final String CARD = CARDS + "/{cardNumber}";
    private static final String CARD_BY_ACCOUNT = CARDS + "/by-account/{accountId}";

    private static final Map<String, List<ErrorExample>> CARD_EXAMPLES = Map.ofEntries(
            Map.entry("GET " + CARDS + " 400", List.of(
                    new ErrorExample("accountFilterNotNumeric", "INVREQ", CardKeys.ACCOUNT_FIELD,
                            CardKeys.MSG_ACCOUNT_INVALID),
                    new ErrorExample("cardFilterNotNumeric", "INVREQ", CardKeys.CARD_FIELD, CardKeys.MSG_CARD_INVALID),
                    new ErrorExample("badCursor", "INVREQ", "after", CardController.MSG_BAD_CURSOR))),
            Map.entry("GET " + CARDS + " 403", List.of(
                    new ErrorExample("userWithoutAccount", "NOTAUTH", CardKeys.ACCOUNT_FIELD,
                            CardController.MSG_ACCOUNT_REQUIRED_FOR_USER))),
            Map.entry("POST " + CARDS + "/selection 400", List.of(
                    new ErrorExample("moreThanOne", "INVREQ", "rows[0].action", CardSelection.MSG_ONLY_ONE),
                    new ErrorExample("invalidAction", "INVREQ", "rows[0].action", CardSelection.MSG_INVALID_ACTION))),
            Map.entry("GET " + CARD + " 400", cardKeyExamples()),
            Map.entry("GET " + CARD + " 404", List.of(
                    new ErrorExample("cardNotFound", "NOTFND", null, CardLookup.MSG_NOT_FOUND))),
            Map.entry("GET " + CARD_BY_ACCOUNT + " 400", List.of(
                    new ErrorExample("accountNotProvided", "INVREQ", CardKeys.ACCOUNT_FIELD,
                            CardKeys.MSG_ACCOUNT_NOT_PROVIDED),
                    new ErrorExample("accountNotNumeric", "INVREQ", CardKeys.ACCOUNT_FIELD,
                            CardKeys.MSG_ACCOUNT_INVALID))),
            Map.entry("GET " + CARD_BY_ACCOUNT + " 404", List.of(
                    new ErrorExample("accountHasNoCard", "NOTFND", null, CardLookup.MSG_ACCOUNT_NOT_FOUND))),
            Map.entry("PUT " + CARD + " 400", List.of(
                    new ErrorExample("nameNotProvided", "INVREQ", CardUpdateEdits.NAME_FIELD,
                            CardUpdateEdits.MSG_NAME_NOT_PROVIDED),
                    new ErrorExample("nameNotAlphabetic", "INVREQ", CardUpdateEdits.NAME_FIELD,
                            CardUpdateEdits.MSG_NAME_NOT_ALPHA),
                    new ErrorExample("statusNotYesNo", "INVREQ", CardUpdateEdits.STATUS_FIELD,
                            CardUpdateEdits.MSG_STATUS_NOT_YES_NO),
                    new ErrorExample("monthOutOfRange", "INVREQ", CardUpdateEdits.MONTH_FIELD,
                            CardUpdateEdits.MSG_MONTH_INVALID),
                    new ErrorExample("yearOutOfRange", "INVREQ", CardUpdateEdits.YEAR_FIELD,
                            CardUpdateEdits.MSG_YEAR_INVALID),
                    new ErrorExample("accountNotProvided", "INVREQ", CardKeys.ACCOUNT_FIELD,
                            CardKeys.MSG_ACCOUNT_NOT_PROVIDED))),
            Map.entry("PUT " + CARD + " 404", List.of(
                    new ErrorExample("cardNotFound", "NOTFND", null, CardLookup.MSG_NOT_FOUND))),
            Map.entry("PUT " + CARD + " 409", List.of(new ErrorExample("changedByAnotherUser", "CHANGED", null, CHANGED))),
            Map.entry("PUT " + CARD + " 500", List.of(
                    new ErrorExample("lockFailed", "ABEND", null, CardUpdateService.MSG_LOCK_FAILED),
                    new ErrorExample("rewriteFailed", "ABEND", null, CardUpdateService.MSG_UPDATE_FAILED))));

    private static List<ErrorExample> cardKeyExamples() {
        return List.of(
                new ErrorExample("noInput", "INVREQ", CardKeys.ACCOUNT_FIELD, CardKeys.MSG_NO_INPUT),
                new ErrorExample("accountNotProvided", "INVREQ", CardKeys.ACCOUNT_FIELD,
                        CardKeys.MSG_ACCOUNT_NOT_PROVIDED),
                new ErrorExample("accountNotNumeric", "INVREQ", CardKeys.ACCOUNT_FIELD, CardKeys.MSG_ACCOUNT_INVALID),
                new ErrorExample("cardNotNumeric", "INVREQ", CardKeys.CARD_FIELD, CardKeys.MSG_CARD_INVALID));
    }

    private static List<ErrorExample> notFoundExamples() {
        return List.of(
                new ErrorExample("notInCrossReference", "NOTFND", null, AccountLookup.xrefNotFound(MISSING_ACCOUNT)),
                new ErrorExample("notInAccountMaster", "NOTFND", null, AccountLookup.accountNotFound(MISSING_ACCOUNT)),
                new ErrorExample("notInCustomerMaster", "NOTFND", null, AccountLookup.customerNotFound(999_999_999)));
    }

    private static final ErrorExample SIGNON_REQUIRED = new ErrorExample("signOnRequired",
            ProblemResponses.SIGNON_REQUIRED, null, ProblemResponses.MSG_SIGNON_REQUIRED);

    @Bean
    OpenAPI cardDemoOpenApi() {
        return new OpenAPI()
                .info(new Info().title("CardDemo online API").version("v1")
                        .description("Online CICS programs of CardDemo migrated to REST. Sign on with POST "
                                + "/api/v1/auth/login, then send the token as 'Authorization: Bearer <token>'. "
                                + "Errors use the uniform body {code, field, message} (ADR-0019)."))
                .components(new Components().addSecuritySchemes(BEARER, new SecurityScheme()
                        .type(SecurityScheme.Type.HTTP).scheme("bearer").bearerFormat("JWT")
                        .description("HS256 token from POST /api/v1/auth/login (ADR-0017)")));
    }

    @Bean
    OpenApiCustomizer errorResponseCustomizer() {
        return openApi -> {
            if (openApi.getPaths() == null) {
                return;
            }
            openApi.getPaths().forEach((path, item) -> item.readOperationsMap()
                    .forEach((method, operation) -> customize(path, method, operation)));
        };
    }

    private static void customize(String path, PathItem.HttpMethod method, Operation operation) {
        if (operation.getResponses() == null) {
            operation.setResponses(new ApiResponses());
        }
        ApiResponses responses = operation.getResponses();
        boolean secured = operation.getSecurity() != null && !operation.getSecurity().isEmpty();
        if (secured && !responses.containsKey("401")) {
            responses.addApiResponse("401", new ApiResponse()
                    .description("SIGNON_REQUIRED: missing, invalid or expired bearer token")
                    .content(new Content().addMediaType(PROBLEM_JSON,
                            new MediaType().schema(new Schema<>().$ref(API_ERROR_REF)))));
        }
        responses.forEach((status, response) -> {
            MediaType problem = response.getContent() == null ? null : response.getContent().get(PROBLEM_JSON);
            if (problem == null || (problem.getExamples() != null && !problem.getExamples().isEmpty())) {
                return;
            }
            String key = method + " " + path + " " + status;
            List<ErrorExample> examples = EXAMPLES.getOrDefault(key,
                    ACCOUNT_EXAMPLES.getOrDefault(key, CARD_EXAMPLES.get(key)));
            if (examples == null && "401".equals(status) && secured) {
                examples = List.of(SIGNON_REQUIRED);
            }
            if (examples == null) {
                return;
            }
            int code = Integer.parseInt(status);
            examples.forEach(e -> problem.addExamples(e.name(), new Example().summary(e.message())
                    .value(e.body(code, path))));
        });
    }

    private record ErrorExample(String name, String code, String field, String message) {

        Map<String, Object> body(int status, String instance) {
            Map<String, Object> body = new LinkedHashMap<>();
            body.put("type", "about:blank");
            body.put("title", HttpStatus.valueOf(status).getReasonPhrase());
            body.put("status", status);
            body.put("detail", message);
            body.put("instance", instance);
            body.put("code", code);
            body.put("field", field);
            body.put("message", message);
            return body;
        }
    }
}
