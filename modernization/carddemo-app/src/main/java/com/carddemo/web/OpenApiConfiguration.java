package com.carddemo.web;

import com.carddemo.user.menu.MenuService;
import com.carddemo.user.signon.SignOnService;
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
            List<ErrorExample> examples = EXAMPLES.get(method + " " + path + " " + status);
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
