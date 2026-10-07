package com.carddemo.web;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/** OpenAPI document ({@code /v3/api-docs}, Swagger UI at {@code /swagger-ui.html}) with the bearer-token scheme. */
@Configuration(proxyBeanMethods = false)
public class OpenApiConfiguration {

    public static final String BEARER = "bearerAuth";

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
}
