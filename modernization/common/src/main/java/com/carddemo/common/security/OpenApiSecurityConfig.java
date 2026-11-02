package com.carddemo.common.security;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.springdoc.core.customizers.OpenApiCustomizer;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.util.AntPathMatcher;

import java.util.List;

/** Declares the sign-on bearer token in every service's OpenAPI document so Swagger UI shows "Authorize". */
@Configuration
@ConditionalOnClass(OpenApiCustomizer.class)
public class OpenApiSecurityConfig {

    public static final String SCHEME = "bearerAuth";

    @Bean
    public OpenApiCustomizer bearerTokenOpenApiCustomizer(SecurityProperties properties) {
        AntPathMatcher matcher = new AntPathMatcher();
        return openApi -> {
            if (openApi.getComponents() == null) {
                openApi.setComponents(new Components());
            }
            openApi.getComponents().addSecuritySchemes(SCHEME, new SecurityScheme()
                    .type(SecurityScheme.Type.HTTP)
                    .scheme("bearer")
                    .bearerFormat("JWT")
                    .description("Token from POST /api/v1/auth/signon"));
            openApi.addSecurityItem(new SecurityRequirement().addList(SCHEME));
            if (openApi.getPaths() != null) {
                openApi.getPaths().forEach((path, item) -> {
                    if (properties.publicPaths().stream().anyMatch(p -> matcher.match(p, path))) {
                        item.readOperations().forEach(op -> op.setSecurity(List.of()));
                    }
                });
            }
        };
    }
}
