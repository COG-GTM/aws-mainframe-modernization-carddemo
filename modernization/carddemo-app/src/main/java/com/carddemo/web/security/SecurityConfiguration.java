package com.carddemo.web.security;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.jwk.source.ImmutableSecret;
import java.nio.charset.StandardCharsets;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.oauth2.jose.jws.MacAlgorithm;
import org.springframework.security.oauth2.jwt.JwtDecoder;
import org.springframework.security.oauth2.jwt.JwtEncoder;
import org.springframework.security.oauth2.jwt.JwtValidators;
import org.springframework.security.oauth2.jwt.NimbusJwtDecoder;
import org.springframework.security.oauth2.jwt.NimbusJwtEncoder;
import org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationConverter;
import org.springframework.security.oauth2.server.resource.authentication.JwtGrantedAuthoritiesConverter;
import org.springframework.security.web.SecurityFilterChain;

/**
 * Stateless bearer-token security of the online API (ADR-0017): no HTTP session, no CSRF token (no cookies are
 * used), HS256 tokens signed with {@code carddemo.security.jwt.secret}. Only the sign-on endpoints, health, info and
 * the OpenAPI document are anonymous; {@code /api/v1/menu/admin/**} needs role {@code ADMIN} (ADR-0007).
 * Only active in a servlet web application, so batch launches ({@code --job=}) need no secret.
 */
@Configuration(proxyBeanMethods = false)
@EnableWebSecurity
@ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
public class SecurityConfiguration {

    /** HS256 needs a key of at least 256 bits (RFC 7518 section 3.2). */
    static final int MIN_SECRET_BYTES = 32;

    @Bean
    SecretKey jwtSigningKey(JwtProperties properties) {
        String secret = properties.secret();
        if (secret == null || secret.isBlank()) {
            throw new IllegalStateException("carddemo.security.jwt.secret is not set: export CARDDEMO_JWT_SECRET "
                    + "(at least " + MIN_SECRET_BYTES + " bytes); only the local and test profiles have a default");
        }
        byte[] key = secret.getBytes(StandardCharsets.UTF_8);
        if (key.length < MIN_SECRET_BYTES) {
            throw new IllegalStateException("CARDDEMO_JWT_SECRET must be at least " + MIN_SECRET_BYTES
                    + " bytes for HS256, got " + key.length);
        }
        return new SecretKeySpec(key, "HmacSHA256");
    }

    @Bean
    JwtEncoder jwtEncoder(SecretKey jwtSigningKey) {
        return new NimbusJwtEncoder(new ImmutableSecret<>(jwtSigningKey));
    }

    @Bean
    JwtDecoder jwtDecoder(SecretKey jwtSigningKey, JwtProperties properties) {
        NimbusJwtDecoder decoder = NimbusJwtDecoder.withSecretKey(jwtSigningKey)
                .macAlgorithm(MacAlgorithm.HS256)
                .build();
        decoder.setJwtValidator(JwtValidators.createDefaultWithIssuer(properties.issuer()));
        return decoder;
    }

    @Bean
    JwtAuthenticationConverter jwtAuthenticationConverter() {
        JwtGrantedAuthoritiesConverter roles = new JwtGrantedAuthoritiesConverter();
        roles.setAuthoritiesClaimName(TokenService.ROLE_CLAIM);
        roles.setAuthorityPrefix("ROLE_");
        JwtAuthenticationConverter converter = new JwtAuthenticationConverter();
        converter.setJwtGrantedAuthoritiesConverter(roles);
        return converter;
    }

    @Bean
    SecurityFilterChain apiSecurity(HttpSecurity http, JwtAuthenticationConverter jwtAuthenticationConverter,
            ObjectMapper objectMapper) throws Exception {
        ProblemResponses problems = new ProblemResponses(objectMapper);
        http
                .csrf(AbstractHttpConfigurer::disable)
                .httpBasic(AbstractHttpConfigurer::disable)
                .formLogin(AbstractHttpConfigurer::disable)
                .logout(AbstractHttpConfigurer::disable)
                .requestCache(AbstractHttpConfigurer::disable)
                .sessionManagement(s -> s.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
                .authorizeHttpRequests(auth -> auth
                        .requestMatchers("/api/v1/auth/**").permitAll()
                        .requestMatchers(HttpMethod.GET, "/actuator/health", "/actuator/health/**", "/actuator/info")
                        .permitAll()
                        .requestMatchers(HttpMethod.GET, "/v3/api-docs", "/v3/api-docs/**", "/swagger-ui.html",
                                "/swagger-ui/**")
                        .permitAll()
                        .requestMatchers("/error").permitAll()
                        .requestMatchers("/api/v1/menu/admin", "/api/v1/menu/admin/**").hasRole("ADMIN")
                        .anyRequest().authenticated())
                .oauth2ResourceServer(o -> o
                        .jwt(jwt -> jwt.jwtAuthenticationConverter(jwtAuthenticationConverter))
                        .authenticationEntryPoint(problems)
                        .accessDeniedHandler(problems))
                .exceptionHandling(e -> e.authenticationEntryPoint(problems).accessDeniedHandler(problems));
        return http.build();
    }
}
