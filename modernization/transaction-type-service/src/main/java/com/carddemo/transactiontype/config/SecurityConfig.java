package com.carddemo.transactiontype.config;

import jakarta.servlet.DispatcherType;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.factory.PasswordEncoderFactories;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.provisioning.InMemoryUserDetailsManager;
import org.springframework.security.web.SecurityFilterChain;

/**
 * HTTP Basic with two roles: READER may call GET endpoints, MAINTAINER may also create, update, delete and run
 * the batch. Health and the Swagger UI / OpenAPI document stay public so the Authorize button works.
 */
@Configuration
@EnableConfigurationProperties(ApiSecurityProperties.class)
public class SecurityConfig {

    public static final String READER = "READER";
    public static final String MAINTAINER = "MAINTAINER";

    @Bean
    SecurityFilterChain apiSecurity(HttpSecurity http) throws Exception {
        return http
                .csrf(csrf -> csrf.disable())
                .sessionManagement(s -> s.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
                .authorizeHttpRequests(auth -> auth
                        .dispatcherTypeMatchers(DispatcherType.ERROR).permitAll()
                        .requestMatchers("/error").permitAll()
                        .requestMatchers("/actuator/health", "/actuator/health/**").permitAll()
                        .requestMatchers("/swagger-ui.html", "/swagger-ui/**", "/v3/api-docs", "/v3/api-docs/**")
                        .permitAll()
                        .requestMatchers(HttpMethod.GET, "/api/**").hasAnyRole(READER, MAINTAINER)
                        .requestMatchers("/api/**").hasRole(MAINTAINER)
                        .anyRequest().authenticated())
                .httpBasic(basic -> basic.realmName("transaction-type-service"))
                .build();
    }

    @Bean
    PasswordEncoder passwordEncoder() {
        return PasswordEncoderFactories.createDelegatingPasswordEncoder();
    }

    @Bean
    UserDetailsService users(ApiSecurityProperties props, PasswordEncoder encoder) {
        ApiSecurityProperties.Credentials maintainer = props.maintainer();
        ApiSecurityProperties.Credentials reader = props.reader();
        if (maintainer == null || !maintainer.isComplete() || reader == null || !reader.isComplete()) {
            throw new IllegalStateException("API credentials missing: set TT_API_USER, TT_API_PASSWORD, "
                    + "TT_API_READER_USER and TT_API_READER_PASSWORD (see .env.example)");
        }
        if (maintainer.username().equals(reader.username())) {
            throw new IllegalStateException("TT_API_USER and TT_API_READER_USER must be different users");
        }
        return new InMemoryUserDetailsManager(
                User.withUsername(maintainer.username()).password(encoder.encode(maintainer.password()))
                        .roles(MAINTAINER).build(),
                User.withUsername(reader.username()).password(encoder.encode(reader.password()))
                        .roles(READER).build());
    }
}
