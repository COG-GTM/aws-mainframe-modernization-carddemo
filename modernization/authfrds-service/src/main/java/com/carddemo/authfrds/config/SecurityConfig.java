package com.carddemo.authfrds.config;

import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.enums.SecuritySchemeType;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.security.SecurityScheme;
import java.util.Arrays;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.factory.PasswordEncoderFactories;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.provisioning.InMemoryUserDetailsManager;
import org.springframework.security.web.SecurityFilterChain;

/**
 * BR-AF-040 (Java-only; legacy relied on CICS signon / RACF upstream of COPAUS1C): stateless HTTP Basic.
 * GET /api/** -> READER or MAINTAINER; any write on /api/** -> MAINTAINER. Health and Swagger stay public.
 */
@Configuration
@OpenAPIDefinition(security = @SecurityRequirement(name = "basicAuth"))
@SecurityScheme(name = "basicAuth", type = SecuritySchemeType.HTTP, scheme = "basic")
public class SecurityConfig {

    public static final String READER = "READER";
    public static final String MAINTAINER = "MAINTAINER";

    @Bean
    SecurityFilterChain api(HttpSecurity http) throws Exception {
        return http
                .csrf(c -> c.disable())
                .sessionManagement(s -> s.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
                .authorizeHttpRequests(a -> a
                        // /error: the container error-dispatches 404/400/500 there; denying it turns them into 403
                        .requestMatchers("/error", "/actuator/health", "/actuator/health/**", "/swagger-ui.html",
                                "/swagger-ui/**", "/v3/api-docs/**").permitAll()
                        .requestMatchers(HttpMethod.GET, "/api/**").hasAnyRole(READER, MAINTAINER)
                        .requestMatchers("/api/**").hasRole(MAINTAINER)
                        .anyRequest().denyAll())
                .httpBasic(Customizer.withDefaults())
                .build();
    }

    @Bean
    PasswordEncoder passwordEncoder() {
        return PasswordEncoderFactories.createDelegatingPasswordEncoder();
    }

    @Bean
    UserDetailsService users(ApiCredentials creds, PasswordEncoder encoder) {
        return new InMemoryUserDetailsManager(
                User.withUsername(creds.maintainerUser()).password(encoder.encode(creds.maintainerPassword()))
                        .roles(MAINTAINER).build(),
                User.withUsername(creds.readerUser()).password(encoder.encode(creds.readerPassword()))
                        .roles(READER).build());
    }

    @Bean
    ApiCredentials apiCredentials(Environment env,
                                  @Value("${carddemo.security.maintainer.username:}") String maintainerUser,
                                  @Value("${carddemo.security.maintainer.password:}") String maintainerPassword,
                                  @Value("${carddemo.security.reader.username:}") String readerUser,
                                  @Value("${carddemo.security.reader.password:}") String readerPassword) {
        ApiCredentials c = new ApiCredentials(maintainerUser, maintainerPassword, readerUser, readerPassword);
        c.validate(Arrays.asList(env.getActiveProfiles()).contains("test"));
        return c;
    }
}
