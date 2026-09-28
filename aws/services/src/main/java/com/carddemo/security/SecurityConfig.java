package com.carddemo.security;

import com.carddemo.common.ErrorCode;
import com.carddemo.common.LegacyMessages;
import com.carddemo.user.UserRepository;
import java.util.List;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationToken;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.access.AccessDeniedHandler;

@Configuration
@EnableWebSecurity
public class SecurityConfig {

    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity http, ErrorResponseWriter writer, UserRepository users)
            throws Exception {
        AuthenticationEntryPoint entryPoint = (request, response, ex) -> writer.write(response,
                ErrorCode.UNAUTHENTICATED, "Authentication required", null);
        AccessDeniedHandler deniedHandler = (request, response, ex) -> writer.write(response, ErrorCode.FORBIDDEN,
                LegacyMessages.ADMIN_ONLY, null);

        http
                // Stateless bearer-token API: no cookies or sessions, so CSRF tokens do not apply.
                .csrf(AbstractHttpConfigurer::disable)
                .sessionManagement(s -> s.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
                .authorizeHttpRequests(auth -> auth
                        .requestMatchers(HttpMethod.POST, "/api/v1/auth/signon").permitAll()
                        .requestMatchers("/actuator/health", "/actuator/health/**", "/actuator/info").permitAll()
                        .requestMatchers("/error").permitAll()
                        .requestMatchers("/api/v1/menus/admin").hasRole(Role.ADMIN.name())
                        .requestMatchers("/api/v1/users", "/api/v1/users/**").hasRole(Role.ADMIN.name())
                        .requestMatchers(HttpMethod.POST, "/api/v1/transaction-types/**").hasRole(Role.ADMIN.name())
                        .requestMatchers(HttpMethod.POST, "/api/v1/transaction-types").hasRole(Role.ADMIN.name())
                        .requestMatchers(HttpMethod.PUT, "/api/v1/transaction-types/**").hasRole(Role.ADMIN.name())
                        .requestMatchers(HttpMethod.DELETE, "/api/v1/transaction-types/**").hasRole(Role.ADMIN.name())
                        .requestMatchers("/api/v1/**").authenticated()
                        .anyRequest().denyAll())
                .oauth2ResourceServer(o -> o
                        .jwt(jwt -> jwt.jwtAuthenticationConverter(token -> authenticate(token, users)))
                        .authenticationEntryPoint(entryPoint)
                        .accessDeniedHandler(deniedHandler))
                .exceptionHandling(e -> e.authenticationEntryPoint(entryPoint).accessDeniedHandler(deniedHandler));
        return http.build();
    }

    /** The role is taken from the current user_security row, so deleted or re-typed users lose access at once. */
    private static JwtAuthenticationToken authenticate(Jwt token, UserRepository users) {
        Role role = users.findById(token.getSubject())
                .map(user -> Role.fromUserType(user.userType()))
                .orElseThrow(() -> new BadCredentialsException("User no longer exists"));
        return new JwtAuthenticationToken(token, List.of(new SimpleGrantedAuthority("ROLE_" + role.name())),
                token.getSubject());
    }
}
