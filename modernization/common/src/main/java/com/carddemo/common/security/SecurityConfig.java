package com.carddemo.common.security;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.Clock;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

/** Picked up by every service through its {@code com.carddemo.common} component scan. */
@Configuration
@EnableConfigurationProperties(SecurityProperties.class)
public class SecurityConfig implements WebMvcConfigurer {

    @Bean
    public TokenService tokenService(SecurityProperties properties) {
        return new TokenService(properties, Clock.systemUTC());
    }

    @Bean
    public FilterRegistrationBean<AuthenticationFilter> authenticationFilter(TokenService tokens,
                                                                             SecurityProperties properties,
                                                                             ObjectMapper objectMapper) {
        FilterRegistrationBean<AuthenticationFilter> registration = new FilterRegistrationBean<>(
                new AuthenticationFilter(tokens, properties.publicPaths(), objectMapper));
        registration.addUrlPatterns("/api/*");
        registration.setOrder(Ordered.HIGHEST_PRECEDENCE + 10);
        return registration;
    }

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(new AdminOnlyInterceptor());
    }
}
