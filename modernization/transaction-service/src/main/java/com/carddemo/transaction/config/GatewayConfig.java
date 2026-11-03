package com.carddemo.transaction.config;

import com.carddemo.common.security.CallerTokenForwarder;
import com.carddemo.common.security.TokenService;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.MediaType;
import org.springframework.web.client.RestClient;
import org.springframework.web.context.WebApplicationContext;

@Configuration
public class GatewayConfig {

    /** The web service forwards its callers' tokens; a {@code --job=} launch (non-web context) signs as BATCH. */
    @Bean
    public CallerTokenForwarder callerTokenForwarder(ApplicationContext context, TokenService tokens) {
        return context instanceof WebApplicationContext
                ? CallerTokenForwarder.forCallers()
                : CallerTokenForwarder.forBatchProcess(tokens);
    }

    @Bean
    public RestClient accountRestClient(RestClient.Builder builder, CallerTokenForwarder forwarder,
                                        @Value("${carddemo.services.account-url}") String baseUrl) {
        return builder.baseUrl(baseUrl).defaultHeader("Accept", MediaType.APPLICATION_JSON_VALUE)
                .requestInterceptor(forwarder).build();
    }

    @Bean
    public RestClient cardRestClient(RestClient.Builder builder, CallerTokenForwarder forwarder,
                                     @Value("${carddemo.services.card-url}") String baseUrl) {
        return builder.baseUrl(baseUrl).defaultHeader("Accept", MediaType.APPLICATION_JSON_VALUE)
                .requestInterceptor(forwarder).build();
    }
}
