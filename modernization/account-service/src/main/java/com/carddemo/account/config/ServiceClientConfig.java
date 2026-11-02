package com.carddemo.account.config;

import com.carddemo.common.security.CallerTokenForwarder;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.MediaType;
import org.springframework.web.client.RestClient;

/** card-service and customer-service clients; both forward the caller's bearer token. */
@Configuration
public class ServiceClientConfig {

    @Bean
    public RestClient cardRestClient(RestClient.Builder builder,
                                     @Value("${carddemo.services.card-url}") String baseUrl) {
        return client(builder, baseUrl);
    }

    @Bean
    public RestClient customerRestClient(RestClient.Builder builder,
                                         @Value("${carddemo.services.customer-url}") String baseUrl) {
        return client(builder, baseUrl);
    }

    private static RestClient client(RestClient.Builder builder, String baseUrl) {
        return builder.clone().baseUrl(baseUrl).defaultHeader("Accept", MediaType.APPLICATION_JSON_VALUE)
                .requestInterceptor(CallerTokenForwarder.forCallers()).build();
    }
}
