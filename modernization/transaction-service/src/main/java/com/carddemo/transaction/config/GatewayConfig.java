package com.carddemo.transaction.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.MediaType;
import org.springframework.web.client.RestClient;

@Configuration
public class GatewayConfig {

    @Bean
    public RestClient accountRestClient(RestClient.Builder builder,
                                        @Value("${carddemo.services.account-url}") String baseUrl) {
        return builder.baseUrl(baseUrl).defaultHeader("Accept", MediaType.APPLICATION_JSON_VALUE).build();
    }

    @Bean
    public RestClient cardRestClient(RestClient.Builder builder,
                                     @Value("${carddemo.services.card-url}") String baseUrl) {
        return builder.baseUrl(baseUrl).defaultHeader("Accept", MediaType.APPLICATION_JSON_VALUE).build();
    }
}
