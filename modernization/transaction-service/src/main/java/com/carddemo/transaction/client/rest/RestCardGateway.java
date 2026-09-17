package com.carddemo.transaction.client.rest;

import com.carddemo.transaction.client.CardGateway;
import com.carddemo.transaction.client.CardXrefView;
import java.util.Optional;
import org.springframework.http.HttpStatusCode;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestClient;

@Component
public class RestCardGateway implements CardGateway {

    private final RestClient restClient;

    public RestCardGateway(RestClient cardRestClient) {
        this.restClient = cardRestClient;
    }

    @Override
    public Optional<CardXrefView> xrefByCard(String cardNumber) {
        return Optional.ofNullable(restClient.get()
                .uri("/api/v1/cards/{cardNumber}/xref", cardNumber)
                .retrieve()
                .onStatus(HttpStatusCode::is4xxClientError, (request, response) -> {
                })
                .body(CardXrefView.class));
    }

    @Override
    public Optional<CardXrefView> xrefByAccount(long accountId) {
        return Optional.ofNullable(restClient.get()
                .uri("/api/v1/card-xrefs/by-account/{accountId}", accountId)
                .retrieve()
                .onStatus(HttpStatusCode::is4xxClientError, (request, response) -> {
                })
                .body(CardXrefView.class));
    }
}
