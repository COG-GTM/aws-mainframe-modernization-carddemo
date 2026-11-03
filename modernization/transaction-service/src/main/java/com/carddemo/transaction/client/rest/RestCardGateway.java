package com.carddemo.transaction.client.rest;

import com.carddemo.transaction.client.CardGateway;
import com.carddemo.transaction.client.CardXrefView;
import java.util.Optional;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestClient;
import org.springframework.web.client.RestClientResponseException;

@Component
public class RestCardGateway implements CardGateway {

    private final RestClient restClient;

    public RestCardGateway(RestClient cardRestClient) {
        this.restClient = cardRestClient;
    }

    @Override
    public Optional<CardXrefView> xrefByCard(String cardNumber) {
        return get("/api/v1/cards/{key}/xref", cardNumber);
    }

    @Override
    public Optional<CardXrefView> xrefByAccount(long accountId) {
        return get("/api/v1/card-xrefs/by-account/{key}", accountId);
    }

    /** Only a 404 means "no such cross reference"; every other error is a failed lookup. */
    private Optional<CardXrefView> get(String uri, Object key) {
        try {
            return Optional.ofNullable(restClient.get()
                    .uri(uri, key)
                    .retrieve()
                    .body(CardXrefView.class));
        } catch (RestClientResponseException e) {
            if (e.getStatusCode() == HttpStatus.NOT_FOUND) {
                return Optional.empty();
            }
            throw e;
        }
    }
}
