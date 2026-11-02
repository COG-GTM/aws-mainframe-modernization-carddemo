package com.carddemo.account.client;

import java.util.Optional;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestClient;
import org.springframework.web.client.RestClientResponseException;

/** The CXACAIX read of COACTVWC/COACTUPC 9200-GETCARDXREF-BYACCT, served by card-service. */
@Component
public class CardXrefClient {

    private final RestClient restClient;

    public CardXrefClient(RestClient cardRestClient) {
        this.restClient = cardRestClient;
    }

    /** Empty only on 404 (CICS NOTFND); any other failure propagates. */
    public Optional<CardXrefView> byAccount(long accountId) {
        try {
            return Optional.ofNullable(restClient.get()
                    .uri("/api/v1/card-xrefs/by-account/{accountId}", accountId)
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
