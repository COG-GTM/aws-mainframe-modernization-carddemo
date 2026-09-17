package com.carddemo.transaction.client.rest;

import com.carddemo.transaction.client.AccountGateway;
import com.carddemo.transaction.client.AccountView;
import com.carddemo.transaction.client.PostingResult;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Map;
import java.util.Optional;
import org.springframework.http.HttpStatusCode;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestClient;

@Component
public class RestAccountGateway implements AccountGateway {

    private final RestClient restClient;

    public RestAccountGateway(RestClient accountRestClient) {
        this.restClient = accountRestClient;
    }

    @Override
    public Optional<AccountView> find(long accountId) {
        return Optional.ofNullable(restClient.get()
                .uri("/api/v1/accounts/{id}", accountId)
                .retrieve()
                .onStatus(HttpStatusCode::is4xxClientError, (request, response) -> {
                })
                .body(AccountView.class));
    }

    @Override
    public PostingResult post(long accountId, BigDecimal amount, LocalDate transactionDate) {
        return restClient.post()
                .uri("/api/v1/accounts/{id}/postings", accountId)
                .body(Map.of("amount", amount, "transactionDate", transactionDate.toString()))
                .retrieve()
                .onStatus(HttpStatusCode::is4xxClientError, (request, response) -> {
                })
                .body(PostingResult.class);
    }

    @Override
    public void settleInterest(long accountId, BigDecimal totalInterest) {
        restClient.post()
                .uri("/api/v1/accounts/{id}/interest-settlements", accountId)
                .body(Map.of("totalInterest", totalInterest))
                .retrieve()
                .toBodilessEntity();
    }
}
