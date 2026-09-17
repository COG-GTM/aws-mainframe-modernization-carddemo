package com.carddemo.transaction.client.rest;

import com.carddemo.transaction.client.AccountGateway;
import com.carddemo.transaction.client.AccountView;
import com.carddemo.transaction.client.PostingResult;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Map;
import java.util.Optional;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestClient;
import org.springframework.web.client.RestClientResponseException;

@Component
public class RestAccountGateway implements AccountGateway {

    private final RestClient restClient;

    public RestAccountGateway(RestClient accountRestClient) {
        this.restClient = accountRestClient;
    }

    @Override
    public Optional<AccountView> find(long accountId) {
        try {
            return Optional.ofNullable(restClient.get()
                    .uri("/api/v1/accounts/{id}", accountId)
                    .retrieve()
                    .body(AccountView.class));
        } catch (RestClientResponseException e) {
            if (e.getStatusCode() == HttpStatus.NOT_FOUND) {
                return Optional.empty();
            }
            throw e;
        }
    }

    /**
     * Account-service answers a business rejection (reason 101, 102, 103) with 422 and a posting
     * body; any other error status carries the shared {@code ApiError} envelope and must not be
     * read as a posting result.
     */
    @Override
    public PostingResult post(long accountId, BigDecimal amount, LocalDate transactionDate) {
        PostingResult result = restClient.post()
                .uri("/api/v1/accounts/{id}/postings", accountId)
                .body(Map.of("amount", amount, "transactionDate", transactionDate.toString()))
                .retrieve()
                .onStatus(status -> status == HttpStatus.UNPROCESSABLE_ENTITY, (request, response) -> {
                })
                .body(PostingResult.class);
        if (result == null || (!result.posted() && result.reasonCode() == null)) {
            throw new IllegalStateException("Account " + accountId + " returned an unusable posting result");
        }
        return result;
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
