package com.carddemo.account.client;

import com.carddemo.common.error.RecordChangedException;
import java.util.Optional;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestClient;
import org.springframework.web.client.RestClientResponseException;

/** CUSTDAT access of COACTVWC/COACTUPC (9400-GETCUSTDATA-BYCUST, the CUSTDAT REWRITE), served by customer-service. */
@Component
public class CustomerClient {

    private final RestClient restClient;

    public CustomerClient(RestClient customerRestClient) {
        this.restClient = customerRestClient;
    }

    /** Empty only on 404 (CICS NOTFND); any other failure propagates. */
    public Optional<CustomerView> get(long customerId) {
        try {
            return Optional.ofNullable(restClient.get()
                    .uri("/api/v1/customers/{customerId}", customerId)
                    .retrieve()
                    .body(CustomerView.class));
        } catch (RestClientResponseException e) {
            if (e.getStatusCode() == HttpStatus.NOT_FOUND) {
                return Optional.empty();
            }
            throw e;
        }
    }

    /** A 409 (the customer changed since it was read) becomes {@link RecordChangedException}. */
    public CustomerView update(long customerId, CustomerUpdate update) {
        try {
            return restClient.put()
                    .uri("/api/v1/customers/{customerId}", customerId)
                    .contentType(MediaType.APPLICATION_JSON)
                    .body(update)
                    .retrieve()
                    .body(CustomerView.class);
        } catch (RestClientResponseException e) {
            if (e.getStatusCode() == HttpStatus.CONFLICT) {
                throw new RecordChangedException();
            }
            throw e;
        }
    }
}
