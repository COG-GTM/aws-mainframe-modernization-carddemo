package com.carddemo.batch.tranrept;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpStatus;
import org.springframework.web.client.RestClient;
import org.springframework.web.client.RestClientResponseException;

/**
 * The KSDS reads of CBTRN03C against the services that own the data: CARDXREF from card-service
 * ({@code GET /api/v1/cards/{card}/xref}, 404 = INVALID KEY), TRANTYPE and TRANCATG from
 * transaction-service ({@code GET /api/v1/transaction-types}, {@code /api/v1/transaction-categories},
 * read once per run). One instance per run.
 */
public final class ServiceReferenceData implements ReferenceData {

    private final RestClient cards;
    private final RestClient transactions;
    private final Map<String, Optional<Long>> xref = new HashMap<>();
    private Map<String, String> types;
    private Map<String, String> categories;

    public ServiceReferenceData(RestClient cardRestClient, RestClient transactionRestClient) {
        this.cards = cardRestClient;
        this.transactions = transactionRestClient;
    }

    record XrefView(String cardNumber, String customerId, String accountId) {
    }

    record TypeView(String typeCode, String description) {
    }

    record CategoryView(String typeCode, Integer categoryCode, String description) {
    }

    @Override
    public Optional<Long> accountIdForCard(String cardNumber) {
        return xref.computeIfAbsent(cardNumber, this::fetchXref);
    }

    private Optional<Long> fetchXref(String cardNumber) {
        if (cardNumber.isBlank()) {
            return Optional.empty();
        }
        try {
            XrefView view = cards.get().uri("/api/v1/cards/{card}/xref", cardNumber.strip())
                    .retrieve().body(XrefView.class);
            return Optional.ofNullable(view).map(XrefView::accountId).map(Long::valueOf);
        } catch (RestClientResponseException e) {
            if (e.getStatusCode() == HttpStatus.NOT_FOUND) {
                return Optional.empty();
            }
            throw e;
        }
    }

    @Override
    public Optional<String> typeDescription(String typeCode) {
        if (types == null) {
            types = new HashMap<>();
            for (TypeView type : all("/api/v1/transaction-types",
                    new ParameterizedTypeReference<ServicePages.Page<TypeView>>() { }, TypeView::typeCode)) {
                types.put(type.typeCode(), nullToEmpty(type.description()));
            }
        }
        return Optional.ofNullable(types.get(typeCode));
    }

    @Override
    public Optional<String> categoryDescription(String typeCode, int categoryCode) {
        if (categories == null) {
            categories = new HashMap<>();
            for (CategoryView category : all("/api/v1/transaction-categories",
                    new ParameterizedTypeReference<ServicePages.Page<CategoryView>>() { },
                    c -> key(c.typeCode(), c.categoryCode()))) {
                categories.put(key(category.typeCode(), category.categoryCode()), nullToEmpty(category.description()));
            }
        }
        return Optional.ofNullable(categories.get(key(typeCode, categoryCode)));
    }

    private <T> List<T> all(String path, ParameterizedTypeReference<ServicePages.Page<T>> type,
                            java.util.function.Function<T, ?> key) {
        return ServicePages.all(transactions, path, type, key);
    }

    private static String key(String typeCode, int categoryCode) {
        return typeCode + String.format("%04d", categoryCode);
    }

    private static String nullToEmpty(String value) {
        return value == null ? "" : value;
    }
}
