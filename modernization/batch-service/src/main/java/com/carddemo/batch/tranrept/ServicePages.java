package com.carddemo.batch.tranrept;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.web.client.RestClient;

/**
 * Reads every page of a {@code PageResponse} collection of another service as one consistent
 * unload. Pages are separate queries, so a row inserted or deleted between them shifts the page
 * boundaries (a row read twice or missed). A read counts only when every page reports the same
 * {@code totalElements} and the rows have exactly that many distinct keys; otherwise it is retried,
 * and after {@link #ATTEMPTS} attempts the unload fails rather than report a wrong set.
 */
final class ServicePages {

    static final int PAGE_SIZE = 200;
    static final int ATTEMPTS = 3;

    record Page<T>(List<T> content, int page, long totalElements, int totalPages) {
    }

    private ServicePages() {
    }

    static <T> List<T> all(RestClient client, String path, ParameterizedTypeReference<Page<T>> type,
                           Function<T, ?> key) {
        for (int attempt = 1; attempt <= ATTEMPTS; attempt++) {
            List<T> rows = read(client, path, type, key);
            if (rows != null) {
                return rows;
            }
        }
        throw new IllegalStateException(path + " changed while it was read " + ATTEMPTS
                + " times; no consistent unload");
    }

    /** One pass over all pages; null when the collection changed during the pass. */
    private static <T> List<T> read(RestClient client, String path, ParameterizedTypeReference<Page<T>> type,
                                    Function<T, ?> key) {
        List<T> rows = new ArrayList<>();
        Long total = null;
        for (int page = 0; ; page++) {
            int current = page;
            Page<T> response = client.get()
                    .uri(uri -> uri.path(path).queryParam("page", current).queryParam("size", PAGE_SIZE).build())
                    .retrieve().body(type);
            if (response == null || response.content() == null) {
                throw new IllegalStateException(path + " returned no page " + page);
            }
            if (total == null) {
                total = response.totalElements();
            } else if (total != response.totalElements()) {
                return null;
            }
            rows.addAll(response.content());
            if (page + 1 >= response.totalPages()) {
                break;
            }
        }
        Set<Object> keys = new HashSet<>();
        rows.forEach(row -> keys.add(key.apply(row)));
        return keys.size() == rows.size() && rows.size() == total ? rows : null;
    }
}
