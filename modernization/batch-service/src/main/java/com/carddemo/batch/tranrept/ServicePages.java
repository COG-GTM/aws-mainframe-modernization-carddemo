package com.carddemo.batch.tranrept;

import java.util.ArrayList;
import java.util.List;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.web.client.RestClient;

/** Reads every page of a {@code PageResponse} collection of another service. */
final class ServicePages {

    static final int PAGE_SIZE = 200;

    record Page<T>(List<T> content, int page, int totalPages) {
    }

    private ServicePages() {
    }

    static <T> List<T> all(RestClient client, String path, ParameterizedTypeReference<Page<T>> type) {
        List<T> rows = new ArrayList<>();
        for (int page = 0; ; page++) {
            int current = page;
            Page<T> response = client.get()
                    .uri(uri -> uri.path(path).queryParam("page", current).queryParam("size", PAGE_SIZE).build())
                    .retrieve().body(type);
            if (response == null || response.content() == null) {
                return rows;
            }
            rows.addAll(response.content());
            if (page + 1 >= response.totalPages()) {
                return rows;
            }
        }
    }
}
