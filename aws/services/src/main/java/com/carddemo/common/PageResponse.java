package com.carddemo.common;

import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.List;

public record PageResponse<T>(
        List<T> items,
        String firstKey,
        String lastKey,
        boolean hasNext,
        boolean hasPrev,
        @JsonInclude(JsonInclude.Include.NON_NULL) String message) {
}
