package com.carddemo.transactiontype.api;

import io.swagger.v3.oas.annotations.media.Schema;

public record DescriptionRequest(@Schema(example = "PURCHASE RETAIL") String description) {
}
