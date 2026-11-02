package com.carddemo.transaction.api;

import com.carddemo.common.api.PageResponse;
import com.carddemo.transaction.api.dto.TransactionCategoryResponse;
import com.carddemo.transaction.api.dto.TransactionTypeResponse;
import com.carddemo.transaction.repository.TransactionCategoryTypeRepository;
import com.carddemo.transaction.repository.TransactionTypeRepository;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * Read access to the TRANTYPE and TRANCATG reference files, in key order. Used by batch-service
 * TRANREPT (CBTRN03C 1500-B/1500-C lookups) when those DDs are not given as files.
 */
@RestController
@Validated
@RequestMapping("/api/v1")
public class TransactionReferenceController {

    private static final int MAX_PAGE_SIZE = 200;

    private final TransactionTypeRepository types;
    private final TransactionCategoryTypeRepository categories;

    public TransactionReferenceController(TransactionTypeRepository types,
                                          TransactionCategoryTypeRepository categories) {
        this.types = types;
        this.categories = categories;
    }

    @GetMapping("/transaction-types")
    public PageResponse<TransactionTypeResponse> types(
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(MAX_PAGE_SIZE) int size) {
        return PageResponse.of(types.findAll(PageRequest.of(page, size, Sort.by("typeCd")))
                .map(TransactionTypeResponse::from));
    }

    @GetMapping("/transaction-categories")
    public PageResponse<TransactionCategoryResponse> categories(
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(MAX_PAGE_SIZE) int size) {
        return PageResponse.of(categories.findAll(PageRequest.of(page, size, Sort.by("id.typeCd", "id.catCd")))
                .map(TransactionCategoryResponse::from));
    }
}
