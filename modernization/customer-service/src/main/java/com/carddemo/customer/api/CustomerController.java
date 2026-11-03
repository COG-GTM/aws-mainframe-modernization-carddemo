package com.carddemo.customer.api;

import com.carddemo.common.api.PageResponse;
import com.carddemo.customer.api.dto.CustomerResponse;
import com.carddemo.customer.api.dto.CustomerUpdateRequest;
import com.carddemo.customer.service.CustomerService;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.RestController;

@RestController
@Validated
@RequestMapping("/api/v1/customers")
public class CustomerController {

    private static final int MAX_PAGE_SIZE = 200;

    private final CustomerService customerService;

    public CustomerController(CustomerService customerService) {
        this.customerService = customerService;
    }

    @GetMapping("/{customerId}")
    public CustomerResponse get(@PathVariable long customerId) {
        return CustomerResponse.from(customerService.get(customerId));
    }

    @GetMapping
    public PageResponse<CustomerResponse> list(@RequestParam(defaultValue = "0") @Min(0) int page,
                                               @RequestParam(defaultValue = "20") @Min(1) @Max(MAX_PAGE_SIZE) int size) {
        return PageResponse.of(customerService.list(PageRequest.of(page, size, Sort.by("custId")))
                .map(CustomerResponse::from));
    }

    @PutMapping("/{customerId}")
    public CustomerResponse update(@PathVariable long customerId,
                                   @Valid @RequestBody CustomerUpdateRequest request) {
        return CustomerResponse.from(customerService.update(customerId, request));
    }
}
