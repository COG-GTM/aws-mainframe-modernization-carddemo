package com.carddemo.account.api.dto;

import jakarta.validation.constraints.DecimalMin;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * Update payload of COACTUPC. The edit rules of the 3270 screen become bean validation
 * constraints; the rules that need the stored record (see AccountService) stay in the service.
 */
public record AccountUpdateRequest(
        @NotNull @Pattern(regexp = "[YNyn]", message = "active status must be Y or N") String activeStatus,
        @NotNull @DecimalMin(value = "0.00", message = "credit limit cannot be negative") BigDecimal creditLimit,
        @NotNull @DecimalMin(value = "0.00", message = "cash credit limit cannot be negative") BigDecimal cashCreditLimit,
        @NotNull LocalDate openDate,
        @NotNull LocalDate expirationDate,
        LocalDate reissueDate,
        @Size(max = 10) String addressZip,
        @Size(max = 10) String groupId) {
}
