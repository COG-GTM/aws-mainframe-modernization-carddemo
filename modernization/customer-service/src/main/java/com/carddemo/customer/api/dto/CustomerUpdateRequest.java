package com.carddemo.customer.api.dto;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import java.time.LocalDate;

/** Customer half of the COACTUPC update. */
public record CustomerUpdateRequest(
        @NotBlank @Size(max = 25) String firstName,
        @Size(max = 25) String middleName,
        @NotBlank @Size(max = 25) String lastName,
        @Size(max = 50) String addressLine1,
        @Size(max = 50) String addressLine2,
        @Size(max = 50) String addressLine3,
        @Pattern(regexp = "[A-Z]{2}", message = "state code must be two upper case letters") String stateCode,
        @Pattern(regexp = "[A-Z]{3}", message = "country code must be three upper case letters") String countryCode,
        @Size(max = 10) String zip,
        @Size(max = 15) String phone1,
        @Size(max = 15) String phone2,
        @Size(max = 20) String governmentIssuedId,
        LocalDate dateOfBirth,
        @Size(max = 10) String eftAccountId,
        @Pattern(regexp = "[YN]") String primaryCardHolderIndicator,
        @Min(300) @Max(850) Integer ficoScore) {
}
