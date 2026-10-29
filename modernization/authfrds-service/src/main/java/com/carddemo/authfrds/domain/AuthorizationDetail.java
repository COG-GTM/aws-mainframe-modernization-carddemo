package com.carddemo.authfrds.domain;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.DecimalMax;
import jakarta.validation.constraints.DecimalMin;
import jakarta.validation.constraints.Digits;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import java.math.BigDecimal;

/**
 * IMS segment PAUTDTL1 (copybook CIPAUDTY), i.e. WS-FRAUD-AUTH-RECORD in the COPAUS2C commarea.
 * BR-AF-011: field sizes and numeric PICs bound the commarea, so they are validated here.
 */
public record AuthorizationDetail(
        @Schema(description = "PA-AUTH-DATE-9C S9(05) COMP-3 = 99999 - YYDDD", example = "73697")
        @NotNull @Min(0) @Max(99999) Long authDate9c,
        @Schema(description = "PA-AUTH-TIME-9C S9(09) COMP-3 = 999999999 - HHMMSSmmm", example = "849999877")
        @NotNull @Min(0) @Max(999999999) Long authTime9c,
        @Schema(description = "PA-AUTH-ORIG-DATE X(06) YYMMDD", example = "261029")
        @NotNull @Pattern(regexp = "\\d{6}", message = "must be 6 digits (YYMMDD)") String authOrigDate,
        @Schema(example = "150000") @Size(max = 6) String authOrigTime,
        @Schema(example = "4111222233334444")
        @NotNull @Pattern(regexp = "\\d{16}", message = "must be 16 digits") String cardNum,
        @Schema(example = "0100") @Size(max = 4) String authType,
        @Schema(example = "1128") @Size(max = 4) String cardExpiryDate,
        @Schema(example = "0100") @Size(max = 6) String messageType,
        @Schema(example = "POS") @Size(max = 6) String messageSource,
        @Schema(example = "A1B2C3") @Size(max = 6) String authIdCode,
        @Schema(example = "00") @Size(max = 2) String authRespCode,
        @Schema(example = "0000") @Size(max = 4) String authRespReason,
        @Schema(example = "000000") @Pattern(regexp = "\\d{0,6}", message = "must be up to 6 digits") String processingCode,
        @Schema(example = "125.50") @Digits(integer = 10, fraction = 2)
        @DecimalMin("-9999999999.99") @DecimalMax("9999999999.99") BigDecimal transactionAmt,
        @Schema(example = "125.50") @Digits(integer = 10, fraction = 2)
        @DecimalMin("-9999999999.99") @DecimalMax("9999999999.99") BigDecimal approvedAmt,
        @Schema(example = "5411") @Size(max = 4) String merchantCategoryCode,
        @Schema(example = "USA") @Size(max = 3) String acqrCountryCode,
        @Schema(example = "05") @Pattern(regexp = "\\d{0,2}", message = "must be up to 2 digits") String posEntryMode,
        @Schema(example = "MERCH0000000001") @Size(max = 15) String merchantId,
        @Schema(example = "Demo Grocery") @Size(max = 22) String merchantName,
        @Schema(example = "Seattle") @Size(max = 13) String merchantCity,
        @Schema(example = "WA") @Size(max = 2) String merchantState,
        @Schema(example = "98101") @Size(max = 9) String merchantZip,
        @Schema(example = "TXN000000000001") @Size(max = 15) String transactionId,
        @Schema(example = "P") @Size(max = 1) String matchStatus,
        @Schema(description = "PA-AUTH-FRAUD: F = fraud confirmed, R = fraud removed", example = " ")
        @Size(max = 1) String authFraud,
        @Schema(description = "PA-FRAUD-RPT-DATE X(08), MM/DD/YY", example = "        ")
        @Size(max = 8) String fraudRptDate) {

    public AuthKey key() {
        return new AuthKey(authDate9c, authTime9c);
    }

    public AuthorizationDetail withFraud(String flag, String rptDate) {
        return new AuthorizationDetail(authDate9c, authTime9c, authOrigDate, authOrigTime, cardNum, authType,
                cardExpiryDate, messageType, messageSource, authIdCode, authRespCode, authRespReason, processingCode,
                transactionAmt, approvedAmt, merchantCategoryCode, acqrCountryCode, posEntryMode, merchantId,
                merchantName, merchantCity, merchantState, merchantZip, transactionId, matchStatus, flag, rptDate);
    }
}
