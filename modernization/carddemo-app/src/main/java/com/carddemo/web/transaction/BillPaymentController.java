package com.carddemo.web.transaction;

import static com.carddemo.web.transaction.TransactionNavigation.BILL_PAY_PROGRAM;
import static com.carddemo.web.transaction.TransactionNavigation.BILL_PAY_TRAN;

import com.carddemo.common.web.ApiError;
import com.carddemo.transaction.online.BillPaymentService;
import com.carddemo.web.OpenApiConfiguration;
import com.carddemo.web.ScreenHeaders;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
@Tag(name = "Bill payment (COBIL00C)",
        description = "Bill payment CB00 (map COBIL0A): pay the account's current balance in full. COBOL ties no "
                + "user to an account, so any signed-on user may pay any account (ADR-0020).")
@SecurityRequirement(name = OpenApiConfiguration.BEARER)
public class BillPaymentController {

    public static final String PATH = "/api/v1/accounts/{id}/bill-payment";

    private final BillPaymentService payments;
    private final ScreenHeaders headers;

    public BillPaymentController(BillPaymentService payments, ScreenHeaders headers) {
        this.payments = payments;
        this.headers = headers;
    }

    @PostMapping(path = PATH, consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Pay the account balance (COBIL00C)",
            description = "confirm blank → 200 SHOW with the balance and the account version ('Confirm to make a "
                    + "bill payment...'); confirm Y with that version → one database transaction: lock the "
                    + "account row (SELECT ... FOR UPDATE), re-check the version (409 CHANGED when it changed or "
                    + "was paid meanwhile), write the bill-payment transaction (type 02, category 2, 'BILL PAYMENT "
                    + "- ONLINE', the full balance, the account's first card) and set the balance to 0.00 → 200 "
                    + "PAID. A balance <= 0 is 400 'You have nothing to pay...'. confirm N → 200 CLEARED.",
            requestBody = @io.swagger.v3.oas.annotations.parameters.RequestBody(required = true,
                    content = @Content(schema = @Schema(implementation = BillPaymentRequest.class))))
    @ApiResponse(responseCode = "200", description = "SHOW, PAID or CLEARED",
            content = @Content(schema = @Schema(implementation = BillPaymentResponse.class)))
    @ApiResponse(responseCode = "400", description = "INVREQ: account blank (R-7), confirm not Y/N (R-8), nothing "
            + "to pay (R-10), version missing with Y",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "404", description = "NOTFND: no such account (R-16)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "409", description = "CHANGED: account changed or already paid since the balance "
            + "was shown (concurrent payment); DUPREC: Tran ID already exist (R-20)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "500", description = "ABEND: lookup, write or update failed (everything rolled "
            + "back)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    public BillPaymentResponse pay(
            @Parameter(description = "ACTIDIN, up to 11 digits", example = "00000000001")
            @PathVariable("id") String accountId,
            @RequestParam(name = "fromProgram", required = false) String fromProgram,
            @Valid @RequestBody BillPaymentRequest request) {
        BillPaymentService.Outcome outcome = payments.pay(accountId, request.confirm(), request.version());
        var account = outcome.account();
        return new BillPaymentResponse(headers.of(BILL_PAY_TRAN, BILL_PAY_PROGRAM), outcome.state(),
                account == null ? null : String.format("%011d", account.getAcctId()),
                account == null ? null : account.getCurrBal().setScale(2).toPlainString(),
                account == null ? null : account.getVersion(),
                outcome.transaction() == null ? null : TransactionFields.of(outcome.transaction()), outcome.message(),
                TransactionNavigation.exit(BILL_PAY_TRAN, BILL_PAY_PROGRAM, fromProgram));
    }
}
