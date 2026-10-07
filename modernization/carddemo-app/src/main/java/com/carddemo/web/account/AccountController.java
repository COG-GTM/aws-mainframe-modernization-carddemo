package com.carddemo.web.account;

import com.carddemo.account.online.AccountDetails;
import com.carddemo.account.online.AccountLookup;
import com.carddemo.account.online.AccountUpdateService;
import com.carddemo.common.web.ApiError;
import com.carddemo.web.NavigationContext;
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
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
@RequestMapping(AccountController.PATH)
@Tag(name = "Accounts (COACTVWC, COACTUPC)",
        description = "Account view CAVW (map CACTVWA) and account update CAUP (map CACTUPA)")
@SecurityRequirement(name = OpenApiConfiguration.BEARER)
public class AccountController {

    public static final String PATH = "/api/v1/accounts";

    static final String VIEW_TRAN = "CAVW";
    static final String VIEW_PROGRAM = "COACTVWC";
    static final String UPDATE_TRAN = "CAUP";
    static final String UPDATE_PROGRAM = "COACTUPC";
    static final String MENU_TRAN = "CM00";
    static final String MENU_PROGRAM = "COMEN01C";
    public static final String MSG_VIEW_PROMPT = "Enter or update id of account to display";

    private final AccountLookup lookup;
    private final AccountUpdateService updates;
    private final ScreenHeaders headers;

    public AccountController(AccountLookup lookup, AccountUpdateService updates, ScreenHeaders headers) {
        this.lookup = lookup;
        this.updates = updates;
        this.headers = headers;
    }

    @GetMapping("/{id}")
    @Operation(summary = "View an account (COACTVWC)",
            description = "Edits the id like 2210-EDIT-ACCOUNT (1-11 digits, not zero), then reads CXACAIX "
                    + "(account → customer, card), ACCTDAT and CUSTDAT. Returns every CACTVWA field, the card "
                    + "numbers of the account, the versions and the update form for PUT. Any signed-on user.")
    @ApiResponse(responseCode = "200", description = "Account, customer and cards",
            content = @Content(schema = @Schema(implementation = AccountViewScreen.class)))
    @ApiResponse(responseCode = "400", description = "INVREQ: id blank, not numeric or zero (R-7, R-8)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "404", description = "NOTFND: not in CXACAIX, ACCTDAT or CUSTDAT (R-11, R-13, R-14)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    public AccountViewScreen view(@Parameter(description = "Account id (ACCTSID), up to 11 digits",
            example = "00000000001") @PathVariable("id") String id) {
        long acctId = AccountLookup.accountId(id, AccountLookup.MSG_VIEW_ACCOUNT_INVALID);
        AccountDetails details = lookup.read(acctId);
        return AccountViewScreen.of(headers.of(VIEW_TRAN, VIEW_PROGRAM), MSG_VIEW_PROMPT, "", details,
                exit(VIEW_TRAN, VIEW_PROGRAM));
    }

    @PutMapping(path = "/{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Update an account and its customer (COACTUPC)",
            description = "One request runs the CAUP dialogue: lookup as in the GET (404), version check against "
                    + "the versions that were displayed (409 CHANGED 'Record changed by some one else. Please "
                    + "review'), change detection against the stored values (200 SHOW 'No change detected with "
                    + "respect to values fetched.'), the field edits in COBOL order (400 with the first failing "
                    + "field and its COBOL message, all failing fields in invalidFields), then: confirm=false is "
                    + "ENTER → 200 VALIDATED 'Changes validated.Press F5 to save', nothing written; confirm=true "
                    + "is PF5 → account and customer rewritten in one transaction → 200 COMMITTED 'Changes "
                    + "committed to database' with the new versions. Any signed-on user.",
            requestBody = @io.swagger.v3.oas.annotations.parameters.RequestBody(required = true,
                    description = "Start from updateForm of GET /api/v1/accounts/{id}",
                    content = @Content(schema = @Schema(implementation = AccountUpdateRequest.class))))
    @ApiResponse(responseCode = "200", description = "SHOW (no change), VALIDATED or COMMITTED",
            content = @Content(schema = @Schema(implementation = AccountUpdateResponse.class)))
    @ApiResponse(responseCode = "400", description = "INVREQ: a field edit failed (COBOL message, R-10..R-30)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "404", description = "NOTFND: not in CXACAIX, ACCTDAT or CUSTDAT (R-31)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "409", description = "CHANGED: account or customer changed since it was read (R-38)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "500", description = "ABEND: rewrite failed, both records rolled back (R-40)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    public AccountUpdateResponse update(@Parameter(description = "Account id (ACCTSID), up to 11 digits",
            example = "00000000001") @PathVariable("id") String id,
            @Valid @RequestBody AccountUpdateRequest request) {
        long acctId = AccountLookup.accountId(id, AccountLookup.MSG_UPDATE_ACCOUNT_INVALID);
        AccountUpdateService.Outcome outcome = updates.update(acctId, request.toChanges(),
                request.accountVersion(), request.customerVersion(), request.confirmed());
        AccountViewScreen account = AccountViewScreen.of(headers.of(UPDATE_TRAN, UPDATE_PROGRAM),
                outcome.state().infoMessage(), outcome.message(), outcome.details(),
                exit(UPDATE_TRAN, UPDATE_PROGRAM));
        return new AccountUpdateResponse(headers.of(UPDATE_TRAN, UPDATE_PROGRAM), outcome.state(),
                outcome.state() == AccountUpdateService.State.COMMITTED, outcome.state().infoMessage(),
                outcome.message(), account);
    }

    /** PF3: back to the caller, the main menu when there is none (stateless: always the main menu). */
    private static NavigationContext exit(String tranId, String program) {
        return NavigationContext.transfer(tranId, program, MENU_TRAN, MENU_PROGRAM);
    }
}
