package com.carddemo.web.signon;

import com.carddemo.common.online.CommonMessages;
import com.carddemo.common.web.ApiError;
import com.carddemo.common.web.ApiErrors;
import com.carddemo.user.signon.SignOnResult;
import com.carddemo.user.signon.SignOnService;
import com.carddemo.user.menu.MenuCatalog;
import com.carddemo.web.NavigationContext;
import com.carddemo.web.ScreenHeaders;
import com.carddemo.web.menu.MenuController;
import com.carddemo.web.security.IssuedToken;
import com.carddemo.web.security.TokenService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * {@code COSGN00C} (transaction CC00, map COSGN0A). ENTER → {@code POST /login}, PF3 → {@code POST /logout}, first
 * entry → {@code GET /login}; any other method answers the invalid-key message (R-4).
 */
@RestController
@ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
@RequestMapping("/api/v1/auth")
@Tag(name = "Sign-on (COSGN00C)", description = "Transaction CC00: sign-on against USRSEC, routing by user type")
public class SignOnController {

    static final String PATH = "/api/v1/auth/login";

    private final SignOnService signOn;
    private final TokenService tokens;
    private final ScreenHeaders headers;
    private final MenuCatalog menus;

    public SignOnController(SignOnService signOn, TokenService tokens, ScreenHeaders headers, MenuCatalog menus) {
        this.signOn = signOn;
        this.tokens = tokens;
        this.headers = headers;
        this.menus = menus;
    }

    @GetMapping("/login")
    @Operation(summary = "Sign-on screen (first entry, R-1)", description = "Empty COSGN0A map with header fields")
    public SignOnScreen screen() {
        return new SignOnScreen(headers.of(SignOnService.TRANID, SignOnService.PROGRAM), "",
                SignOnService.USER_ID_FIELD);
    }

    @PostMapping(path = "/login", consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Sign on (ENTER, R-5..R-14)",
            description = "Checks the user id/password against USRSEC (upper-cased, plain-text compare, ADR-0018) and "
                    + "returns a bearer token plus the menu to go to: COADM01C for type A, COMEN01C otherwise.",
            requestBody = @io.swagger.v3.oas.annotations.parameters.RequestBody(required = true, content = @Content(
                    schema = @Schema(implementation = LoginRequest.class),
                    examples = {
                        @ExampleObject(name = "admin", summary = "Administrator (sample USRSEC row)",
                                value = "{\"userId\": \"ADMIN001\", \"password\": \"PASSWORD\"}"),
                        @ExampleObject(name = "user", summary = "Regular user (sample USRSEC row)",
                                value = "{\"userId\": \"USER0001\", \"password\": \"PASSWORD\"}"),
                        @ExampleObject(name = "lowercase", summary = "Input is upper-cased (R-7)",
                                value = "{\"userId\": \"user0001\", \"password\": \"password\"}"),
                        @ExampleObject(name = "blankUserId", summary = "R-5: Please enter User ID ...",
                                value = "{\"userId\": \"\", \"password\": \"PASSWORD\"}"),
                        @ExampleObject(name = "blankPassword", summary = "R-6: Please enter Password ...",
                                value = "{\"userId\": \"USER0001\", \"password\": \"\"}"),
                        @ExampleObject(name = "wrongPassword", summary = "R-12: Wrong Password. Try again ...",
                                value = "{\"userId\": \"USER0001\", \"password\": \"WRONG\"}"),
                        @ExampleObject(name = "unknownUser", summary = "R-13: User not found. Try again ...",
                                value = "{\"userId\": \"NOBODY\", \"password\": \"PASSWORD\"}")
                    })))
    @ApiResponse(responseCode = "200", description = "Signed on",
            content = @Content(schema = @Schema(implementation = LoginResponse.class)))
    @ApiResponse(responseCode = "400", description = "INVREQ: blank user id (R-5) or password (R-6)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "401", description = "NOTFND: user not found (R-13); WRONG_PASSWORD (R-12)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "500", description = "OTHER: USRSEC could not be read (R-14)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    public ResponseEntity<?> login(@Valid @RequestBody LoginRequest request) {
        SignOnResult result = signOn.signOn(request.userId(), request.password());
        return switch (result) {
            case SignOnResult.SignedOn user -> ResponseEntity.ok(signedOn(user));
            case SignOnResult.Rejected rejected -> rejected(rejected);
        };
    }

    @PostMapping("/logout")
    @Operation(summary = "Sign off (PF3, R-3)",
            description = "Ends the conversation. Tokens are stateless: the client discards its token.")
    public SignOffResponse logout() {
        return new SignOffResponse(CommonMessages.THANK_YOU);
    }

    private LoginResponse signedOn(SignOnResult.SignedOn user) {
        IssuedToken token = tokens.issue(user.userId(), user.userType());
        var menu = menus.forUserType(user.userType());
        NavigationContext navigation = NavigationContext.transfer(SignOnService.TRANID, SignOnService.PROGRAM,
                menu.tranId(), menu.programId());
        return new LoginResponse(token.value(), "Bearer", token.expiresAt(), user.userId(), user.userType().name(),
                user.userType().code(), user.targetProgram(), MenuController.PATH + "/" + menu.key(), navigation);
    }

    private static ResponseEntity<?> rejected(SignOnResult.Rejected rejected) {
        HttpStatus status = switch (rejected.reason()) {
            case USER_ID_BLANK, PASSWORD_BLANK -> HttpStatus.BAD_REQUEST;
            case WRONG_PASSWORD, USER_NOT_FOUND -> HttpStatus.UNAUTHORIZED;
            case UNABLE_TO_VERIFY -> HttpStatus.INTERNAL_SERVER_ERROR;
        };
        String code = switch (rejected.reason()) {
            case USER_ID_BLANK, PASSWORD_BLANK -> "INVREQ";
            case WRONG_PASSWORD -> "WRONG_PASSWORD";
            case USER_NOT_FOUND -> "NOTFND";
            case UNABLE_TO_VERIFY -> "OTHER";
        };
        return ResponseEntity.status(status).contentType(MediaType.APPLICATION_PROBLEM_JSON)
                .body(ApiErrors.problem(status, code, rejected.field(), rejected.message()));
    }
}
