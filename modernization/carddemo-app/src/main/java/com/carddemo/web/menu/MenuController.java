package com.carddemo.web.menu;

import com.carddemo.common.RecordNotFoundException;
import com.carddemo.common.online.MessageColor;
import com.carddemo.common.web.ApiError;
import com.carddemo.common.web.ApiErrors;
import com.carddemo.user.menu.MenuDefinition;
import com.carddemo.user.menu.MenuSelection;
import com.carddemo.user.menu.MenuService;
import com.carddemo.user.signon.SignOnService;
import com.carddemo.web.NavigationContext;
import com.carddemo.web.OpenApiConfiguration;
import com.carddemo.web.ScreenHeaders;
import com.carddemo.web.security.CurrentUser;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ProblemDetail;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * {@code COMEN01C} (CM00, {@code /main}) and {@code COADM01C} (CA00, {@code /admin}, role ADMIN). First entry →
 * {@code GET}, ENTER → {@code POST /{menu}/selection}, PF3 → {@code POST /{menu}/exit}; other methods answer the
 * invalid-key message (R-5). Without a token the caller is sent back to sign-on (R-1).
 */
@RestController
@ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
@RequestMapping(MenuController.PATH)
@Tag(name = "Menus (COMEN01C, COADM01C)", description = "Main menu CM00 (COMEN02Y) and admin menu CA00 (COADM02Y)")
@SecurityRequirement(name = OpenApiConfiguration.BEARER)
public class MenuController {

    public static final String PATH = "/api/v1/menu";

    private final MenuService menus;
    private final ScreenHeaders headers;

    public MenuController(MenuService menus, ScreenHeaders headers) {
        this.menus = menus;
        this.headers = headers;
    }

    @GetMapping
    @Operation(summary = "Menu of the signed-on user",
            description = "COADM01C for role ADMIN, COMEN01C for role USER (COSGN00C R-10/R-11), with the BMS "
                    + "option numbering and labels.")
    public MenuScreen menu(@AuthenticationPrincipal Jwt jwt) {
        return screen(menus.catalog().forUserType(CurrentUser.of(jwt).userType()));
    }

    @GetMapping("/{menu}")
    @Operation(summary = "Menu screen (first entry, R-2)", description = "main = COMEN01C, admin = COADM01C (ADMIN)")
    @ApiResponse(responseCode = "200", description = "Menu",
            content = @Content(schema = @Schema(implementation = MenuScreen.class)))
    @ApiResponse(responseCode = "403", description = "NOTAUTH: admin menu for a USER token",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    public MenuScreen menuScreen(@Parameter(example = "main") @PathVariable("menu") String menu) {
        return screen(definition(menu));
    }

    @PostMapping(path = "/{menu}/selection", consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Select an option (ENTER, R-7..R-12)",
            description = "Normalises the option like COBOL (right-trim, spaces to zeros), validates it and returns "
                    + "the navigation to the target program, or the menu message for DUMMY / not-installed rows.",
            requestBody = @io.swagger.v3.oas.annotations.parameters.RequestBody(required = true, content = @Content(
                    schema = @Schema(implementation = MenuSelectionRequest.class),
                    examples = {
                        @ExampleObject(name = "accountView", summary = "01 Account View → COACTVWC",
                                value = "{\"option\": \"1\"}"),
                        @ExampleObject(name = "leadingSpace", summary = "' 1' is normalised to 01 (R-7)",
                                value = "{\"option\": \" 1\"}"),
                        @ExampleObject(name = "notInstalled", summary = "11 Pending Authorization View (R-10)",
                                value = "{\"option\": \"11\"}"),
                        @ExampleObject(name = "invalid", summary = "R-8: Please enter a valid option number...",
                                value = "{\"option\": \"99\"}")
                    })))
    @ApiResponse(responseCode = "200", description = "Navigation or informational message",
            content = @Content(schema = @Schema(implementation = MenuSelectionResponse.class)))
    @ApiResponse(responseCode = "400", description = "INVREQ: not a valid option number (R-8)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "403", description = "NOTAUTH: admin-only option for a USER (COMEN01C R-9)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    public ResponseEntity<?> select(@Parameter(example = "main") @PathVariable("menu") String menu,
            @AuthenticationPrincipal Jwt jwt, @Valid @RequestBody MenuSelectionRequest request) {
        MenuSelection selection = menus.select(definition(menu), CurrentUser.of(jwt).userType(), request.option());
        return switch (selection) {
            case MenuSelection.Transfer t -> ResponseEntity.ok(new MenuSelectionResponse(t.option(),
                    NavigationContext.transfer(t.fromTranId(), t.fromProgram(), t.targetTranId(),
                            t.target().programId()),
                    "", MessageColor.DEFAULT));
            case MenuSelection.Info i -> ResponseEntity.ok(new MenuSelectionResponse(i.option(), null, i.message(),
                    i.color()));
            case MenuSelection.Rejected r -> rejected(r);
        };
    }

    @PostMapping("/{menu}/exit")
    @Operation(summary = "Back to sign-on (PF3, R-4)", description = "Navigation to COSGN00C; discard the token.")
    public NavigationContext exit(@Parameter(example = "main") @PathVariable("menu") String menu) {
        MenuDefinition definition = definition(menu);
        return NavigationContext.transfer(definition.tranId(), definition.programId(), SignOnService.TRANID,
                SignOnService.PROGRAM);
    }

    private MenuDefinition definition(String key) {
        return menus.catalog().byKey(key)
                .orElseThrow(() -> new RecordNotFoundException("No such menu: " + key));
    }

    private MenuScreen screen(MenuDefinition menu) {
        return new MenuScreen(headers.of(menu.tranId(), menu.programId()), menu.key(), menu.programId(),
                menu.tranId(), menu.mapset(), menu.map(), menu.options().stream().map(MenuOptionView::of).toList(),
                menu.optionLines(), "");
    }

    private static ResponseEntity<ProblemDetail> rejected(MenuSelection.Rejected rejected) {
        HttpStatus status = switch (rejected.reason()) {
            case INVALID_OPTION -> HttpStatus.BAD_REQUEST;
            case ADMIN_ONLY -> HttpStatus.FORBIDDEN;
        };
        String code = switch (rejected.reason()) {
            case INVALID_OPTION -> "INVREQ";
            case ADMIN_ONLY -> "NOTAUTH";
        };
        ProblemDetail problem = ApiErrors.problem(status, code, "option", rejected.message());
        problem.setProperty("option", rejected.option());
        return ResponseEntity.status(status).contentType(MediaType.APPLICATION_PROBLEM_JSON).body(problem);
    }
}
