package com.carddemo.web.card;

import com.carddemo.card.Card;
import com.carddemo.card.online.CardBrowse;
import com.carddemo.card.online.CardKeys;
import com.carddemo.card.online.CardLookup;
import com.carddemo.card.online.CardSelection;
import com.carddemo.card.online.CardUpdateService;
import com.carddemo.common.InvalidRequestException;
import com.carddemo.common.NotAuthorizedException;
import com.carddemo.common.PanMask;
import com.carddemo.common.web.ApiError;
import com.carddemo.user.UserType;
import com.carddemo.web.NavigationContext;
import com.carddemo.web.OpenApiConfiguration;
import com.carddemo.web.ScreenHeaders;
import com.carddemo.web.security.CurrentUser;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import java.util.ArrayList;
import java.util.List;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.http.MediaType;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
@RequestMapping(CardController.PATH)
@Tag(name = "Cards (COCRDLIC, COCRDSLC, COCRDUPC)",
        description = "Card list CCLI (map CCRDLIA), card detail CCDL (map CCRDSLA) and card update CCUP (map "
                + "CCRDUPA). List responses mask card numbers; the detail shows the full number (ADR-0020).")
@SecurityRequirement(name = OpenApiConfiguration.BEARER)
public class CardController {

    public static final String PATH = "/api/v1/cards";

    static final String LIST_TRAN = "CCLI";
    static final String LIST_PROGRAM = "COCRDLIC";
    static final String VIEW_TRAN = "CCDL";
    static final String VIEW_PROGRAM = "COCRDSLC";
    static final String UPDATE_TRAN = "CCUP";
    static final String UPDATE_PROGRAM = "COCRDUPC";
    static final String MENU_TRAN = "CM00";
    static final String MENU_PROGRAM = "COMEN01C";

    public static final String MSG_ACCOUNT_REQUIRED_FOR_USER =
            "A regular user can only list the cards of the account in context: supply accountId";
    public static final String MSG_LIMIT = "limit must be 7 (WS-MAX-SCREEN-LINES)";
    public static final String MSG_ONE_CURSOR = "Send either after (PF8) or before (PF7), not both";
    public static final String MSG_BAD_CURSOR = "Cursor must be a cardRef or a card number from the page shown";

    private final CardBrowse browse;
    private final CardLookup lookup;
    private final CardUpdateService updates;
    private final CardReferences refs;
    private final ScreenHeaders headers;

    public CardController(CardBrowse browse, CardLookup lookup, CardUpdateService updates, CardReferences refs,
            ScreenHeaders headers) {
        this.browse = browse;
        this.lookup = lookup;
        this.updates = updates;
        this.refs = refs;
        this.headers = headers;
    }

    @GetMapping
    @Operation(summary = "List cards, seven per page (COCRDLIC)",
            description = "Browses CARDDAT in card-number order (STARTBR GTEQ + READNEXT, PF8 = after, PF7 = "
                    + "before = READPREV), seven rows and one look-ahead record. Filters are edited like "
                    + "2210/2220 (blank = no filter, otherwise numeric) and both apply when given. An ADMIN sees "
                    + "every card; a USER only the cards of the account in context, i.e. accountId is required "
                    + "(403 NOTAUTH otherwise). Card numbers are masked; use cardRef to address a card.")
    @ApiResponse(responseCode = "200", description = "One page (possibly empty)",
            content = @Content(schema = @Schema(implementation = CardListScreen.class)))
    @ApiResponse(responseCode = "400", description = "INVREQ: filter not numeric (R-15, R-16), bad cursor or limit",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "403", description = "NOTAUTH: a USER without the account in context",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    public CardListScreen list(
            @Parameter(description = "ACCTSID filter, up to 11 digits", example = "00000000050")
            @RequestParam(name = "accountId", required = false) String accountId,
            @Parameter(description = "CARDSID filter, up to 16 digits or a cardRef")
            @RequestParam(name = "cardNumber", required = false) String cardNumber,
            @Parameter(description = "PF8: nextPage of the page shown (its last cardRef)")
            @RequestParam(name = "after", required = false) String after,
            @Parameter(description = "PF7: previousPage of the page shown (its first cardRef)")
            @RequestParam(name = "before", required = false) String before,
            @Parameter(description = "Rows per page; only 7 is accepted", example = "7")
            @RequestParam(name = "limit", required = false) Integer limit,
            @AuthenticationPrincipal Jwt jwt) {
        if (limit != null && limit != 7) {
            throw new InvalidRequestException("limit", MSG_LIMIT);
        }
        if (after != null && before != null) {
            throw new InvalidRequestException("before", MSG_ONE_CURSOR);
        }
        CardKeys filter = CardKeys.listFilters(accountId, refs.cardNumberOrRef(cardNumber));
        if (!isAdmin(jwt) && filter.acctId() == null) {
            throw new NotAuthorizedException(CardKeys.ACCOUNT_FIELD, MSG_ACCOUNT_REQUIRED_FOR_USER);
        }
        CardBrowse.Screen screen = browse.browse(filter, cursor(after, "after"), cursor(before, "before"));
        List<CardListRow> rows = new ArrayList<>();
        for (int i = 0; i < screen.rows().size(); i++) {
            Card card = screen.rows().get(i);
            rows.add(new CardListRow(i + 1, String.format("%011d", card.getAcctId()),
                    PanMask.mask(card.getCardNum()), card.getActiveStatus().code(), refs.encode(card.getCardNum())));
        }
        String previous = screen.hasPrevious() ? rows.get(0).cardRef() : null;
        String next = screen.hasNext() ? rows.get(rows.size() - 1).cardRef() : null;
        return new CardListScreen(headers.of(LIST_TRAN, LIST_PROGRAM),
                filter.acctId() == null ? null : String.format("%011d", filter.acctId()),
                PanMask.mask(filter.cardNum()), 7, rows, screen.hasPrevious(), screen.hasNext(), previous, next,
                screen.infoMessage(), screen.message(), exit(LIST_TRAN, LIST_PROGRAM, null));
    }

    @PostMapping(path = "/selection", consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Select a row of the page shown (COCRDLIC ENTER)",
            description = "2250-EDIT-ARRAY on the CRDSEL codes: one S routes to the detail (COCRDSLC), one U to "
                    + "the update (COCRDUPC); more than one S/U or any other code is rejected. No selection stays "
                    + "on the list.")
    @ApiResponse(responseCode = "200", description = "XCTL target, or no selection",
            content = @Content(schema = @Schema(implementation = CardSelectionResponse.class)))
    @ApiResponse(responseCode = "400", description = "INVREQ: more than one selection or invalid code (R-17)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    public CardSelectionResponse select(@Valid @RequestBody CardSelectionRequest request) {
        CardSelection selection = CardSelection.of(request.rows().stream()
                .map(CardSelectionRequest.Row::action).toList());
        if (selection.none()) {
            return new CardSelectionResponse(headers.of(LIST_TRAN, LIST_PROGRAM), null, null, null,
                    CardBrowse.MSG_SELECT_ACTIONS);
        }
        String ref = request.rows().get(selection.row()).cardRef();
        String cardNum = refs.decode(ref).orElseThrow(() ->
                new InvalidRequestException("rows[" + selection.row() + "].cardRef", MSG_BAD_CURSOR));
        Card card = lookup.byCardNumber(new CardKeys(null, cardNum), false);
        boolean view = CardSelection.VIEW.equals(selection.action());
        NavigationContext target = NavigationContext.transfer(LIST_TRAN, LIST_PROGRAM,
                view ? VIEW_TRAN : UPDATE_TRAN, view ? VIEW_PROGRAM : UPDATE_PROGRAM)
                .withSelection(null, card.getAcctId(), null);
        String next = PATH + "/" + ref + "?accountId=" + String.format("%011d", card.getAcctId()) + "&fromProgram="
                + LIST_PROGRAM;
        return new CardSelectionResponse(headers.of(LIST_TRAN, LIST_PROGRAM), target, ref, next, "");
    }

    @GetMapping("/{cardNumber}")
    @Operation(summary = "View a card (COCRDSLC)",
            description = "Edits account and card like 2210/2220 (both required), then READ CARDDAT by card "
                    + "number. COBOL does not cross-check the account for an ADMIN; a USER only sees a card of the "
                    + "account given (otherwise NOTFND). The full card number is returned, as CCRDSLA shows it.")
    @ApiResponse(responseCode = "200", description = "Card detail with its version and the update form",
            content = @Content(schema = @Schema(implementation = CardDetailScreen.class)))
    @ApiResponse(responseCode = "400", description = "INVREQ: account/card missing or not numeric (R-11..R-15)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "404", description = "NOTFND: no such card (R-17)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    public CardDetailScreen view(
            @Parameter(description = "CARDSID: 16 digits or a cardRef", example = "0500024453765740")
            @PathVariable("cardNumber") String cardNumber,
            @Parameter(description = "ACCTSID, up to 11 digits", example = "00000000050")
            @RequestParam(name = "accountId", required = false) String accountId,
            @Parameter(description = "CDEMO-FROM-PROGRAM: COCRDLIC when arriving from the list (PF3 returns there)")
            @RequestParam(name = "fromProgram", required = false) String fromProgram,
            @AuthenticationPrincipal Jwt jwt) {
        CardKeys keys = CardKeys.searchKeys(accountId, refs.cardNumberOrRef(cardNumber));
        Card card = lookup.byCardNumber(keys, !isAdmin(jwt));
        return CardDetailScreen.of(headers.of(VIEW_TRAN, VIEW_PROGRAM), CardLookup.MSG_DISPLAYING, "", card,
                refs.encode(card.getCardNum()), exit(VIEW_TRAN, VIEW_PROGRAM, fromProgram));
    }

    @GetMapping("/by-account/{accountId}")
    @Operation(summary = "View the card of an account via the account path (COCRDSLC 9150-GETCARD-BYACCT)",
            description = "READ CARDAIX by account id: the account's lowest card number. Any signed-on user.")
    @ApiResponse(responseCode = "200", description = "Card detail",
            content = @Content(schema = @Schema(implementation = CardDetailScreen.class)))
    @ApiResponse(responseCode = "400", description = "INVREQ: account missing or not numeric (R-11, R-12)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "404", description = "NOTFND: account has no card in CARDAIX",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    public CardDetailScreen viewByAccount(
            @Parameter(description = "ACCTSID, up to 11 digits", example = "00000000050")
            @PathVariable("accountId") String accountId,
            @RequestParam(name = "fromProgram", required = false) String fromProgram) {
        Card card = lookup.byAccount(CardKeys.accountKey(accountId));
        return CardDetailScreen.of(headers.of(VIEW_TRAN, VIEW_PROGRAM), CardLookup.MSG_DISPLAYING, "", card,
                refs.encode(card.getCardNum()), exit(VIEW_TRAN, VIEW_PROGRAM, fromProgram));
    }

    @PutMapping(path = "/{cardNumber}", consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Update a card (COCRDUPC)",
            description = "One request runs the CCUP dialogue: key edits and read as in the GET (400/404), "
                    + "version check against the version displayed (409 CHANGED 'Record changed by some one else. "
                    + "Please review'), change detection after upper-case/trim (200 SHOW 'No change detected with "
                    + "respect to values fetched.'), the edits 1230..1260 in COBOL order (400 with the first "
                    + "failing field and its message, all failing fields in invalidFields), then confirm=false = "
                    + "ENTER → 200 VALIDATED, nothing written; confirm=true = PF5 → READ UPDATE (SELECT ... FOR "
                    + "UPDATE), version re-check, REWRITE → 200 COMMITTED with the new version.",
            requestBody = @io.swagger.v3.oas.annotations.parameters.RequestBody(required = true,
                    description = "Start from updateForm of GET /api/v1/cards/{cardNumber}",
                    content = @Content(schema = @Schema(implementation = CardUpdateRequest.class))))
    @ApiResponse(responseCode = "200", description = "SHOW (no change), VALIDATED or COMMITTED",
            content = @Content(schema = @Schema(implementation = CardUpdateResponse.class)))
    @ApiResponse(responseCode = "400", description = "INVREQ: a key or field edit failed (R-10..R-18)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "404", description = "NOTFND: no such card (R-26)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "409", description = "CHANGED: card changed since it was read (R-29)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "500", description = "ABEND: lock or rewrite failed (R-28, R-31)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    public CardUpdateResponse update(
            @Parameter(description = "CARDSID: 16 digits or a cardRef", example = "0500024453765740")
            @PathVariable("cardNumber") String cardNumber,
            @RequestParam(name = "fromProgram", required = false) String fromProgram,
            @Valid @RequestBody CardUpdateRequest request,
            @AuthenticationPrincipal Jwt jwt) {
        CardKeys keys = CardKeys.searchKeys(request.accountId(), refs.cardNumberOrRef(cardNumber));
        CardUpdateService.Outcome outcome = updates.update(keys, request.toChanges(), request.version(),
                request.confirmed(), !isAdmin(jwt));
        CardDetailScreen card = CardDetailScreen.of(headers.of(UPDATE_TRAN, UPDATE_PROGRAM),
                outcome.state().infoMessage(), outcome.message(), outcome.card(),
                refs.encode(outcome.card().getCardNum()), exit(UPDATE_TRAN, UPDATE_PROGRAM, fromProgram));
        return new CardUpdateResponse(headers.of(UPDATE_TRAN, UPDATE_PROGRAM), outcome.state(),
                outcome.state() == CardUpdateService.State.COMMITTED, outcome.state().infoMessage(),
                outcome.message(), card);
    }

    private static boolean isAdmin(Jwt jwt) {
        return CurrentUser.of(jwt).userType() == UserType.ADMIN;
    }

    private String cursor(String value, String field) {
        if (value == null) {
            return null;
        }
        return refs.cursor(value).orElseThrow(() -> new InvalidRequestException(field, MSG_BAD_CURSOR));
    }

    /** PF3: back to the list when arrived from it (COCRDSLC R-4, COCRDUPC R-3), else the main menu. */
    private static NavigationContext exit(String tranId, String program, String fromProgram) {
        if (LIST_PROGRAM.equals(fromProgram) && !LIST_PROGRAM.equals(program)) {
            return NavigationContext.transfer(tranId, program, LIST_TRAN, LIST_PROGRAM);
        }
        return NavigationContext.transfer(tranId, program, MENU_TRAN, MENU_PROGRAM);
    }
}
