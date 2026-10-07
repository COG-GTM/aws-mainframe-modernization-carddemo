package com.carddemo.web.transaction;

import static com.carddemo.web.transaction.TransactionNavigation.ADD_PROGRAM;
import static com.carddemo.web.transaction.TransactionNavigation.ADD_TRAN;
import static com.carddemo.web.transaction.TransactionNavigation.LIST_PROGRAM;
import static com.carddemo.web.transaction.TransactionNavigation.LIST_TRAN;
import static com.carddemo.web.transaction.TransactionNavigation.VIEW_PROGRAM;
import static com.carddemo.web.transaction.TransactionNavigation.VIEW_TRAN;

import com.carddemo.common.InvalidRequestException;
import com.carddemo.common.online.ScreenInput;
import com.carddemo.common.web.ApiError;
import com.carddemo.transaction.Transaction;
import com.carddemo.transaction.online.TransactionAddService;
import com.carddemo.transaction.online.TransactionBrowse;
import com.carddemo.transaction.online.TransactionFormat;
import com.carddemo.transaction.online.TransactionLookup;
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
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
@RequestMapping(TransactionController.PATH)
@Tag(name = "Transactions (COTRN00C, COTRN01C, COTRN02C)",
        description = "Transaction list CT00 (map COTRN0A), detail CT01 (map COTRN1A) and add CT02 (map COTRN2A) "
                + "on the shared transaction table that the batch jobs (TRANREPT, CREASTMT) read. COBOL ties no "
                + "user to an account, so any signed-on user (ADMIN or USER) may use them (ADR-0020).")
@SecurityRequirement(name = OpenApiConfiguration.BEARER)
public class TransactionController {

    public static final String PATH = "/api/v1/transactions";
    public static final int PAGE_SIZE = 10;
    public static final String MSG_LIMIT = "limit must be 10 (rows on COTRN0A)";
    public static final String MSG_ONE_CURSOR = "Send either after (PF8) or before (PF7), not both";
    public static final String MSG_INVALID_SELECTION = "Invalid selection. Valid value is S";

    private final TransactionBrowse browse;
    private final TransactionLookup lookup;
    private final TransactionAddService adds;
    private final ScreenHeaders headers;

    public TransactionController(TransactionBrowse browse, TransactionLookup lookup, TransactionAddService adds,
            ScreenHeaders headers) {
        this.browse = browse;
        this.lookup = lookup;
        this.adds = adds;
        this.headers = headers;
    }

    @GetMapping
    @Operation(summary = "List transactions, ten per page (COTRN00C)",
            description = "Browses TRANSACT in TRAN-ID order: ENTER = from startTranId (blank = first, numeric = "
                    + "exact-or-greater), PF8 = after (last id shown), PF7 = before (first id shown). Send the "
                    + "pageNumber shown back as page= so PF7 on page 1 answers 'You are already at the top of the "
                    + "page...' like the program. Rows show id, MM/DD/YY, the first 26 characters of the "
                    + "description and the amount edited +99999999.99; no card numbers are listed.")
    @ApiResponse(responseCode = "200", description = "One page (possibly empty, with the program's message)",
            content = @Content(schema = @Schema(implementation = TransactionListScreen.class)))
    @ApiResponse(responseCode = "400", description = "INVREQ: start id not numeric (R-11), bad cursor or limit",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "500", description = "ABEND: browse failed (R-18..R-20 other RESP)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    public TransactionListScreen list(
            @Parameter(description = "TRNIDIN: start the list at this id (up to 16 digits)", example = "")
            @RequestParam(name = "startTranId", required = false) String startTranId,
            @Parameter(description = "PF8: nextPage of the page shown (its last id)")
            @RequestParam(name = "after", required = false) String after,
            @Parameter(description = "PF7: previousPage of the page shown (its first id)")
            @RequestParam(name = "before", required = false) String before,
            @Parameter(description = "PAGENUM of the page shown (with after/before)", example = "1")
            @RequestParam(name = "page", required = false) Integer page,
            @Parameter(description = "Rows per page; only 10 is accepted", example = "10")
            @RequestParam(name = "limit", required = false) Integer limit) {
        if (limit != null && limit != PAGE_SIZE) {
            throw new InvalidRequestException("limit", MSG_LIMIT);
        }
        if (after != null && before != null) {
            throw new InvalidRequestException("before", MSG_ONE_CURSOR);
        }
        TransactionBrowse.Screen screen = browse.browse(startTranId, after, before, page);
        List<TransactionListRow> rows = new ArrayList<>();
        for (int i = 0; i < screen.rows().size(); i++) {
            Transaction t = screen.rows().get(i);
            rows.add(new TransactionListRow(i + 1, t.getTranId(), TransactionFormat.listDate(t.getOrigTs()),
                    TransactionFormat.listDescription(t.getDescription()), TransactionFormat.amount(t.getAmount())));
        }
        String previous = screen.hasPrevious() && !rows.isEmpty() ? rows.get(0).tranId() : null;
        String next = screen.hasNext() && !rows.isEmpty() ? rows.get(rows.size() - 1).tranId() : null;
        return new TransactionListScreen(headers.of(LIST_TRAN, LIST_PROGRAM), screen.pageNumber(), PAGE_SIZE, rows,
                screen.hasPrevious(), screen.hasNext(), previous, next, screen.message(),
                TransactionNavigation.exit(LIST_TRAN, LIST_PROGRAM, null));
    }

    @PostMapping(path = "/selection", consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Select a row of the page shown (COTRN00C ENTER)",
            description = "The first row with a non-blank SELnnnn decides (R-6): S/s routes to the detail "
                    + "(COTRN01C); any other code is 'Invalid selection. Valid value is S' (R-8). No selection "
                    + "stays on the list.")
    @ApiResponse(responseCode = "200", description = "XCTL target, or no selection",
            content = @Content(schema = @Schema(implementation = TransactionSelectionResponse.class)))
    @ApiResponse(responseCode = "400", description = "INVREQ: selection code other than S (R-8)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    public TransactionSelectionResponse select(@Valid @RequestBody TransactionSelectionRequest request) {
        for (int i = 0; i < request.rows().size(); i++) {
            TransactionSelectionRequest.Row row = request.rows().get(i);
            if (ScreenInput.isSpacesOrLowValues(row.selection())) {
                continue;
            }
            if (!"S".equalsIgnoreCase(row.selection().strip())) {
                throw new InvalidRequestException("rows[" + i + "].selection", MSG_INVALID_SELECTION);
            }
            String tranId = row.tranId().strip();
            NavigationContext target = NavigationContext.transfer(LIST_TRAN, LIST_PROGRAM, VIEW_TRAN, VIEW_PROGRAM);
            return new TransactionSelectionResponse(headers.of(LIST_TRAN, LIST_PROGRAM), target, tranId,
                    PATH + "/" + tranId + "?fromProgram=" + LIST_PROGRAM);
        }
        return new TransactionSelectionResponse(headers.of(LIST_TRAN, LIST_PROGRAM), null, null, null);
    }

    @GetMapping("/{tranId}")
    @Operation(summary = "View a transaction (COTRN01C)",
            description = "READ TRANSACT by the id as typed (no numeric check, R-10). The card number is shown "
                    + "in full, as COTRN1A does (ADR-0020).")
    @ApiResponse(responseCode = "200", description = "Transaction detail",
            content = @Content(schema = @Schema(implementation = TransactionDetailScreen.class)))
    @ApiResponse(responseCode = "400", description = "INVREQ: id blank (R-9)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "404", description = "NOTFND: no such transaction (R-13)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    public TransactionDetailScreen view(
            @Parameter(description = "TRNIDIN", example = "0000000000000001") @PathVariable("tranId") String tranId,
            @Parameter(description = "CDEMO-FROM-PROGRAM: COTRN00C when arriving from the list (PF3 returns there)")
            @RequestParam(name = "fromProgram", required = false) String fromProgram) {
        Transaction t = lookup.byId(tranId);
        return new TransactionDetailScreen(headers.of(VIEW_TRAN, VIEW_PROGRAM), TransactionFields.of(t), "",
                TransactionNavigation.exit(VIEW_TRAN, VIEW_PROGRAM, fromProgram),
                NavigationContext.transfer(VIEW_TRAN, VIEW_PROGRAM, LIST_TRAN, LIST_PROGRAM));
    }

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Add a transaction (COTRN02C)",
            description = "Key edits (account wins over card, both resolved through CARDXREF), then the data "
                    + "edits in COBOL order (first failing field → 400 with its message), then TRANTYPE/TRANCATG "
                    + "must know the type and category. confirm blank/N → 200 VALIDATED 'Confirm to add this "
                    + "transaction...'; confirm Y → next 16-digit id (last id + 1, serialised by a database lock) "
                    + "and WRITE to the shared transaction table → 201 ADDED. copyLast=true is PF5: the last "
                    + "transaction's data replaces the typed data before the edits.",
            requestBody = @io.swagger.v3.oas.annotations.parameters.RequestBody(required = true,
                    content = @Content(schema = @Schema(implementation = TransactionAddRequest.class))))
    @ApiResponse(responseCode = "200", description = "VALIDATED: nothing written",
            content = @Content(schema = @Schema(implementation = TransactionAddResponse.class)))
    @ApiResponse(responseCode = "201", description = "ADDED",
            content = @Content(schema = @Schema(implementation = TransactionAddResponse.class)))
    @ApiResponse(responseCode = "400", description = "INVREQ: a key or field edit failed (R-8..R-27c)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "404", description = "NOTFND: account/card not in CARDXREF (R-11, R-13)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "409", description = "DUPREC: Tran ID already exist (R-31)",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "500", description = "ABEND: lookup or write failed",
            content = @Content(mediaType = "application/problem+json",
                    schema = @Schema(implementation = ApiError.class)))
    public ResponseEntity<TransactionAddResponse> add(
            @RequestParam(name = "fromProgram", required = false) String fromProgram,
            @Valid @RequestBody TransactionAddRequest request) {
        TransactionAddService.Outcome outcome = adds.add(request.toForm(), request.confirm(),
                request.copyLastRequested());
        boolean added = outcome.state() == TransactionAddService.State.ADDED;
        TransactionAddResponse body = new TransactionAddResponse(headers.of(ADD_TRAN, ADD_PROGRAM), outcome.state(),
                added ? TransactionAddRequest.cleared() : TransactionAddRequest.of(outcome.form(), ""),
                added ? TransactionFields.of(outcome.transaction()) : null, outcome.message(),
                TransactionNavigation.exit(ADD_TRAN, ADD_PROGRAM, fromProgram));
        if (added) {
            return ResponseEntity.created(URI.create(PATH + "/" + outcome.transaction().getTranId())).body(body);
        }
        return ResponseEntity.status(HttpStatus.OK).body(body);
    }
}
