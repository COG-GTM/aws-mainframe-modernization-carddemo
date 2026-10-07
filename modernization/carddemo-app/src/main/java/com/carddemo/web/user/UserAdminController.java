package com.carddemo.web.user;

import static com.carddemo.web.user.UserNavigation.ADD_PROGRAM;
import static com.carddemo.web.user.UserNavigation.ADD_TRAN;
import static com.carddemo.web.user.UserNavigation.DELETE_PROGRAM;
import static com.carddemo.web.user.UserNavigation.DELETE_TRAN;
import static com.carddemo.web.user.UserNavigation.LIST_PROGRAM;
import static com.carddemo.web.user.UserNavigation.LIST_TRAN;
import static com.carddemo.web.user.UserNavigation.UPDATE_PROGRAM;
import static com.carddemo.web.user.UserNavigation.UPDATE_TRAN;

import com.carddemo.common.InvalidRequestException;
import com.carddemo.common.online.ScreenInput;
import com.carddemo.common.web.ApiError;
import com.carddemo.user.UserSecurity;
import com.carddemo.user.admin.UserAddService;
import com.carddemo.user.admin.UserAdminMessages;
import com.carddemo.user.admin.UserDeleteService;
import com.carddemo.user.admin.UserForm;
import com.carddemo.user.admin.UserListBrowse;
import com.carddemo.user.admin.UserLookup;
import com.carddemo.user.admin.UserUpdateService;
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
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.util.UriComponentsBuilder;

/**
 * COUSR00C (CU00 list), COUSR01C (CU01 add), COUSR02C (CU02 update) and COUSR03C (CU03 delete). In CICS these are
 * reached only from the admin menu COADM01C; here the whole {@code /api/v1/users} tree is ADMIN-only in
 * {@code SecurityConfiguration} and a USER token gets 403 NOTAUTH {@code No access - Admin Only option...}.
 */
@RestController
@ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
@RequestMapping(UserAdminController.PATH)
@Tag(name = "Users", description = "User security administration (COUSR00C-COUSR03C, ADMIN only)")
@SecurityRequirement(name = OpenApiConfiguration.BEARER)
public class UserAdminController {

    public static final String PATH = "/api/v1/users";
    public static final String MSG_ONE_CURSOR = "Send either after= (PF8) or before= (PF7), not both";
    public static final String MSG_PAGE_SIZE = "limit must be 10: COUSR0A shows ten rows";

    private static final String PROBLEM = "application/problem+json";

    private final UserListBrowse browse;
    private final UserLookup lookup;
    private final UserAddService adds;
    private final UserUpdateService updates;
    private final UserDeleteService deletes;
    private final ScreenHeaders headers;

    public UserAdminController(UserListBrowse browse, UserLookup lookup, UserAddService adds,
            UserUpdateService updates, UserDeleteService deletes, ScreenHeaders headers) {
        this.browse = browse;
        this.lookup = lookup;
        this.adds = adds;
        this.updates = updates;
        this.deletes = deletes;
        this.headers = headers;
    }

    @GetMapping
    @Operation(summary = "List users, ten per page (COUSR00C)",
            description = "Browses USRSEC in SEC-USR-ID order: ENTER = from startUserId (blank = first, else "
                    + "exact-or-greater, no validation), PF8 = after (last id shown), PF7 = before (first id "
                    + "shown). Send the pageNumber shown back as page= so PF7 on page 1 answers 'You are already "
                    + "at the top of the page...' like the program.")
    @ApiResponse(responseCode = "200", description = "One page (possibly empty, with the program's message)",
            content = @Content(schema = @Schema(implementation = UserListScreen.class)))
    @ApiResponse(responseCode = "400", description = "INVREQ: bad cursor, both cursors or limit other than 10",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "403", description = "NOTAUTH: not an admin (COADM01C gate)",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "500", description = "ABEND: browse failed (R-24..R-26 other RESP)",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    public UserListScreen list(
            @Parameter(description = "USRIDIN: start the list at this id (up to 8 characters)", example = "")
            @RequestParam(name = "startUserId", required = false) String startUserId,
            @Parameter(description = "PF8: the last user id shown") @RequestParam(name = "after", required = false)
            String after,
            @Parameter(description = "PF7: the first user id shown") @RequestParam(name = "before", required = false)
            String before,
            @Parameter(description = "PAGENUM of the page shown (with after/before)")
            @RequestParam(name = "page", required = false) Integer page,
            @Parameter(description = "Rows per page; must be 10") @RequestParam(name = "limit", defaultValue = "10")
            int limit) {
        if (limit != UserListBrowse.PAGE_SIZE) {
            throw new InvalidRequestException("limit", MSG_PAGE_SIZE);
        }
        if (after != null && before != null) {
            throw new InvalidRequestException("before", MSG_ONE_CURSOR);
        }
        UserListBrowse.Screen screen = browse.browse(startUserId, after, before, page);
        List<UserListRow> rows = new ArrayList<>();
        for (UserSecurity user : screen.rows()) {
            rows.add(new UserListRow(rows.size() + 1, user.getUsrId(), user.getFirstName(), user.getLastName(),
                    user.getUsrType().code()));
        }
        String previous = screen.hasPrevious() && !rows.isEmpty() ? rows.get(0).userId() : null;
        String next = screen.hasNext() && !rows.isEmpty() ? rows.get(rows.size() - 1).userId() : null;
        return new UserListScreen(headers.of(LIST_TRAN, LIST_PROGRAM), screen.pageNumber(), UserListBrowse.PAGE_SIZE,
                rows, screen.hasPrevious(), screen.hasNext(), previous, next, screen.message(),
                UserNavigation.adminMenu(LIST_TRAN, LIST_PROGRAM));
    }

    @PostMapping(path = "/selection", consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Select a row of the page shown (COUSR00C ENTER)",
            description = "The first row with a non-blank SELnnnn decides (R-9): U/u routes to COUSR02C, D/d to "
                    + "COUSR03C (R-10/R-11); any other code is 'Invalid selection. Valid values are U and D' "
                    + "(R-12). No selection stays on the list.")
    @ApiResponse(responseCode = "200", description = "XCTL target, or no selection",
            content = @Content(schema = @Schema(implementation = UserSelectionResponse.class)))
    @ApiResponse(responseCode = "400", description = "INVREQ: selection code other than U or D (R-12)",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "403", description = "NOTAUTH: not an admin (COADM01C gate)",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    public UserSelectionResponse select(@Valid @RequestBody UserSelectionRequest request) {
        for (int i = 0; i < request.rows().size(); i++) {
            UserSelectionRequest.Row row = request.rows().get(i);
            if (ScreenInput.isSpacesOrLowValues(row.selection())) {
                continue;
            }
            UserListBrowse.Action action = UserListBrowse.Action.of(row.selection(), "rows[" + i + "].selection");
            String userId = ScreenInput.rightTrim(row.userId());
            NavigationContext target = action == UserListBrowse.Action.UPDATE
                    ? NavigationContext.transfer(LIST_TRAN, LIST_PROGRAM, UPDATE_TRAN, UPDATE_PROGRAM)
                    : NavigationContext.transfer(LIST_TRAN, LIST_PROGRAM, DELETE_TRAN, DELETE_PROGRAM);
            String next = action == UserListBrowse.Action.UPDATE
                    ? "GET " + userPath(userId) + "?fromProgram=" + LIST_PROGRAM
                    : "DELETE " + userPath(userId) + "?fromProgram=" + LIST_PROGRAM;
            return new UserSelectionResponse(headers.of(LIST_TRAN, LIST_PROGRAM), target, userId, next);
        }
        return new UserSelectionResponse(headers.of(LIST_TRAN, LIST_PROGRAM), null, null, null);
    }

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Add a user (COUSR01C)",
            description = "All five fields are required, checked in screen order (first name, last name, user id, "
                    + "password, user type; R-8..R-12). User id is up to 8 characters and type A or U. The password "
                    + "is stored in plaintext like USRSEC (ADR-0018). A duplicate id answers 409 'User ID already "
                    + "exist...' (R-16).")
    @ApiResponse(responseCode = "201", description = "ADDED: 'User <id> has been added ...' (R-15)",
            content = @Content(schema = @Schema(implementation = UserScreen.class)))
    @ApiResponse(responseCode = "400", description = "INVREQ: a field is blank, too long, or the type is not A/U",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "403", description = "NOTAUTH: not an admin (COADM01C gate)",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "409", description = "DUPREC: 'User ID already exist...' (R-16)",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "500", description = "ABEND: 'Unable to Add User...' (R-17)",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    public ResponseEntity<UserScreen> add(@Valid @RequestBody UserAddRequest request) {
        UserSecurity user = adds.add(new UserForm(request.userId(), request.firstName(), request.lastName(),
                request.password(), request.userType()));
        UserScreen screen = new UserScreen(headers.of(ADD_TRAN, ADD_PROGRAM), UserScreen.State.ADDED,
                UserScreen.User.of(user, false), UserAdminMessages.added(user.getUsrId()),
                UserNavigation.adminMenu(ADD_TRAN, ADD_PROGRAM));
        return ResponseEntity.created(URI.create(userPath(user.getUsrId()))).body(screen);
    }

    @GetMapping("/{id}")
    @Operation(summary = "Fetch a user for update (COUSR02C ENTER)",
            description = "READ USRSEC by id (R-9/R-10): the four editable fields and the version to send back "
                    + "with PUT; 'Press PF5 key to save your updates ...' (R-17).")
    @ApiResponse(responseCode = "200", description = "SHOW",
            content = @Content(schema = @Schema(implementation = UserScreen.class)))
    @ApiResponse(responseCode = "400", description = "INVREQ: user id blank (R-9)",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "403", description = "NOTAUTH: not an admin (COADM01C gate)",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "404", description = "NOTFND: 'User ID NOT found...' (R-18)",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "500", description = "ABEND: 'Unable to lookup User...' (R-19)",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    public UserScreen fetch(@Parameter(description = "USRIDIN", example = "USER0001") @PathVariable("id") String id,
            @Parameter(description = "CDEMO-FROM-PROGRAM (PF3 target), e.g. COUSR00C")
            @RequestParam(name = "fromProgram", required = false) String fromProgram) {
        UserSecurity user = lookup.byId(id);
        return new UserScreen(headers.of(UPDATE_TRAN, UPDATE_PROGRAM), UserScreen.State.SHOW,
                UserScreen.User.of(user, true), UserAdminMessages.MSG_PRESS_PF5_TO_UPDATE,
                UserNavigation.exit(UPDATE_TRAN, UPDATE_PROGRAM, fromProgram));
    }

    @PutMapping(path = "/{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Update a user (COUSR02C PF5)",
            description = "First name, last name, password and type are required (R-12..R-15). The user is read "
                    + "for update and the version re-checked (409 CHANGED when stale); when nothing differs the "
                    + "answer is SHOW with 'Please modify to update ...' (R-16), else UPDATED with 'User <id> has "
                    + "been updated ...' (R-20).")
    @ApiResponse(responseCode = "200", description = "UPDATED, or SHOW when nothing changed",
            content = @Content(schema = @Schema(implementation = UserScreen.class)))
    @ApiResponse(responseCode = "400", description = "INVREQ: a field is blank, the type is not A/U, or no version",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "403", description = "NOTAUTH: not an admin (COADM01C gate)",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "404", description = "NOTFND: 'User ID NOT found...' (R-18/R-21)",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "409", description = "CHANGED: updated by someone else since it was fetched",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "500", description = "ABEND: 'Unable to Update User...' (R-22)",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    public UserScreen update(@Parameter(description = "USRIDIN", example = "USER0001") @PathVariable("id") String id,
            @Valid @RequestBody UserUpdateRequest request,
            @Parameter(description = "CDEMO-FROM-PROGRAM (PF3 target), e.g. COUSR00C")
            @RequestParam(name = "fromProgram", required = false) String fromProgram) {
        UserUpdateService.Outcome outcome = updates.update(id, new UserForm(id, request.firstName(),
                request.lastName(), request.password(), request.userType()), request.version());
        UserScreen.State state = outcome.state() == UserUpdateService.State.UPDATED ? UserScreen.State.UPDATED
                : UserScreen.State.SHOW;
        return new UserScreen(headers.of(UPDATE_TRAN, UPDATE_PROGRAM), state, UserScreen.User.of(outcome.user(), true),
                outcome.message(), UserNavigation.exit(UPDATE_TRAN, UPDATE_PROGRAM, fromProgram));
    }

    @DeleteMapping("/{id}")
    @Operation(summary = "Delete a user (COUSR03C)",
            description = "confirm blank = ENTER: show the user with 'Press PF5 key to delete this user ...' "
                    + "(R-11/R-14); confirm Y = PF5: read for update, re-check the version, delete and answer "
                    + "'User <id> has been deleted ...' (R-13/R-17); N = clear the screen; any other value is "
                    + "INVREQ. No self-delete or last-admin guard, as in the COBOL.")
    @ApiResponse(responseCode = "200", description = "VALIDATED, CANCELLED or DELETED",
            content = @Content(schema = @Schema(implementation = UserScreen.class)))
    @ApiResponse(responseCode = "400", description = "INVREQ: user id blank, invalid confirm, or no version with Y",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "403", description = "NOTAUTH: not an admin (COADM01C gate)",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "404", description = "NOTFND: 'User ID NOT found...' (R-15/R-18)",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "409", description = "CHANGED: updated by someone else since it was shown",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "500", description = "ABEND: 'Unable to Update User...' (R-19, sic)",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    public UserScreen delete(@Parameter(description = "USRIDIN", example = "USER0001") @PathVariable("id") String id,
            @Parameter(description = "blank = show, Y = delete, N = clear") @RequestParam(name = "confirm",
                    required = false) String confirm,
            @Parameter(description = "version shown (required with confirm=Y)")
            @RequestParam(name = "version", required = false) Long version,
            @Parameter(description = "CDEMO-FROM-PROGRAM (PF3 target), e.g. COUSR00C")
            @RequestParam(name = "fromProgram", required = false) String fromProgram) {
        UserDeleteService.Outcome outcome = deletes.delete(id, confirm, version);
        UserScreen.State state = switch (outcome.state()) {
            case VALIDATED -> UserScreen.State.VALIDATED;
            case CANCELLED -> UserScreen.State.CANCELLED;
            case DELETED -> UserScreen.State.DELETED;
        };
        UserScreen.User user = outcome.user() == null ? null : UserScreen.User.of(outcome.user(), false);
        return new UserScreen(headers.of(DELETE_TRAN, DELETE_PROGRAM), state, user, outcome.message(),
                UserNavigation.exit(DELETE_TRAN, DELETE_PROGRAM, fromProgram));
    }

    /** PATH/{id} with the id encoded as one path segment (ids may contain spaces). */
    private static String userPath(String userId) {
        return UriComponentsBuilder.fromPath(PATH).pathSegment("{id}").buildAndExpand(userId).encode().toUriString();
    }
}
