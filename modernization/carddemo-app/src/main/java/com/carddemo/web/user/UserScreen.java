package com.carddemo.web.user;

import com.carddemo.user.UserSecurity;
import com.carddemo.web.NavigationContext;
import com.carddemo.web.ScreenHeader;
import io.swagger.v3.oas.annotations.media.Schema;

/** COUSR1A/COUSR2A/COUSR3A: the user shown, what the request did and {@code ERRMSG}. */
@Schema(description = "User maintenance screen (COUSR01C / COUSR02C / COUSR03C)")
public record UserScreen(
        ScreenHeader header,
        @Schema(description = "SHOW = fetched or nothing to save, ADDED, UPDATED, VALIDATED = delete not yet "
                + "confirmed, CANCELLED = confirm N (fields cleared), DELETED") State state,
        @Schema(description = "The user; null when the screen was cleared", nullable = true) User user,
        @Schema(description = "ERRMSG", example = "Press PF5 key to save your updates ...") String message,
        @Schema(description = "PF3 target") NavigationContext exit) {

    /** What the request did. */
    public enum State { SHOW, ADDED, UPDATED, VALIDATED, CANCELLED, DELETED }

    /**
     * SEC-USER-DATA as displayed; the password is shown in clear on COUSR2A (COUSR02C R-10) and not at all on
     * COUSR3A.
     */
    @Schema(description = "SEC-USER-DATA")
    public record User(
            @Schema(example = "USER0001") String userId,
            @Schema(example = "JOHN") String firstName,
            @Schema(example = "DOE") String lastName,
            @Schema(description = "Only on the update screen (COUSR2A shows it in clear)", nullable = true,
                    example = "PASSWORD") String password,
            @Schema(example = "U") String userType,
            @Schema(description = "Send back with PUT / DELETE confirm=Y", example = "0") long version) {

        static User of(UserSecurity user, boolean withPassword) {
            return new User(user.getUsrId(), user.getFirstName(), user.getLastName(),
                    withPassword ? user.getPassword() : null, user.getUsrType().code(), user.getVersion());
        }
    }
}
