package com.carddemo.user.signon;

import static com.carddemo.common.online.ScreenInput.isSpacesOrLowValues;
import static com.carddemo.common.online.ScreenInput.rightTrim;
import static com.carddemo.common.online.ScreenInput.upperCase;

import com.carddemo.user.UserSecurity;
import com.carddemo.user.UserSecurityRepository;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.stereotype.Service;

/**
 * {@code COSGN00C} {@code PROCESS-ENTER-KEY} + {@code READ-USER-SEC-FILE}: validates the two input fields, reads
 * {@code USRSEC} by the upper-cased user id and compares the stored password with the upper-cased input, in plain
 * text (ADR-0018). Messages are the program's literals.
 */
@Service
public class SignOnService {

    public static final String PROGRAM = "COSGN00C";
    public static final String TRANID = "CC00";
    public static final String ADMIN_MENU_PROGRAM = "COADM01C";
    public static final String MAIN_MENU_PROGRAM = "COMEN01C";

    public static final String USER_ID_FIELD = "userId";
    public static final String PASSWORD_FIELD = "password";

    public static final String MSG_USER_ID_BLANK = "Please enter User ID ...";
    public static final String MSG_PASSWORD_BLANK = "Please enter Password ...";
    public static final String MSG_WRONG_PASSWORD = "Wrong Password. Try again ...";
    public static final String MSG_USER_NOT_FOUND = "User not found. Try again ...";
    public static final String MSG_UNABLE_TO_VERIFY = "Unable to verify the User ...";

    /** {@code SEC-USR-ID} / {@code SEC-USR-PWD} are {@code PIC X(08)}. */
    static final int FIELD_LENGTH = 8;

    private static final Logger log = LoggerFactory.getLogger(SignOnService.class);

    private final UserSecurityRepository users;

    public SignOnService(UserSecurityRepository users) {
        this.users = users;
    }

    /** {@code PROCESS-ENTER-KEY}: R-5 (user id first), R-6, R-7 upper-case, R-8 no read after an edit error. */
    public SignOnResult signOn(String userIdInput, String passwordInput) {
        if (isSpacesOrLowValues(userIdInput)) {
            return new SignOnResult.Rejected(SignOnFailure.USER_ID_BLANK, USER_ID_FIELD, MSG_USER_ID_BLANK);
        }
        if (isSpacesOrLowValues(passwordInput)) {
            return new SignOnResult.Rejected(SignOnFailure.PASSWORD_BLANK, PASSWORD_FIELD, MSG_PASSWORD_BLANK);
        }
        String userId = rightTrim(upperCase(userIdInput));
        String password = upperCase(passwordInput);
        return readUserSecFile(userId, password);
    }

    /** {@code READ-USER-SEC-FILE}: full-key read of USRSEC, R-9 .. R-14. */
    private SignOnResult readUserSecFile(String userId, String password) {
        Optional<UserSecurity> found;
        try {
            found = users.findById(userId);
        } catch (DataAccessException e) {
            log.warn("USRSEC read failed for user {}", userId, e);
            return new SignOnResult.Rejected(SignOnFailure.UNABLE_TO_VERIFY, USER_ID_FIELD, MSG_UNABLE_TO_VERIFY);
        }
        if (found.isEmpty()) {
            return new SignOnResult.Rejected(SignOnFailure.USER_NOT_FOUND, USER_ID_FIELD, MSG_USER_NOT_FOUND);
        }
        UserSecurity user = found.get();
        if (!pic8(user.getPassword()).equals(pic8(password))) {
            return new SignOnResult.Rejected(SignOnFailure.WRONG_PASSWORD, PASSWORD_FIELD, MSG_WRONG_PASSWORD);
        }
        return new SignOnResult.SignedOn(user.getUsrId(), user.getUsrType(), user.getFirstName(),
                user.getLastName());
    }

    /**
     * The value space-padded to the 8-byte field COBOL compares ({@code SEC-USR-PWD = WS-USER-PWD}); longer input
     * is never truncated, so it cannot match.
     */
    static String pic8(String value) {
        String v = value == null ? "" : value;
        return v.length() >= FIELD_LENGTH ? v : v + " ".repeat(FIELD_LENGTH - v.length());
    }
}
