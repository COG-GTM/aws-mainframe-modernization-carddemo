package com.carddemo.user.signon;

import static com.carddemo.common.online.ScreenInput.isSpacesOrLowValues;
import static com.carddemo.common.online.ScreenInput.rightTrim;
import static com.carddemo.common.online.ScreenInput.upperCase;

import com.carddemo.user.UserPasswords;
import com.carddemo.user.UserSecurity;
import com.carddemo.user.UserSecurityRepository;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.stereotype.Service;

/**
 * {@code COSGN00C} {@code PROCESS-ENTER-KEY} + {@code READ-USER-SEC-FILE}: validates the two input fields, reads
 * {@code USRSEC} by the upper-cased user id and compares the stored password with the upper-cased input: against the
 * BCrypt hash once stored, otherwise in plain text, storing the hash after the first plain-text match (ADR-0023,
 * superseding the storage part of ADR-0018). Messages are the program's literals.
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

    private static final Logger log = LoggerFactory.getLogger(SignOnService.class);

    private final UserSecurityRepository users;
    private final UserPasswords passwords;

    public SignOnService(UserSecurityRepository users, UserPasswords passwords) {
        this.users = users;
        this.passwords = passwords;
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
        if (!passwords.matches(user, password)) {
            return new SignOnResult.Rejected(SignOnFailure.WRONG_PASSWORD, PASSWORD_FIELD, MSG_WRONG_PASSWORD);
        }
        if (user.getPasswordHash() == null) {
            storeHash(user);
        }
        return new SignOnResult.SignedOn(user.getUsrId(), user.getUsrType(), user.getFirstName(),
                user.getLastName());
    }

    /** ADR-0023 upgrade-on-sign-on; a failure is logged and the sign-on still succeeds (plain text stays valid). */
    private void storeHash(UserSecurity user) {
        try {
            users.storePasswordHash(user.getUsrId(), user.getPassword(), passwords.hash(user.getPassword()));
        } catch (DataAccessException e) {
            log.warn("Could not store the password hash of user {}", user.getUsrId(), e);
        }
    }

    static String pic8(String value) {
        return UserPasswords.pic8(value);
    }
}
