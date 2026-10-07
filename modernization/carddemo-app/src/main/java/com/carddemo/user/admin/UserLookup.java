package com.carddemo.user.admin;

import com.carddemo.common.AbendException;
import com.carddemo.common.InvalidRequestException;
import com.carddemo.common.RecordNotFoundException;
import com.carddemo.common.online.ScreenInput;
import com.carddemo.user.UserSecurity;
import com.carddemo.user.UserSecurityRepository;
import java.util.Optional;
import java.util.function.Supplier;
import org.springframework.dao.DataAccessException;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * {@code READ-USER-SEC-FILE} of COUSR02C/COUSR03C (ENTER): the user id is required, NOTFND answers
 * {@code User ID NOT found...}, any other RESP {@code Unable to lookup User...}.
 */
@Component
public class UserLookup {

    private final UserSecurityRepository users;

    public UserLookup(UserSecurityRepository users) {
        this.users = users;
    }

    /** COUSR02C R-9/R-10, COUSR03C R-10/R-11. */
    @Transactional(readOnly = true)
    public UserSecurity byId(String userId) {
        String key = key(userId);
        return read(() -> users.findById(key)).orElseThrow(() -> new RecordNotFoundException(
                UserAdminMessages.MSG_NOT_FOUND));
    }

    /** {@code USRIDINI} blank → {@code User ID can NOT be empty...}; else the id as typed, right-trimmed. */
    static String key(String userId) {
        if (ScreenInput.isSpacesOrLowValues(userId)) {
            throw new InvalidRequestException(UserAdminMessages.USER_ID_FIELD, UserAdminMessages.MSG_USER_ID_EMPTY);
        }
        return ScreenInput.rightTrim(userId);
    }

    /** {@code READ ... UPDATE}: locks the row and returns its current version; NOTFND → 404. */
    long lockVersion(String key) {
        return read(() -> users.lockVersion(key)).orElseThrow(() -> new RecordNotFoundException(
                UserAdminMessages.MSG_NOT_FOUND));
    }

    static <T> Optional<T> read(Supplier<Optional<T>> read) {
        try {
            return read.get();
        } catch (DataAccessException e) {
            throw AbendException.carddemo(UserAdminMessages.MSG_LOOKUP_FAILED, e);
        }
    }
}
