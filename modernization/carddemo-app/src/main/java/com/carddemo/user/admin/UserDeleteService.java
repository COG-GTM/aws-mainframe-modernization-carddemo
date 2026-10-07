package com.carddemo.user.admin;

import com.carddemo.common.AbendException;
import com.carddemo.common.InvalidRequestException;
import com.carddemo.common.RecordNotFoundException;
import com.carddemo.common.Versions;
import com.carddemo.common.online.ScreenInput;
import com.carddemo.user.UserSecurity;
import com.carddemo.user.UserSecurityRepository;
import java.util.Locale;
import org.springframework.dao.DataAccessException;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * COUSR03C (CU03): ENTER shows the user with {@code Press PF5 key to delete this user ...}; PF5 reads it for update
 * and deletes it. In the API the PF5 is {@code confirm=Y} (blank = show, N = clear), with the version re-checked under
 * the row lock (ADR-0010). Like the COBOL there is no self-delete or last-admin guard.
 */
@Service
public class UserDeleteService {

    /** What the request did. */
    public enum State { VALIDATED, CANCELLED, DELETED }

    /** The user shown (or deleted) and {@code ERRMSG}; {@code user} is null when the screen was cleared. */
    public record Outcome(State state, UserSecurity user, String message) {
    }

    private final UserSecurityRepository users;
    private final UserLookup lookup;

    public UserDeleteService(UserSecurityRepository users, UserLookup lookup) {
        this.users = users;
        this.lookup = lookup;
    }

    @Transactional
    public Outcome delete(String userId, String confirm, Long version) {
        String key = UserLookup.key(userId);
        String answer = ScreenInput.isSpacesOrLowValues(confirm) ? "" : confirm.strip().toUpperCase(Locale.ROOT);
        switch (answer) {
            case "" -> {
                return new Outcome(State.VALIDATED, lookup.byId(key), UserAdminMessages.MSG_PRESS_PF5_TO_DELETE);
            }
            case "N" -> {
                return new Outcome(State.CANCELLED, null, "");
            }
            case "Y" -> {
                return deleteUserInfo(key, version);
            }
            default -> throw new InvalidRequestException(UserAdminMessages.CONFIRM_FIELD,
                    UserAdminMessages.invalidConfirm(confirm));
        }
    }

    /** {@code DELETE-USER-INFO}: READ UPDATE (R-13..R-16), then {@code DELETE-USER-SEC-FILE} (R-17..R-19). */
    private Outcome deleteUserInfo(String key, Long version) {
        if (version == null) {
            throw new InvalidRequestException(UserAdminMessages.VERSION_FIELD, UserAdminMessages.MSG_VERSION_REQUIRED);
        }
        long locked = lookup.lockVersion(key);
        Versions.requireCurrent(UserSecurity.class, key, version, locked);
        UserSecurity user = UserLookup.read(() -> users.findById(key))
                .orElseThrow(() -> new RecordNotFoundException(UserAdminMessages.MSG_NOT_FOUND));
        try {
            users.delete(user);
            users.flush();
        } catch (ObjectOptimisticLockingFailureException e) {
            throw e;
        } catch (DataAccessException e) {
            throw AbendException.carddemo(UserAdminMessages.MSG_UPDATE_FAILED, e);
        }
        return new Outcome(State.DELETED, user, UserAdminMessages.deleted(key));
    }
}
