package com.carddemo.user.admin;

import static com.carddemo.user.admin.UserAdminMessages.FIRST_NAME_FIELD;
import static com.carddemo.user.admin.UserAdminMessages.LAST_NAME_FIELD;
import static com.carddemo.user.admin.UserAdminMessages.PASSWORD_FIELD;
import static com.carddemo.user.admin.UserAdminMessages.USER_TYPE_FIELD;

import com.carddemo.common.AbendException;
import com.carddemo.common.InvalidRequestException;
import com.carddemo.common.RecordNotFoundException;
import com.carddemo.common.Versions;
import com.carddemo.user.UserSecurity;
import com.carddemo.user.UserSecurityRecord;
import com.carddemo.user.UserSecurityRepository;
import com.carddemo.user.UserType;
import java.util.Objects;
import org.springframework.dao.DataAccessException;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * COUSR02C (CU02) PF5 {@code UPDATE-USER-INFO}: the four editable fields are required, the record is read for update
 * (row lock + version re-check, ADR-0010), compared field by field and rewritten only when something changed.
 */
@Service
public class UserUpdateService {

    /** What PF5 did. */
    public enum State { UNCHANGED, UPDATED }

    /** The user after PF5 and {@code ERRMSG}. */
    public record Outcome(State state, UserSecurity user, String message) {
    }

    private final UserSecurityRepository users;
    private final UserLookup lookup;

    public UserUpdateService(UserSecurityRepository users, UserLookup lookup) {
        this.users = users;
        this.lookup = lookup;
    }

    @Transactional
    public Outcome update(String userId, UserForm form, Long version) {
        String key = UserLookup.key(userId);
        UserType type = edits(form);
        if (version == null) {
            throw new InvalidRequestException(UserAdminMessages.VERSION_FIELD, UserAdminMessages.MSG_VERSION_REQUIRED);
        }
        long locked = lookup.lockVersion(key);
        Versions.requireCurrent(UserSecurity.class, key, version, locked);
        UserSecurity user = UserLookup.read(() -> users.findById(key))
                .orElseThrow(() -> new RecordNotFoundException(UserAdminMessages.MSG_NOT_FOUND));
        UserSecurityRecord typed = new UserSecurityRecord(key, UserEdits.text(form.firstName()),
                UserEdits.text(form.lastName()), UserEdits.text(form.password()), type);
        if (!modified(user, typed)) {
            return new Outcome(State.UNCHANGED, user, UserAdminMessages.MSG_NO_CHANGES);
        }
        return new Outcome(State.UPDATED, updateUserSecFile(user, typed), UserAdminMessages.updated(key));
    }

    /** R-11..R-15 (R-11, the user id, is checked by {@link UserLookup#key}); then the type value. */
    static UserType edits(UserForm form) {
        UserEdits.required(form.firstName(), FIRST_NAME_FIELD, UserAdminMessages.MSG_FIRST_NAME_EMPTY);
        UserEdits.required(form.lastName(), LAST_NAME_FIELD, UserAdminMessages.MSG_LAST_NAME_EMPTY);
        UserEdits.required(form.password(), PASSWORD_FIELD, UserAdminMessages.MSG_PASSWORD_EMPTY);
        UserEdits.required(form.userType(), USER_TYPE_FIELD, UserAdminMessages.MSG_USER_TYPE_EMPTY);
        return UserEdits.userType(form.userType());
    }

    /** R-16: FNAME, LNAME, PWD and TYPE compared with the record (PIC X compare: trailing spaces ignored). */
    static boolean modified(UserSecurity user, UserSecurityRecord typed) {
        return !Objects.equals(UserEdits.text(user.getFirstName()), typed.firstName())
                || !Objects.equals(UserEdits.text(user.getLastName()), typed.lastName())
                || !Objects.equals(UserEdits.text(user.getPassword()), typed.password())
                || user.getUsrType() != typed.usrType();
    }

    /** {@code UPDATE-USER-SEC-FILE}: NORMAL (R-20), other RESP (R-22). */
    private UserSecurity updateUserSecFile(UserSecurity user, UserSecurityRecord typed) {
        user.update(typed);
        try {
            return users.saveAndFlush(user);
        } catch (ObjectOptimisticLockingFailureException e) {
            throw e;
        } catch (DataAccessException e) {
            throw AbendException.carddemo(UserAdminMessages.MSG_UPDATE_FAILED, e);
        }
    }
}
