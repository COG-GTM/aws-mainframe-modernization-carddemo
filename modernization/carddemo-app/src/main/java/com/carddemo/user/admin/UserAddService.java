package com.carddemo.user.admin;

import static com.carddemo.user.admin.UserAdminMessages.FIRST_NAME_FIELD;
import static com.carddemo.user.admin.UserAdminMessages.LAST_NAME_FIELD;
import static com.carddemo.user.admin.UserAdminMessages.PASSWORD_FIELD;
import static com.carddemo.user.admin.UserAdminMessages.USER_ID_FIELD;
import static com.carddemo.user.admin.UserAdminMessages.USER_TYPE_FIELD;

import com.carddemo.common.AbendException;
import com.carddemo.common.DuplicateRecordException;
import com.carddemo.user.UserSecurity;
import com.carddemo.user.UserSecurityRecord;
import com.carddemo.user.UserSecurityRepository;
import com.carddemo.user.UserType;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.stereotype.Service;

/**
 * COUSR01C (CU01): all five fields required in screen order, then {@code WRITE} USRSEC. The password is stored in
 * plaintext like the VSAM record (ADR-0018; hashing is step s6.4).
 */
@Service
public class UserAddService {

    private final UserSecurityRepository users;

    public UserAddService(UserSecurityRepository users) {
        this.users = users;
    }

    /** ENTER: R-8..R-13, then {@link #writeUserSecFile}; returns the user written (R-15). */
    public UserSecurity add(UserForm form) {
        UserSecurityRecord record = processEnterKey(form);
        return writeUserSecFile(record);
    }

    /** {@code PROCESS-ENTER-KEY}: first name, last name, user id, password, user type (R-8..R-12), then R-13. */
    UserSecurityRecord processEnterKey(UserForm form) {
        UserEdits.required(form.firstName(), FIRST_NAME_FIELD, UserAdminMessages.MSG_FIRST_NAME_EMPTY);
        UserEdits.required(form.lastName(), LAST_NAME_FIELD, UserAdminMessages.MSG_LAST_NAME_EMPTY);
        UserEdits.required(form.userId(), USER_ID_FIELD, UserAdminMessages.MSG_USER_ID_EMPTY);
        UserEdits.required(form.password(), PASSWORD_FIELD, UserAdminMessages.MSG_PASSWORD_EMPTY);
        UserEdits.required(form.userType(), USER_TYPE_FIELD, UserAdminMessages.MSG_USER_TYPE_EMPTY);
        UserType type = UserEdits.userType(form.userType());
        return new UserSecurityRecord(UserEdits.text(form.userId()), UserEdits.text(form.firstName()),
                UserEdits.text(form.lastName()), UserEdits.text(form.password()), type);
    }

    /** {@code WRITE-USER-SEC-FILE}: NORMAL (R-15), DUPKEY/DUPREC (R-16), other (R-17). */
    UserSecurity writeUserSecFile(UserSecurityRecord record) {
        try {
            return users.saveAndFlush(UserSecurity.newRecord(record));
        } catch (DataIntegrityViolationException e) {
            if (existsQuietly(record.usrId())) {
                throw new DuplicateRecordException(UserAdminMessages.MSG_DUPLICATE);
            }
            throw AbendException.carddemo(UserAdminMessages.MSG_ADD_FAILED, e);
        } catch (DataAccessException e) {
            throw AbendException.carddemo(UserAdminMessages.MSG_ADD_FAILED, e);
        }
    }

    private boolean existsQuietly(String userId) {
        try {
            return users.existsById(userId);
        } catch (DataAccessException e) {
            return false;
        }
    }
}
