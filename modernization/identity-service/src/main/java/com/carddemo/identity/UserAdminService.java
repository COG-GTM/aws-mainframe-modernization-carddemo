package com.carddemo.identity;

import com.carddemo.common.domain.UserSecurity;
import com.carddemo.common.domain.UserSecurityRepository;
import com.carddemo.common.error.ApiException;
import com.carddemo.common.web.KeysetBrowser;
import com.carddemo.common.web.KeysetPage;
import java.util.Locale;
import java.util.Objects;
import org.springframework.data.domain.PageRequest;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** COUSR00C (list), COUSR01C (add), COUSR02C (update), COUSR03C (delete). */
@Service
public class UserAdminService {

    static final int PAGE_SIZE = 10;

    private final UserSecurityRepository users;
    private final PasswordEncoder passwordEncoder;

    public UserAdminService(UserSecurityRepository users, PasswordEncoder passwordEncoder) {
        this.users = users;
        this.passwordEncoder = passwordEncoder;
    }

    @Transactional(readOnly = true)
    public KeysetPage<UserView> list(String startKey, String after, String before, int pageNumber) {
        return KeysetBrowser.browse(PAGE_SIZE, pageNumber, startKey, after, before,
                (from, n) -> users.findByUserIdGreaterThanEqualOrderByUserIdAsc(from, PageRequest.of(0, n))
                        .stream().map(UserView::of).toList(),
                (from, n) -> users.findByUserIdLessThanOrderByUserIdDesc(from, PageRequest.of(0, n))
                        .stream().map(UserView::of).toList(),
                UserView::userId);
    }

    @Transactional(readOnly = true)
    public UserView get(String userId) {
        return UserView.of(find(userId));
    }

    @Transactional
    public String add(UserRequest r) {
        require(r.firstName(), "First Name");
        require(r.lastName(), "Last Name");
        require(r.userId(), "User ID");
        require(r.password(), "Password");
        require(r.userType(), "User Type");
        String type = validType(r.userType());
        String id = r.userId().trim();
        if (users.existsById(id)) {
            throw ApiException.conflict("USER_EXISTS", "User ID already exist...");
        }
        UserSecurity u = new UserSecurity();
        u.setUserId(id);
        u.setFirstName(r.firstName().trim());
        u.setLastName(r.lastName().trim());
        u.setPasswordHash(hash(r.password()));
        u.setUserType(type);
        users.save(u);
        return "User " + id + " has been added ...";
    }

    @Transactional
    public String update(String userId, UserRequest r) {
        UserSecurity u = find(userId);
        require(r.firstName(), "First Name");
        require(r.lastName(), "Last Name");
        require(r.password(), "Password");
        require(r.userType(), "User Type");
        String type = validType(r.userType());
        boolean modified = false;
        if (!Objects.equals(u.getFirstName(), r.firstName().trim())) {
            u.setFirstName(r.firstName().trim());
            modified = true;
        }
        if (!Objects.equals(u.getLastName(), r.lastName().trim())) {
            u.setLastName(r.lastName().trim());
            modified = true;
        }
        if (!passwordEncoder.matches(normalisePassword(r.password()), u.getPasswordHash())) {
            u.setPasswordHash(hash(r.password()));
            modified = true;
        }
        if (!Objects.equals(u.getUserType(), type)) {
            u.setUserType(type);
            modified = true;
        }
        if (!modified) {
            throw ApiException.badRequest("NOT_MODIFIED", "Please modify to update ...");
        }
        return "User " + u.getUserId() + " has been updated ...";
    }

    @Transactional
    public String delete(String userId) {
        UserSecurity u = find(userId);
        users.delete(u);
        return "User " + u.getUserId() + " has been deleted ...";
    }

    private UserSecurity find(String userId) {
        if (userId == null || userId.isBlank()) {
            throw ApiException.badRequest("USER_ID_REQUIRED", "User ID can NOT be empty...");
        }
        return users.findById(userId.trim())
                .orElseThrow(() -> ApiException.notFound("USER_NOT_FOUND", "User ID NOT found..."));
    }

    private static void require(String value, String label) {
        if (value == null || value.isBlank()) {
            throw ApiException.badRequest("REQUIRED", label + " can NOT be empty...");
        }
    }

    /** Hardening: COUSR01C accepts any character; RBAC needs a known role. */
    private static String validType(String type) {
        String t = type.trim().toUpperCase(Locale.ROOT);
        if (!t.equals("A") && !t.equals("U")) {
            throw ApiException.badRequest("INVALID_USER_TYPE", "User Type must be A (Admin) or U (User)...");
        }
        return t;
    }

    /**
     * COSGN00C upper-cases the password before comparing, but COUSR01C/02C store it as typed, so mixed-case
     * passwords could never sign on. Storing the upper-cased hash keeps sign-on semantics and fixes that defect.
     */
    private static String normalisePassword(String password) {
        return password.trim().toUpperCase(Locale.ROOT);
    }

    private String hash(String password) {
        return passwordEncoder.encode(normalisePassword(password));
    }
}
