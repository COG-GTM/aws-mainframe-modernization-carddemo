package com.carddemo.auth.service;

import com.carddemo.auth.api.dto.SignonRequest;
import com.carddemo.auth.api.dto.SignonResponse;
import com.carddemo.auth.api.dto.UserRequest;
import com.carddemo.auth.domain.User;
import com.carddemo.auth.repository.UserRepository;
import com.carddemo.common.error.BusinessRuleException;
import com.carddemo.common.error.DuplicateKeyException;
import com.carddemo.common.error.NotFoundException;
import com.carddemo.common.security.TokenService;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class AuthService {

    public static final String WRONG_PASSWORD = "Wrong Password. Try again ...";
    public static final String USER_NOT_FOUND = "User not found. Try again ...";

    private final UserRepository users;
    private final PasswordEncoder passwordEncoder;
    private final TokenService tokens;

    public AuthService(UserRepository users, PasswordEncoder passwordEncoder, TokenService tokens) {
        this.users = users;
        this.passwordEncoder = passwordEncoder;
        this.tokens = tokens;
    }

    /**
     * COSGN00C. The COBOL upper-cases both map fields (FUNCTION UPPER-CASE) before the USRSEC read,
     * so only upper-case stored passwords can ever match; the password is accepted as typed or
     * upper-cased so USRSEC passwords sign on case-insensitively as in CICS while mixed-case
     * passwords set through the user API still work. It then answers "User not found" (RESP 13) or "Wrong Password" with the exact texts below so flow
     * tests can assert on them. The success path returns the signed token that replaces the COMMAREA.
     */
    @Transactional(readOnly = true)
    public SignonResponse signon(SignonRequest request) {
        User user = users.findById(request.userId().toUpperCase()).orElse(null);
        if (user == null) {
            throw new BusinessRuleException(USER_NOT_FOUND);
        }
        if (!passwordMatches(request.password(), user.getPasswordHash())) {
            throw new BusinessRuleException(WRONG_PASSWORD);
        }
        TokenService.IssuedToken token = tokens.issue(user.getUserId(), user.getUserType());
        return new SignonResponse(user.getUserId(), user.getFirstName(), user.getLastName(),
                user.getUserType(), user.isAdmin() ? "ADMIN" : "MAIN",
                token.token(), "Bearer", token.expiresAt());
    }

    /** COUSR00C: browse USRSEC. */
    @Transactional(readOnly = true)
    public Page<User> list(Pageable pageable) {
        return users.findAll(pageable);
    }

    @Transactional(readOnly = true)
    public User get(String userId) {
        return users.findById(userId.toUpperCase())
                .orElseThrow(() -> new NotFoundException("User " + userId + " not found"));
    }

    /** COUSR01C: add a user, rejecting a duplicate key as the COBOL DUPKEY path does. */
    @Transactional
    public User create(UserRequest request) {
        String userId = request.userId().toUpperCase();
        if (users.existsById(userId)) {
            throw new DuplicateKeyException("User " + userId + " already exists");
        }
        User user = new User(userId, passwordEncoder.encode(request.password()), request.userType());
        user.setFirstName(request.firstName());
        user.setLastName(request.lastName());
        return users.save(user);
    }

    /** COUSR02C: update a user. */
    @Transactional
    public User update(String userId, UserRequest request) {
        User user = get(userId);
        user.setFirstName(request.firstName());
        user.setLastName(request.lastName());
        user.setUserType(request.userType());
        user.setPasswordHash(passwordEncoder.encode(request.password()));
        return users.save(user);
    }

    /** COUSR03C: delete a user. */
    @Transactional
    public void delete(String userId) {
        users.delete(get(userId));
    }

    private boolean passwordMatches(String typed, String hash) {
        return passwordEncoder.matches(typed, hash)
                || (!typed.equals(typed.toUpperCase()) && passwordEncoder.matches(typed.toUpperCase(), hash));
    }
}
