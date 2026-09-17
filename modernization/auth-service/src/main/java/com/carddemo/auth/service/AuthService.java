package com.carddemo.auth.service;

import com.carddemo.auth.api.dto.SignonRequest;
import com.carddemo.auth.api.dto.SignonResponse;
import com.carddemo.auth.api.dto.UserRequest;
import com.carddemo.auth.domain.User;
import com.carddemo.auth.repository.UserRepository;
import com.carddemo.common.error.BusinessRuleException;
import com.carddemo.common.error.DuplicateKeyException;
import com.carddemo.common.error.NotFoundException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class AuthService {

    private final UserRepository users;
    private final PasswordEncoder passwordEncoder;

    public AuthService(UserRepository users, PasswordEncoder passwordEncoder) {
        this.users = users;
        this.passwordEncoder = passwordEncoder;
    }

    /**
     * COSGN00C. The COBOL compares SEC-USR-PWD byte for byte and answers "Wrong Password" or
     * "User not found"; the modern service answers one message for both so it does not confirm
     * which user ids exist.
     */
    @Transactional(readOnly = true)
    public SignonResponse signon(SignonRequest request) {
        User user = users.findById(request.userId().toUpperCase()).orElse(null);
        if (user == null || !passwordEncoder.matches(request.password(), user.getPasswordHash())) {
            throw new BusinessRuleException("Invalid user id or password");
        }
        return new SignonResponse(user.getUserId(), user.getFirstName(), user.getLastName(),
                user.getUserType(), user.isAdmin() ? "ADMIN" : "MAIN");
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
}
