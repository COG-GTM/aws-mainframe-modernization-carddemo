package com.carddemo.auth;

import com.carddemo.common.ApiException;
import com.carddemo.common.ErrorCode;
import com.carddemo.common.Text;
import com.carddemo.common.ValidationErrors;
import com.carddemo.security.JwtService;
import com.carddemo.security.Role;
import com.carddemo.user.UserRecord;
import com.carddemo.user.UserRepository;
import java.util.Optional;
import org.springframework.dao.DataAccessException;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

/** COSGN00C: validates the credentials against USRSEC (user_security) and routes by SEC-USR-TYPE. */
@Service
public class SignonService {

    static final String PROGRAM = "COSGN00C";

    private final UserRepository users;
    private final PasswordEncoder passwordEncoder;
    private final JwtService jwtService;

    public SignonService(UserRepository users, PasswordEncoder passwordEncoder, JwtService jwtService) {
        this.users = users;
        this.passwordEncoder = passwordEncoder;
        this.jwtService = jwtService;
    }

    public SignonResponse signon(SignonRequest request) {
        String userIdIn = request == null ? null : request.userId();
        String passwordIn = request == null ? null : request.password();
        ValidationErrors errors = new ValidationErrors(PROGRAM);
        if (Text.isBlank(userIdIn)) {
            errors.add("userId", "Please enter User ID ...");
        } else if (Text.isBlank(passwordIn)) {
            errors.add("password", "Please enter Password ...");
        }
        errors.throwIfAny();

        String userId = Text.upperTrim(userIdIn);
        String password = Text.upperTrim(passwordIn);
        Optional<UserRecord> found;
        try {
            found = users.findById(userId);
        } catch (DataAccessException ex) {
            throw new ApiException(ErrorCode.INTERNAL_ERROR, "Unable to verify the User ...", PROGRAM);
        }
        UserRecord user = found.orElseThrow(
                () -> new ApiException(ErrorCode.INVALID_CREDENTIALS, "User not found. Try again ...", PROGRAM));
        if (!passwordEncoder.matches(password, user.passwordHash())) {
            throw new ApiException(ErrorCode.INVALID_CREDENTIALS, "Wrong Password. Try again ...", PROGRAM);
        }
        Role role = Role.fromUserType(user.userType());
        String name = (user.firstName() + " " + user.lastName()).strip();
        JwtService.IssuedToken token = jwtService.issue(user.userId(), role, name);
        return new SignonResponse(token.token(), "Bearer", token.expiresAt(), user.userId(), user.firstName(), user.lastName(),
                role.name(), role == Role.ADMIN ? "/admin" : "/menu");
    }
}
