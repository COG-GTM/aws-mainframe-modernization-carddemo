package com.carddemo.identity;

import com.carddemo.common.domain.UserSecurity;
import com.carddemo.common.domain.UserSecurityRepository;
import com.carddemo.common.error.ApiException;
import com.carddemo.common.security.CardDemoPrincipal;
import com.carddemo.common.security.JwtService;
import java.util.Locale;
import org.springframework.http.HttpStatus;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

/** COSGN00C (transaction CC00): validates credentials against USRSEC and routes admins vs. regular users. */
@Service
public class SignOnService {

    private final UserSecurityRepository users;
    private final PasswordEncoder passwordEncoder;
    private final JwtService jwtService;

    public SignOnService(UserSecurityRepository users, PasswordEncoder passwordEncoder, JwtService jwtService) {
        this.users = users;
        this.passwordEncoder = passwordEncoder;
        this.jwtService = jwtService;
    }

    public SignOnResponse signOn(SignOnRequest request) {
        if (isBlank(request.userId())) {
            throw ApiException.badRequest("USER_ID_REQUIRED", "Please enter User ID ...");
        }
        if (isBlank(request.password())) {
            throw ApiException.badRequest("PASSWORD_REQUIRED", "Please enter Password ...");
        }
        // COSGN00C applies FUNCTION UPPER-CASE to both fields before the USRSEC READ.
        String userId = request.userId().trim().toUpperCase(Locale.ROOT);
        String password = request.password().trim().toUpperCase(Locale.ROOT);
        UserSecurity user = users.findById(userId).orElseThrow(() ->
                new ApiException(HttpStatus.UNAUTHORIZED, "USER_NOT_FOUND", "User not found. Try again ..."));
        if (!passwordEncoder.matches(password, user.getPasswordHash())) {
            throw new ApiException(HttpStatus.UNAUTHORIZED, "WRONG_PASSWORD", "Wrong Password. Try again ...");
        }
        CardDemoPrincipal principal = new CardDemoPrincipal(user.getUserId(), user.getUserType(),
                user.getFirstName(), user.getLastName());
        return new SignOnResponse(jwtService.issue(principal), "Bearer", jwtService.getTtl().toSeconds(),
                user.getUserId(), user.getUserType(), user.getFirstName(), user.getLastName(),
                principal.isAdmin() ? "COADM01C" : "COMEN01C", Menus.forUser(principal));
    }

    private static boolean isBlank(String s) {
        return s == null || s.isBlank();
    }
}
