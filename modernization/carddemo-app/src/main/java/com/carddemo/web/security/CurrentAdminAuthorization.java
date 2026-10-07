package com.carddemo.web.security;

import com.carddemo.user.UserSecurityRepository;
import com.carddemo.user.UserType;
import java.util.function.Supplier;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.security.authorization.AuthorizationDecision;
import org.springframework.security.authorization.AuthorizationManager;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.access.intercept.RequestAuthorizationContext;

/**
 * Admin paths need the {@code ADMIN} role in the token <em>and</em> {@code SEC-USR-TYPE = 'A'} in USRSEC at request
 * time (ADR-0023), so a demoted or deleted administrator is refused (403 {@code NOTAUTH}) at once instead of when the
 * token expires. One indexed single-column read per admin request; a read failure denies.
 */
class CurrentAdminAuthorization implements AuthorizationManager<RequestAuthorizationContext> {

    private static final Logger log = LoggerFactory.getLogger(CurrentAdminAuthorization.class);
    private static final String ADMIN_AUTHORITY = "ROLE_ADMIN";

    private final UserSecurityRepository users;

    CurrentAdminAuthorization(UserSecurityRepository users) {
        this.users = users;
    }

    @Override
    public AuthorizationDecision check(Supplier<Authentication> authentication, RequestAuthorizationContext context) {
        Authentication auth = authentication.get();
        if (auth == null || !auth.isAuthenticated()
                || auth.getAuthorities().stream().noneMatch(a -> ADMIN_AUTHORITY.equals(a.getAuthority()))) {
            return new AuthorizationDecision(false);
        }
        try {
            return new AuthorizationDecision(
                    users.findUsrTypeByUsrId(auth.getName()).map(t -> t == UserType.ADMIN).orElse(false));
        } catch (DataAccessException e) {
            log.warn("USRSEC read failed during the admin check of user {}", auth.getName(), e);
            return new AuthorizationDecision(false);
        }
    }
}
