package com.carddemo.common.security;

import jakarta.servlet.http.HttpServletRequest;
import java.util.Optional;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;

/**
 * The two COMMAREA fields every CardDemo program inherits from COSGN00C, {@code CDEMO-USER-ID} and
 * {@code CDEMO-USER-TYPE}, recovered from the verified bearer token of the current request.
 */
public record CallerIdentity(String userId, String userType) {

    public static final String ADMIN = "A";
    public static final String USER = "U";

    static final String REQUEST_ATTRIBUTE = CallerIdentity.class.getName();

    /** {@code CDEMO-USRTYP-ADMIN}. */
    public boolean isAdmin() {
        return ADMIN.equals(userType);
    }

    public static Optional<CallerIdentity> from(HttpServletRequest request) {
        return Optional.ofNullable((CallerIdentity) request.getAttribute(REQUEST_ATTRIBUTE));
    }

    /** The caller of the request bound to the current thread, if any. */
    public static Optional<CallerIdentity> current() {
        RequestAttributes attributes = RequestContextHolder.getRequestAttributes();
        if (attributes == null) {
            return Optional.empty();
        }
        return Optional.ofNullable((CallerIdentity) attributes.getAttribute(REQUEST_ATTRIBUTE,
                RequestAttributes.SCOPE_REQUEST));
    }
}
