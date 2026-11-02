package com.carddemo.common.security;

import com.carddemo.common.error.ForbiddenException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerInterceptor;

/** Enforces {@link AdminOnly}; the 403 is rendered by the shared exception handler as {@code ApiError}. */
public class AdminOnlyInterceptor implements HandlerInterceptor {

    /** COMEN01C's text for a {@code U} user picking an {@code 'A'} option. */
    public static final String ADMIN_ONLY_MESSAGE = "No access - Admin Only option... ";

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) {
        if (handler instanceof HandlerMethod method && isAdminOnly(method)) {
            boolean admin = CallerIdentity.from(request).map(CallerIdentity::isAdmin).orElse(false);
            if (!admin) {
                throw new ForbiddenException(ADMIN_ONLY_MESSAGE);
            }
        }
        return true;
    }

    private static boolean isAdminOnly(HandlerMethod method) {
        return method.hasMethodAnnotation(AdminOnly.class)
                || AnnotatedElementUtils.hasAnnotation(method.getBeanType(), AdminOnly.class);
    }
}
