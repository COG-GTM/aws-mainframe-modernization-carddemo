package com.carddemo.common.security;

import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

public final class CurrentUser {

    private CurrentUser() {
    }

    public static CardDemoPrincipal get() {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if (auth != null && auth.getPrincipal() instanceof CardDemoPrincipal principal) {
            return principal;
        }
        throw new IllegalStateException("No authenticated CardDemo principal");
    }
}
