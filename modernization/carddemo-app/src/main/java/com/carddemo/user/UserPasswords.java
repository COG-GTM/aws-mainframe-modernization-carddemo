package com.carddemo.user;

import org.springframework.security.crypto.factory.PasswordEncoderFactories;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;

/**
 * USRSEC password verification (ADR-0023). The hash is taken over the 8-byte {@code SEC-USR-PWD} field exactly as
 * COSGN00C compares it (space padded, never truncated), so a hashed user signs on with precisely the inputs that
 * matched the plain-text compare (ADR-0018). Users without a hash yet are compared in plain text.
 */
@Component
public class UserPasswords {

    /** {@code SEC-USR-PWD PIC X(08)}. */
    public static final int FIELD_LENGTH = 8;

    private final PasswordEncoder encoder = PasswordEncoderFactories.createDelegatingPasswordEncoder();

    /** {@code {bcrypt}...} hash of the stored password value. */
    public String hash(String storedPassword) {
        return encoder.encode(pic8(storedPassword));
    }

    /** True when {@code candidate} (already upper-cased by the caller where COBOL does so) matches the user. */
    public boolean matches(UserSecurity user, String candidate) {
        String hash = user.getPasswordHash();
        if (hash == null) {
            return pic8(user.getPassword()).equals(pic8(candidate));
        }
        return encoder.matches(pic8(candidate), hash);
    }

    /**
     * The value space-padded to the 8-byte field COBOL compares ({@code SEC-USR-PWD = WS-USER-PWD}); longer input
     * is never truncated, so it cannot match.
     */
    public static String pic8(String value) {
        String v = value == null ? "" : value;
        return v.length() >= FIELD_LENGTH ? v : v + " ".repeat(FIELD_LENGTH - v.length());
    }
}
