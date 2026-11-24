package com.carddemo.authfrds.config;

import java.util.ArrayList;
import java.util.List;

/** BR-AF-040: API users from AUTHFRDS_API_USER / _PASSWORD (maintainer) and AUTHFRDS_API_READER_USER / _PASSWORD. */
public record ApiCredentials(String maintainerUser, String maintainerPassword, String readerUser,
                             String readerPassword) {

    /** Fail fast on missing or clashing credentials. The unit-test {@code test} profile is exempt. */
    public void validate(boolean testProfile) {
        if (testProfile) {
            return;
        }
        List<String> missing = new ArrayList<>();
        if (blank(maintainerUser)) missing.add("AUTHFRDS_API_USER");
        if (blank(maintainerPassword)) missing.add("AUTHFRDS_API_PASSWORD");
        if (blank(readerUser)) missing.add("AUTHFRDS_API_READER_USER");
        if (blank(readerPassword)) missing.add("AUTHFRDS_API_READER_PASSWORD");
        if (!missing.isEmpty()) {
            throw new IllegalStateException("API credentials not set: " + String.join(", ", missing)
                    + " (see .env.example)");
        }
        if (maintainerUser.equals(readerUser)) {
            throw new IllegalStateException("AUTHFRDS_API_USER and AUTHFRDS_API_READER_USER must differ");
        }
    }

    private static boolean blank(String s) {
        return s == null || s.isBlank();
    }
}
