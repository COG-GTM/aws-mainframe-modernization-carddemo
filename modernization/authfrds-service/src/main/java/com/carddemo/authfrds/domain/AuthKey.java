package com.carddemo.authfrds.domain;

/** PA-AUTHORIZATION-KEY (IMS field PAUT9CTS): PA-AUTH-DATE-9C + PA-AUTH-TIME-9C. Text form "ddddd-ttttttttt". */
public record AuthKey(long date9c, long time9c) implements Comparable<AuthKey> {

    public static AuthKey parse(String s) {
        String[] p = s == null ? new String[0] : s.trim().split("-");
        if (p.length != 2 || !p[0].matches("\\d{1,5}") || !p[1].matches("\\d{1,9}")) {
            throw new LegacyRuleException("BR-AF-026", LegacyRuleException.Kind.VALIDATION,
                    "authorization key must be <PA-AUTH-DATE-9C>-<PA-AUTH-TIME-9C>, e.g. 76699-998747444");
        }
        return new AuthKey(Long.parseLong(p[0]), Long.parseLong(p[1]));
    }

    @Override
    public String toString() {
        return String.format("%05d-%09d", date9c, time9c);
    }

    @Override
    public int compareTo(AuthKey o) {
        int c = Long.compare(date9c, o.date9c);
        return c != 0 ? c : Long.compare(time9c, o.time9c);
    }
}
