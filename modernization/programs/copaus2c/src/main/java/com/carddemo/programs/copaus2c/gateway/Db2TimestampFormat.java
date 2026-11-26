package com.carddemo.programs.copaus2c.gateway;

import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/** DB2 {@code TIMESTAMP_FORMAT(string, 'YY-MM-DD HH24.MI.SSNNNNNN')} as used by COPAUS2C. */
public final class Db2TimestampFormat {

    private static final Pattern FORMAT =
            Pattern.compile("(\\d{2})-(\\d{2})-(\\d{2}) (\\d{2})\\.(\\d{2})\\.(\\d{2})(\\d{6})");

    private Db2TimestampFormat() {
    }

    /**
     * Parses the CHAR(26) host variable (trailing blanks ignored). YY takes its century from the
     * current year. Throws {@link DateTimeException} for an invalid string (DB2 SQLCODE -20448).
     */
    public static LocalDateTime parse(String hostVar, LocalDate currentDate) {
        String v = hostVar.stripTrailing();
        Matcher m = FORMAT.matcher(v);
        if (!m.matches()) {
            throw new DateTimeException("Invalid TIMESTAMP_FORMAT input: '" + v + "'");
        }
        int century = currentDate.getYear() / 100 * 100;
        return LocalDateTime.of(
                century + Integer.parseInt(m.group(1)),
                Integer.parseInt(m.group(2)),
                Integer.parseInt(m.group(3)),
                Integer.parseInt(m.group(4)),
                Integer.parseInt(m.group(5)),
                Integer.parseInt(m.group(6)),
                Integer.parseInt(m.group(7)) * 1000);
    }
}
