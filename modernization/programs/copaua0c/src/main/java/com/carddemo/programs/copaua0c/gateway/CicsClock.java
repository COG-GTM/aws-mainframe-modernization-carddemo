package com.carddemo.programs.copaua0c.gateway;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/** EXEC CICS ASKTIME + FORMATTIME. */
public interface CicsClock {

    /** FORMATTIME outputs: YYMMDD, YYDDD (5 chars), TIME (hhmmss) and MILLISECONDS. */
    record FormattedTime(String yymmdd, String yyddd, String hhmmss, int milliseconds) {

        public static FormattedTime of(LocalDateTime t) {
            return new FormattedTime(
                    t.format(DateTimeFormatter.ofPattern("yyMMdd")),
                    t.format(DateTimeFormatter.ofPattern("yyDDD")),
                    t.format(DateTimeFormatter.ofPattern("HHmmss")),
                    t.getNano() / 1_000_000);
        }
    }

    FormattedTime now();

    static CicsClock system() {
        return () -> FormattedTime.of(LocalDateTime.now());
    }
}
