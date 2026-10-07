package com.carddemo.common.time;

import java.time.LocalDateTime;
import java.time.ZoneId;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Business clock settings (ADR-0014).
 *
 * @param fixed when set, the application clock is frozen at this local date-time (the equivalent of
 *              GnuCOBOL's {@code COB_CURRENT_DATE}); when empty the system clock is used
 * @param zone  zone in which {@code FUNCTION CURRENT-DATE} / {@code ASKTIME} values are interpreted
 */
@ConfigurationProperties("carddemo.clock")
public record ClockProperties(LocalDateTime fixed, ZoneId zone) {

    public ClockProperties {
        zone = zone == null ? ZoneId.of("UTC") : zone;
    }
}
