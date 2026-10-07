package com.carddemo.web;

import com.carddemo.common.online.CommonMessages;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import org.springframework.stereotype.Component;

/** Builds {@link ScreenHeader}s from the business clock (ADR-0014) and the region ids. */
@Component
public class ScreenHeaders {

    private static final DateTimeFormatter DATE = DateTimeFormatter.ofPattern("MM/dd/yy");
    private static final DateTimeFormatter TIME = DateTimeFormatter.ofPattern("HH:mm:ss");

    private final Clock clock;
    private final OnlineProperties region;

    public ScreenHeaders(Clock clock, OnlineProperties region) {
        this.clock = clock;
        this.region = region;
    }

    /** {@code POPULATE-HEADER-INFO}: titles, tran id, program name, business-clock date/time and the region ids. */
    public ScreenHeader of(String tranId, String programName) {
        LocalDateTime now = LocalDateTime.now(clock);
        return new ScreenHeader(CommonMessages.TITLE01, CommonMessages.TITLE02, tranId, programName, DATE.format(now),
                TIME.format(now), region.applid(), region.sysid());
    }
}
