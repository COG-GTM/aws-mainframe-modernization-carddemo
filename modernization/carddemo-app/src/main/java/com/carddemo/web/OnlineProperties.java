package com.carddemo.web;

import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Values of {@code EXEC CICS ASSIGN APPLID/SYSID} shown in every screen header.
 *
 * @param applid region application id ({@code APPLIDO}, 8 characters)
 * @param sysid  region system id ({@code SYSIDO}, 4 characters)
 */
@ConfigurationProperties("carddemo.online")
public record OnlineProperties(String applid, String sysid) {

    public OnlineProperties {
        applid = applid == null ? "CARDDEMO" : applid;
        sysid = sysid == null ? "CDMO" : sysid;
    }
}
