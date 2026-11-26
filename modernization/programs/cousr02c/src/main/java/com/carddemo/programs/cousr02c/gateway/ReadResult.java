package com.carddemo.programs.cousr02c.gateway;

import com.carddemo.programs.cousr02c.copybook.SecUserData;

/** Result of {@code EXEC CICS READ ... INTO(SEC-USER-DATA)}; {@code record} is null unless RESP is NORMAL. */
public record ReadResult(CicsResult result, SecUserData record) {
}
