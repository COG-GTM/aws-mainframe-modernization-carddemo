package com.carddemo.common.online;

import java.util.Arrays;
import java.util.Optional;

/**
 * Online programs defined in the CSD of this estate ({@code app/csd/CARDDEMO.CSD}, group {@code CARDDEMO}) with
 * their transaction id. A program that is not listed here is "not installed": {@code EXEC CICS INQUIRE PROGRAM}
 * returns {@code PGMIDERR} and {@code XCTL} raises it (e.g. {@code COPAUS0C}, {@code COTRTLIC}, {@code COTRTUPC}).
 */
public enum OnlineProgram {
    COSGN00C("CC00"),
    COMEN01C("CM00"),
    COADM01C("CA00"),
    COACTVWC("CAVW"),
    COACTUPC("CAUP"),
    COCRDLIC("CCLI"),
    COCRDSLC("CCDL"),
    COCRDUPC("CCUP"),
    COCRDSEC("CDV1"),
    COTRN00C("CT00"),
    COTRN01C("CT01"),
    COTRN02C("CT02"),
    CORPT00C("CR00"),
    COBIL00C("CB00"),
    COUSR00C("CU00"),
    COUSR01C("CU01"),
    COUSR02C("CU02"),
    COUSR03C("CU03");

    private final String tranId;

    OnlineProgram(String tranId) {
        this.tranId = tranId;
    }

    public String programId() {
        return name();
    }

    public String tranId() {
        return tranId;
    }

    public static Optional<OnlineProgram> find(String programId) {
        return Arrays.stream(values()).filter(p -> p.name().equals(programId)).findFirst();
    }
}
