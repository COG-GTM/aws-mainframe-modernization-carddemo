package com.carddemo.programs.copaus1c;

import com.carddemo.programs.copaus1c.copybook.CardDemoCommarea;

/** 01 CARDDEMO-COMMAREA as declared in COPAUS1C: COPY COCOM01Y followed by 05 CDEMO-CPVD-INFO. */
public final class Copaus1cCommarea extends CardDemoCommarea {

    private final CpvdInfo cpvdInfo = new CpvdInfo();

    public CpvdInfo cpvd() {
        return cpvdInfo;
    }

    @Override
    public void initialize() {
        super.initialize();
        cpvdInfo.initialize();
    }

    public Copaus1cCommarea copy() {
        Copaus1cCommarea c = new Copaus1cCommarea();
        c.copyFrom(this);
        c.cpvdInfo.copyFrom(cpvdInfo);
        return c;
    }
}
