package com.carddemo.programs.comen01c.gateway;

import com.carddemo.programs.comen01c.dto.MenuScreenRequest;
import com.carddemo.programs.comen01c.dto.MenuScreenResponse;

/** EXEC CICS SEND MAP / RECEIVE MAP for map COMEN1A in mapset COMEN01. */
public interface ScreenGateway {

    /** RECEIVE MAP('COMEN1A') MAPSET('COMEN01') INTO(COMEN1AI). */
    MenuScreenRequest receive();

    /** SEND MAP('COMEN1A') MAPSET('COMEN01') FROM(COMEN1AO) ERASE. */
    void send(MenuScreenResponse screen);
}
