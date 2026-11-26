package com.carddemo.programs.cotrtlic;

import com.carddemo.programs.cotrtlic.copybook.CardDemoCommarea;
import com.carddemo.programs.cotrtlic.copybook.CotrtliMap;

/** How a COTRTLIC task ended. */
public sealed interface CicsResponse {

    /** SEND MAP(CTRTLIA) MAPSET(COTRTLI) CURSOR ERASE FREEKB, then RETURN TRANSID(CTLI) COMMAREA. */
    record SendMapAndReturn(CotrtliMap map, String mapName, String mapset, String transId,
            ProgramCommarea commarea) implements CicsResponse {
    }

    /** XCTL PROGRAM(program) COMMAREA(CARDDEMO-COMMAREA). */
    record Xctl(String program, CardDemoCommarea commarea) implements CicsResponse {
    }

    /** SEND TEXT FROM(text) ERASE FREEKB, then RETURN (no TRANSID: the conversation ends). */
    record SendTextAndReturn(String text) implements CicsResponse {
    }
}
