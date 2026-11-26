package com.carddemo.programs.cocrdlic;

import com.carddemo.programs.cocrdlic.copybook.CardDemoCommarea;
import com.carddemo.programs.cocrdlic.copybook.CardListMap;

/** Outcome of a task: XCTL to another program, or SEND MAP + RETURN TRANSID(CCLI). */
public sealed interface CardListResponse {

    CardDemoCommarea commarea();

    /** EXEC CICS XCTL PROGRAM(program) COMMAREA(CARDDEMO-COMMAREA). */
    record Xctl(String program, CardDemoCommarea commarea) implements CardListResponse {
    }

    /**
     * EXEC CICS SEND MAP(CCRDLIA) MAPSET(COCRDLI) CURSOR ERASE FREEKB followed by
     * EXEC CICS RETURN TRANSID(transid) COMMAREA(WS-COMMAREA) LENGTH(2000).
     */
    record SendMapAndReturn(String transid, CardListMap screen, String commareaImage)
            implements CardListResponse {

        @Override
        public CardDemoCommarea commarea() {
            return CardDemoCommarea.fromFixedWidth(commareaImage.substring(0, CardDemoCommarea.LENGTH));
        }

        public ThisProgCommarea programState() {
            return ThisProgCommarea.fromFixedWidth(commareaImage.substring(CardDemoCommarea.LENGTH,
                    CardDemoCommarea.LENGTH + ThisProgCommarea.LENGTH));
        }
    }
}
