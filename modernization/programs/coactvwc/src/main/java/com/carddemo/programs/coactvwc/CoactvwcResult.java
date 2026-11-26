package com.carddemo.programs.coactvwc;

import com.carddemo.programs.coactvwc.copybook.AbendData;
import com.carddemo.programs.coactvwc.copybook.CactvwaOutput;
import com.carddemo.programs.coactvwc.copybook.CardDemoCommarea;

/** How a COACTVWC task ends. */
public sealed interface CoactvwcResult {

    /** SEND MAP(CACTVWA) MAPSET(COACTVW) then RETURN TRANSID('CAVW') COMMAREA(WS-COMMAREA) LENGTH(2000). */
    record SendMapAndReturn(String mapset, String map, CactvwaOutput screen, String transId, String commarea)
            implements CoactvwcResult {
        public CardDemoCommarea carddemoCommarea() {
            return CardDemoCommarea.fromString(commarea.substring(0, CardDemoCommarea.LENGTH));
        }

        public ThisProgCommarea thisProgCommarea() {
            return ThisProgCommarea.fromString(commarea.substring(CardDemoCommarea.LENGTH,
                    CardDemoCommarea.LENGTH + ThisProgCommarea.LENGTH));
        }
    }

    /** XCTL PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA). */
    record Xctl(String program, CardDemoCommarea commarea) implements CoactvwcResult {
    }

    /** SEND TEXT FROM(WS-RETURN-MSG) ERASE FREEKB then RETURN (no next transaction). */
    record SendTextAndReturn(String text) implements CoactvwcResult {
    }

    /** ABEND-ROUTINE: SEND FROM(ABEND-DATA) then ABEND ABCODE('9999'). */
    record Abend(String abcode, AbendData abendData, Throwable cause) implements CoactvwcResult {
    }
}
