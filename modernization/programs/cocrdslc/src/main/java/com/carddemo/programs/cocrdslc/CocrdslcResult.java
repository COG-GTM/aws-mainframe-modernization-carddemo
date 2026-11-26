package com.carddemo.programs.cocrdslc;

import com.carddemo.programs.cocrdslc.copybook.AbendData;
import com.carddemo.programs.cocrdslc.copybook.CarddemoCommarea;
import com.carddemo.programs.cocrdslc.copybook.CcrdslaOutput;

/** How the COCRDSLC task ended. */
public sealed interface CocrdslcResult {

    /**
     * 1400-SEND-SCREEN then COMMON-RETURN: SEND MAP('CCRDSLA') MAPSET('COCRDSL') then
     * RETURN TRANSID('CCDL') COMMAREA(WS-COMMAREA) LENGTH(2000).
     */
    record ScreenSent(CcrdslaOutput map, String transid, String commarea) implements CocrdslcResult {
        public CarddemoCommarea carddemoCommarea() {
            return CarddemoCommarea.parse(commarea);
        }

        public ThisProgCommarea thisProgCommarea() {
            return ThisProgCommarea.parse(commarea.substring(CarddemoCommarea.LENGTH));
        }
    }

    /** EXEC CICS XCTL PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA). */
    record Xctl(String program, CarddemoCommarea commarea) implements CocrdslcResult {
    }

    /** SEND-PLAIN-TEXT / SEND-LONG-TEXT: SEND TEXT ERASE FREEKB then RETURN (no TRANSID). */
    record TextSent(String text, AbendData abendData) implements CocrdslcResult {
    }

    /** ABEND-ROUTINE: SEND FROM(ABEND-DATA) then ABEND ABCODE('9999'). */
    record Abended(String abcode, AbendData abendData, Throwable cause) implements CocrdslcResult {
    }
}
