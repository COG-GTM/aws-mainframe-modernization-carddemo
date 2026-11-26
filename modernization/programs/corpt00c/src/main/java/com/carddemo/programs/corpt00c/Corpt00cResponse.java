package com.carddemo.programs.corpt00c;

import com.carddemo.programs.corpt00c.copybook.CarddemoCommarea;
import com.carddemo.programs.corpt00c.copybook.Corpt0aOutput;

/**
 * How the task ended.
 *
 * @param action     SEND_MAP (SEND MAP then RETURN TRANSID), XCTL, or RETURN (RETURN TRANSID without a map)
 * @param transId    TRANSID of the pseudo-conversational RETURN (null for XCTL)
 * @param program    XCTL target program (null otherwise)
 * @param commarea   COMMAREA passed on RETURN / XCTL
 * @param screen     CORPT0AO as sent (SEND_MAP only)
 * @param erase      SEND ... ERASE was used
 */
public record Corpt00cResponse(Action action, String transId, String program, CarddemoCommarea commarea,
                               Corpt0aOutput screen, boolean erase) {

    public enum Action { SEND_MAP, XCTL, RETURN }

    public static final String MAP = "CORPT0A";
    public static final String MAPSET = "CORPT00";
}
