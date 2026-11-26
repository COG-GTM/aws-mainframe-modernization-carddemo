package com.carddemo.programs.cobil00c;

import com.carddemo.programs.cobil00c.copybook.CardDemoCommarea;
import java.util.List;
import java.util.Optional;

/**
 * Outcome of one COBIL00C task.
 *
 * @param action        how the task ended: {@code XCTL} to another program or {@code RETURN TRANSID}
 * @param nextProgram   XCTL target (CDEMO-TO-PROGRAM) when action is XCTL
 * @param nextTransId   TRANSID on RETURN (always CB00)
 * @param commarea      CARDDEMO-COMMAREA passed on XCTL / RETURN
 * @param screensSent   every SEND MAP issued, in order; the terminal ends up showing the last one
 * @param displayLines  DISPLAY output (SYSOUT) produced by the task
 */
public record BillPayResult(
        Action action,
        String nextProgram,
        String nextTransId,
        CardDemoCommarea commarea,
        List<BillPayScreen> screensSent,
        List<String> displayLines) {

    public enum Action { XCTL, RETURN }

    public Optional<BillPayScreen> finalScreen() {
        return screensSent.isEmpty() ? Optional.empty() : Optional.of(screensSent.get(screensSent.size() - 1));
    }
}
