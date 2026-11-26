package com.carddemo.programs.copaus0c;

import com.carddemo.programs.copaus0c.copybook.CardDemoCommarea;
import com.carddemo.programs.copaus0c.copybook.Copau0aScreen;

import java.util.List;

/**
 * Outcome of one COPAUS0C task.
 *
 * @param screens       every EXEC CICS SEND MAP issued, in order (error paths may send more than once)
 * @param xctlProgram   program receiving control via EXEC CICS XCTL, or {@code null} when the task ended
 *                      with EXEC CICS RETURN
 * @param returnTransid TRANSID of EXEC CICS RETURN ({@code CPVS}), or {@code null} after XCTL
 * @param commarea      CARDDEMO-COMMAREA passed on RETURN/XCTL
 */
public record Copaus0cResult(List<Copau0aScreen> screens, String xctlProgram, String returnTransid,
                             CardDemoCommarea commarea) {

    public Copaus0cResult {
        screens = List.copyOf(screens);
    }

    /** The last map sent - what the terminal finally shows. */
    public Copau0aScreen screen() {
        return screens.isEmpty() ? null : screens.get(screens.size() - 1);
    }
}
