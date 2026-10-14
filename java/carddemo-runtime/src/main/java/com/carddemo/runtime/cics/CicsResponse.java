package com.carddemo.runtime.cics;

import java.util.List;

/** Result of running one task: how it ended, what it sent to the terminal, and the final EIB. */
public record CicsResponse(String program, CicsOutcome outcome, List<TerminalOutput> terminalOutputs, Eib eib) {

    /** The last SEND MAP of the task, if any. */
    public TerminalOutput.MapOutput lastMap() {
        for (int i = terminalOutputs.size() - 1; i >= 0; i--) {
            if (terminalOutputs.get(i) instanceof TerminalOutput.MapOutput m) {
                return m;
            }
        }
        return null;
    }

    /** The last SEND TEXT of the task, if any. */
    public TerminalOutput.TextOutput lastText() {
        for (int i = terminalOutputs.size() - 1; i >= 0; i--) {
            if (terminalOutputs.get(i) instanceof TerminalOutput.TextOutput t) {
                return t;
            }
        }
        return null;
    }
}
