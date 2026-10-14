package com.carddemo.runtime.cics;

import com.carddemo.runtime.bms.BmsScreen;

/** Something a task sent to the terminal. */
public sealed interface TerminalOutput {

    /** EXEC CICS SEND MAP. */
    record MapOutput(BmsScreen screen) implements TerminalOutput {
    }

    /** EXEC CICS SEND TEXT. */
    record TextOutput(String text, boolean erase, boolean freekb) implements TerminalOutput {
    }
}
