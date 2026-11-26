package com.carddemo.programs.copaua0c;

/**
 * Outcome of one COPAUA0C task.
 *
 * @param messagesProcessed WS-MSG-PROCESSED
 * @param endedByCriticalError true when 9500-LOG-ERROR hit ERR-CRITICAL and 9990-END-ROUTINE issued CICS RETURN
 * @param endedByProcessLimit true when WS-LOOP-END was set (WS-MSG-PROCESSED > WS-REQSTS-PROCESS-LIMIT)
 */
public record Copaua0cRunResult(int messagesProcessed, boolean endedByCriticalError, boolean endedByProcessLimit) {
}
