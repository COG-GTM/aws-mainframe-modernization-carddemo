package com.carddemo.programs.coacct01;

/**
 * Outcome of one COACCT01 task (the CICS program itself has no return code).
 *
 * @param messageCount   MQ-MSG-COUNT, requests processed (PIC 9(09))
 * @param mqErrDisplay   final MQ-ERR-DISPLAY contents
 * @param noMoreMessages NO-MORE-MSGS: the input queue was drained (MQRC 2033) rather than ending on an error
 */
public record Coacct01Result(long messageCount, String mqErrDisplay, boolean noMoreMessages) {
}
