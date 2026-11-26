package com.carddemo.programs.coactupc.gateway;

/**
 * Result of the CSUTLDTC call (LE CEEDAYS feedback): WS-SEVERITY, WS-MSG-NO and WS-RESULT.
 *
 * @param severity CEEDAYS feedback severity (0 = valid date)
 * @param msgNo    CEEDAYS feedback message number
 * @param result   WS-RESULT text, e.g. 'Date is valid'
 */
public record DateValidationResult(int severity, int msgNo, String result) {
}
