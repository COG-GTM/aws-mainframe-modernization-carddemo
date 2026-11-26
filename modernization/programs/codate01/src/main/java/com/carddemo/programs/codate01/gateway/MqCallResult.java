package com.carddemo.programs.codate01.gateway;

/** Completion code and reason code returned by MQPUT / MQCLOSE. */
public record MqCallResult(int conditionCode, int reasonCode) {
}
