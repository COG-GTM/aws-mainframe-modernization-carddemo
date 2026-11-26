package com.carddemo.programs.codate01.gateway;

/** MQOPEN outputs: object handle (MQ-HOBJ), completion code and reason code. */
public record MqOpenResult(int hobj, int conditionCode, int reasonCode) {
}
