package com.carddemo.programs.codate01.gateway;

/**
 * MQGET outputs. {@code data} holds the bytes the queue manager wrote into MQ-BUFFER (at most the
 * buffer length); {@code dataLength} is MQ-DATA-LENGTH (the full message length, even when truncated).
 */
public record MqGetResult(int conditionCode, int reasonCode, int dataLength, String data) {
}
