package com.carddemo.programs.codate01;

/**
 * State of CODATE01 at EXEC CICS RETURN.
 *
 * @param messagesProcessed MQ-MSG-COUNT
 * @param inputQueueName    INPUT-QUEUE-NAME (from MQTM-QNAME)
 * @param replyQueueName    REPLY-QUEUE-NAME
 * @param errorQueueName    ERROR-QUEUE-NAME
 * @param errDisplay        final MQ-ERR-DISPLAY contents
 * @param lastRequest       final REQUEST-MSG-COPY (WS-KEY is re-zeroed by every MQGET attempt)
 * @param lastReply         final REPLY-MESSAGE, or null when no request was processed
 */
public record Codate01Result(long messagesProcessed, String inputQueueName, String replyQueueName,
                             String errorQueueName, String errDisplay, RequestMsgCopy lastRequest,
                             DateReplyMessage lastReply) {
}
