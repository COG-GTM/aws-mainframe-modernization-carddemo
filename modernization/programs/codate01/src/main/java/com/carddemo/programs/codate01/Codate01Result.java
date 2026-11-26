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
 * @param lastReply         last REPLY-MESSAGE built by 4000-PROCESS-REQUEST-REPLY (even if its MQPUT failed), or null when none was built
 */
public record Codate01Result(long messagesProcessed, String inputQueueName, String replyQueueName,
                             String errorQueueName, String errDisplay, RequestMsgCopy lastRequest,
                             DateReplyMessage lastReply) {
}
