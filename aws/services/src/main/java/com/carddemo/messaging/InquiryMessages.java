package com.carddemo.messaging;

import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.UUID;

/** JSON bodies of the VSAM/MQ inquiry queues (messaging.md section 3). */
public final class InquiryMessages {

    private InquiryMessages() {
    }

    public record InquiryRequest(String schemaVersion, UUID messageId, String function, Long acctId, String sentAt) {
    }

    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record AccountInquiryReply(String schemaVersion, UUID messageId, UUID correlationId, String status,
            String text, AccountPayload account) {
    }

    public record AccountPayload(long acctId, String activeStatus, String currBal, String creditLimit,
            String cashCreditLimit, String openDate, String expirationDate, String reissueDate, String currCycCredit,
            String currCycDebit, String groupId) {
    }

    public record DateInquiryReply(String schemaVersion, UUID messageId, UUID correlationId, String status,
            String systemDate, String systemTime, String text) {
    }

    @JsonInclude(JsonInclude.Include.ALWAYS)
    public record ErrorMessage(String schemaVersion, UUID messageId, UUID correlationId, String errDate,
            String errTime, String application, String program, String location, String level, String subsystem,
            String code1, String code2, String message, String eventKey, String sourceQueue) {
    }
}
