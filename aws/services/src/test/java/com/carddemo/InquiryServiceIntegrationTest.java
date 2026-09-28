package com.carddemo;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.messaging.InquiryMessages.AccountInquiryReply;
import com.carddemo.messaging.InquiryMessages.DateInquiryReply;
import com.carddemo.messaging.InquiryMessages.InquiryRequest;
import com.carddemo.messaging.InquiryService;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

class InquiryServiceIntegrationTest extends IntegrationTestBase {

    @Autowired
    private InquiryService inquiries;

    @Test
    void accountInquiryReturnsAccountLikeCoacct01() {
        UUID id = UUID.randomUUID();
        AccountInquiryReply reply = inquiries.accountInquiry(new InquiryRequest("1", id, "INQA", 1L, null));
        assertThat(reply.status()).isEqualTo("OK");
        assertThat(reply.correlationId()).isEqualTo(id);
        assertThat(reply.account().currBal()).isEqualTo("194.00");
        assertThat(reply.account().creditLimit()).isEqualTo("2020.00");
        assertThat(reply.account().openDate()).isEqualTo("2014-11-20");
    }

    @Test
    void accountInquiryRejectsBadFunctionAndUnknownAccount() {
        AccountInquiryReply bad = inquiries.accountInquiry(new InquiryRequest("1", UUID.randomUUID(), "XXXX", 1L,
                null));
        assertThat(bad.status()).isEqualTo("INVALID_REQUEST");
        assertThat(bad.text()).isEqualTo("INVALID REQUEST PARAMETERS ACCT ID : 00000000001");
        AccountInquiryReply missing = inquiries.accountInquiry(new InquiryRequest("1", UUID.randomUUID(), "INQA",
                99999999999L, null));
        assertThat(missing.status()).isEqualTo("NOT_FOUND");
        assertThat(missing.account()).isNull();
    }

    @Test
    void dateInquiryFormatsLikeCodate01() {
        DateInquiryReply reply = inquiries.dateInquiry(new InquiryRequest("1", UUID.randomUUID(), "INQD", null,
                null));
        assertThat(reply.systemDate()).matches("\\d{2}-\\d{2}-\\d{4}");
        assertThat(reply.systemTime()).matches("\\d{2}:\\d{2}:\\d{2}");
        assertThat(reply.text()).isEqualTo("SYSTEM DATE : " + reply.systemDate() + "SYSTEM TIME : "
                + reply.systemTime());
    }
}
