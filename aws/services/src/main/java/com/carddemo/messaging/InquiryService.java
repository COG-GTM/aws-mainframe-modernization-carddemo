package com.carddemo.messaging;

import com.carddemo.account.AccountRecord;
import com.carddemo.account.AccountRepository;
import com.carddemo.common.Money;
import com.carddemo.messaging.InquiryMessages.AccountInquiryReply;
import com.carddemo.messaging.InquiryMessages.AccountPayload;
import com.carddemo.messaging.InquiryMessages.DateInquiryReply;
import com.carddemo.messaging.InquiryMessages.InquiryRequest;
import java.time.Clock;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Optional;
import java.util.UUID;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** Request handling of COACCT01 (account inquiry) and CODATE01 (date inquiry), transport independent. */
@Service
public class InquiryService {

    private static final DateTimeFormatter MM_DD_YYYY = DateTimeFormatter.ofPattern("MM-dd-yyyy");
    private static final DateTimeFormatter HH_MM_SS = DateTimeFormatter.ofPattern("HH:mm:ss");
    private static final long MAX_ACCT_ID = 99_999_999_999L;

    private final AccountRepository accounts;
    private final Clock clock;

    public InquiryService(AccountRepository accounts, Clock clock) {
        this.accounts = accounts;
        this.clock = clock;
    }

    @Transactional(readOnly = true)
    public AccountInquiryReply accountInquiry(InquiryRequest request) {
        UUID correlationId = request.messageId();
        Long acctId = request.acctId();
        String invalidText = "INVALID REQUEST PARAMETERS ACCT ID : " + String.format("%011d",
                acctId == null ? 0 : Math.max(0, Math.min(acctId, MAX_ACCT_ID)));
        if (!"INQA".equals(request.function()) || acctId == null || acctId <= 0 || acctId > MAX_ACCT_ID) {
            return new AccountInquiryReply("1", UUID.randomUUID(), correlationId, "INVALID_REQUEST", invalidText,
                    null);
        }
        Optional<AccountRecord> found = accounts.findAccount(acctId);
        if (found.isEmpty()) {
            return new AccountInquiryReply("1", UUID.randomUUID(), correlationId, "NOT_FOUND", invalidText, null);
        }
        AccountRecord a = found.get();
        AccountPayload payload = new AccountPayload(a.acctId(), a.activeStatus(), Money.format(a.currBal()),
                Money.format(a.creditLimit()), Money.format(a.cashCreditLimit()), iso(a.openDate()),
                iso(a.expirationDate()), iso(a.reissueDate()), Money.format(a.currCycCredit()),
                Money.format(a.currCycDebit()), a.groupId());
        return new AccountInquiryReply("1", UUID.randomUUID(), correlationId, "OK", null, payload);
    }

    public DateInquiryReply dateInquiry(InquiryRequest request) {
        LocalDateTime now = LocalDateTime.now(clock);
        String date = now.format(MM_DD_YYYY);
        String time = now.format(HH_MM_SS);
        return new DateInquiryReply("1", UUID.randomUUID(), request.messageId(), "OK", date, time,
                "SYSTEM DATE : " + date + "SYSTEM TIME : " + time);
    }

    private static String iso(LocalDate date) {
        return date == null ? null : date.toString();
    }
}
