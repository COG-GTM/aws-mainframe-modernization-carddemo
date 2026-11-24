package com.carddemo.transaction;

import com.carddemo.common.domain.Account;
import com.carddemo.common.domain.AccountRepository;
import com.carddemo.common.domain.CardTransaction;
import com.carddemo.common.domain.CardTransactionRepository;
import com.carddemo.common.domain.CardXref;
import com.carddemo.common.domain.CardXrefRepository;
import com.carddemo.common.domain.TransactionIdGenerator;
import com.carddemo.common.error.ApiException;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDateTime;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** COBIL00C (CB00): pays the full current balance and posts a type 02 / category 2 transaction. */
@Service
public class BillPaymentService {

    private final AccountRepository accounts;
    private final CardXrefRepository xrefs;
    private final CardTransactionRepository transactions;
    private final TransactionIdGenerator ids;
    private final Clock clock;

    public BillPaymentService(AccountRepository accounts, CardXrefRepository xrefs,
                              CardTransactionRepository transactions, TransactionIdGenerator ids,
                              java.util.Optional<Clock> clock) {
        this.accounts = accounts;
        this.xrefs = xrefs;
        this.transactions = transactions;
        this.ids = ids;
        this.clock = clock.orElse(Clock.systemDefaultZone());
    }

    @Transactional
    public OperationResult pay(BillPaymentRequest r) {
        if (r.acctId() == null || r.acctId().isBlank()) {
            TransactionService.fail("Acct ID can NOT be empty...");
        }
        if (!r.acctId().trim().matches("\\d{1,11}")) {
            throw ApiException.notFound("ACCOUNT_NOT_FOUND", "Account ID NOT found...");
        }
        long acctId = Long.parseLong(r.acctId().trim());
        Account account = accounts.findById(acctId)
                .orElseThrow(() -> ApiException.notFound("ACCOUNT_NOT_FOUND", "Account ID NOT found..."));
        String confirm = r.confirm() == null ? "" : r.confirm().trim().toUpperCase();
        if (!confirm.isEmpty() && !confirm.equals("Y") && !confirm.equals("N")) {
            TransactionService.fail("Invalid value. Valid values are (Y/N)...");
        }
        if (account.getCurrBal().signum() <= 0) {
            TransactionService.fail("You have nothing to pay...");
        }
        if (!confirm.equals("Y")) {
            return new OperationResult("Confirm to make a bill payment...", true, null, account.getCurrBal());
        }
        CardXref xref = xrefs.findFirstByAcctIdOrderByCardNumAsc(acctId)
                .orElseThrow(() -> ApiException.notFound("ACCOUNT_NOT_FOUND", "Account ID NOT found..."));
        BigDecimal amount = account.getCurrBal();
        LocalDateTime now = LocalDateTime.now(clock).withNano(0);
        CardTransaction t = new CardTransaction();
        t.setTranId(ids.next());
        t.setTypeCd("02");
        t.setCatCd(2);
        t.setSource("POS TERM");
        t.setDescription("BILL PAYMENT - ONLINE");
        t.setAmount(amount);
        t.setMerchantId(999999999L);
        t.setMerchantName("BILL PAYMENT");
        t.setMerchantCity("N/A");
        t.setMerchantZip("N/A");
        t.setCardNum(xref.getCardNum());
        t.setOrigTs(now);
        t.setProcTs(now);
        transactions.save(t);
        account.setCurrBal(account.getCurrBal().subtract(amount));
        return new OperationResult("Payment successful.  Your Transaction ID is " + t.getTranId() + ".", false,
                t.getTranId(), account.getCurrBal());
    }
}
