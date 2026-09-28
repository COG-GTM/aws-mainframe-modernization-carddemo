package com.carddemo.billpay;

import com.carddemo.account.AccountRecord;
import com.carddemo.account.AccountRepository;
import com.carddemo.account.AccountService;
import com.carddemo.common.ApiException;
import com.carddemo.common.ErrorCode;
import com.carddemo.common.Money;
import com.carddemo.common.Text;
import com.carddemo.transaction.TranIdAllocator;
import com.carddemo.transaction.TransactionRecord;
import com.carddemo.transaction.TransactionRepository;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** COBIL00C: pays the full current balance of an account. */
@Service
public class BillPaymentService {

    static final String PROGRAM = "COBIL00C";

    private final AccountRepository accounts;
    private final TransactionRepository transactions;
    private final TranIdAllocator allocator;
    private final JdbcClient jdbc;
    private final Clock clock;

    public BillPaymentService(AccountRepository accounts, TransactionRepository transactions,
            TranIdAllocator allocator, JdbcClient jdbc, Clock clock) {
        this.accounts = accounts;
        this.transactions = transactions;
        this.allocator = allocator;
        this.jdbc = jdbc;
        this.clock = clock;
    }

    public record BalanceView(long acctId, String currBal) {
    }

    public record PaymentResult(String tranId, String amount, String message) {
    }

    @Transactional(readOnly = true)
    public BalanceView balance(String acctIdIn) {
        long acctId = parseAcctId(acctIdIn);
        AccountRecord account = accounts.findAccount(acctId)
                .orElseThrow(() -> ApiException.notFound(PROGRAM, "Account ID NOT found..."));
        return new BalanceView(acctId, Money.format(account.currBal()));
    }

    @Transactional
    public PaymentResult pay(String acctIdIn) {
        long acctId = parseAcctId(acctIdIn);
        AccountRecord account = jdbc.sql("SELECT * FROM account WHERE acct_id = :id FOR UPDATE")
                .param("id", acctId)
                .query(AccountRepository.accountMapper())
                .optional()
                .orElseThrow(() -> ApiException.notFound(PROGRAM, "Account ID NOT found..."));
        BigDecimal balance = account.currBal() == null ? BigDecimal.ZERO : account.currBal();
        if (balance.signum() <= 0) {
            throw ApiException.businessRule(PROGRAM, "You have nothing to pay...");
        }
        String cardNum = accounts.findFirstXrefByAccount(acctId)
                .map(AccountRepository.XrefRow::cardNum)
                .orElseThrow(() -> new ApiException(ErrorCode.INTERNAL_ERROR, "Unable to lookup XREF AIX file...",
                        PROGRAM));

        String tranId = allocator.next();
        LocalDateTime now = LocalDateTime.now(clock).truncatedTo(ChronoUnit.MICROS);
        TransactionRecord payment = new TransactionRecord(tranId, "02", 2, "POS TERM", "BILL PAYMENT - ONLINE",
                balance, 999999999, "BILL PAYMENT", "N/A", "N/A", cardNum, now, now);
        try {
            transactions.insert(payment);
        } catch (DuplicateKeyException ex) {
            throw new ApiException(ErrorCode.DUPLICATE, "Tran ID already exist...", PROGRAM);
        }
        int updated = jdbc.sql("UPDATE account SET curr_bal = curr_bal - :amt, version = version + 1 "
                + "WHERE acct_id = :id")
                .param("amt", balance).param("id", acctId).update();
        if (updated != 1) {
            throw new ApiException(ErrorCode.INTERNAL_ERROR, "Unable to Update Account...", PROGRAM);
        }
        return new PaymentResult(tranId, Money.format(balance),
                "Payment successful. Your Transaction ID is " + tranId + ".");
    }

    private static long parseAcctId(String value) {
        if (Text.isBlank(value)) {
            throw ApiException.validation(PROGRAM, "acctId", "Acct ID can NOT be empty...");
        }
        return AccountService.parseAccountId(value, PROGRAM);
    }
}
