package com.carddemo.transaction.client;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Optional;

/** Account domain seen from the transaction domain. */
public interface AccountGateway {

    Optional<AccountView> find(long accountId);

    /**
     * Account side of CBTRN02C: balance and cycle update, or a reject reason code. Idempotent per
     * {@code transactionId}: calling again with the same id after the update committed returns the
     * first result and does not post the amount twice.
     */
    PostingResult post(long accountId, String transactionId, BigDecimal amount, LocalDate transactionDate);

    /**
     * Account side of COBIL00C (UPDATE-ACCTDAT-FILE): {@code amount} is added to ACCT-CURR-BAL only,
     * without the CBTRN02C limit and expiration checks. Same endpoint and idempotency key as {@link #post}.
     */
    PostingResult postBillPayment(long accountId, String transactionId, BigDecimal amount, LocalDate paymentDate);

    /** Account break of CBACT04C. */
    void settleInterest(long accountId, BigDecimal totalInterest);
}
