package com.carddemo.transaction.client;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Optional;

/** Account domain seen from the transaction domain. */
public interface AccountGateway {

    Optional<AccountView> find(long accountId);

    /** Account side of CBTRN02C: balance and cycle update, or a reject reason code. */
    PostingResult post(long accountId, BigDecimal amount, LocalDate transactionDate);

    /** Account break of CBACT04C. */
    void settleInterest(long accountId, BigDecimal totalInterest);
}
