package com.carddemo.transaction.batch;

import com.carddemo.transaction.client.AccountGateway;
import com.carddemo.transaction.client.CardGateway;
import com.carddemo.transaction.client.CardXrefView;
import com.carddemo.transaction.client.PostingResult;
import com.carddemo.transaction.domain.CategoryBalance;
import com.carddemo.transaction.domain.CategoryBalanceId;
import com.carddemo.transaction.domain.DailyTransaction;
import com.carddemo.transaction.domain.Transaction;
import com.carddemo.transaction.domain.TransactionReject;
import com.carddemo.transaction.repository.CategoryBalanceRepository;
import com.carddemo.transaction.repository.DailyTransactionRepository;
import com.carddemo.transaction.repository.TransactionRejectRepository;
import com.carddemo.transaction.repository.TransactionRepository;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.Optional;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Port of CBTRN02C (JCL POSTTRAN). One daily transaction is validated, then either posted or
 * written to the reject table with the original reason code:
 *
 * <ul>
 *   <li>100 INVALID CARD NUMBER FOUND</li>
 *   <li>101 ACCOUNT RECORD NOT FOUND</li>
 *   <li>102 OVERLIMIT TRANSACTION</li>
 *   <li>103 TRANSACTION RECEIVED AFTER ACCT EXPIRATION</li>
 * </ul>
 *
 * Reasons 101 to 103 are decided by account-service, which owns the balance; reason 100 is the
 * cross reference lookup done here.
 */
@Service
public class PostingService {

    private final DailyTransactionRepository dailyTransactions;
    private final TransactionRepository transactions;
    private final TransactionRejectRepository rejects;
    private final CategoryBalanceRepository categoryBalances;
    private final CardGateway cards;
    private final AccountGateway accounts;
    private final Clock clock;

    public PostingService(DailyTransactionRepository dailyTransactions,
                          TransactionRepository transactions,
                          TransactionRejectRepository rejects,
                          CategoryBalanceRepository categoryBalances,
                          CardGateway cards,
                          AccountGateway accounts,
                          Clock clock) {
        this.dailyTransactions = dailyTransactions;
        this.transactions = transactions;
        this.rejects = rejects;
        this.categoryBalances = categoryBalances;
        this.cards = cards;
        this.accounts = accounts;
        this.clock = clock;
    }

    @Transactional
    public PostingReport postPendingTransactions() {
        int posted = 0;
        int rejected = 0;
        for (DailyTransaction daily : dailyTransactions.findByProcessedFalseOrderByTranId()) {
            if (postOne(daily)) {
                posted++;
            } else {
                rejected++;
            }
        }
        return new PostingReport(posted, rejected);
    }

    @Transactional
    public boolean postOne(DailyTransaction daily) {
        Optional<CardXrefView> xref = cards.xrefByCard(daily.getCardNum());
        if (xref.isEmpty()) {
            return reject(daily, 100, "INVALID CARD NUMBER FOUND");
        }

        PostingResult result = accounts.post(xref.get().accountId(), daily.getAmount(),
                daily.getOrigTs().toLocalDate());
        if (result == null) {
            return reject(daily, 101, "ACCOUNT RECORD NOT FOUND");
        }
        if (!result.posted()) {
            return reject(daily, result.reasonCode(), result.reasonDescription());
        }

        updateCategoryBalance(xref.get().accountId(), daily);
        writeTransaction(daily);
        daily.setProcessed(true);
        daily.setProcTs(LocalDateTime.now(clock));
        dailyTransactions.save(daily);
        return true;
    }

    /** Paragraph 2700-UPDATE-TCATBAL-REC: create the category balance row when it is missing. */
    private void updateCategoryBalance(Long accountId, DailyTransaction daily) {
        CategoryBalanceId id = new CategoryBalanceId(accountId, daily.getTypeCd(), daily.getCatCd());
        CategoryBalance balance = categoryBalances.findById(id)
                .orElseGet(() -> new CategoryBalance(id, BigDecimal.ZERO));
        balance.add(daily.getAmount());
        categoryBalances.save(balance);
    }

    /** Paragraph 2600-WRITE-TRANSACTION-FILE. */
    private void writeTransaction(DailyTransaction daily) {
        Transaction transaction = new Transaction(daily.getTranId(), daily.getCardNum(),
                daily.getTypeCd(), daily.getCatCd(), daily.getAmount());
        transaction.setSource(daily.getSource());
        transaction.setDescription(daily.getDescription());
        transaction.setMerchantId(daily.getMerchantId());
        transaction.setMerchantName(daily.getMerchantName());
        transaction.setMerchantCity(daily.getMerchantCity());
        transaction.setMerchantZip(daily.getMerchantZip());
        transaction.setOrigTs(daily.getOrigTs());
        transaction.setProcTs(LocalDateTime.now(clock));
        transactions.save(transaction);
    }

    /** Paragraph 2500-WRITE-REJECT-REC. */
    private boolean reject(DailyTransaction daily, int reasonCode, String reasonDescription) {
        rejects.save(new TransactionReject(daily.getTranId(), daily.getCardNum(), reasonCode,
                reasonDescription));
        daily.setProcessed(true);
        daily.setProcTs(LocalDateTime.now(clock));
        dailyTransactions.save(daily);
        return false;
    }

    public record PostingReport(int posted, int rejected) {
    }
}
