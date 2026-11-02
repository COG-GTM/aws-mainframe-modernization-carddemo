package com.carddemo.transaction.service;

import com.carddemo.common.error.BusinessRuleException;
import com.carddemo.common.error.NotFoundException;
import com.carddemo.transaction.api.dto.TransactionCreateRequest;
import com.carddemo.transaction.client.AccountGateway;
import com.carddemo.transaction.client.AccountView;
import com.carddemo.transaction.client.CardGateway;
import com.carddemo.transaction.client.CardXrefView;
import com.carddemo.transaction.client.PostingResult;
import com.carddemo.transaction.domain.Transaction;
import com.carddemo.transaction.repository.TransactionRepository;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDateTime;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** COTRN02C (add transaction) and COBIL00C (bill payment). */
@Service
public class TransactionCommandService {

    private static final String BILL_PAYMENT_TYPE_CD = "02";
    private static final int BILL_PAYMENT_CAT_CD = 2;

    private final TransactionRepository transactions;
    private final TransactionIdGenerator idGenerator;
    private final CardGateway cards;
    private final AccountGateway accounts;
    private final Clock clock;

    public TransactionCommandService(TransactionRepository transactions,
                                     TransactionIdGenerator idGenerator,
                                     CardGateway cards,
                                     AccountGateway accounts,
                                     Clock clock) {
        this.transactions = transactions;
        this.idGenerator = idGenerator;
        this.cards = cards;
        this.accounts = accounts;
        this.clock = clock;
    }

    /** COTRN02C: the card must resolve through CARDXREF before the record is written. */
    @Transactional
    public Transaction add(TransactionCreateRequest request) {
        cards.xrefByCard(request.cardNumber())
                .orElseThrow(() -> new NotFoundException("Card " + request.cardNumber() + " not found"));
        LocalDateTime now = LocalDateTime.now(clock);
        Transaction transaction = new Transaction(idGenerator.next(), request.cardNumber(),
                request.typeCode(), request.categoryCode(), request.amount());
        transaction.setSource(request.source());
        transaction.setDescription(request.description());
        transaction.setMerchantId(request.merchantId());
        transaction.setMerchantName(request.merchantName());
        transaction.setMerchantCity(request.merchantCity());
        transaction.setMerchantZip(request.merchantZip());
        transaction.setOrigTs(request.originTimestamp() == null ? now : request.originTimestamp());
        transaction.setProcTs(now);
        return transactions.save(transaction);
    }

    /**
     * COBIL00C. The COBOL writes a type 02 category 2 transaction for the full current balance
     * and then subtracts that amount from ACCT-CURR-BAL, so the posting is sent as a negative
     * amount to account-service.
     */
    @Transactional
    public Transaction payBill(long accountId) {
        AccountView account = accounts.find(accountId)
                .orElseThrow(() -> new NotFoundException("Account " + accountId + " not found"));
        BigDecimal balance = account.currentBalance();
        if (balance.signum() <= 0) {
            throw new BusinessRuleException("Account " + accountId + " has no balance to pay");
        }
        CardXrefView xref = cards.xrefByAccount(accountId)
                .orElseThrow(() -> new NotFoundException("No card cross reference for account " + accountId));

        PostingResult posting = accounts.post(accountId, balance.negate(),
                LocalDateTime.now(clock).toLocalDate());
        if (!posting.posted()) {
            throw new BusinessRuleException("Payment for account " + accountId + " rejected with reason "
                    + posting.reasonCode() + ": " + posting.reasonDescription());
        }

        LocalDateTime now = LocalDateTime.now(clock);
        Transaction transaction = new Transaction(idGenerator.next(), xref.cardNumber(),
                BILL_PAYMENT_TYPE_CD, BILL_PAYMENT_CAT_CD, balance);
        transaction.setSource("POS TERM");
        transaction.setDescription("BILL PAYMENT - ONLINE");
        transaction.setMerchantId(999999999L);
        transaction.setMerchantName("BILL PAYMENT");
        transaction.setMerchantCity("N/A");
        transaction.setMerchantZip("N/A");
        transaction.setOrigTs(now);
        transaction.setProcTs(now);
        return transactions.save(transaction);
    }
}
