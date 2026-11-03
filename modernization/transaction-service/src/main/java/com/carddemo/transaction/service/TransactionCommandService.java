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
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.ResourceAccessException;

/** COTRN02C (add transaction) and COBIL00C (bill payment). */
@Service
public class TransactionCommandService {

    private static final String BILL_PAYMENT_TYPE_CD = "02";
    private static final int BILL_PAYMENT_CAT_CD = 2;
    static final int POSTING_ATTEMPTS = 3;

    private final TransactionRepository transactions;
    private final TransactionIdGenerator idGenerator;
    private final TransactionAddValidator validator;
    private final CardGateway cards;
    private final AccountGateway accounts;
    private final JdbcTemplate jdbc;
    private final Clock clock;

    public TransactionCommandService(TransactionRepository transactions,
                                     TransactionIdGenerator idGenerator,
                                     TransactionAddValidator validator,
                                     CardGateway cards,
                                     AccountGateway accounts,
                                     JdbcTemplate jdbc,
                                     Clock clock) {
        this.transactions = transactions;
        this.idGenerator = idGenerator;
        this.validator = validator;
        this.cards = cards;
        this.accounts = accounts;
        this.jdbc = jdbc;
        this.clock = clock;
    }

    /**
     * COTRN02C PROCESS-ENTER-KEY with {@code CONFIRMI = 'Y'}: field edits, then the card through
     * CXACAIX (account id given) or CCXREF (card number given), then ADD-TRANSACTION. The typed dates
     * go into TRAN-ORIG-TS / TRAN-PROC-TS as the COBOL moves them (date only, time 00:00).
     */
    @Transactional
    public Transaction add(TransactionCreateRequest request) {
        validator.validate(request);
        String cardNumber;
        if (request.accountId() != null && !request.accountId().isBlank()) {
            cardNumber = cards.xrefByAccount(TransactionAddValidator.accountId(request))
                    .map(CardXrefView::cardNumber)
                    .orElseThrow(() -> new NotFoundException("Account ID NOT found..."));
        } else {
            cardNumber = TransactionAddValidator.cardNumber(request);
            cards.xrefByCard(cardNumber)
                    .orElseThrow(() -> new NotFoundException("Card Number NOT found..."));
        }
        Transaction transaction = new Transaction(idGenerator.next(), cardNumber,
                TransactionAddValidator.typeCode(request), TransactionAddValidator.categoryCode(request),
                TransactionAddValidator.amount(request));
        transaction.setSource(request.source().trim());
        transaction.setDescription(request.description().trim());
        transaction.setMerchantId(TransactionAddValidator.merchantId(request));
        transaction.setMerchantName(request.merchantName().trim());
        transaction.setMerchantCity(request.merchantCity().trim());
        transaction.setMerchantZip(request.merchantZip().trim());
        transaction.setOrigTs(TransactionAddValidator.date(request.originDate()).atStartOfDay());
        transaction.setProcTs(TransactionAddValidator.date(request.processDate()).atStartOfDay());
        return transactions.save(transaction);
    }

    /**
     * COBIL00C with {@code CONFIRMI = 'Y'}: pay the whole ACCT-CURR-BAL as a type 02 / category 2
     * transaction, then lower the balance by that amount.
     *
     * <p>The COBOL holds ACCTDAT with READ UPDATE from the balance read to the REWRITE. Here the
     * account lives in account-service, so: a transaction-scoped advisory lock on the account id
     * keeps two payments of the same account from both paying the same balance; the TRANSACT row is
     * written (and flushed, so a row the table refuses never debits the account) before the posting,
     * in the COBOL order WRITE-TRANSACT-FILE then UPDATE-ACCTDAT-FILE; the posting carries the new
     * TRAN-ID as its idempotency key, so retrying it after a lost response never debits twice; a
     * rejected or failed posting rolls the row back. The posting is account-service's COBIL00C type:
     * balance only, no CBTRN02C over-limit or expiration reject.
     */
    @Transactional
    public Transaction payBill(long accountId) {
        jdbc.queryForObject("SELECT pg_advisory_xact_lock(?)", Object.class, accountId);
        AccountView account = accounts.find(accountId)
                .orElseThrow(() -> new NotFoundException("Account ID NOT found..."));
        BigDecimal balance = account.currentBalance();
        if (balance.signum() <= 0) {
            throw new BusinessRuleException("You have nothing to pay...");
        }
        CardXrefView xref = cards.xrefByAccount(accountId)
                .orElseThrow(() -> new NotFoundException("Account ID NOT found..."));

        // GET-CURRENT-TIMESTAMP: date and time to the second, microseconds zero.
        LocalDateTime now = LocalDateTime.now(clock).truncatedTo(ChronoUnit.SECONDS);
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
        Transaction saved = transactions.saveAndFlush(transaction);

        PostingResult posting = postIdempotently(accountId, saved.getTranId(), balance.negate(), now.toLocalDate());
        if (!posting.posted()) {
            throw new BusinessRuleException("Payment for account " + accountId + " rejected with reason "
                    + posting.reasonCode() + ": " + posting.reasonDescription());
        }
        return saved;
    }

    /** Retries only when the outcome is unknown (no answer, or a 5xx); the same id makes a retry safe. */
    private PostingResult postIdempotently(long accountId, String tranId, BigDecimal amount, LocalDate date) {
        RuntimeException last = null;
        for (int attempt = 1; attempt <= POSTING_ATTEMPTS; attempt++) {
            try {
                PostingResult result = accounts.postBillPayment(accountId, tranId, amount, date);
                if (result == null) {
                    throw new NotFoundException("Account ID NOT found...");
                }
                return result;
            } catch (ResourceAccessException | HttpServerErrorException unknownOutcome) {
                last = unknownOutcome;
            }
        }
        throw last;
    }
}
