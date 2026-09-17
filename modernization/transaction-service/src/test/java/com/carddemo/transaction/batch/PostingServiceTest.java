package com.carddemo.transaction.batch;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.transaction.client.PostingResult;
import com.carddemo.transaction.domain.CategoryBalanceId;
import com.carddemo.transaction.domain.DailyTransaction;
import com.carddemo.transaction.repository.CategoryBalanceRepository;
import com.carddemo.transaction.repository.DailyTransactionRepository;
import com.carddemo.transaction.repository.TransactionRejectRepository;
import com.carddemo.transaction.repository.TransactionRepository;
import com.carddemo.transaction.support.StubGateways;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;

/** Parity tests for the CBTRN02C port. */
@SpringBootTest
@Import(StubGateways.class)
class PostingServiceTest {

    private static final String CARD = "4111111111111111";
    private static final long ACCOUNT = 11111111111L;

    @Autowired
    private PostingService postingService;

    @Autowired
    private DailyTransactionRepository dailyTransactions;

    @Autowired
    private TransactionRepository transactions;

    @Autowired
    private TransactionRejectRepository rejects;

    @Autowired
    private CategoryBalanceRepository categoryBalances;

    @Autowired
    private StubGateways.StubCardGateway cards;

    @Autowired
    private StubGateways.StubAccountGateway accounts;

    @BeforeEach
    void setUp() {
        transactions.deleteAll();
        dailyTransactions.deleteAll();
        rejects.deleteAll();
        categoryBalances.deleteAll();
        cards.clear();
        accounts.clear();
        cards.register(CARD, 100000001L, ACCOUNT);
        accounts.register(ACCOUNT, "DEFAULT", new BigDecimal("100.00"));
    }

    private DailyTransaction daily(String tranId, String cardNumber, String amount) {
        DailyTransaction transaction = new DailyTransaction(tranId, cardNumber, "01", 1,
                new BigDecimal(amount), LocalDateTime.of(2024, 5, 1, 10, 0));
        transaction.setDescription("POS PURCHASE");
        return dailyTransactions.save(transaction);
    }

    @Test
    void postsTheTransactionAndUpdatesTheCategoryBalance() {
        daily("0000000000000001", CARD, "25.00");

        PostingService.PostingReport report = postingService.postPendingTransactions();

        assertThat(report.posted()).isEqualTo(1);
        assertThat(report.rejected()).isZero();
        assertThat(transactions.findById("0000000000000001")).isPresent();
        assertThat(categoryBalances.findById(new CategoryBalanceId(ACCOUNT, "01", 1)).orElseThrow()
                .getBalance()).isEqualByComparingTo("25.00");
        assertThat(dailyTransactions.findById("0000000000000001").orElseThrow().isProcessed()).isTrue();
    }

    @Test
    void rejectsAnUnknownCardWithReason100() {
        daily("0000000000000002", "4999999999999999", "25.00");

        PostingService.PostingReport report = postingService.postPendingTransactions();

        assertThat(report.rejected()).isEqualTo(1);
        assertThat(rejects.findAll()).singleElement()
                .satisfies(reject -> {
                    assertThat(reject.getReasonCode()).isEqualTo(100);
                    assertThat(reject.getReasonDesc()).isEqualTo("INVALID CARD NUMBER FOUND");
                });
        assertThat(transactions.count()).isZero();
    }

    @Test
    void propagatesTheAccountRejectReasonCode() {
        accounts.forceResult(ACCOUNT, new PostingResult(false, 102, "OVERLIMIT TRANSACTION",
                null, null, null));
        daily("0000000000000003", CARD, "9999.00");

        postingService.postPendingTransactions();

        assertThat(rejects.findAll()).singleElement()
                .satisfies(reject -> assertThat(reject.getReasonCode()).isEqualTo(102));
        assertThat(categoryBalances.count()).isZero();
    }

    @Test
    void addsNegativeAmountsToTheCategoryBalanceLikeTheCobol() {
        daily("0000000000000004", CARD, "25.00");
        postingService.postPendingTransactions();
        daily("0000000000000005", CARD, "-10.00");

        postingService.postPendingTransactions();

        assertThat(categoryBalances.findById(new CategoryBalanceId(ACCOUNT, "01", 1)).orElseThrow()
                .getBalance()).isEqualByComparingTo("15.00");
    }
}
