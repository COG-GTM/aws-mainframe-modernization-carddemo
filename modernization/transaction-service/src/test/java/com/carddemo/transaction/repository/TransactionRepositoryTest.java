package com.carddemo.transaction.repository;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.transaction.domain.DailyTransaction;
import com.carddemo.transaction.domain.Transaction;
import com.carddemo.transaction.domain.TransactionReject;
import com.carddemo.transaction.support.StubGateways;
import jakarta.persistence.EntityManager;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.PageRequest;
import org.springframework.transaction.annotation.Transactional;

@SpringBootTest
@Import(StubGateways.class)
@Transactional
class TransactionRepositoryTest {

    private static final LocalDateTime DAY = LocalDateTime.of(2022, 6, 10, 0, 0);

    @Autowired
    private TransactionRepository transactions;

    @Autowired
    private TransactionRejectRepository rejects;

    @Autowired
    private EntityManager entityManager;

    @BeforeEach
    void setUp() {
        transactions.deleteAll();
        save("0000000000000001", "4111111111111111", DAY.plusHours(1));
        save("0000000000000002", "4000000000000002", DAY.plusHours(2));
        save("0000000000000003", "4111111111111111", DAY.plusDays(1));
        save("0000000000000004", "4000000000000002", null);
    }

    private void save(String tranId, String cardNum, LocalDateTime procTs) {
        Transaction tran = new Transaction(tranId, cardNum, "01", 1, new BigDecimal("10.00"));
        tran.setProcTs(procTs);
        transactions.save(tran);
    }

    @Test
    void readsHighestTranIdLikeStartbrHighValuesReadprev() {
        assertThat(transactions.findTopByOrderByTranIdDesc()).get()
                .extracting(Transaction::getTranId).isEqualTo("0000000000000004");
    }

    @Test
    void browsesByTranIdInBothDirections() {
        assertThat(transactions.findByTranIdGreaterThanEqualOrderByTranIdAsc("0000000000000002", PageRequest.ofSize(2)))
                .extracting(Transaction::getTranId).containsExactly("0000000000000002", "0000000000000003");
        assertThat(transactions.findByTranIdLessThanOrderByTranIdDesc("0000000000000003", PageRequest.ofSize(10)))
                .extracting(Transaction::getTranId).containsExactly("0000000000000002", "0000000000000001");
    }

    @Test
    void selectsByProcessingDateSortedByCardNumber() {
        assertThat(transactions.findByProcTsGreaterThanEqualAndProcTsLessThanOrderByCardNumAscTranIdAsc(
                DAY, DAY.plusDays(1)))
                .extracting(Transaction::getTranId).containsExactly("0000000000000002", "0000000000000001");
    }

    @Test
    void rejectKeepsTheWholeDailyTransactionRecord() {
        DailyTransaction daily = new DailyTransaction("0000000000683580", "4859452612877065", "01", 1,
                new BigDecimal("-12.34"), LocalDateTime.of(2022, 6, 10, 19, 27, 53));
        daily.setSource("POS TERM");
        daily.setDescription("Purchase at Abshire-Lowe");
        daily.setMerchantId(800000000L);
        daily.setMerchantName("Abshire-Lowe");
        daily.setMerchantCity("North Enrique");
        daily.setMerchantZip("72112");
        daily.setProcTs(DAY);

        Long id = rejects.saveAndFlush(TransactionReject.of(daily, 102, "OVERLIMIT TRANSACTION")).getRejectId();
        entityManager.clear();

        TransactionReject stored = rejects.findById(id).orElseThrow();
        assertThat(stored).usingRecursiveComparison()
                .comparingOnlyFields("tranId", "typeCd", "catCd", "source", "description", "amount", "merchantId",
                        "merchantName", "merchantCity", "merchantZip", "cardNum", "origTs", "procTs")
                .isEqualTo(daily);
        assertThat(stored.getReasonCode()).isEqualTo(102);
        assertThat(stored.getReasonDesc()).isEqualTo("OVERLIMIT TRANSACTION");
    }
}
