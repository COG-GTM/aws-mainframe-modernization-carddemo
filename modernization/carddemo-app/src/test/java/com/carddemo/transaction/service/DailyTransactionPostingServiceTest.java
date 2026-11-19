package com.carddemo.transaction.service;

import com.carddemo.account.domain.Account;
import com.carddemo.account.repository.AccountRepository;
import com.carddemo.app.CardDemoApplication;
import com.carddemo.app.PostgresContainerSupport;
import com.carddemo.app.data.KsdsDataService;
import com.carddemo.app.data.KsdsDataService.Ksds;
import com.carddemo.app.golden.GoldenCase;
import com.carddemo.common.batch.SysoutWriter;
import com.carddemo.reference.domain.TranCatBalanceId;
import com.carddemo.reference.repository.TranCatBalanceRepository;
import com.carddemo.transaction.domain.Transaction;
import com.carddemo.transaction.repository.TransactionRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;

import static com.carddemo.transaction.service.DailyTransactions.CARD_OF_ACCOUNT_1;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * {@code CBTRN02C 2000-POST-TRANSACTION} on PostgreSQL 16 with the sample masters (account 1: CURR-BAL 194.00, CYC
 * CREDIT/DEBIT 0.00; TCATBAL 00000000001/01/0001 = 0.00).
 */
@SpringBootTest(classes = CardDemoApplication.class)
class DailyTransactionPostingServiceTest extends PostgresContainerSupport {

    static final String PROC_TS = "2022-07-18-10.30.45.120000";

    @Autowired
    DailyTransactionPostingService posting;
    @Autowired
    KsdsDataService data;
    @Autowired
    AccountRepository accounts;
    @Autowired
    TranCatBalanceRepository balances;
    @Autowired
    TransactionRepository transactions;

    ByteArrayOutputStream out;
    SysoutWriter sysout;

    @BeforeEach
    void seed() {
        data.deleteAll();
        data.loadSampleData(GoldenCase.SAMPLE_DATA);
        out = new ByteArrayOutputStream();
        sysout = new SysoutWriter(null, new PrintStream(out, true, StandardCharsets.ISO_8859_1));
    }

    List<String> sysoutLines() {
        String text = out.toString(StandardCharsets.ISO_8859_1);
        return text.isEmpty() ? List.of() : Arrays.asList(text.split("\\R"));
    }

    BigDecimal tcatbal(String type, int cat) {
        return balances.findById(new TranCatBalanceId(1L, type, cat)).orElseThrow().getBalance();
    }

    @Test
    void PR_CBTRN02C_09_transactionIsTheDailyRecordWithTheProcessingTimestamp() {
        var daily = DailyTransactions.of("TID-0000000001", CARD_OF_ACCOUNT_1, "12.34");
        posting.post(daily, 1L, PROC_TS, sysout);
        Transaction t = transactions.findById("TID-0000000001").orElseThrow();
        assertThat(t.getProcTs()).isEqualTo(PROC_TS);
        assertThat(t.getAmount()).isEqualTo(new BigDecimal("12.34"));
        assertThat(t.getCardNum()).isEqualTo(CARD_OF_ACCOUNT_1);
        assertThat(t.getOrigTs()).isEqualTo(daily.origTs().stripTrailing());
        assertThat(t.getTypeCd()).isEqualTo(daily.typeCd().stripTrailing());
        assertThat(t.getCatCd()).isEqualTo(daily.catCd());
        assertThat(t.getMerchantName()).isEqualTo(daily.merchantName().stripTrailing());
        // the unloaded record is the DALYTRAN record byte for byte except TRAN-PROC-TS (cbl:424-438)
        String unloaded = data.unload(Ksds.TRANSACT).get(0);
        assertThat(unloaded.substring(0, 304)).isEqualTo(daily.record().substring(0, 304));
        assertThat(unloaded.substring(304, 330)).isEqualTo(PROC_TS);
    }

    @Test
    void PR_CBTRN02C_10_existingCategoryBalanceIsIncreasedWithoutSysoutLine() {
        var daily = DailyTransactions.of("TID-1", CARD_OF_ACCOUNT_1, "50.25");
        assertThat(daily.typeCd()).isEqualTo("01");
        assertThat(daily.catCd()).isEqualTo(1);
        posting.post(daily, 1L, PROC_TS, sysout);
        posting.post(DailyTransactions.of("TID-2", CARD_OF_ACCOUNT_1, "-10.00"), 1L, PROC_TS, sysout);
        assertThat(tcatbal("01", 1)).isEqualTo(new BigDecimal("40.25"));
        assertThat(sysoutLines()).isEmpty();
    }

    @Test
    void PR_CBTRN02C_10_missingCategoryBalanceIsCreatedWithTheAmountAndAnnounced() {
        var daily = new com.carddemo.transaction.domain.DailyTransaction(com.carddemo.common.record.Copybooks.CVTRA06Y
                .parse(DailyTransactions.of("TID-1", CARD_OF_ACCOUNT_1, "7.50").record()).set("DALYTRAN-CAT-CD", 99).toString());
        assertThat(balances.findById(new TranCatBalanceId(1L, "01", 99))).isEmpty();
        posting.post(daily, 1L, PROC_TS, sysout);
        assertThat(tcatbal("01", 99)).isEqualTo(new BigDecimal("7.50"));
        assertThat(sysoutLines()).containsExactly("TCATBAL record not found for key : 00000000001010099.. Creating.");
    }

    @Test
    void PR_CBTRN02C_11_amountGoesToCurrentBalanceAndToCycleCreditOrDebitBySign() {
        posting.post(DailyTransactions.of("TID-1", CARD_OF_ACCOUNT_1, "100.00"), 1L, PROC_TS, sysout);
        posting.post(DailyTransactions.of("TID-2", CARD_OF_ACCOUNT_1, "0.00"), 1L, PROC_TS, sysout);
        posting.post(DailyTransactions.of("TID-3", CARD_OF_ACCOUNT_1, "-30.05"), 1L, PROC_TS, sysout);
        Account a = accounts.findById(1L).orElseThrow();
        assertThat(a.getCurrBal()).isEqualTo(new BigDecimal("263.95"));
        assertThat(a.getCurrCycCredit()).isEqualTo(new BigDecimal("100.00"));
        assertThat(a.getCurrCycDebit()).isEqualTo(new BigDecimal("-30.05"));
    }

    @Test
    void PR_CBTRN02C_11_accountMissingOnRewriteIsReason109LoggedAndTheRecordStillPosted() {
        // XREF says account 99999 but there is no such account: TCATBAL is created, TRANSACT written, no abend (D-027)
        posting.post(DailyTransactions.of("TID-109", CARD_OF_ACCOUNT_1, "1.00"), 99999L, PROC_TS, sysout);
        assertThat(transactions.findById("TID-109")).isPresent();
        assertThat(balances.findById(new TranCatBalanceId(99999L, "01", 1)).orElseThrow().getBalance())
                .isEqualTo(new BigDecimal("1.00"));
        assertThat(accounts.findById(99999L)).isEmpty();
    }

    @Test
    void PR_CBTRN02C_12_PR_CBTRN02C_13_duplicateTransactionIdAbendsAndRollsBackTheWholeRecord() {
        posting.post(DailyTransactions.of("TID-DUP", CARD_OF_ACCOUNT_1, "10.00"), 1L, PROC_TS, sysout);
        assertThatThrownBy(() -> posting.post(DailyTransactions.of("TID-DUP", CARD_OF_ACCOUNT_1, "20.00"), 1L, PROC_TS, sysout))
                .isInstanceOf(PosttranAbendException.class).hasMessageContaining("FILE STATUS 22");
        assertThat(sysoutLines()).containsExactly("ERROR WRITING TO TRANSACTION FILE", "FILE STATUS IS: NNNN0022",
                "ABENDING PROGRAM");
        // TCATBAL and ACCOUNT were updated before the failed WRITE in COBOL (Q-04); here the record is atomic (D-026)
        assertThat(tcatbal("01", 1)).isEqualTo(new BigDecimal("10.00"));
        assertThat(accounts.findById(1L).orElseThrow().getCurrBal()).isEqualTo(new BigDecimal("204.00"));
        assertThat(transactions.findById("TID-DUP").orElseThrow().getAmount()).isEqualTo(new BigDecimal("10.00"));
    }

    @Test
    void cardNumbersAreMaskedInTechnicalLogs() {
        assertThat(DailyTransactionPostingService.maskCard("9680294154603697")).isEqualTo("************3697");
    }
}
