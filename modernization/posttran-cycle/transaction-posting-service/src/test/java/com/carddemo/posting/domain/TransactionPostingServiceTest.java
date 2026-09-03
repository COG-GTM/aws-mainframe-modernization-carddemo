package com.carddemo.posting.domain;

import com.carddemo.posting.Fixtures;
import com.carddemo.recordio.codec.FixedWidthRecord;
import com.carddemo.recordio.layout.Account;
import com.carddemo.recordio.layout.TransactionCategoryBalance;
import com.carddemo.recordio.layout.TransactionLayout;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;

import static com.carddemo.posting.Fixtures.*;
import static org.assertj.core.api.Assertions.assertThat;

/** CBTRN02C 2000-POST-TRANSACTION, 2700-UPDATE-TCATBAL, 2800-UPDATE-ACCOUNT-REC (lines 424-559). */
class TransactionPostingServiceTest {

    private static final Clock FIXED = Clock.fixed(Instant.parse("2022-07-18T13:45:12.340Z"), ZoneOffset.UTC);
    private PostingLedger ledger;
    private TransactionPostingService service;

    @BeforeEach
    void setUp() {
        ledger = new PostingLedger(Fixtures.xref(), Fixtures.accounts(), Fixtures.balances());
        service = new TransactionPostingService(ledger, FIXED);
    }

    @Test
    void postedTransactionCarriesDb2StyleProcessingTimestamp() {
        PostingOutcome out = service.process(daily("T1", CARD_OK, "10.00", "2022-06-10"));
        assertThat(out).isInstanceOf(PostingOutcome.Posted.class);
        assertThat(out.transaction().processingTimestamp()).isEqualTo("2022-07-18-13.45.12.340000");
        assertThat(out.transaction().processingTimestamp()).hasSize(26);
    }

    @Test
    void existingCategoryBalanceIsIncremented() {
        service.process(daily("T1", CARD_OK, "10.50", "2022-06-10"));
        TransactionCategoryBalance b = ledger.categoryBalances().read(ACCT_OK + "010001").orElseThrow();
        assertThat(b.balance()).isEqualByComparingTo("110.50");
        assertThat(ledger.categoryBalances().size()).isEqualTo(1);
    }

    @Test
    void missingCategoryBalanceIsCreatedWithTheAmount() {
        service.process(daily("T1", CARD_OK, "01", 2, "-7.25", "2022-06-10"));
        TransactionCategoryBalance b = ledger.categoryBalances().read(ACCT_OK + "010002").orElseThrow();
        assertThat(b.balance()).isEqualByComparingTo("-7.25");
        assertThat(ledger.categoryBalances().size()).isEqualTo(2);
    }

    @Test
    void positiveAmountGoesToCycleCredit_negativeIsAddedToCycleDebitWithoutNegation() {
        service.process(daily("T1", CARD_OK, "10.00", "2022-06-10"));
        Account a = ledger.findAccount(ACCT_OK).orElseThrow();
        assertThat(a.currentBalance()).isEqualByComparingTo("260.00");
        assertThat(a.currentCycleCredit()).isEqualByComparingTo("610.00");
        assertThat(a.currentCycleDebit()).isEqualByComparingTo("100.00");

        service.process(daily("T2", CARD_OK, "-30.00", "2022-06-10"));
        a = ledger.findAccount(ACCT_OK).orElseThrow();
        assertThat(a.currentBalance()).isEqualByComparingTo("230.00");
        assertThat(a.currentCycleCredit()).isEqualByComparingTo("610.00");
        assertThat(a.currentCycleDebit()).isEqualByComparingTo("70.00");   // 100 + (-30), as the COBOL ADDs it
    }

    @Test
    void laterTransactionSeesEarlierOnesCycleTotals() {
        assertThat(service.process(daily("T1", CARD_OK, "400.00", "2022-06-10"))).isInstanceOf(PostingOutcome.Posted.class);
        // headroom was 500, now 100 -> 100.01 is over limit
        PostingOutcome second = service.process(daily("T2", CARD_OK, "100.01", "2022-06-10"));
        assertThat(second).isInstanceOf(PostingOutcome.Rejected.class);
        assertThat(((PostingOutcome.Rejected) second).reason()).isEqualTo(RejectReason.OVER_LIMIT);
    }

    @Test
    void rejectDoesNotTouchAnyMaster() {
        service.process(daily("T1", CARD_UNKNOWN, "10.00", "2022-06-10"));
        assertThat(ledger.findAccount(ACCT_OK).orElseThrow().currentBalance()).isEqualByComparingTo("250.00");
        assertThat(ledger.categoryBalances().size()).isEqualTo(1);
    }

    @Test
    void rewrittenAccountKeepsUntouchedBytesAndLength() {
        service.process(daily("T1", CARD_OK, "10.00", "2022-06-10"));
        FixedWidthRecord image = ledger.accounts().images().get(0);
        assertThat(image.length()).isEqualTo(Account.LENGTH);
        assertThat(image.text(112, 10)).isEqualTo("DEFAULT   ");
    }

    @Test
    void rejectRecordIs430BytesWithCodeAndDescriptionTrailer() {
        var daily = daily("T1", CARD_UNKNOWN, "10.00", "2022-06-10");
        var rejected = (PostingOutcome.Rejected) service.process(daily);
        FixedWidthRecord r = RejectRecordLayout.encode(rejected, ENC);
        assertThat(r.length()).isEqualTo(430);
        assertThat(r.bytes()).startsWith(TransactionLayout.INSTANCE.encode(daily, ENC).bytes());
        assertThat(r.text(350, 4)).isEqualTo("0100");
        assertThat(r.trimmedText(354, 76)).isEqualTo("INVALID CARD NUMBER FOUND");
    }

    @Test
    void amountsAreStoredWithCobolTruncationNotRounding() {
        // account balance field is S9(10)V99; a 3-decimal amount cannot occur from a 350-byte record,
        // so the truncation rule is exercised through the codec directly
        assertThat(com.carddemo.recordio.codec.CobolNumeric.truncate(new BigDecimal("1.999"), 10, 2))
                .isEqualByComparingTo("1.99");
    }
}
