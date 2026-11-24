package com.carddemo.transaction.io;

import com.carddemo.transaction.domain.Account;
import com.carddemo.transaction.domain.Card;
import com.carddemo.transaction.domain.CardXref;
import com.carddemo.transaction.domain.Customer;
import com.carddemo.transaction.domain.DailyTransaction;
import com.carddemo.transaction.domain.Transaction;
import com.carddemo.transaction.domain.TransactionCategoryBalance;
import com.carddemo.transaction.support.TestData;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.time.LocalDateTime;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Field positions verified against the first record of each app/data/ASCII extract.
 */
class RecordLayoutsTest {

    private static String first(String file) throws Exception {
        return Files.readAllLines(TestData.SAMPLE_DIR.resolve(file), StandardCharsets.US_ASCII).get(0);
    }

    @Test
    void dalytranRecordFollowsCvtra06yPositions() throws Exception {
        DailyTransaction t = RecordLayouts.dailyTransaction(first("dailytran.txt"));
        assertThat(t.id()).isEqualTo("0000000000683580");
        assertThat(t.typeCd()).isEqualTo("01");
        assertThat(t.catCd()).isEqualTo(1);
        assertThat(t.source()).isEqualTo("POS TERM  ");
        assertThat(t.desc()).hasSize(100).startsWith("Purchase at Abshire-Lowe");
        assertThat(t.amt()).isEqualByComparingTo("504.77");
        assertThat(t.merchantId()).isEqualTo(800000000L);
        assertThat(t.merchantName()).hasSize(50).startsWith("Abshire-Lowe");
        assertThat(t.merchantCity().stripTrailing()).isEqualTo("North Enoshaven");
        assertThat(t.merchantZip()).isEqualTo("72112     ");
        assertThat(t.cardNum()).isEqualTo("4859452612877065");
        assertThat(t.origTs()).isEqualTo("2022-06-10 19:27:53.000000");
        assertThat(t.procTs()).isBlank();
        assertThat(t.rawRecord()).hasSize(DailyTransaction.RECORD_LENGTH);
    }

    @Test
    void everySampleDalytranRecordParses() throws Exception {
        List<String> lines = Files.readAllLines(TestData.SAMPLE_DALYTRAN, StandardCharsets.US_ASCII);
        assertThat(lines).hasSize(300);
        assertThat(lines.stream().map(RecordLayouts::dailyTransaction)).hasSize(300)
                .allSatisfy(t -> assertThat(t.id()).hasSize(16));
    }

    @Test
    void accountRecordFollowsCvact01yPositions() throws Exception {
        Account a = RecordLayouts.account(first("acctdata.txt"));
        assertThat(a.getAcctId()).isEqualTo(1L);
        assertThat(a.getActiveStatus()).isEqualTo("Y");
        assertThat(a.getCurrBal()).isEqualByComparingTo("194.00");
        assertThat(a.getCreditLimit()).isEqualByComparingTo("2020.00");
        assertThat(a.getCashCreditLimit()).isEqualByComparingTo("1020.00");
        assertThat(a.getOpenDate()).isEqualTo("2014-11-20");
        assertThat(a.getExpirationDate()).isEqualTo("2025-05-20");
        assertThat(a.getReissueDate()).isEqualTo("2025-05-20");
        assertThat(a.getCurrCycCredit()).isEqualByComparingTo("0");
        assertThat(a.getCurrCycDebit()).isEqualByComparingTo("0");
        assertThat(a.getCurrBal().scale()).isEqualTo(2);
    }

    @Test
    void cardXrefRecordFollowsCvact03yPositions() throws Exception {
        CardXref x = RecordLayouts.cardXref(first("cardxref.txt"));
        assertThat(x.getCardNum()).isEqualTo("0500024453765740");
        assertThat(x.getCustId()).isEqualTo(50L);
        assertThat(x.getAcctId()).isEqualTo(50L);
    }

    @Test
    void cardRecordFollowsCvact02yPositions() throws Exception {
        Card c = RecordLayouts.card(first("carddata.txt"));
        assertThat(c.getCardNum()).isEqualTo("0500024453765740");
        assertThat(c.getAcctId()).isEqualTo(50L);
        assertThat(c.getCvvCd()).isEqualTo(747);
        assertThat(c.getEmbossedName().stripTrailing()).isEqualTo("Aniya Von");
        assertThat(c.getExpirationDate()).isEqualTo("2023-03-09");
        assertThat(c.getActiveStatus()).isEqualTo("Y");
    }

    @Test
    void customerRecordFollowsCvcus01yPositions() throws Exception {
        Customer c = RecordLayouts.customer(first("custdata.txt"));
        assertThat(c.getCustId()).isEqualTo(1L);
        assertThat(c.getFirstName().stripTrailing()).isEqualTo("Immanuel");
        assertThat(c.getLastName().stripTrailing()).isEqualTo("Kessler");
        assertThat(c.getAddrStateCd()).isEqualTo("NC");
        assertThat(c.getAddrCountryCd()).isEqualTo("USA");
        assertThat(c.getSsn()).isEqualTo(20973888L);
        assertThat(c.getDob()).isEqualTo("1961-06-08");
        assertThat(c.getFicoCreditScore()).isEqualTo(274);
    }

    @Test
    void transactionCategoryBalanceRecordFollowsCvtra01yPositions() throws Exception {
        TransactionCategoryBalance b = RecordLayouts.transactionCategoryBalance(first("tcatbal.txt"));
        assertThat(b.getId().getAcctId()).isEqualTo(1L);
        assertThat(b.getId().getTypeCd()).isEqualTo("01");
        assertThat(b.getId().getCatCd()).isEqualTo(1);
        assertThat(b.getBalance()).isEqualByComparingTo("0");
    }

    @Test
    void transactionRecordImageIs350BytesAndRoundTripsThroughTheDalytranLayout() throws Exception {
        DailyTransaction d = RecordLayouts.dailyTransaction(first("dailytran.txt"));
        Transaction t = new Transaction();
        t.setTranId(d.id());
        t.setTypeCd(d.typeCd());
        t.setCatCd(d.catCd());
        t.setSource(d.source());
        t.setDesc(d.desc());
        t.setAmt(d.amt());
        t.setMerchantId(d.merchantId());
        t.setMerchantName(d.merchantName());
        t.setMerchantCity(d.merchantCity());
        t.setMerchantZip(d.merchantZip());
        t.setCardNum(d.cardNum());
        t.setOrigTs(d.origTs());
        t.setProcTs(d.procTs());
        String image = RecordLayouts.format(t);
        assertThat(image).hasSize(350);
        assertThat(RecordLayouts.dailyTransaction(image)).isEqualTo(d);
    }

    @Test
    void db2TimestampMatchesZGetDb2FormatTimestamp() {
        assertThat(Db2Timestamp.format(LocalDateTime.of(2026, 10, 27, 9, 5, 7, 987_654_321)))
                .isEqualTo("2026-10-27-09.05.07.980000")
                .hasSize(26);
    }

    @Test
    void rejectRecordIs350BytesOfInputPlusAnEightyByteValidationTrailer() {
        RejectRecord r = new RejectRecord("X".repeat(350), 102, "OVERLIMIT TRANSACTION");
        assertThat(r.format()).hasSize(RejectRecord.RECORD_LENGTH);
        assertThat(r.validationTrailer()).hasSize(80).startsWith("0102OVERLIMIT TRANSACTION");
    }
}
