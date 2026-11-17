package com.carddemo.data.repository;

import com.carddemo.data.entity.Account;
import com.carddemo.data.entity.Card;
import com.carddemo.data.entity.CardXref;
import com.carddemo.data.entity.DailyTransaction;
import com.carddemo.data.entity.Transaction;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

/** Record layouts as in {@code app/data/ASCII}, with obviously fake personal data. */
final class Fixtures {

    private Fixtures() {
    }

    static Account account(long acctId) {
        Account a = new Account();
        a.setAcctId(acctId);
        a.setActiveStatus("Y");
        a.setCurrBal(new BigDecimal("194.00"));
        a.setCreditLimit(new BigDecimal("2020.00"));
        a.setCashCreditLimit(new BigDecimal("1020.00"));
        a.setOpenDate(LocalDate.of(2014, 11, 20));
        a.setExpirationDate(LocalDate.of(2025, 5, 20));
        a.setReissueDate(LocalDate.of(2025, 5, 20));
        a.setCurrCycCredit(BigDecimal.ZERO.setScale(2));
        a.setCurrCycDebit(BigDecimal.ZERO.setScale(2));
        a.setAddrZip("A000000000");
        a.setGroupId("DEFAULT");
        return a;
    }

    static Card card(String cardNum, long acctId) {
        Card c = new Card();
        c.setCardNum(cardNum);
        c.setAcctId(acctId);
        c.setCvvCd((short) 747);
        c.setEmbossedName("TEST CARDHOLDER");
        c.setExpirationDate(LocalDate.of(2023, 3, 9));
        c.setActiveStatus("Y");
        return c;
    }

    static CardXref xref(String cardNum, int custId, long acctId) {
        CardXref x = new CardXref();
        x.setCardNum(cardNum);
        x.setCustId(custId);
        x.setAcctId(acctId);
        return x;
    }

    static Transaction transaction(String tranId, LocalDateTime procTs) {
        Transaction t = new Transaction();
        t.setTranId(tranId);
        t.setTypeCd("01");
        t.setCatCd((short) 1);
        t.setSource("POS TERM");
        t.setDescription("Purchase at TEST MERCHANT");
        t.setAmt(new BigDecimal("50.47"));
        t.setMerchantId(800000000);
        t.setMerchantName("TEST MERCHANT");
        t.setMerchantCity("TESTVILLE");
        t.setMerchantZip("00000");
        t.setCardNum("4000000000000999");
        t.setOrigTs(LocalDateTime.of(2022, 6, 10, 19, 27, 53));
        t.setProcTs(procTs);
        return t;
    }

    static DailyTransaction dailyTransaction(String tranId, BigDecimal amt) {
        DailyTransaction t = new DailyTransaction();
        t.setTranId(tranId);
        t.setTypeCd("01");
        t.setCatCd((short) 1);
        t.setSource("POS TERM");
        t.setDescription("Purchase at TEST MERCHANT");
        t.setAmt(amt);
        t.setMerchantId(800000000);
        t.setMerchantName("TEST MERCHANT");
        t.setMerchantCity("TESTVILLE");
        t.setMerchantZip("00000");
        t.setCardNum("4000000000000999");
        t.setOrigTs(LocalDateTime.of(2022, 6, 10, 19, 27, 53));
        return t;
    }
}
