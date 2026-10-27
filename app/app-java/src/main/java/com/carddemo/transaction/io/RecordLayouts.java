package com.carddemo.transaction.io;

import com.carddemo.transaction.cobol.FieldCursor;
import com.carddemo.transaction.cobol.Pic;
import com.carddemo.transaction.cobol.ZonedDecimal;
import com.carddemo.transaction.domain.Account;
import com.carddemo.transaction.domain.Card;
import com.carddemo.transaction.domain.CardXref;
import com.carddemo.transaction.domain.Customer;
import com.carddemo.transaction.domain.DailyTransaction;
import com.carddemo.transaction.domain.Transaction;
import com.carddemo.transaction.domain.TransactionCategoryBalance;
import com.carddemo.transaction.domain.TransactionCategoryBalanceId;

/**
 * Fixed-length record layouts transcribed from the copybooks in {@code app/cpy}. Field order and sizes
 * follow the PIC clauses exactly; trailing FILLER is skipped.
 */
public final class RecordLayouts {

    public static final int DALYTRAN_RECLN = 350;   // CVTRA06Y
    public static final int TRAN_RECLN = 350;       // CVTRA05Y
    public static final int ACCOUNT_RECLN = 300;    // CVACT01Y
    public static final int CARD_RECLN = 150;       // CVACT02Y
    public static final int XREF_RECLN = 50;        // CVACT03Y
    public static final int CUSTOMER_RECLN = 500;   // CVCUS01Y
    public static final int TCATBAL_RECLN = 50;     // CVTRA01Y / FD-TRAN-CAT-BAL-RECORD

    private RecordLayouts() {
    }

    /** DALYTRAN-RECORD (CVTRA06Y). */
    public static DailyTransaction dailyTransaction(String line) {
        FieldCursor c = FieldCursor.over(line, DALYTRAN_RECLN);
        return new DailyTransaction(
                c.x(16),                 // DALYTRAN-ID
                c.x(2),                  // DALYTRAN-TYPE-CD
                (int) c.n9(4),           // DALYTRAN-CAT-CD
                c.x(10),                 // DALYTRAN-SOURCE
                c.x(100),                // DALYTRAN-DESC
                c.s9(9, 2),              // DALYTRAN-AMT
                c.n9(9),                 // DALYTRAN-MERCHANT-ID
                c.x(50),                 // DALYTRAN-MERCHANT-NAME
                c.x(50),                 // DALYTRAN-MERCHANT-CITY
                c.x(10),                 // DALYTRAN-MERCHANT-ZIP
                c.x(16),                 // DALYTRAN-CARD-NUM
                c.x(26),                 // DALYTRAN-ORIG-TS
                c.x(26),                 // DALYTRAN-PROC-TS
                c.record().raw());       // + FILLER X(20)
    }

    /** ACCOUNT-RECORD (CVACT01Y). */
    public static Account account(String line) {
        FieldCursor c = FieldCursor.over(line, ACCOUNT_RECLN);
        Account a = new Account();
        a.setAcctId(c.n9(11));
        a.setActiveStatus(c.x(1));
        a.setCurrBal(c.s9(10, 2));
        a.setCreditLimit(c.s9(10, 2));
        a.setCashCreditLimit(c.s9(10, 2));
        a.setOpenDate(c.x(10));
        a.setExpirationDate(c.x(10));
        a.setReissueDate(c.x(10));
        a.setCurrCycCredit(c.s9(10, 2));
        a.setCurrCycDebit(c.s9(10, 2));
        a.setAddrZip(c.x(10));
        a.setGroupId(c.x(10));
        return a;
    }

    /** CARD-RECORD (CVACT02Y). */
    public static Card card(String line) {
        FieldCursor c = FieldCursor.over(line, CARD_RECLN);
        Card card = new Card();
        card.setCardNum(c.x(16));
        card.setAcctId(c.n9(11));
        card.setCvvCd((int) c.n9(3));
        card.setEmbossedName(c.x(50));
        card.setExpirationDate(c.x(10));
        card.setActiveStatus(c.x(1));
        return card;
    }

    /** CARD-XREF-RECORD (CVACT03Y). */
    public static CardXref cardXref(String line) {
        FieldCursor c = FieldCursor.over(line, XREF_RECLN);
        CardXref x = new CardXref();
        x.setCardNum(c.x(16));
        x.setCustId(c.n9(9));
        x.setAcctId(c.n9(11));
        return x;
    }

    /** CUSTOMER-RECORD (CVCUS01Y). */
    public static Customer customer(String line) {
        FieldCursor c = FieldCursor.over(line, CUSTOMER_RECLN);
        Customer cu = new Customer();
        cu.setCustId(c.n9(9));
        cu.setFirstName(c.x(25));
        cu.setMiddleName(c.x(25));
        cu.setLastName(c.x(25));
        cu.setAddrLine1(c.x(50));
        cu.setAddrLine2(c.x(50));
        cu.setAddrLine3(c.x(50));
        cu.setAddrStateCd(c.x(2));
        cu.setAddrCountryCd(c.x(3));
        cu.setAddrZip(c.x(10));
        cu.setPhoneNum1(c.x(15));
        cu.setPhoneNum2(c.x(15));
        cu.setSsn(c.n9(9));
        cu.setGovtIssuedId(c.x(20));
        cu.setDob(c.x(10));
        cu.setEftAccountId(c.x(10));
        cu.setPriCardHolderInd(c.x(1));
        cu.setFicoCreditScore((int) c.n9(3));
        return cu;
    }

    /** TRAN-CAT-BAL-RECORD (CVTRA01Y). */
    public static TransactionCategoryBalance transactionCategoryBalance(String line) {
        FieldCursor c = FieldCursor.over(line, TCATBAL_RECLN);
        TransactionCategoryBalanceId id = new TransactionCategoryBalanceId(c.n9(11), c.x(2), (int) c.n9(4));
        TransactionCategoryBalance b = new TransactionCategoryBalance(id);
        b.setBalance(c.s9(9, 2));
        return b;
    }

    /** TRAN-RECORD image (CVTRA05Y), e.g. for a TRANFILE extract. */
    public static String format(Transaction t) {
        String rec = t.getTranId()
                + t.getTypeCd()
                + Pic.zeroFill(t.getCatCd(), 4)
                + t.getSource()
                + t.getDesc()
                + ZonedDecimal.encode(t.getAmt(), 9, 2)
                + Pic.zeroFill(t.getMerchantId(), 9)
                + t.getMerchantName()
                + t.getMerchantCity()
                + t.getMerchantZip()
                + t.getCardNum()
                + t.getOrigTs()
                + t.getProcTs();
        return Pic.x(rec, TRAN_RECLN);
    }
}
