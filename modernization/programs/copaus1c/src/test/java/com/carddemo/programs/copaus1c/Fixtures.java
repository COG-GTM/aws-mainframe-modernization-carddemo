package com.carddemo.programs.copaus1c;

import com.carddemo.programs.copaus1c.copybook.AidKey;
import com.carddemo.programs.copaus1c.copybook.AuthorizationKey;
import com.carddemo.programs.copaus1c.copybook.CardDemoCommarea;
import com.carddemo.programs.copaus1c.copybook.PendingAuthDetails;
import com.carddemo.programs.copaus1c.copybook.PendingAuthSummary;
import com.carddemo.programs.copaus1c.inmemory.InMemoryPendingAuthDb;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.ZoneOffset;

final class Fixtures {
    static final long ACCT = 1L;
    static final long CUST = 1L;
    static final AuthorizationKey KEY1 = AuthorizationKey.of(76699, 998747444L);
    static final AuthorizationKey KEY2 = AuthorizationKey.of(76699, 998748388L);
    static final AuthorizationKey KEY3 = AuthorizationKey.of(76700, 769089115L);
    static final Clock CLOCK = Clock.fixed(LocalDateTime.of(2026, 3, 4, 5, 6, 7).toInstant(ZoneOffset.UTC),
            ZoneOffset.UTC);

    private Fixtures() {
    }

    static PendingAuthDetails detail(AuthorizationKey key, String origDate, String origTime, String resp,
                                     String reason, String amt) {
        PendingAuthDetails d = PendingAuthDetails.blank();
        d.setAuthorizationKey(key);
        d.setAuthOrigDate(origDate);
        d.setAuthOrigTime(origTime);
        d.setCardNum("4111222233334444");
        d.setAuthType("MAN ");
        d.setCardExpiryDate("0927");
        d.setMessageType("0100  ");
        d.setMessageSource("POS   ");
        d.setAuthIdCode("ABC123");
        d.setAuthRespCode(resp);
        d.setAuthRespReason(reason);
        d.setProcessingCode(3000);
        d.setTransactionAmt(new BigDecimal(amt));
        d.setApprovedAmt(new BigDecimal(amt));
        d.setMerchantCategoryCode("5411");
        d.setAcqrCountryCode("USA");
        d.setPosEntryMode(5);
        d.setMerchantId("MERCH0000000001");
        d.setMerchantName("CORNER GROCERY");
        d.setMerchantCity("SPRINGFIELD");
        d.setMerchantState("IL");
        d.setMerchantZip("62701");
        d.setTransactionId("TXN000000000001");
        d.setMatchStatus('P');
        return d;
    }

    static InMemoryPendingAuthDb db() {
        InMemoryPendingAuthDb db = new InMemoryPendingAuthDb();
        db.addSummary(PendingAuthSummary.of(ACCT, CUST));
        db.addDetail(ACCT, detail(KEY1, "231027", "041252", "00", "0000", "1.24"));
        db.addDetail(ACCT, detail(KEY2, "231027", "041251", "05", "4100", "-12345.67"));
        db.addDetail(ACCT, detail(KEY3, "231027", "030910", "05", "7777", "123456789.99"));
        return db;
    }

    static Copaus1cCommarea commarea(AuthorizationKey selected, boolean reenter) {
        Copaus1cCommarea c = new Copaus1cCommarea();
        c.initialize();
        c.setFromTranid("CPVS");
        c.setFromProgram("COPAUS0C");
        c.setUserId("USER0001");
        c.setUserType('U');
        c.setCustId(CUST);
        c.setAcctId(ACCT);
        c.setPgmContext(reenter ? CardDemoCommarea.PgmContext.CDEMO_PGM_REENTER
                : CardDemoCommarea.PgmContext.CDEMO_PGM_ENTER);
        c.cpvd().setPauSelected(selected);
        c.cpvd().setPauSelFlg('S');
        return c;
    }

    static AuthViewRequest req(AuthorizationKey selected, AidKey aid) {
        return new AuthViewRequest(commarea(selected, true), aid);
    }
}
