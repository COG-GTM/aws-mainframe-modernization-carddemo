package com.carddemo.programs.copaua0c;

import com.carddemo.programs.copaua0c.copybook.AccountRecord;
import com.carddemo.programs.copaua0c.copybook.CardXrefRecord;
import com.carddemo.programs.copaua0c.copybook.CustomerRecord;
import com.carddemo.programs.copaua0c.copybook.MqTriggerMessage;
import com.carddemo.programs.copaua0c.gateway.CicsClock;
import com.carddemo.programs.copaua0c.gateway.inmemory.InMemoryImsGateway;
import com.carddemo.programs.copaua0c.gateway.inmemory.InMemoryKsdsFile;
import com.carddemo.programs.copaua0c.gateway.inmemory.InMemoryMqGateway;
import com.carddemo.programs.copaua0c.gateway.inmemory.InMemoryTdQueue;
import java.math.BigDecimal;
import java.util.Optional;

/** Wires COPAUA0C to in-memory gateways with a fixed clock. */
final class Harness {

    static final String CARD = "4111111111111111";
    static final long ACCT = 12345678901L;
    static final long CUST = 123456789L;
    static final String REPLY_Q = "CARD.AUTH.REPLY";
    static final String REQUEST_Q = "CARD.AUTH.REQUEST";
    /** 2026-11-26 10:15:30.250 -> YYDDD 26330, TIME 101530, MS 250. */
    static final CicsClock.FormattedTime NOW = new CicsClock.FormattedTime("261126", "26330", "101530", 250);

    final InMemoryMqGateway mq = new InMemoryMqGateway();
    final InMemoryKsdsFile<CardXrefRecord> xref = new InMemoryKsdsFile<>(CardXrefRecord::cardNum);
    final InMemoryKsdsFile<AccountRecord> acct = new InMemoryKsdsFile<>(a -> String.format("%011d", a.acctId()));
    final InMemoryKsdsFile<CustomerRecord> cust = new InMemoryKsdsFile<>(c -> String.format("%09d", c.custId()));
    final InMemoryImsGateway ims = new InMemoryImsGateway();
    final InMemoryTdQueue tdq = new InMemoryTdQueue();
    int syncpoints;

    Harness withStandardCard(String limit, String balance) {
        xref.put(new CardXrefRecord(CARD, CUST, ACCT));
        acct.put(account(ACCT, limit, balance, "500.00"));
        cust.put(customer(CUST));
        return this;
    }

    static AccountRecord account(long id, String limit, String balance, String cashLimit) {
        return new AccountRecord(id, "Y", new BigDecimal(balance), new BigDecimal(limit), new BigDecimal(cashLimit),
                "2020-01-01", "2030-01-01", "2030-01-01", BigDecimal.ZERO, BigDecimal.ZERO, "2000", "A");
    }

    static CustomerRecord customer(long id) {
        return new CustomerRecord(id, "TEST", "", "CUSTOMER", "1 TEST ST", "", "", "NS", "AUS", "2000",
                "", "", 0, "", "1990-01-01", "", "Y", 700);
    }

    static String request(String card, String amount, String txnId) {
        return "261126,101530," + card + ",AUTH,1228,0100,POS001,000000," + amount
                + ",5411,AUS,05,MERCHANT0000001,CORNER STORE,SYDNEY,NS,2000," + txnId;
    }

    Harness send(String card, String amount, String txnId) {
        mq.enqueue(request(card, amount, txnId), "CORREL" + txnId, REPLY_Q);
        return this;
    }

    Copaua0cService service() {
        return new Copaua0cService(mq, xref, acct, cust, ims, tdq, () -> NOW, () -> {
            syncpoints++;
            ims.release();
        });
    }

    Copaua0cRunResult run() {
        return service().run(Optional.of(MqTriggerMessage.forQueue(REQUEST_Q, "TRIGGER")));
    }

    String replyFields(int index) {
        return mq.replies().get(index).payload();
    }
}
