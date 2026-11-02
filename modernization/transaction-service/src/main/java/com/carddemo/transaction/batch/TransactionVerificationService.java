package com.carddemo.transaction.batch;

import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.layout.DalytranRecordLayout;
import com.carddemo.transaction.client.AccountGateway;
import com.carddemo.transaction.client.CardGateway;
import com.carddemo.transaction.client.CardXrefView;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import org.springframework.stereotype.Service;

/**
 * Port of CBTRN01C, the validate-and-print variant of CBTRN02C: every DALYTRAN record is displayed
 * and its card and account are looked up, nothing is written. The result is the program's SYSOUT,
 * line for line.
 *
 * <p>The lookups of MAIN-PARA sit outside its {@code IF END-OF-DAILY-TRANS-FILE = 'N'}, so after the
 * read that hits end of file the last record (still in {@code DALYTRAN-RECORD}) is looked up once
 * more; with an empty file the blank initial record is. Both are kept.
 */
@Service
public class TransactionVerificationService {

    private final CardGateway cards;
    private final AccountGateway accounts;

    public TransactionVerificationService(CardGateway cards, AccountGateway accounts) {
        this.cards = cards;
        this.accounts = accounts;
    }

    public List<String> verify(List<CobolRecord> dalytran) {
        List<String> sysout = new ArrayList<>();
        sysout.add("START OF EXECUTION OF PROGRAM CBTRN01C");
        String cardNum = " ".repeat(DalytranRecordLayout.DALYTRAN_CARD_NUM.length());
        String tranId = " ".repeat(DalytranRecordLayout.DALYTRAN_ID.length());
        for (CobolRecord record : dalytran) {
            byte[] bytes = record.toBytes();
            sysout.add(record.codePage().decodeText(bytes, 0, bytes.length));
            cardNum = record.get(DalytranRecordLayout.DALYTRAN_CARD_NUM);
            tranId = record.get(DalytranRecordLayout.DALYTRAN_ID);
            lookUp(cardNum, tranId, sysout);
        }
        lookUp(cardNum, tranId, sysout);
        sysout.add("END OF EXECUTION OF PROGRAM CBTRN01C");
        return sysout;
    }

    private void lookUp(String cardNum, String tranId, List<String> sysout) {
        // 2000-LOOKUP-XREF
        Optional<CardXrefView> xref = cardNum.isBlank() ? Optional.empty() : cards.xrefByCard(cardNum.trim());
        if (xref.isEmpty()) {
            sysout.add("INVALID CARD NUMBER FOR XREF");
            sysout.add("CARD NUMBER " + cardNum + " COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-" + tranId);
            return;
        }
        sysout.add("SUCCESSFUL READ OF XREF");
        sysout.add("CARD NUMBER: " + xref.get().cardNumber());
        sysout.add("ACCOUNT ID : " + "%011d".formatted(xref.get().accountId()));
        sysout.add("CUSTOMER ID: " + "%09d".formatted(xref.get().customerId()));
        // 3000-READ-ACCOUNT
        if (accounts.find(xref.get().accountId()).isPresent()) {
            sysout.add("SUCCESSFUL READ OF ACCOUNT FILE");
        } else {
            sysout.add("INVALID ACCOUNT NUMBER FOUND");
            sysout.add("ACCOUNT " + "%011d".formatted(xref.get().accountId()) + " NOT FOUND");
        }
    }
}
