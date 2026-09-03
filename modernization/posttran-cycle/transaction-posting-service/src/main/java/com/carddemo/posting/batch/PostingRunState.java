package com.carddemo.posting.batch;

import com.carddemo.posting.PostingProperties;
import com.carddemo.posting.domain.PostingLedger;
import com.carddemo.posting.domain.PostingOutcome;
import com.carddemo.posting.domain.RejectRecordLayout;
import com.carddemo.recordio.codec.FixedWidthRecord;
import com.carddemo.recordio.codec.RecordEncoding;
import com.carddemo.recordio.layout.Account;
import com.carddemo.recordio.layout.AccountLayout;
import com.carddemo.recordio.layout.CardXref;
import com.carddemo.recordio.layout.CardXrefLayout;
import com.carddemo.recordio.layout.Transaction;
import com.carddemo.recordio.layout.TransactionCategoryBalance;
import com.carddemo.recordio.layout.TransactionCategoryBalanceLayout;
import com.carddemo.recordio.layout.TransactionLayout;
import com.carddemo.recordio.store.FixedWidthFile;
import com.carddemo.recordio.store.KeyedRecordStore;

import java.util.ArrayList;
import java.util.List;

/**
 * Everything one posting run holds open: the three masters, the (fresh) transaction master and the
 * reject file. Mirrors the OPEN ... CLOSE bracket of CBTRN02C: masters are loaded when the step
 * starts and flushed when it ends.
 *
 * <p>TRANSACT is {@code OPEN OUTPUT} in the COBOL (line 256), so the run starts from an empty
 * transaction master and writes only today's posted transactions; it does not merge into an
 * existing one. That is COMBTRAN's job, outside this flow.
 */
public class PostingRunState {

    private final PostingProperties properties;
    private final RecordEncoding encoding;
    private final PostingLedger ledger;
    private final KeyedRecordStore<Transaction> transactionMaster;
    private final List<FixedWidthRecord> rejects = new ArrayList<>();
    private long transactionCount;
    private long rejectCount;

    public PostingRunState(PostingProperties properties) {
        this.properties = properties;
        this.encoding = RecordEncoding.of(properties.encoding());
        KeyedRecordStore<CardXref> xref = KeyedRecordStore.load("XREFFILE", properties.cardXref(),
                CardXrefLayout.INSTANCE, encoding, CardXref::cardNumber);
        KeyedRecordStore<Account> accounts = KeyedRecordStore.load("ACCTFILE", properties.accountMaster(),
                AccountLayout.INSTANCE, encoding, Account::accountId);
        KeyedRecordStore<TransactionCategoryBalance> balances = KeyedRecordStore.load("TCATBALF",
                properties.categoryBalances(), TransactionCategoryBalanceLayout.INSTANCE, encoding,
                TransactionCategoryBalance::key);
        this.ledger = new PostingLedger(xref, accounts, balances);
        this.transactionMaster = new KeyedRecordStore<>("TRANFILE", TransactionLayout.INSTANCE, encoding,
                Transaction::id);
    }

    public RecordEncoding encoding() {
        return encoding;
    }

    public PostingLedger ledger() {
        return ledger;
    }

    public void record(PostingOutcome outcome) {
        transactionCount++;
        switch (outcome) {
            case PostingOutcome.Posted posted -> transactionMaster.write(posted.transaction());
            case PostingOutcome.Rejected rejected -> {
                rejectCount++;
                rejects.add(RejectRecordLayout.encode(rejected, encoding));
            }
        }
    }

    /** The CLOSE paragraphs: persist the rewritten masters and the two output datasets. */
    public void flush() {
        ledger.accounts().save(properties.accountMaster());
        ledger.categoryBalances().save(properties.categoryBalances());
        transactionMaster.save(properties.transactionMaster());
        FixedWidthFile.write(properties.rejects(), rejects);
    }

    public long transactionCount() {
        return transactionCount;
    }

    public long rejectCount() {
        return rejectCount;
    }
}
