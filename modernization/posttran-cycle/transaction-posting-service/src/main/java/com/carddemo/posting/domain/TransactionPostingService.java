package com.carddemo.posting.domain;

import com.carddemo.recordio.layout.Account;
import com.carddemo.recordio.layout.CardXref;
import com.carddemo.recordio.layout.Transaction;

import java.time.Clock;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Optional;

/**
 * Posts one daily transaction: validate, then update category balance and account, then stamp the
 * processing timestamp (CBTRN02C main loop lines 201-216 and 2000-POST-TRANSACTION lines 424-442).
 *
 * <p>The COBOL order inside 2000-POST-TRANSACTION is: stamp timestamp, update TCATBAL, update
 * account, write TRANSACT. Any failure in those file operations abends the whole job (no partial
 * record is rolled back), which is why the writer treats a store failure as fatal.
 */
public final class TransactionPostingService {

    /** DB2 timestamp form built by Z-GET-DB2-FORMAT-TIMESTAMP: yyyy-MM-dd-HH.mm.ss.SS0000. */
    private static final DateTimeFormatter DB2_TS = DateTimeFormatter.ofPattern("yyyy-MM-dd-HH.mm.ss.SS'0000'");

    private final PostingLedger ledger;
    private final Clock clock;

    public TransactionPostingService(PostingLedger ledger, Clock clock) {
        this.ledger = ledger;
        this.clock = clock;
    }

    public PostingOutcome process(DailyTransaction item) {
        Transaction daily = item.transaction();
        Optional<CardXref> card = ledger.findCard(daily.cardNumber());
        Optional<RejectReason> reason = TransactionValidator.validate(daily, card, ledger::findAccount);
        if (reason.isPresent()) {
            return new PostingOutcome.Rejected(daily, item.image(), reason.get());
        }
        String accountId = card.orElseThrow().accountId();
        Transaction posted = daily.withProcessingTimestamp(LocalDateTime.now(clock).format(DB2_TS));
        ledger.addToCategoryBalance(accountId, posted);
        Account account = ledger.findAccount(accountId).orElseThrow();
        ledger.applyToAccount(account, posted);
        return new PostingOutcome.Posted(posted);
    }
}
