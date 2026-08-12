package com.carddemo.interest.rules;

import com.carddemo.interest.domain.Account;
import com.carddemo.interest.domain.CardXref;
import com.carddemo.interest.domain.InterestTransaction;

import java.math.BigDecimal;

/** Builds the interest transaction that documents one category's monthly interest. */
public final class InterestTransactionFactory {

    /** {@code STRING 'Int. for a/c ' ...} ({@code app/cbl/CBACT04C.cbl:485-489}). */
    private static final String DESCRIPTION_PREFIX = "Int. for a/c ";

    private final TransactionIdSequence idSequence;
    private final Db2TimestampFormatter timestamps;

    public InterestTransactionFactory(TransactionIdSequence idSequence, Db2TimestampFormatter timestamps) {
        this.idSequence = idSequence;
        this.timestamps = timestamps;
    }

    /**
     * Business rule BR-7 — creation of the interest transaction record.
     *
     * <p>COBOL paragraph {@code 1300-B-WRITE-TX} ({@code app/cbl/CBACT04C.cbl:473-500}):
     * type code {@code '01'} and category {@code '05'} identify the row as system-generated
     * interest, the source is {@code 'System'}, the description is
     * {@code 'Int. for a/c ' + ACCT-ID}, the amount is the month's interest for the category, all
     * merchant fields are cleared, the card number comes from the cross-reference record and both
     * timestamps carry the same Db2-formatted current time.
     */
    public InterestTransaction create(Account account, CardXref cardXref, BigDecimal monthlyInterest) {
        String timestamp = timestamps.now();
        return new InterestTransaction(
                idSequence.next(),
                InterestTransaction.INTEREST_TYPE_CODE,
                InterestTransaction.INTEREST_CATEGORY_CODE,
                InterestTransaction.SYSTEM_SOURCE,
                DESCRIPTION_PREFIX + account.id().value(),
                monthlyInterest,
                "0",
                "",
                "",
                "",
                cardXref.cardNumber(),
                timestamp,
                timestamp);
    }
}
