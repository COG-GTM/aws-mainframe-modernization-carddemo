package com.carddemo.posting.rules;

import com.carddemo.posting.domain.Account;
import com.carddemo.posting.domain.CardXref;
import com.carddemo.posting.domain.RejectReason;

/**
 * The result of validating one daily transaction: either the account it posts to, or the reason
 * it cannot be posted ({@code app/cbl/CBTRN02C.cbl:370-422}).
 */
public sealed interface ValidationOutcome {

    /** The transaction may be posted to {@code account}, reached through {@code xref}. */
    record Accepted(CardXref xref, Account account) implements ValidationOutcome { }

    /** The transaction must go to the reject file. */
    record Rejected(RejectReason reason) implements ValidationOutcome { }
}
