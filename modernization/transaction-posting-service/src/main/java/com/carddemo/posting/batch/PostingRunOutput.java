package com.carddemo.posting.batch;

import com.carddemo.posting.service.PostingResult;

/**
 * Everything one posting run produced: the business outcome, plus the four dataset images the
 * job's output DDs would hold afterwards ({@code app/jcl/POSTTRAN.jcl:28-43}).
 *
 * <p>{@code TRANSACT} and {@code DALYREJS} are written from empty, in write order.
 * {@code ACCTFILE} and {@code TCATBALF} are updated in place and are returned in key order, as a
 * VSAM KSDS unload of each would produce.
 */
public record PostingRunOutput(PostingResult result,
                               byte[] transactionFileImage,
                               byte[] rejectFileImage,
                               byte[] accountFileImage,
                               byte[] categoryBalanceFileImage,
                               int categoryBalancesCreated) {
}
