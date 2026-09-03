package com.carddemo.posting.repository;

import com.carddemo.posting.domain.TransactionCategoryBalance;
import com.carddemo.posting.domain.TransactionCategoryKey;

import java.util.Optional;

/**
 * The transaction-category-balance file ({@code TCATBALF} DD,
 * {@code AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS}, {@code app/jcl/POSTTRAN.jcl:42-43}), opened
 * {@code I-O} so that missing buckets can be created as the run discovers them.
 *
 * <p>Replaces {@code 2700-UPDATE-TCATBAL} and its two branches
 * ({@code app/cbl/CBTRN02C.cbl:467-542}).
 */
public interface TransactionCategoryBalanceRepository {

    Optional<TransactionCategoryBalance> findByKey(TransactionCategoryKey key);

    /** Writes a new bucket or rewrites an existing one, as {@code 2700-A}/{@code 2700-B} do. */
    void save(TransactionCategoryBalance balance);
}
