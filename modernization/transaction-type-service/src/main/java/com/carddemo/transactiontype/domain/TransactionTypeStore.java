package com.carddemo.transactiontype.domain;

import java.util.List;
import java.util.Optional;

/**
 * Outbound port over CARDDEMO.TRANSACTION_TYPE / TRANSACTION_TYPE_CATEGORY.
 * Mutations throw {@link SqlCodeException} with the Db2 SQLCODE on failure.
 */
public interface TransactionTypeStore {

    Optional<TransactionType> findByCode(String code);

    /** C-TR-TYPE-FORWARD: TR_TYPE >= startKey, filters applied, ORDER BY TR_TYPE. */
    List<TransactionType> findForward(String startKey, ListFilter filter, int limit);

    /** C-TR-TYPE-BACKWARD: TR_TYPE < beforeKey, filters applied, ORDER BY TR_TYPE DESC. */
    List<TransactionType> findBackward(String beforeKey, ListFilter filter, int limit);

    /** 9100-CHECK-FILTERS: SELECT COUNT(1) with the same filters. */
    long count(ListFilter filter);

    void insert(TransactionType transactionType);

    /** @return rows updated (0 == SQLCODE +100) */
    int updateDescription(String code, String description);

    /** @return rows deleted (0 == SQLCODE +100) */
    int delete(String code);

    List<TransactionTypeCategory> findCategories(String typeCode);

    /** TRANEXTR STEP40: 60-byte TRANTYPE.PS records. */
    List<String> extractTransactionTypes();

    /** TRANEXTR STEP50: 60-byte TRANCATG.PS records. */
    List<String> extractCategories();
}
