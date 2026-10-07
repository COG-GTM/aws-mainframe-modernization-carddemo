package com.carddemo.transaction;

import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

/** TRANCATG access paths: {@code READ} by TRAN-CAT-KEY (CBTRN03C) and the sequential read in key order. */
public interface TransactionCategoryRepository extends JpaRepository<TransactionCategory, TransactionCategoryId> {

    @Query("select c from TransactionCategory c order by c.id.tranTypeCd, c.id.tranCatCd")
    List<TransactionCategory> findAllInKeyOrder();
}
