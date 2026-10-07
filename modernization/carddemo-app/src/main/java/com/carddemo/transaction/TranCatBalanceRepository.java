package com.carddemo.transaction;

import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

/**
 * TCATBALF access paths: {@code READ}/{@code WRITE}/{@code REWRITE} by TRAN-CAT-KEY (CBTRN02C
 * {@code 2700-UPDATE-TCATBAL}) and the sequential read in key order that drives interest calculation (CBACT04C).
 */
public interface TranCatBalanceRepository extends JpaRepository<TranCatBalance, TranCatBalanceId> {

    @Query("select b from TranCatBalance b order by b.id.acctId, b.id.tranTypeCd, b.id.tranCatCd")
    List<TranCatBalance> findAllInKeyOrder();
}
