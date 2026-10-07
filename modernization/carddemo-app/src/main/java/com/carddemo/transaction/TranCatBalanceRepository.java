package com.carddemo.transaction;

import java.util.List;
import org.springframework.data.domain.Limit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

/**
 * TCATBALF access paths: {@code READ}/{@code WRITE}/{@code REWRITE} by TRAN-CAT-KEY (CBTRN02C
 * {@code 2700-UPDATE-TCATBAL}) and the sequential read in key order that drives interest calculation (CBACT04C).
 */
public interface TranCatBalanceRepository extends JpaRepository<TranCatBalance, TranCatBalanceId> {

    @Query("select b from TranCatBalance b order by b.id.acctId, b.id.tranTypeCd, b.id.tranCatCd")
    List<TranCatBalance> findAllInKeyOrder();

    /** Keyset browse in TRAN-CAT-KEY order after {@code (acctId, tranTypeCd, tranCatCd)} (CBACT04C {@code READ NEXT}). */
    @Query("select b from TranCatBalance b where b.id.acctId > :acctId"
            + " or (b.id.acctId = :acctId and b.id.tranTypeCd > :tranTypeCd)"
            + " or (b.id.acctId = :acctId and b.id.tranTypeCd = :tranTypeCd and b.id.tranCatCd > :tranCatCd)"
            + " order by b.id.acctId, b.id.tranTypeCd, b.id.tranCatCd")
    List<TranCatBalance> findAfter(@Param("acctId") long acctId, @Param("tranTypeCd") String tranTypeCd,
                                   @Param("tranCatCd") int tranCatCd, Limit limit);

    default List<TranCatBalance> findAfter(TranCatBalanceId last, Limit limit) {
        return findAfter(last.acctId(), last.tranTypeCd(), last.tranCatCd(), limit);
    }
}
