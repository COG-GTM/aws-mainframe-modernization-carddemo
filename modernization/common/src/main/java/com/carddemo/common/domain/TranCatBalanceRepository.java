package com.carddemo.common.domain;

import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TranCatBalanceRepository extends JpaRepository<TranCatBalance, TranCatBalanceId> {

    /** Sequential read of TCATBALF in key order (acct, type, category), as INTCALC does. */
    List<TranCatBalance> findAllByOrderByAcctIdAscTypeCdAscCatCdAsc();
}
