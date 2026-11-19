package com.carddemo.reference.repository;

import com.carddemo.reference.domain.TranCatBalance;
import com.carddemo.reference.domain.TranCatBalanceId;
import org.springframework.data.jpa.repository.JpaRepository;

/** Table {@code reference.tran_cat_balances} (TCATBALF). */
public interface TranCatBalanceRepository extends JpaRepository<TranCatBalance, TranCatBalanceId> {
}
