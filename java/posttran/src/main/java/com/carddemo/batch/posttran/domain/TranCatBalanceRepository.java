package com.carddemo.batch.posttran.domain;

import org.springframework.data.jpa.repository.JpaRepository;

public interface TranCatBalanceRepository extends JpaRepository<TranCatBalance, TranCatBalanceKey> {
}
