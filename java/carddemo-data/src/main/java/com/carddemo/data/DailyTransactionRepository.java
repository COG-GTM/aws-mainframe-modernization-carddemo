package com.carddemo.data;

import org.springframework.data.jpa.repository.JpaRepository;

public interface DailyTransactionRepository extends JpaRepository<DailyTransaction, Integer> {
}
