package com.carddemo.transaction;

import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

/** TRANTYPE access paths: {@code READ} by TRAN-TYPE (CBTRN03C) and the sequential read in key order. */
public interface TransactionTypeRepository extends JpaRepository<TransactionType, String> {

    List<TransactionType> findAllByOrderByTranTypeCdAsc();
}
