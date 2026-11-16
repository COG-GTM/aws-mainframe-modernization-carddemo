package com.carddemo.data.repository;

import com.carddemo.data.entity.TransactionCategoryBalance;
import com.carddemo.data.entity.TransactionCategoryBalanceId;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

/** TCATBALF keyed by TRAN-CAT-KEY (account, type, category). */
public interface TransactionCategoryBalanceRepository
        extends JpaRepository<TransactionCategoryBalance, TransactionCategoryBalanceId> {

    /** Sequential READ in key order, as INTCALC (CBACT04C) walks the file. */
    List<TransactionCategoryBalance> findAllByOrderByIdAcctIdAscIdTypeCdAscIdCatCdAsc();

    /** Every category balance of one account, in key order. */
    List<TransactionCategoryBalance> findByIdAcctIdOrderByIdTypeCdAscIdCatCdAsc(Long acctId);
}
