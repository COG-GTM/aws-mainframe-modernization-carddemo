package com.carddemo.transaction.repository;

import com.carddemo.transaction.domain.CategoryBalance;
import com.carddemo.transaction.domain.CategoryBalanceId;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CategoryBalanceRepository extends JpaRepository<CategoryBalance, CategoryBalanceId> {

    /** TCATBAL is browsed in account order by CBACT04C. */
    List<CategoryBalance> findByIdAcctIdOrderByIdTypeCdAscIdCatCdAsc(Long acctId);

    List<CategoryBalance> findAllByOrderByIdAcctIdAscIdTypeCdAscIdCatCdAsc();
}
