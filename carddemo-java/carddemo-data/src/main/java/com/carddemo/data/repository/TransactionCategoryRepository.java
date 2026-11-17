package com.carddemo.data.repository;

import com.carddemo.data.entity.TransactionCategory;
import com.carddemo.data.entity.TransactionCategoryId;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

/** TRANCATG: keyed READ by TRAN-CAT-KEY (type, category). */
public interface TransactionCategoryRepository extends JpaRepository<TransactionCategory, TransactionCategoryId> {

    List<TransactionCategory> findByIdTypeCdOrderByIdCatCd(String typeCd);
}
