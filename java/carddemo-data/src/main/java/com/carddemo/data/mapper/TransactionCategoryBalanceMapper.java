package com.carddemo.data.mapper;

import com.carddemo.data.entity.TransactionCategoryBalance;
import com.carddemo.data.record.TranCatBalanceRecord;

import org.mapstruct.Mapper;

/** TranCatBalanceRecord (copybook layout) <-> TransactionCategoryBalance (JPA entity). */
@Mapper(uses = CobolFormats.class)
public interface TransactionCategoryBalanceMapper {

    TransactionCategoryBalance toEntity(TranCatBalanceRecord record);

    TranCatBalanceRecord toRecord(TransactionCategoryBalance entity);
}
