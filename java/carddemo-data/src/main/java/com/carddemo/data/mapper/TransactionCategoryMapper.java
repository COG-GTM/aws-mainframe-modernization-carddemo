package com.carddemo.data.mapper;

import com.carddemo.data.entity.TransactionCategory;
import com.carddemo.data.record.TransactionCategoryRecord;

import org.mapstruct.Mapper;

/** TransactionCategoryRecord (copybook layout) <-> TransactionCategory (JPA entity). */
@Mapper(uses = CobolFormats.class)
public interface TransactionCategoryMapper {

    TransactionCategory toEntity(TransactionCategoryRecord record);

    TransactionCategoryRecord toRecord(TransactionCategory entity);
}
