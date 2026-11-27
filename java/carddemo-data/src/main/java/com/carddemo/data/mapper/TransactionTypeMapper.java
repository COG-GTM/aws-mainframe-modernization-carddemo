package com.carddemo.data.mapper;

import com.carddemo.data.entity.TransactionType;
import com.carddemo.data.record.TransactionTypeRecord;

import org.mapstruct.Mapper;

/** TransactionTypeRecord (copybook layout) <-> TransactionType (JPA entity). */
@Mapper(uses = CobolFormats.class)
public interface TransactionTypeMapper {

    TransactionType toEntity(TransactionTypeRecord record);

    TransactionTypeRecord toRecord(TransactionType entity);
}
