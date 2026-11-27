package com.carddemo.data.mapper;

import com.carddemo.data.entity.Transaction;
import com.carddemo.data.record.TransactionRecord;

import org.mapstruct.Mapper;

/** TransactionRecord (copybook layout) <-> Transaction (JPA entity). */
@Mapper(uses = CobolFormats.class)
public interface TransactionMapper {

    Transaction toEntity(TransactionRecord record);

    TransactionRecord toRecord(Transaction entity);
}
