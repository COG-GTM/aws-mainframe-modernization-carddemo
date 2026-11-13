package com.carddemo.data.mapper;

import com.carddemo.data.entity.DailyTransaction;
import com.carddemo.data.record.DailyTransactionRecord;

import org.mapstruct.Mapper;

/** DailyTransactionRecord (copybook layout) <-> DailyTransaction (JPA entity). */
@Mapper(uses = CobolFormats.class)
public interface DailyTransactionMapper {

    DailyTransaction toEntity(DailyTransactionRecord record);

    DailyTransactionRecord toRecord(DailyTransaction entity);
}
