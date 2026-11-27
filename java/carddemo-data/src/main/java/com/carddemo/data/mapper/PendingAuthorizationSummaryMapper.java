package com.carddemo.data.mapper;

import com.carddemo.data.entity.PendingAuthorizationSummary;
import com.carddemo.data.record.PendingAuthSummaryRecord;

import org.mapstruct.Mapper;

/** PendingAuthSummaryRecord (copybook layout) <-> PendingAuthorizationSummary (JPA entity). */
@Mapper(uses = CobolFormats.class)
public interface PendingAuthorizationSummaryMapper {

    PendingAuthorizationSummary toEntity(PendingAuthSummaryRecord record);

    PendingAuthSummaryRecord toRecord(PendingAuthorizationSummary entity);
}
